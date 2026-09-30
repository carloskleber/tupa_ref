//! Study orchestration (`mStudy`): assembly, frequency-independent geometry
//! (computed once), per-frequency impedance fill, solve (current and ideal
//! voltage sources, ADR 0010/0016), frequency sweep and convenience queries.

use crate::ctes::{MU0, PI, ZERO};
use crate::electrode::Electrode;
use crate::error::{Result, TupaError};
use crate::geometry::{GeometryMatrices, GeometryOptions, build_geometry_matrices};
use crate::impedance::internal_impedance;
use crate::linalg::{CMatrix, solve_in_place};
use crate::mesh::{AIR, Mesh, SOIL};
use crate::result::ResultSet;
use crate::structure::Structure;
use crate::verbosity::{VERB_NORMAL, VERB_VERBOSE, verbose, verbosity_level};
use num_complex::Complex64;

/// Frequency-independent data prepared once per study.
#[derive(Debug, Clone)]
pub struct Prepared {
    /// Geometry matrices (factors, distances, cosines)
    pub geom: GeometryMatrices,
    /// Segment lengths (m)
    pub length: Vec<f64>,
    /// Segment radii (m)
    pub radius: Vec<f64>,
    /// Segment medium code: 1 = air, 2 = soil
    pub pos: Vec<u8>,
    /// Mesh with topology matrices
    pub mesh: Mesh,
}

/// A source injected at a named node.
#[derive(Debug, Clone, PartialEq)]
pub struct Source {
    /// Node id
    pub node: String,
    /// Current (A) or voltage (V) value
    pub value: Complex64,
    /// Ideal voltage source (ADR 0016) instead of a current injection
    pub is_voltage: bool,
}

/// One study: structure, numerical options, optional preparation and sweep
/// results.
#[derive(Debug, Clone)]
pub struct Study {
    /// Free-text title
    pub title: String,
    /// Geometry and media
    pub structure: Structure,
    /// Quadrature/cache options
    pub options: GeometryOptions,
    /// Filled by `prepare`
    pub prepared: Option<Prepared>,
    /// Node voltages per frequency
    pub voltage_results: ResultSet,
    /// Electrode end currents `i1` ("longitudinal" store)
    pub long_current_results: ResultSet,
    /// Electrode end currents `i2` ("transversal" store)
    pub trans_current_results: ResultSet,
    /// Last sweep frequency axis (Hz)
    pub sweep_freq_hz: Vec<f64>,
    /// Last sweep source node ids
    pub sweep_source_ids: Vec<String>,
    /// Effective source currents per source and frequency
    pub sweep_source_currents_freq: Vec<Vec<Complex64>>,
}

/// Solution of one frequency point with the effective injected currents.
#[derive(Debug, Clone, PartialEq)]
pub struct RunOutput {
    /// Node voltages (V)
    pub voltage: Vec<Complex64>,
    /// `i1` per electrode (A)
    pub current1: Vec<Complex64>,
    /// `i2` per electrode (A)
    pub current2: Vec<Complex64>,
    /// Effective injected current per source (A)
    pub source_currents: Vec<Complex64>,
}

impl Study {
    /// New study over an existing structure.
    pub fn new(title: impl Into<String>, structure: Structure) -> Self {
        Self {
            title: title.into(),
            structure,
            options: GeometryOptions::default(),
            prepared: None,
            voltage_results: ResultSet::default(),
            long_current_results: ResultSet::default(),
            trans_current_results: ResultSet::default(),
            sweep_freq_hz: Vec::new(),
            sweep_source_ids: Vec::new(),
            sweep_source_currents_freq: Vec::new(),
        }
    }

    /// One-time preparation: assembly + geometry matrices + topology.
    pub fn prepare(&mut self) -> Result<()> {
        if self.prepared.is_some() {
            return Ok(());
        }
        if verbosity_level() == VERB_VERBOSE {
            println!(" Assembling structure and computing geometry factors...");
        }
        self.structure.assemble()?;

        let nno = self.structure.nodes.len();
        let nseg = self.structure.electrodes.len();
        let mut n1 = Vec::with_capacity(nseg);
        let mut n2 = Vec::with_capacity(nseg);
        let mut p1 = Vec::with_capacity(nseg);
        let mut p2 = Vec::with_capacity(nseg);
        let mut radius = Vec::with_capacity(nseg);
        let mut length = Vec::with_capacity(nseg);
        let mut pos = Vec::with_capacity(nseg);
        for e in &self.structure.electrodes {
            let a = e.node_indices[0];
            let b = e.node_indices[1];
            n1.push(a);
            n2.push(b);
            let pa = self.structure.nodes[a].p;
            let pb = self.structure.nodes[b].p;
            p1.push(pa);
            p2.push(pb);
            radius.push(e.radius);
            let d = [pb[0] - pa[0], pb[1] - pa[1], pb[2] - pa[2]];
            length.push((d[0] * d[0] + d[1] * d[1] + d[2] * d[2]).sqrt());
            pos.push(if 0.5 * (pa[2] + pb[2]) > 0.0 {
                AIR
            } else {
                SOIL
            });
        }

        let geom = build_geometry_matrices(&p1, &p2, &radius, Some(&pos), &self.options);
        if verbosity_level() == VERB_VERBOSE {
            println!(
                " Geometry-factor quadrature cache: {} hits, {} misses ({} entries)",
                geom.cache_stats.hits, geom.cache_stats.misses, geom.cache_stats.entries
            );
        }
        let mesh = Mesh::new(nno, nseg, &n1, &n2);
        self.prepared = Some(Prepared {
            geom,
            length,
            radius,
            pos,
            mesh,
        });
        Ok(())
    }

    fn segment_internal_impedance(
        e: &Electrode,
        radius: f64,
        length: f64,
        omega: f64,
    ) -> Complex64 {
        internal_impedance(radius, length, omega, e.material.sigma, e.material.mur)
    }

    /// Fill the impedance matrices at `omega` and return the assembled mesh.
    fn fill(&mut self, omega: f64) -> Result<()> {
        let mu_air = self.structure.air.mur * MU0;
        let mu_soil = self.structure.soil.mur() * MU0;
        let w_air = self.structure.air.admittance(omega);
        let w_soil = self.structure.soil.admittance(omega);

        let prep = self.prepared.as_mut().expect("prepared");
        prep.mesh
            .calc_param_w(omega, mu_air, w_air, mu_soil, w_soil);
        let n = prep.geom.n;
        for i in 0..n {
            for j in i..n {
                let ij = prep.geom.idx(i, j);
                if i == j {
                    let zint = Self::segment_internal_impedance(
                        &self.structure.electrodes[i],
                        prep.radius[i],
                        prep.length[i],
                        omega,
                    );
                    prep.mesh.calc_z_self(
                        i,
                        prep.pos[i],
                        prep.geom.rbar[ij],
                        prep.geom.rbari[ij],
                        prep.length[i],
                        zint,
                        prep.geom.g[ij],
                        prep.geom.gi[ij],
                        prep.geom.cos_theta_i[ij],
                    );
                } else {
                    prep.mesh.calc_z_mutual(
                        i,
                        j,
                        prep.pos[i],
                        prep.pos[j],
                        prep.geom.rbar[ij],
                        prep.geom.rbari[ij],
                        prep.length[i],
                        prep.length[j],
                        prep.geom.g[ij],
                        prep.geom.gi[ij],
                        prep.geom.cos_theta[ij],
                        prep.geom.cos_theta_i[ij],
                    );
                }
            }
        }
        Ok(())
    }

    /// Solve one frequency point. Sources are current injections unless
    /// `is_voltage`, which adds ideal voltage sources by unit-injection
    /// superposition (ADR 0016).
    pub fn run(&mut self, omega: f64, sources: &[Source]) -> Result<RunOutput> {
        self.prepare()?;
        self.fill(omega)?;

        let mut pos = Vec::with_capacity(sources.len());
        for s in sources {
            let idx = self.structure.find_node_index(&s.node).ok_or_else(|| {
                TupaError::new(format!("tStudy%run: source node '{}' not found", s.node))
            })?;
            pos.push(idx);
        }
        let mesh = &self.prepared.as_ref().expect("prepared").mesh;

        if sources.iter().any(|s| s.is_voltage) {
            return solve_with_voltage_sources(mesh, &pos, sources);
        }
        let currents: Vec<Complex64> = sources.iter().map(|s| s.value).collect();
        let sol = mesh
            .inject_signal(&pos, &currents)
            .map_err(|e| TupaError::new(format!("tStudy%run: injectSignal failed ({e})")))?;
        Ok(RunOutput {
            voltage: sol.voltage,
            current1: sol.current1,
            current2: sol.current2,
            source_currents: currents,
        })
    }

    /// Sweep over `freq_hz`, storing all node voltages and electrode currents.
    pub fn run_sweep(&mut self, freq_hz: &[f64], sources: &[Source]) -> Result<()> {
        self.prepare()?;
        let omega: Vec<f64> = freq_hz.iter().map(|f| 2.0 * PI * f).collect();
        let node_ids: Vec<String> = self.structure.nodes.iter().map(|n| n.id.clone()).collect();
        let electrode_ids: Vec<String> = self
            .structure
            .electrodes
            .iter()
            .map(|e| e.id.clone())
            .collect();
        let nno = node_ids.len();
        let nseg = electrode_ids.len();

        let mut volt = ResultSet::new(node_ids, omega.clone());
        let mut i1 = ResultSet::new(electrode_ids.clone(), omega.clone());
        let mut i2 = ResultSet::new(electrode_ids, omega.clone());
        let mut src_freq = vec![Vec::with_capacity(freq_hz.len()); sources.len()];

        for (k, &w) in omega.iter().enumerate() {
            if verbosity_level() == VERB_VERBOSE {
                println!(" f = {} Hz", format_engineering(freq_hz[k]));
            }
            let out = self.run(w, sources)?;
            for i in 0..nno {
                volt.set(i, k, out.voltage[i]);
            }
            for i in 0..nseg {
                i1.set(i, k, out.current1[i]);
                i2.set(i, k, out.current2[i]);
            }
            for (s, c) in out.source_currents.iter().enumerate() {
                src_freq[s].push(*c);
            }
        }

        self.voltage_results = volt;
        self.long_current_results = i1;
        self.trans_current_results = i2;
        self.sweep_freq_hz = freq_hz.to_vec();
        self.sweep_source_ids = sources.iter().map(|s| s.node.clone()).collect();
        self.sweep_source_currents_freq = src_freq;
        Ok(())
    }

    /// Input impedance `V(node)/I_source(node)` per swept frequency; the node
    /// must have been a source of the last sweep.
    pub fn input_impedance(&self, node_id: &str) -> Result<Vec<Complex64>> {
        let i_src = self
            .sweep_source_ids
            .iter()
            .position(|s| s == node_id)
            .ok_or_else(|| {
                TupaError::new(format!(
                    "tStudy%inputImpedance: '{node_id}' was not a runSweep source node"
                ))
            })?;
        let i_node = self.structure.find_node_index(node_id).ok_or_else(|| {
            TupaError::new(format!("tStudy%inputImpedance: node '{node_id}' not found"))
        })?;
        let nf = self.voltage_results.frequency_count();
        Ok((0..nf)
            .map(|k| {
                self.voltage_results.get(i_node, k) / self.sweep_source_currents_freq[i_src][k]
            })
            .collect())
    }

    /// `max_i |V_i|` per swept frequency.
    pub fn max_voltage_magnitude(&self) -> Vec<f64> {
        let nf = self.voltage_results.frequency_count();
        let nno = self.voltage_results.entity_count();
        (0..nf)
            .map(|k| (0..nno).fold(0.0_f64, |m, i| m.max(self.voltage_results.get(i, k).norm())))
            .collect()
    }

    /// Print the study summary (nodes, materials, elements) at NORMAL
    /// verbosity or above.
    pub fn report(&self) {
        if verbosity_level() < VERB_NORMAL {
            return;
        }
        let mut s = String::new();
        s.push_str("=========================================\n");
        s.push_str("Example Study Initialization\n");
        s.push_str("=========================================\n");
        s.push_str(&format!("Study Title: {}\n", self.title));
        s.push_str(&format!(
            "Number of Nodes: {}\n",
            self.structure.nodes.len()
        ));
        s.push_str(&format!(
            "Number of Materials: {}\n",
            self.structure.materials.len()
        ));
        s.push_str(&format!(
            "Number of Elements: {}\n",
            self.structure.elements.len()
        ));
        s.push_str("Nodes:\n");
        for n in &self.structure.nodes {
            s.push_str(&format!(
                "  {} at ({:.2}, {:.2}, {:.2})\n",
                n.id, n.p[0], n.p[1], n.p[2]
            ));
        }
        s.push_str("Materials:\n");
        for m in self.structure.materials.iter().rev() {
            let _ = m;
            s.push_str("linear material\n");
        }
        s.push_str("Elements:\n");
        for e in &self.structure.elements {
            s.push_str(&e.report());
        }
        s.push_str("=========================================\n");
        println!("{s}");
    }

    /// Structure dump for assembly comparison (ROADMAP Phase 8 item 4):
    /// nodes and electrodes with IDs, coordinates, radii and media.
    pub fn dump_structure(&self) -> String {
        let mut s = String::new();
        s.push_str("# nodes: index,id,x,y,z\n");
        for (i, n) in self.structure.nodes.iter().enumerate() {
            s.push_str(&format!(
                "node,{},{},{:.12e},{:.12e},{:.12e}\n",
                i + 1,
                n.id,
                n.p[0],
                n.p[1],
                n.p[2]
            ));
        }
        s.push_str("# electrodes: index,id,node1,node2,radius,medium\n");
        for (i, e) in self.structure.electrodes.iter().enumerate() {
            let pa = self.structure.nodes[e.node_indices[0]].p;
            let pb = self.structure.nodes[e.node_indices[1]].p;
            let medium = if 0.5 * (pa[2] + pb[2]) > 0.0 {
                "air"
            } else {
                "soil"
            };
            s.push_str(&format!(
                "electrode,{},{},{},{},{:.12e},{}\n",
                i + 1,
                e.id,
                e.node_indices[0] + 1,
                e.node_indices[1] + 1,
                e.radius,
                medium
            ));
        }
        s
    }
}

/// Fortran `EN0.1E2`-like engineering formatting (progress output only).
fn format_engineering(f: f64) -> String {
    format!("{f:.3e}")
}

/// Ideal voltage sources by unit-injection superposition (ADR 0016).
fn solve_with_voltage_sources(mesh: &Mesh, pos: &[usize], sources: &[Source]) -> Result<RunOutput> {
    let ns = pos.len();
    let unit: Vec<Vec<Complex64>> = (0..ns)
        .map(|k| {
            (0..ns)
                .map(|j| {
                    if j == k {
                        Complex64::new(1.0, 0.0)
                    } else {
                        ZERO
                    }
                })
                .collect()
        })
        .collect();
    let units = mesh
        .inject_signals(pos, &unit)
        .map_err(|e| TupaError::new(format!("tStudy%run: unit-injection solve failed ({e})")))?;

    let v_idx: Vec<usize> = (0..ns).filter(|&k| sources[k].is_voltage).collect();
    let nv = v_idx.len();
    let mut a = CMatrix::zeros(nv, nv);
    let mut rhs = CMatrix::zeros(nv, 1);
    for j in 0..nv {
        for l in 0..nv {
            a.set(j, l, units[v_idx[l]].voltage[pos[v_idx[j]]]);
        }
        let mut r = sources[v_idx[j]].value;
        for k in 0..ns {
            if !sources[k].is_voltage {
                r -= sources[k].value * units[k].voltage[pos[v_idx[j]]];
            }
        }
        rhs.set(j, 0, r);
    }
    solve_in_place(&mut a, &mut rhs).map_err(|e| {
        TupaError::new(format!(
            "tStudy%run: voltage-source constraint solve failed ({e})"
        ))
    })?;

    let mut ieff: Vec<Complex64> = sources.iter().map(|s| s.value).collect();
    for (j, &k) in v_idx.iter().enumerate() {
        ieff[k] = rhs.get(j, 0);
    }

    let combine = |f: &dyn Fn(&crate::mesh::Solution) -> &Vec<Complex64>, len: usize| {
        let mut out = vec![ZERO; len];
        for (k, sol) in units.iter().enumerate() {
            for (o, v) in out.iter_mut().zip(f(sol)) {
                *o += *v * ieff[k];
            }
        }
        out
    };
    Ok(RunOutput {
        voltage: combine(&|s| &s.voltage, mesh.nno),
        current1: combine(&|s| &s.current1, mesh.nseg),
        current2: combine(&|s| &s.current2, mesh.nseg),
        source_currents: ieff,
    })
}

/// Log-spaced frequency axis `fmin … fmax` with `n_points` samples.
pub fn log_frequency_axis(f_min_hz: f64, f_max_hz: f64, n_points: usize) -> Result<Vec<f64>> {
    if n_points < 2 {
        return Err(TupaError::new("logFrequencyAxis: nPoints must be >= 2"));
    }
    let log_min = f_min_hz.log10();
    let log_max = f_max_hz.log10();
    Ok((0..n_points)
        .map(|k| 10.0_f64.powf(log_min + (log_max - log_min) * k as f64 / (n_points - 1) as f64))
        .collect())
}

/// Silence an unused-import lint when verbosity helpers are compiled out.
#[allow(dead_code)]
fn _touch() {
    verbose(VERB_NORMAL, "");
}
