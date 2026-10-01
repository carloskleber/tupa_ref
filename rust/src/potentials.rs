//! Potential post-processing at observation points (`mPotentials`, ROADMAP
//! Phase 11, theory.md §3.1, ADR 0027): surface potentials, GPR, touch and
//! step voltages from the transversal currents of a solved sweep.
//!
//! The kernel is the impedance fill's own `Z_t` row with the field point
//! moved off the conductor:
//!
//! `ψ(P) = Σ_b cE · I_t,b / l_b · ( e^{-γ R_b} g_b(P) + Γ e^{-γ R_bi} g_bi(P) )`,
//!
//! summed over the segments of the medium that contains `P`.

use crate::ctes::{MU0, PI, ZERO};
use crate::error::{Result, TupaError};
use crate::mesh::{AIR, MediumConstants, SOIL};
use crate::observation::ObservationResults;
use crate::result::ResultSet;
use crate::study::Study;
use num_complex::Complex64;

fn sub(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    [a[0] - b[0], a[1] - b[1], a[2] - b[2]]
}

fn dot(a: [f64; 3], b: [f64; 3]) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

fn norm(a: [f64; 3]) -> f64 {
    dot(a, a).sqrt()
}

/// Geometry factor of a straight segment seen from a point,
/// `g = ∫ dℓ / |P − r(ℓ)|` from `a` to `b` (theory.md §3.1): with `s1`, `s2`
/// the axial coordinates of `P` relative to the two ends and `ρ` its distance
/// to the axis, `g = asinh(s1/ρ) − asinh(s2/ρ)`. A point closer to the axis
/// than the conductor `radius` is treated as lying on the surface.
pub fn segment_potential_factor(a: [f64; 3], b: [f64; 3], p: [f64; 3], radius: f64) -> f64 {
    let ab = sub(b, a);
    let l = norm(ab);
    let u = [ab[0] / l, ab[1] / l, ab[2] / l];
    let ap = sub(p, a);
    let s1 = dot(ap, u);
    let s2 = s1 - l;
    let rho = (dot(ap, ap) - s1 * s1).max(0.0).sqrt().max(radius);
    (s1 / rho).asinh() - (s2 / rho).asinh()
}

/// ψ at every point of `pts` for every frequency of the study's stored sweep
/// (`potentialsAt`); indexed `[point][frequency]`. Points at `z <= 0` are in
/// soil, above it in air.
pub fn potentials_at(study: &Study, pts: &[[f64; 3]]) -> Result<Vec<Vec<Complex64>>> {
    let prep = study
        .prepared
        .as_ref()
        .ok_or_else(|| TupaError::new("potentials: the study has no solved sweep"))?;
    let nf = study.voltage_results.frequency_count();
    let nseg = study.structure.electrodes.len();
    let mu_air = study.structure.air.mur * MU0;
    let mu_soil = study.structure.soil.mur() * MU0;
    let model = study.image_model.unwrap_or(study.default_image_model);

    let consts: Vec<MediumConstants> = (0..nf)
        .map(|k| {
            let omega = study.voltage_results.omega()[k];
            MediumConstants::from_immittance(
                omega,
                mu_air,
                study.structure.air.admittance(omega),
                mu_soil,
                study.structure.soil.admittance(omega),
                model,
            )
        })
        .collect();
    let it: Vec<Vec<Complex64>> = (0..nseg)
        .map(|b| {
            (0..nf)
                .map(|k| {
                    study.long_current_results.get(b, k) + study.trans_current_results.get(b, k)
                })
                .collect()
        })
        .collect();

    let mut p1 = Vec::with_capacity(nseg);
    let mut p2 = Vec::with_capacity(nseg);
    let mut q1 = Vec::with_capacity(nseg);
    let mut q2 = Vec::with_capacity(nseg);
    let mut mid = Vec::with_capacity(nseg);
    let mut midi = Vec::with_capacity(nseg);
    for e in &study.structure.electrodes {
        let a = study.structure.nodes[e.node_indices[0]].p;
        let b = study.structure.nodes[e.node_indices[1]].p;
        let (qa, qb) = ([a[0], a[1], -a[2]], [b[0], b[1], -b[2]]);
        p1.push(a);
        p2.push(b);
        q1.push(qa);
        q2.push(qb);
        mid.push([
            0.5 * (a[0] + b[0]),
            0.5 * (a[1] + b[1]),
            0.5 * (a[2] + b[2]),
        ]);
        midi.push([
            0.5 * (qa[0] + qb[0]),
            0.5 * (qa[1] + qb[1]),
            0.5 * (qa[2] + qb[2]),
        ]);
    }

    let mut psi = Vec::with_capacity(pts.len());
    for &pt in pts {
        let soil = pt[2] <= 0.0;
        let want = if soil { SOIL } else { AIR };
        let mut acc = vec![ZERO; nf];
        for b in 0..nseg {
            if prep.pos[b] != want {
                continue;
            }
            let g = segment_potential_factor(p1[b], p2[b], pt, prep.radius[b]);
            let gi = segment_potential_factor(q1[b], q2[b], pt, prep.radius[b]);
            let r = norm(sub(pt, mid[b]));
            let ri = norm(sub(pt, midi[b]));
            for (k, c) in consts.iter().enumerate() {
                let (ce, gam, img) = if soil {
                    (c.c_e_soil, c.prop_soil, c.gamma_soil)
                } else {
                    (c.c_e_air, c.prop_air, c.gamma_air)
                };
                let w = (-gam * r).exp() * g + img * (-gam * ri).exp() * gi;
                acc[k] += ce * it[b][k] * w / prep.length[b];
            }
        }
        psi.push(acc);
    }
    Ok(psi)
}

/// Evaluate the study's `observation` request on its stored sweep and fill
/// `study.observation_results` (`computeObservations`). A no-op when no
/// observation was requested.
pub fn compute_observations(study: &mut Study) -> Result<()> {
    let obs = study.observation.clone();
    if obs.is_empty() {
        return Ok(());
    }
    let nf = study.voltage_results.frequency_count();
    if nf == 0 {
        return Err(TupaError::new(
            "mPotentials: observation needs a solved harmonic sweep (sources + frequencies)",
        ));
    }
    if study.sweep_damping != 0.0 {
        return Err(TupaError::new(
            "mPotentials: observation is not defined for a Numerical Laplace Transform sweep",
        ));
    }
    let omega: Vec<f64> = study.voltage_results.omega().to_vec();

    let n_sites = obs.site_count();
    let n_points = obs.points.len();
    let (n_grid, n_dir) = match &obs.grid {
        Some(g) => (g.nx * g.ny, if g.step { g.step_directions } else { 0 }),
        None => (0, 0),
    };

    // One flat list of evaluation points: sites, touch circles, step pairs,
    // step-map neighbours
    let mut pts: Vec<[f64; 3]> = Vec::new();
    let mut ids = Vec::with_capacity(n_sites);
    let mut site_pos = Vec::with_capacity(n_sites);
    for i in 0..n_sites {
        let (id, p) = obs.site(i);
        ids.push(id);
        site_pos.push(p);
        pts.push(p);
    }
    let o_touch = pts.len();
    let mut touch_nodes = Vec::new();
    for t in &obs.touch {
        let node = study.structure.find_node_index(&t.node).ok_or_else(|| {
            TupaError::new(format!(
                "mPotentials: observation.touch references unknown node '{}'",
                t.node
            ))
        })?;
        touch_nodes.push(node);
        let c = study.structure.nodes[node].p;
        for j in 0..t.n_points {
            let ang = 2.0 * PI * j as f64 / t.n_points as f64;
            pts.push([
                c[0] + t.radius * ang.sin(),
                c[1] + t.radius * ang.cos(),
                t.z,
            ]);
        }
    }
    let o_step = pts.len();
    for s in &obs.steps {
        pts.push(s.from);
        pts.push(s.to);
    }
    let o_map = pts.len();
    if let (Some(g), true) = (&obs.grid, n_dir > 0) {
        for i in 0..n_grid {
            let base = site_pos[n_points + i];
            for j in 0..n_dir {
                let ang = 2.0 * PI * j as f64 / n_dir as f64;
                pts.push([
                    base[0] + g.step_length * ang.cos(),
                    base[1] + g.step_length * ang.sin(),
                    base[2],
                ]);
            }
        }
    }

    let psi_all = potentials_at(study, &pts)?;
    let psi = |m: usize, k: usize| psi_all[m][k];

    let mut res = ObservationResults {
        potentials: ResultSet::new(ids.clone(), omega.clone()),
        gpr: ResultSet::new(
            obs.touch.iter().map(|t| t.id.clone()).collect(),
            omega.clone(),
        ),
        touch: ResultSet::new(
            obs.touch.iter().map(|t| t.id.clone()).collect(),
            omega.clone(),
        ),
        steps: ResultSet::new(
            obs.steps.iter().map(|s| s.id.clone()).collect(),
            omega.clone(),
        ),
        step_map: ResultSet::default(),
        site_pos,
        freq_hz: study.sweep_freq_hz.clone(),
    };
    for i in 0..n_sites {
        for k in 0..nf {
            res.potentials.set(i, k, psi(i, k));
        }
    }

    let mut first = o_touch;
    for (i, t) in obs.touch.iter().enumerate() {
        for k in 0..nf {
            let u = study.voltage_results.get(touch_nodes[i], k);
            let worst = (0..t.n_points)
                .map(|j| (psi(first + j, k) - u).norm())
                .fold(0.0, f64::max);
            res.gpr.set(i, k, u);
            res.touch.set(i, k, Complex64::new(worst, 0.0));
        }
        first += t.n_points;
    }
    for i in 0..obs.steps.len() {
        for k in 0..nf {
            res.steps
                .set(i, k, psi(o_step + 2 * i + 1, k) - psi(o_step + 2 * i, k));
        }
    }
    if n_dir > 0 {
        res.step_map = ResultSet::new(ids[n_points..].to_vec(), omega.clone());
        for i in 0..n_grid {
            for k in 0..nf {
                let worst = (0..n_dir)
                    .map(|j| (psi(n_points + i, k) - psi(o_map + i * n_dir + j, k)).norm())
                    .fold(0.0, f64::max);
                res.step_map.set(i, k, Complex64::new(worst, 0.0));
            }
        }
    }
    study.observation_results = res;
    Ok(())
}
