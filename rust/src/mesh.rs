//! System matrices and the frequency-domain solve (`mMesh`): topology
//! matrices A, B, C, D, impedance matrices, augmented `Zeq` (ADR 0003) and
//! the LU solve. Sign and propagation conventions follow theory.md §2, §5, §6
//! (ADR 0008).

use crate::ctes::{FOUR_PI, ONE, ZERO};
use crate::error::{Result, TupaError};
use crate::linalg::{CMatrix, solve_in_place};
use num_complex::Complex64;

/// Medium constants at one frequency (theory.md §5: `c_E`, `c_M`, `γ`).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct MediumConstants {
    /// `1/(4π W_air)`
    pub c_e_air: Complex64,
    /// `1/(4π W_soil)`
    pub c_e_soil: Complex64,
    /// `jωμ_air/(4π)`
    pub c_m_air: Complex64,
    /// `jωμ_soil/(4π)`
    pub c_m_soil: Complex64,
    /// Air propagation constant
    pub prop_air: Complex64,
    /// Soil propagation constant
    pub prop_soil: Complex64,
}

/// Solution of one injection pattern: node voltages and electrode end
/// currents.
#[derive(Debug, Clone, PartialEq)]
pub struct Solution {
    /// Node voltages `u` (V)
    pub voltage: Vec<Complex64>,
    /// End currents `i1` at node n1 of each segment, positive INTO the segment
    pub current1: Vec<Complex64>,
    /// End currents `i2` at node n2 of each segment, positive INTO the segment
    pub current2: Vec<Complex64>,
}

/// Discretised mesh for the frequency-domain HEM solution.
#[derive(Debug, Clone)]
pub struct Mesh {
    /// Number of nodes
    pub nno: usize,
    /// Number of electrode segments
    pub nseg: usize,
    /// Topology `A` (nseg × nno): −1 at n1, +1 at n2
    pub a: CMatrix,
    /// Topology `B` (nseg × nno): −½ at n1 and n2
    pub b: CMatrix,
    /// Topology `C` (nno × nseg): +1 at (n1, seg)
    pub c: CMatrix,
    /// Topology `D` (nno × nseg): +1 at (n2, seg)
    pub d: CMatrix,
    /// Transversal impedance (nseg × nseg)
    pub ztrans: CMatrix,
    /// Longitudinal impedance (nseg × nseg)
    pub zlong: CMatrix,
    /// Medium constants at the current frequency
    pub medium: MediumConstants,
}

/// Pair of media for impedance evaluation: position code 1 = air, 2 = soil.
pub const AIR: u8 = 1;
/// Soil position code.
pub const SOIL: u8 = 2;

impl Mesh {
    /// Allocate for `nn` nodes and `ns` segments and build the topology
    /// matrices from 0-based node indices `n1`, `n2` (theory.md §6).
    pub fn new(nn: usize, ns: usize, n1: &[usize], n2: &[usize]) -> Self {
        let mut a = CMatrix::zeros(ns, nn);
        let mut b = CMatrix::zeros(ns, nn);
        let mut c = CMatrix::zeros(nn, ns);
        let mut d = CMatrix::zeros(nn, ns);
        for i in 0..ns {
            a.set(i, n1[i], Complex64::new(-1.0, 0.0));
            a.set(i, n2[i], ONE);
            b.set(i, n1[i], Complex64::new(-0.5, 0.0));
            b.set(i, n2[i], Complex64::new(-0.5, 0.0));
            c.set(n1[i], i, ONE);
            d.set(n2[i], i, ONE);
        }
        Self {
            nno: nn,
            nseg: ns,
            a,
            b,
            c,
            d,
            ztrans: CMatrix::zeros(ns, ns),
            zlong: CMatrix::zeros(ns, ns),
            medium: MediumConstants {
                c_e_air: ZERO,
                c_e_soil: ZERO,
                c_m_air: ZERO,
                c_m_soil: ZERO,
                prop_air: ZERO,
                prop_soil: ZERO,
            },
        }
    }

    /// Medium constants from the complex immittance `W(ω)` of each medium
    /// (`calcParamW`; `mu_*` in H/m).
    pub fn calc_param_w(
        &mut self,
        omega: f64,
        mu_air: f64,
        w_air: Complex64,
        mu_soil: f64,
        w_soil: Complex64,
    ) {
        let jw = Complex64::new(0.0, omega);
        self.medium = MediumConstants {
            c_e_air: 1.0 / (FOUR_PI * w_air),
            c_e_soil: 1.0 / (FOUR_PI * w_soil),
            c_m_air: Complex64::new(0.0, omega * mu_air / FOUR_PI),
            c_m_soil: Complex64::new(0.0, omega * mu_soil / FOUR_PI),
            prop_air: (jw * mu_air * w_air).sqrt(),
            prop_soil: (jw * mu_soil * w_soil).sqrt(),
        }
    }

    fn medium_for(&self, pos: u8) -> (Complex64, Complex64, Complex64, f64) {
        if pos == AIR {
            (
                self.medium.prop_air,
                self.medium.c_e_air,
                self.medium.c_m_air,
                -1.0,
            )
        } else {
            (
                self.medium.prop_soil,
                self.medium.c_e_soil,
                self.medium.c_m_soil,
                1.0,
            )
        }
    }

    /// Self impedance of segment `i` with its own image (theory.md §4.3, §5,
    /// ADR 0009):
    ///
    /// `Ztrans = cE (e^{-γd} g ± e^{-γdi} gi) / l²`,
    /// `Zlong  = cM (e^{-γd} g ± cosθi e^{-γdi} gi) + zint`.
    ///
    /// Image sign "−" in air, "+" in soil.
    #[allow(clippy::too_many_arguments)]
    pub fn calc_z_self(
        &mut self,
        i: usize,
        pos: u8,
        d: f64,
        di: f64,
        l: f64,
        zint: Complex64,
        g: f64,
        gi: f64,
        cos_theta_i: f64,
    ) {
        let (prop, ce, cm, s) = self.medium_for(pos);
        let fprop = (-d * prop).exp();
        let fpropi = (-di * prop).exp();
        self.ztrans
            .set(i, i, ce * (fprop * g + s * fpropi * gi) / (l * l));
        self.zlong.set(
            i,
            i,
            cm * (fprop * g + s * cos_theta_i * fpropi * gi) + zint,
        );
    }

    /// Mutual impedance of segments `i`, `j` (theory.md §4.1, §5, ADR 0009);
    /// mixed-media pairs are neglected (zero, ADR 0005).
    #[allow(clippy::too_many_arguments)]
    pub fn calc_z_mutual(
        &mut self,
        i: usize,
        j: usize,
        pos1: u8,
        pos2: u8,
        d: f64,
        di: f64,
        la: f64,
        lb: f64,
        g: f64,
        gi: f64,
        cos_theta: f64,
        cos_theta_i: f64,
    ) {
        let (zt, zl) = if pos1 == pos2 {
            let (prop, ce, cm, s) = self.medium_for(pos1);
            let fprop = (-d * prop).exp();
            let fpropi = (-di * prop).exp();
            (
                ce * (fprop * g + s * fpropi * gi) / (la * lb),
                cm * (cos_theta * fprop * g + s * cos_theta_i * fpropi * gi),
            )
        } else {
            (ZERO, ZERO)
        };
        self.ztrans.set(i, j, zt);
        self.ztrans.set(j, i, zt);
        self.zlong.set(i, j, zl);
        self.zlong.set(j, i, zl);
    }

    /// Assemble the augmented `(nno + 2 nseg)²` matrix (theory.md §6):
    ///
    /// ```text
    /// [ A  |  Zlong/2 | -Zlong/2 ]
    /// [ B  |  Ztrans  |  Ztrans  ]
    /// [ 0  |  C       |  D       ]
    /// ```
    pub fn calc_freq2(&self) -> CMatrix {
        let (nn, ns) = (self.nno, self.nseg);
        let n = nn + 2 * ns;
        let mut z = CMatrix::zeros(n, n);
        for i in 0..ns {
            for j in 0..nn {
                z.set(i, j, self.a.get(i, j));
                z.set(ns + i, j, self.b.get(i, j));
            }
            for j in 0..ns {
                let zl = self.zlong.get(i, j);
                z.set(i, nn + j, zl * 0.5);
                z.set(i, nn + ns + j, zl * (-0.5));
                let zt = self.ztrans.get(i, j);
                z.set(ns + i, nn + j, zt);
                z.set(ns + i, nn + ns + j, zt);
            }
        }
        for i in 0..nn {
            for j in 0..ns {
                z.set(2 * ns + i, nn + j, self.c.get(i, j));
                z.set(2 * ns + i, nn + ns + j, self.d.get(i, j));
            }
        }
        z
    }

    /// Solve `Zeq x = b` for several injection patterns at once (one LU,
    /// multiple right-hand sides, ADR 0003/0016). `sigs[p][k]` is the current
    /// injected at node `pos[k]` in pattern `p`.
    pub fn inject_signals(&self, pos: &[usize], sigs: &[Vec<Complex64>]) -> Result<Vec<Solution>> {
        let (nn, ns) = (self.nno, self.nseg);
        let n = nn + 2 * ns;
        let nrhs = sigs.len();
        let mut zeq = self.calc_freq2();
        let mut y = CMatrix::zeros(n, nrhs);
        for (p, sig) in sigs.iter().enumerate() {
            if sig.len() != pos.len() {
                return Err(TupaError::new("inject_signals: pattern length mismatch"));
            }
            for (k, &node) in pos.iter().enumerate() {
                *y.at_mut(2 * ns + node, p) += sig[k];
            }
        }
        solve_in_place(&mut zeq, &mut y)?;
        Ok((0..nrhs)
            .map(|p| Solution {
                voltage: (0..nn).map(|i| y.get(i, p)).collect(),
                current1: (0..ns).map(|i| y.get(nn + i, p)).collect(),
                current2: (0..ns).map(|i| y.get(nn + ns + i, p)).collect(),
            })
            .collect())
    }

    /// Solve for a single injection pattern.
    pub fn inject_signal(&self, pos: &[usize], sig: &[Complex64]) -> Result<Solution> {
        let mut v = self.inject_signals(pos, &[sig.to_vec()])?;
        Ok(v.remove(0))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ctes::{MU0, PI};

    #[test]
    fn topology_rows_and_columns() {
        let m = Mesh::new(3, 2, &[0, 1], &[1, 2]);
        assert_eq!(m.a.get(0, 0), Complex64::new(-1.0, 0.0));
        assert_eq!(m.a.get(0, 1), ONE);
        assert_eq!(m.b.get(1, 1), Complex64::new(-0.5, 0.0));
        assert_eq!(m.b.get(1, 2), Complex64::new(-0.5, 0.0));
        assert_eq!(m.c.get(1, 1), ONE);
        assert_eq!(m.d.get(2, 1), ONE);
        assert_eq!(m.d.get(0, 0), ZERO);
    }

    #[test]
    fn propagation_constant_sign_and_magnitude() {
        let mut m = Mesh::new(2, 1, &[0], &[1]);
        let omega = 2.0 * PI * 1e6;
        let w_soil = Complex64::new(0.01, omega * 10.0 * crate::ctes::EPSILON0);
        m.calc_param_w(
            omega,
            MU0,
            Complex64::new(0.0, omega * crate::ctes::EPSILON0),
            MU0,
            w_soil,
        );
        assert!(m.medium.prop_soil.re > 0.0 && m.medium.prop_soil.im > 0.0);
        // air: pure phase constant ω/c
        assert!(
            (m.medium.prop_air.im - omega / crate::ctes::C).abs() / m.medium.prop_air.im < 1e-12
        );
    }

    #[test]
    fn image_sign_is_minus_in_air_plus_in_soil() {
        let mut m = Mesh::new(2, 1, &[0], &[1]);
        let omega = 2.0 * PI * 1e3;
        let w = Complex64::new(0.01, 0.0);
        m.calc_param_w(omega, MU0, w, MU0, w);
        m.calc_z_self(0, AIR, 0.01, 2.0, 1.0, ZERO, 3.0, 2.0, 1.0);
        let air = m.ztrans.get(0, 0);
        m.calc_z_self(0, SOIL, 0.01, 2.0, 1.0, ZERO, 3.0, 2.0, 1.0);
        let soil = m.ztrans.get(0, 0);
        // same material constants ⇒ |soil| > |air| because image adds vs subtracts
        assert!(soil.norm() > air.norm());
    }

    #[test]
    fn mixed_media_mutual_is_zero() {
        let mut m = Mesh::new(3, 2, &[0, 1], &[1, 2]);
        m.calc_z_mutual(0, 1, AIR, SOIL, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0);
        assert_eq!(m.ztrans.get(0, 1), ZERO);
        assert_eq!(m.zlong.get(1, 0), ZERO);
    }
}
