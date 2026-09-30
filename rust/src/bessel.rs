//! Modified Bessel functions `I₀`, `I₁` of complex argument — the subset
//! needed by the solid-conductor internal impedance (theory.md §4.3).
//!
//! Replaces SLATEC `ZBESI`. Two regimes:
//!
//! * `|z| < 20`: ascending power series (maximum term growth `e^20`, relative
//!   loss ≲ 1e-13 for the 45° arguments of this code base);
//! * `|z| ≥ 20`: Hankel asymptotic expansion, truncated at its smallest term
//!   (≲ 1e-17 at `|z| = 20`). The `e^{-z}` companion term is dropped; it is
//!   below `e^{-2 Re z}` relative, i.e. < 1e-12 for `Re z ≥ 14`, which holds
//!   for `z = r₀√(jωμσ)` (phase exactly 45°).

use num_complex::Complex64;

const SERIES_LIMIT: f64 = 20.0;

fn series(z: Complex64, nu: u32) -> Complex64 {
    // I_nu(z) = (z/2)^nu Σ_k (z²/4)^k / (k! (k+nu)!)
    let q = z * z * 0.25;
    let mut term = Complex64::new(1.0, 0.0);
    if nu == 1 {
        term = z * 0.5;
    }
    let mut sum = term;
    let mut k = 1.0_f64;
    loop {
        term = term * q / (k * (k + nu as f64));
        sum += term;
        if term.norm() < 1e-18 * sum.norm() {
            break;
        }
        k += 1.0;
        if k > 500.0 {
            break;
        }
    }
    sum
}

/// Bracketed asymptotic sum `Σ (-1)^k a_k(ν)/z^k`; the common factor
/// `e^z/√(2πz)` is omitted (it cancels in the ratio).
fn asymptotic_sum(z: Complex64, nu: u32) -> Complex64 {
    let mu = 4.0 * (nu * nu) as f64;
    let mut term = Complex64::new(1.0, 0.0);
    let mut sum = term;
    let mut prev_norm = f64::INFINITY;
    for k in 1..200 {
        let kf = k as f64;
        let odd = 2.0 * kf - 1.0;
        term = -term * (mu - odd * odd) / (kf * 8.0) / z;
        let n = term.norm();
        if n > prev_norm {
            break; // asymptotic series started to diverge
        }
        sum += term;
        if n < 1e-17 {
            break;
        }
        prev_norm = n;
    }
    sum
}

/// Ratio `I₀(z)/I₁(z)` for `Re z ≥ 0`.
pub fn i0_over_i1(z: Complex64) -> Complex64 {
    if z.norm() < SERIES_LIMIT {
        series(z, 0) / series(z, 1)
    } else {
        asymptotic_sum(z, 0) / asymptotic_sum(z, 1)
    }
}

/// `I₀(z)`, `I₁(z)` for the series regime only (test helper / small args).
pub fn i0_i1_series(z: Complex64) -> (Complex64, Complex64) {
    (series(z, 0), series(z, 1))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn real_argument_matches_known_values() {
        // I0(1) = 1.2660658777520082, I1(1) = 0.5651591039924851
        let (i0, i1) = i0_i1_series(Complex64::new(1.0, 0.0));
        assert!((i0.re - 1.266_065_877_752_008_2).abs() < 1e-15);
        assert!((i1.re - 0.565_159_103_992_485_1).abs() < 1e-15);
    }

    #[test]
    fn asymptotic_and_series_agree_at_the_switch() {
        let ang = std::f64::consts::FRAC_PI_4;
        for r in [19.9_f64, 20.1, 30.0] {
            let z = Complex64::from_polar(r, ang);
            let s = series(z, 0) / series(z, 1);
            let a = asymptotic_sum(z, 0) / asymptotic_sum(z, 1);
            assert!((s - a).norm() / s.norm() < 1e-11, "r={r}: {s} vs {a}");
        }
    }

    #[test]
    fn large_argument_ratio_tends_to_one() {
        let z = Complex64::from_polar(400.0, std::f64::consts::FRAC_PI_4);
        let r = i0_over_i1(z);
        assert!((r - Complex64::new(1.0, 0.0)).norm() < 5.0e-3);
    }

    #[test]
    fn small_argument_ratio_is_two_over_z() {
        let z = Complex64::new(1e-3, 1e-3);
        let r = i0_over_i1(z);
        let expect = Complex64::new(2.0, 0.0) / z;
        assert!((r - expect).norm() / expect.norm() < 1e-5);
    }
}
