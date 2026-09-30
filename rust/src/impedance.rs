//! Adaptive quadrature and solid-conductor internal impedance (`mImpedance`).
//!
//! The mutual geometry factor `g(a,b) = ∬ ds_a ds_b / r` is evaluated by two
//! nested calls of a line-by-line port of `dqag_k15` (adaptive Gauss–Kronrod
//! 7/15, at most `MAXINT` subintervals, same tolerances and subdivision
//! strategy as the Fortran code — a crate would not meet the 1e-6 golden
//! tolerance on the quadrature-dominated cases).

use crate::bessel::i0_over_i1;
use crate::ctes::{MU0, PI};
use num_complex::Complex64;

/// Maximum number of subintervals in the adaptive quadrature.
pub const MAXINT: usize = 500;

/// Default relative-error factor of `geometry_factor_2d` (scaled there by the
/// shorter segment length).
pub const DEFAULT_QUAD_EPS_REL: f64 = 1.0e-6;

const XGK: [f64; 15] = [
    -0.991_455_371_120_812_6,
    -0.949_107_912_342_758_5,
    -0.864_864_423_359_769_1,
    -0.741_531_185_599_394_4,
    -0.586_087_235_467_691_1,
    -0.405_845_151_377_397_2,
    -0.207_784_955_007_898_5,
    0.0,
    0.207_784_955_007_898_5,
    0.405_845_151_377_397_2,
    0.586_087_235_467_691_1,
    0.741_531_185_599_394_4,
    0.864_864_423_359_769_1,
    0.949_107_912_342_758_5,
    0.991_455_371_120_812_6,
];

const WGK: [f64; 15] = [
    0.022_935_322_010_529_22,
    0.063_092_092_629_978_55,
    0.104_790_010_322_250_2,
    0.140_653_259_715_525_9,
    0.169_004_726_639_267_9,
    0.190_350_578_064_785_4,
    0.204_432_940_075_298_9,
    0.209_482_141_084_727_8,
    0.204_432_940_075_298_9,
    0.190_350_578_064_785_4,
    0.169_004_726_639_267_9,
    0.140_653_259_715_525_9,
    0.104_790_010_322_250_2,
    0.063_092_092_629_978_55,
    0.022_935_322_010_529_22,
];

/// Gauss-Legendre weights of the nested 7-point rule (sum to 2).
const WG: [f64; 7] = [
    0.129_484_966_168_869_7,
    0.279_705_391_489_276_7,
    0.381_830_050_505_118_9,
    0.417_959_183_673_469_4,
    0.381_830_050_505_118_9,
    0.279_705_391_489_276_7,
    0.129_484_966_168_869_7,
];

/// 0-based positions in `XGK`/`WGK` of the 7-point Gauss nodes.
const IGAUSS: [usize; 7] = [1, 3, 5, 7, 9, 11, 13];

/// 15-point Gauss–Kronrod rule on `[a, b]`: `(result, abserr)`.
fn qk15<F: FnMut(f64) -> f64>(f: &mut F, a: f64, b: f64) -> (f64, f64) {
    let center = 0.5 * (a + b);
    let hlgth = 0.5 * (b - a);
    let mut fv = [0.0_f64; 15];
    for j in 0..15 {
        fv[j] = f(center + hlgth * XGK[j]);
    }
    let mut resk = 0.0;
    for j in 0..15 {
        resk += WGK[j] * fv[j];
    }
    let mut resg = 0.0;
    for j in 0..7 {
        resg += WG[j] * fv[IGAUSS[j]];
    }
    (resk * hlgth, (resk - resg).abs() * hlgth)
}

/// Adaptive 1-D integration with the 15-point Gauss–Kronrod rule; returns
/// `(result, abserr)`. Always bisects the subinterval with the largest error
/// until `err <= max(epsabs, epsrel*|result|)` or `MAXINT` intervals exist.
pub fn dqag_k15<F: FnMut(f64) -> f64>(
    f: &mut F,
    a: f64,
    b: f64,
    epsabs: f64,
    epsrel: f64,
) -> (f64, f64) {
    let mut alist = Vec::with_capacity(MAXINT);
    let mut blist = Vec::with_capacity(MAXINT);
    let mut rlist = Vec::with_capacity(MAXINT);
    let mut elist = Vec::with_capacity(MAXINT);

    let (r0, e0) = qk15(f, a, b);
    alist.push(a);
    blist.push(b);
    rlist.push(r0);
    elist.push(e0);
    let mut total_result = r0;
    let mut total_error = e0;
    let mut converged = total_error <= epsabs.max(epsrel * total_result.abs());

    while !converged && alist.len() < MAXINT {
        let mut maxind = 0;
        for i in 1..alist.len() {
            if elist[i] > elist[maxind] {
                maxind = i;
            }
        }
        let a1 = alist[maxind];
        let b1 = blist[maxind];
        let c = 0.5 * (a1 + b1);

        let (area1, err1) = qk15(f, a1, c);
        let (area2, err2) = qk15(f, c, b1);

        total_result -= rlist[maxind];
        total_error -= elist[maxind];

        alist[maxind] = a1;
        blist[maxind] = c;
        rlist[maxind] = area1;
        elist[maxind] = err1;

        alist.push(c);
        blist.push(b1);
        rlist.push(area2);
        elist.push(err2);

        total_result = total_result + area1 + area2;
        total_error = total_error + err1 + err2;

        converged = total_error <= epsabs.max(epsrel * total_result.abs());
    }
    (total_result, total_error)
}

/// Double integral of `f(x, y)` over `x ∈ [a, b]`, `y ∈ [glo(x), hhi(x)]`
/// (`TWODQ`): nested adaptive quadrature, inner absolute tolerance
/// `0.5*errabs/max(1, b-a)`.
pub fn twodq<F, G, H>(f: F, a: f64, b: f64, glo: G, hhi: H, errabs: f64, errrel: f64) -> (f64, f64)
where
    F: Fn(f64, f64) -> f64,
    G: Fn(f64) -> f64,
    H: Fn(f64) -> f64,
{
    let mut outer = |x: f64| -> f64 {
        let inner_epsabs = 0.5 * errabs / 1.0_f64.max(b - a);
        let mut inner = |y: f64| f(x, y);
        let (res, _err) = dqag_k15(&mut inner, glo(x), hhi(x), inner_epsabs, errrel);
        res
    };
    dqag_k15(&mut outer, a, b, errabs, errrel)
}

/// Geometry factor `g(a,b)` of two line segments by 2-D adaptive quadrature
/// (theory.md §4.2, "general position"). `a1`/`va`/`la` and `b1`/`vb`/`lb`
/// are start point, unit direction and length of each segment.
pub fn geometry_factor_2d(
    a1: &[f64; 3],
    va: &[f64; 3],
    la: f64,
    b1: &[f64; 3],
    vb: &[f64; 3],
    lb: f64,
    eps_rel: f64,
) -> f64 {
    let integrand = |x: f64, y: f64| -> f64 {
        let mut z = 0.0;
        for j in 0..3 {
            let a = a1[j] + va[j] * x;
            let b = b1[j] + vb[j] * y;
            let c = b - a;
            z += c * c;
        }
        1.0 / z.sqrt()
    };
    let errrel = la.min(lb) * eps_rel;
    let (res, _) = twodq(integrand, 0.0, la, |_| 0.0, |_| lb, 0.0, errrel);
    res
}

/// Internal impedance of a solid cylindrical conductor (theory.md §4.3):
///
/// `z_int = √(jωμ/σ)/(2πr₀) · I₀(ρ)/I₁(ρ)`, `ρ = r₀√(jωμσ)`, `Zint = z_int·l`.
///
/// For `|ρ| > 500` the Bessel ratio is taken as 1 (asymptotic limit), as in
/// the original implementation.
pub fn internal_impedance(radius: f64, length: f64, omega: f64, sigma: f64, mur: f64) -> Complex64 {
    let jw = Complex64::new(0.0, omega);
    let rho = radius * (jw * mur * MU0 * sigma).sqrt();
    let ratio = if rho.norm() > 500.0 {
        Complex64::new(1.0, 0.0)
    } else {
        i0_over_i1(rho)
    };
    let per_length = (jw * mur * MU0 / sigma).sqrt() / (2.0 * PI * radius) * ratio;
    per_length * length
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn integrates_a_polynomial_exactly() {
        let mut f = |x: f64| x * x;
        let (r, e) = dqag_k15(&mut f, 0.0, 3.0, 0.0, 1e-12);
        assert!((r - 9.0).abs() < 1e-13);
        assert!(e < 1e-12);
    }

    #[test]
    fn double_integral_of_one_is_area() {
        let (r, _) = twodq(|_, _| 1.0, 0.0, 2.0, |_| 0.0, |_| 3.0, 0.0, 1e-10);
        assert!((r - 6.0).abs() < 1e-12);
    }

    #[test]
    fn parallel_segments_match_closed_form_limit() {
        // two parallel unit segments 1 m apart, directly facing each other
        let g = geometry_factor_2d(
            &[0.0, 0.0, 0.0],
            &[1.0, 0.0, 0.0],
            1.0,
            &[0.0, 1.0, 0.0],
            &[1.0, 0.0, 0.0],
            1.0,
            1e-8,
        );
        // ∬ dx dy /√((x-y)²+1) = 2(asinh(1) - √2 + 1)
        let exact = 2.0 * (1.0_f64.asinh() - 2.0_f64.sqrt() + 1.0);
        assert!((g - exact).abs() < 1e-7, "{g} vs {exact}");
    }

    #[test]
    fn internal_impedance_low_frequency_is_dc_resistance() {
        let (r0, l, sigma) = (0.007, 1.0, 5.96e7);
        let z = internal_impedance(r0, l, 2.0 * PI * 1.0, sigma, 1.0);
        let rdc = l / (sigma * PI * r0 * r0);
        assert!((z.re - rdc).abs() / rdc < 1e-4, "{z} vs {rdc}");
    }

    #[test]
    fn internal_impedance_high_frequency_is_skin_limit() {
        let (r0, l, sigma) = (0.007, 1.0, 5.96e7);
        let omega = 2.0 * PI * 1.0e8;
        let z = internal_impedance(r0, l, omega, sigma, 1.0);
        // |rho| > 500 branch: z = sqrt(jωμ/σ)/(2π r0) · l, 45° phase
        let expect = (Complex64::new(0.0, omega) * MU0 / sigma).sqrt() / (2.0 * PI * r0) * l;
        assert!((z - expect).norm() / expect.norm() < 1e-12);
    }
}
