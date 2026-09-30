//! Special functions not in `std`: complementary error function.

use crate::ctes::PI;

/// `erfc(x)` to ≈ 1e-15 absolute. Series for `|x| < 2.5`, Lentz continued
/// fraction for the tail, reflection for negative `x`.
pub fn erfc(x: f64) -> f64 {
    if x.is_nan() {
        return f64::NAN;
    }
    if x < 0.0 {
        return 2.0 - erfc(-x);
    }
    if x < 2.5 {
        // erf(x) = 2/√π Σ (-1)^n x^(2n+1) / (n! (2n+1))
        let mut term = x;
        let mut sum = x;
        let x2 = x * x;
        let mut n = 0.0_f64;
        loop {
            n += 1.0;
            term *= -x2 / n;
            let add = term / (2.0 * n + 1.0);
            sum += add;
            if add.abs() < 1e-17 * sum.abs().max(1e-300) {
                break;
            }
            if n > 200.0 {
                break;
            }
        }
        return 1.0 - 2.0 / PI.sqrt() * sum;
    }
    if x > 27.0 {
        return 0.0;
    }
    // erfc(x) = e^{-x²}/√π · 1/(x + (1/2)/(x + 1/(x + (3/2)/(x + ...))))
    let tiny = 1e-300;
    let mut f = x;
    let mut c = x;
    let mut d = 0.0;
    for k in 1..500 {
        let a = 0.5 * k as f64;
        d = x + a * d;
        if d == 0.0 {
            d = tiny;
        }
        c = x + a / c;
        if c == 0.0 {
            c = tiny;
        }
        d = 1.0 / d;
        let delta = c * d;
        f *= delta;
        if (delta - 1.0).abs() < 1e-16 {
            break;
        }
    }
    (-x * x).exp() / PI.sqrt() / f
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn known_values() {
        // reference values (mpmath)
        let cases = [
            (0.0, 1.0),
            (0.5, 0.479_500_122_186_953_5),
            (1.0, 0.157_299_207_050_285_13),
            (2.0, 0.004_677_734_981_047_266),
            (3.0, 2.209_049_699_858_544e-5),
            (4.0, 1.541_725_790_028_002e-8),
            (-1.0, 1.842_700_792_949_715),
        ];
        for (x, e) in cases {
            let v = erfc(x);
            assert!(
                (v - e).abs() <= 1e-13 * e.abs().max(1e-3) + 1e-16,
                "erfc({x}) = {v}, want {e}"
            );
            assert!((v - e).abs() / e < 1e-11, "rel erfc({x})");
        }
    }

    #[test]
    fn extremes() {
        assert_eq!(erfc(30.0), 0.0);
        assert!((erfc(-16.0) - 2.0).abs() < 1e-15);
    }
}
