//! In-repo radix-2 FFT (`mFft`, ADR 0014): iterative decimation with a
//! bit-reversal permutation, ported so the operation order matches Fortran.
//! Forward kernel `e^{-jθ}`, inverse `e^{+jθ}` scaled by `1/N`.

use crate::ctes::PI;
use crate::error::{Result, TupaError};
use num_complex::Complex64;

/// `n > 0` and a power of two.
pub fn is_power_of_two(n: usize) -> bool {
    n > 0 && (n & (n - 1)) == 0
}

/// Smallest power of two ≥ `n`.
pub fn next_power_of_two(n: usize) -> usize {
    let mut p = 1;
    while p < n {
        p *= 2;
    }
    p
}

/// In-place forward transform.
pub fn fft_forward(x: &mut [Complex64]) -> Result<()> {
    fft_core(x, -1.0)
}

/// In-place inverse transform (scaled by `1/N`).
pub fn fft_inverse(x: &mut [Complex64]) -> Result<()> {
    fft_core(x, 1.0)?;
    let n = x.len() as f64;
    for v in x.iter_mut() {
        *v /= n;
    }
    Ok(())
}

fn fft_core(x: &mut [Complex64], sgn: f64) -> Result<()> {
    let n = x.len();
    if n <= 1 {
        return Ok(());
    }
    if !is_power_of_two(n) {
        return Err(TupaError::new("mFft: array length must be a power of two"));
    }

    // bit-reversal permutation (1-indexed j as in the Fortran source)
    let mut j = 1usize;
    for i in 1..=n {
        if j > i {
            x.swap(j - 1, i - 1);
        }
        let mut m = n / 2;
        while m >= 2 && j > m {
            j -= m;
            m /= 2;
        }
        j += m;
    }

    let mut mmax = 1usize;
    while mmax < n {
        let istep = 2 * mmax;
        for m in 1..=mmax {
            let theta = sgn * PI * (m - 1) as f64 / mmax as f64;
            let w = Complex64::new(theta.cos(), theta.sin());
            let mut i = m;
            while i <= n {
                let jj = i + mmax;
                let t = w * x[jj - 1];
                x[jj - 1] = x[i - 1] - t;
                x[i - 1] += t;
                i += istep;
            }
        }
        mmax = istep;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn dft(x: &[Complex64]) -> Vec<Complex64> {
        let n = x.len();
        (0..n)
            .map(|k| {
                let mut s = Complex64::new(0.0, 0.0);
                for (j, xj) in x.iter().enumerate() {
                    let th = -2.0 * PI * (j * k) as f64 / n as f64;
                    s += *xj * Complex64::new(th.cos(), th.sin());
                }
                s
            })
            .collect()
    }

    #[test]
    fn matches_naive_dft() {
        let x: Vec<Complex64> = (0..16)
            .map(|k| Complex64::new((k as f64 * 0.7).sin(), (k as f64 * 0.3).cos()))
            .collect();
        let mut y = x.clone();
        fft_forward(&mut y).unwrap();
        let d = dft(&x);
        for (a, b) in y.iter().zip(&d) {
            assert!((a - b).norm() < 1e-12);
        }
    }

    #[test]
    fn round_trip() {
        let x: Vec<Complex64> = (0..64)
            .map(|k| Complex64::new(k as f64, -(k as f64) / 3.0))
            .collect();
        let mut y = x.clone();
        fft_forward(&mut y).unwrap();
        fft_inverse(&mut y).unwrap();
        for (a, b) in y.iter().zip(&x) {
            assert!((a - b).norm() < 1e-11);
        }
    }

    #[test]
    fn rejects_non_power_of_two() {
        let mut x = vec![Complex64::new(1.0, 0.0); 6];
        assert!(fft_forward(&mut x).is_err());
        assert!(is_power_of_two(1024) && !is_power_of_two(1000));
        assert_eq!(next_power_of_two(1000), 1024);
    }

    #[test]
    fn impulse_has_flat_spectrum() {
        let mut x = vec![Complex64::new(0.0, 0.0); 8];
        x[0] = Complex64::new(1.0, 0.0);
        fft_forward(&mut x).unwrap();
        for v in x {
            assert!((v - Complex64::new(1.0, 0.0)).norm() < 1e-14);
        }
    }
}
