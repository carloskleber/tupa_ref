//! Dense complex linear algebra: row-major matrix and LU with partial
//! pivoting and multiple right-hand sides (ADR 0003, ADR 0016).
//!
//! Pivot selection uses `|re| + |im|` (LAPACK `izamax`'s `cabs1`), so the
//! elimination order matches `ZGESV`.

use crate::error::{Result, TupaError};
use num_complex::Complex64;

/// Dense row-major complex matrix.
#[derive(Debug, Clone, PartialEq)]
pub struct CMatrix {
    rows: usize,
    cols: usize,
    data: Vec<Complex64>,
}

impl CMatrix {
    /// All-zero matrix.
    pub fn zeros(rows: usize, cols: usize) -> Self {
        Self {
            rows,
            cols,
            data: vec![Complex64::new(0.0, 0.0); rows * cols],
        }
    }

    /// Number of rows.
    pub fn rows(&self) -> usize {
        self.rows
    }

    /// Number of columns.
    pub fn cols(&self) -> usize {
        self.cols
    }

    /// Element `(i, j)`, 0-based.
    #[inline]
    pub fn get(&self, i: usize, j: usize) -> Complex64 {
        self.data[i * self.cols + j]
    }

    /// Set element `(i, j)`, 0-based.
    #[inline]
    pub fn set(&mut self, i: usize, j: usize, v: Complex64) {
        self.data[i * self.cols + j] = v;
    }

    /// Mutable reference to element `(i, j)`.
    #[inline]
    pub fn at_mut(&mut self, i: usize, j: usize) -> &mut Complex64 {
        &mut self.data[i * self.cols + j]
    }

    /// `self * x` for a column vector `x`.
    pub fn mul_vec(&self, x: &[Complex64]) -> Vec<Complex64> {
        (0..self.rows)
            .map(|i| {
                let mut s = Complex64::new(0.0, 0.0);
                for (j, xj) in x.iter().enumerate() {
                    s += self.get(i, j) * *xj;
                }
                s
            })
            .collect()
    }
}

#[inline]
fn cabs1(z: Complex64) -> f64 {
    z.re.abs() + z.im.abs()
}

/// Solve `A X = B` in place (`a` is overwritten by its LU factors, `b` by
/// the solution). Fails on an exactly singular pivot (`ZGESV INFO > 0`).
pub fn solve_in_place(a: &mut CMatrix, b: &mut CMatrix) -> Result<()> {
    let n = a.rows;
    if a.cols != n || b.rows != n {
        return Err(TupaError::new("solve: dimension mismatch"));
    }
    let nrhs = b.cols;

    for k in 0..n {
        // partial pivoting
        let mut p = k;
        let mut best = cabs1(a.get(k, k));
        for i in (k + 1)..n {
            let v = cabs1(a.get(i, k));
            if v > best {
                best = v;
                p = i;
            }
        }
        if best == 0.0 {
            return Err(TupaError::new(format!(
                "solve: singular matrix (zero pivot at column {})",
                k + 1
            )));
        }
        if p != k {
            for j in 0..n {
                a.data.swap(k * n + j, p * n + j);
            }
            for j in 0..nrhs {
                b.data.swap(k * nrhs + j, p * nrhs + j);
            }
        }
        let pivot = a.get(k, k);
        for i in (k + 1)..n {
            let m = a.get(i, k) / pivot;
            a.set(i, k, m);
            if m.re == 0.0 && m.im == 0.0 {
                continue;
            }
            for j in (k + 1)..n {
                let akj = a.data[k * n + j];
                a.data[i * n + j] -= m * akj;
            }
            for j in 0..nrhs {
                let bkj = b.data[k * nrhs + j];
                b.data[i * nrhs + j] -= m * bkj;
            }
        }
    }

    // back substitution
    for j in 0..nrhs {
        for i in (0..n).rev() {
            let mut s = b.data[i * nrhs + j];
            for l in (i + 1)..n {
                s -= a.data[i * n + l] * b.data[l * nrhs + j];
            }
            b.data[i * nrhs + j] = s / a.data[i * n + i];
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn solves_small_complex_system() {
        let mut a = CMatrix::zeros(2, 2);
        a.set(0, 0, Complex64::new(0.0, 1.0));
        a.set(0, 1, Complex64::new(2.0, 0.0));
        a.set(1, 0, Complex64::new(3.0, 0.0));
        a.set(1, 1, Complex64::new(1.0, -1.0));
        let a0 = a.clone();
        let x_true = [Complex64::new(1.0, 2.0), Complex64::new(-0.5, 0.25)];
        let rhs = a0.mul_vec(&x_true);
        let mut b = CMatrix::zeros(2, 1);
        for (i, r) in rhs.iter().enumerate() {
            b.set(i, 0, *r);
        }
        solve_in_place(&mut a, &mut b).unwrap();
        for (i, x) in x_true.iter().enumerate() {
            assert!((b.get(i, 0) - x).norm() < 1e-14);
        }
    }

    #[test]
    fn singular_matrix_is_an_error() {
        let mut a = CMatrix::zeros(2, 2);
        let mut b = CMatrix::zeros(2, 1);
        assert!(solve_in_place(&mut a, &mut b).is_err());
    }
}
