//! Physical constants (`mCtes`).

use num_complex::Complex64;

/// π
pub const PI: f64 = std::f64::consts::PI;
/// 4π — frequent in electromagnetic potential kernels
pub const FOUR_PI: f64 = 4.0 * PI;
/// Permeability of free space μ₀ (H/m)
pub const MU0: f64 = 4.0e-7 * PI;
/// Speed of light in vacuum (m/s)
pub const C: f64 = 299_792_458.0;
/// Permittivity of free space ε₀ (F/m)
pub const EPSILON0: f64 = 1.0 / (MU0 * C * C);

/// Complex zero
pub const ZERO: Complex64 = Complex64::new(0.0, 0.0);
/// Complex one
pub const ONE: Complex64 = Complex64::new(1.0, 0.0);
