# Physical constants (`mCtes`). Same definitions as the Fortran and Rust
# code: μ₀ = 4π·10⁻⁷ H/m exactly and ε₀ = 1/(μ₀c²).

"Permeability of free space μ₀ (H/m)"
const MU0 = 4.0e-7 * π
"Speed of light in vacuum (m/s)"
const C0 = 299_792_458.0
"Permittivity of free space ε₀ (F/m)"
const EPSILON0 = 1.0 / (MU0 * C0 * C0)
"4π, frequent in electromagnetic potential kernels"
const FOUR_PI = 4.0 * π
