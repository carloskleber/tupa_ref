//! Electrical material models (`mMaterial`): conductors and soil media.
//!
//! Every medium exposes the complex immittance `W(ω) = σ(ω) + jωε(ω)`
//! (theory.md §7); the propagation constant `γ = √(jωμW)` with `Re γ ≥ 0`
//! (theory.md §2) is shared.

use crate::ctes::{EPSILON0, MU0, PI};
use num_complex::Complex64;

/// Constant-parameter isotropic medium (conductors, air, simple soil).
#[derive(Debug, Clone, PartialEq)]
pub struct Linear {
    /// Identifier
    pub id: String,
    /// Relative permittivity εr
    pub epsilonr: f64,
    /// Relative permeability μr
    pub mur: f64,
    /// Conductivity σ (S/m)
    pub sigma: f64,
}

/// Lima–Portela minimum-phase power-law soil (ADR 0007), ω₀ = 2π·1 MHz.
#[derive(Debug, Clone, PartialEq)]
pub struct PortelaSoil {
    /// Identifier
    pub id: String,
    /// Relative permeability μr
    pub mur: f64,
    /// Low-frequency conductivity σ₀ (S/m)
    pub sigma0: f64,
    /// Power-law exponent α₀
    pub alpha0: f64,
    /// Dispersion magnitude at ω₀ (S/m)
    pub kr: f64,
}

/// Alipio–Visacro causal soil, *mean* parameter set (theory.md §7).
#[derive(Debug, Clone, PartialEq)]
pub struct AlipioVisacroSoil {
    /// Identifier
    pub id: String,
    /// Relative permeability μr
    pub mur: f64,
    /// Low-frequency (100 Hz) conductivity σ₀ (S/m)
    pub sigma0: f64,
}

/// A homogeneous half-space medium.
#[derive(Debug, Clone, PartialEq)]
pub enum Medium {
    /// Constant parameters
    Linear(Linear),
    /// Lima–Portela dispersive soil
    Portela(PortelaSoil),
    /// Alipio–Visacro dispersive soil
    AlipioVisacro(AlipioVisacroSoil),
}

impl Linear {
    /// Construct a linear material.
    pub fn new(id: impl Into<String>, epsilonr: f64, mur: f64, sigma: f64) -> Self {
        Self {
            id: id.into(),
            epsilonr,
            mur,
            sigma,
        }
    }

    /// `W(ω) = σ + jωεrε₀`
    pub fn admittance(&self, omega: f64) -> Complex64 {
        Complex64::new(self.sigma, omega * self.epsilonr * EPSILON0)
    }
}

impl PortelaSoil {
    /// `W(ω) = σ₀ + kr·[cot(πα₀/2) + j]·(ω/ω₀)^α₀` (ADR 0007)
    pub fn admittance(&self, omega: f64) -> Complex64 {
        let omega0 = 2.0 * PI * 1.0e6;
        Complex64::new(self.sigma0, 0.0)
            + self.kr
                * Complex64::new(1.0 / (0.5 * PI * self.alpha0).tan(), 1.0)
                * (omega / omega0).powf(self.alpha0)
    }
}

impl AlipioVisacroSoil {
    /// `W(ω) = σ₀ + Δσ(f)[1 + j tan(πξ/2)] + jωε₀ε∞` (theory.md §7, mean set)
    pub fn admittance(&self, omega: f64) -> Complex64 {
        const F0: f64 = 1.0e6;
        const XI: f64 = 0.54;
        const EPSR_INF: f64 = 12.0;
        let f = omega / (2.0 * PI);
        let h = 1.26 * (1.0e3 * self.sigma0).powf(-0.73);
        let dsigma = self.sigma0 * h * (f / F0).powf(XI);
        Complex64::new(
            self.sigma0 + dsigma,
            omega * EPSILON0 * EPSR_INF + dsigma * (0.5 * PI * XI).tan(),
        )
    }
}

impl Medium {
    /// Relative permeability μr
    pub fn mur(&self) -> f64 {
        match self {
            Medium::Linear(m) => m.mur,
            Medium::Portela(m) => m.mur,
            Medium::AlipioVisacro(m) => m.mur,
        }
    }

    /// Complex immittance `W(ω)` (S/m)
    pub fn admittance(&self, omega: f64) -> Complex64 {
        match self {
            Medium::Linear(m) => m.admittance(omega),
            Medium::Portela(m) => m.admittance(omega),
            Medium::AlipioVisacro(m) => m.admittance(omega),
        }
    }

    /// `γ = √(jωμ₀μrW(ω))`, `Re γ ≥ 0`
    pub fn propagation_constant(&self, omega: f64) -> Complex64 {
        (Complex64::new(0.0, omega) * self.mur() * MU0 * self.admittance(omega)).sqrt()
    }

    /// One-line human-readable description.
    pub fn report(&self) -> String {
        match self {
            Medium::Linear(_) => "linear material".to_string(),
            Medium::Portela(m) => format!(
                "Lima-Portela dispersive soil {}: sigma0={:.3e} S/m, alpha0={:.4}, kr={:.3e} S/m, mur={:.3}",
                m.id, m.sigma0, m.alpha0, m.kr, m.mur
            ),
            Medium::AlipioVisacro(m) => format!(
                "Alipio-Visacro dispersive soil {}: sigma0={:.3e} S/m (mean parameter set), mur={:.3}",
                m.id, m.sigma0, m.mur
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn linear_low_frequency_limit_is_sigma() {
        let m = Linear::new("s", 10.0, 1.0, 0.01);
        let w = m.admittance(1e-9);
        assert!((w.re - 0.01).abs() < 1e-15);
    }

    #[test]
    fn portela_converges_to_resistive_soil() {
        let p = PortelaSoil {
            id: "s".into(),
            mur: 1.0,
            sigma0: 0.01,
            alpha0: 0.7,
            kr: 1e-3,
        };
        let w = p.admittance(1e-6);
        assert!((w.re - 0.01).abs() < 1e-9 && w.im.abs() < 1e-9);
    }

    #[test]
    fn alipio_converges_to_resistive_soil() {
        let a = AlipioVisacroSoil {
            id: "s".into(),
            mur: 1.0,
            sigma0: 0.01,
        };
        let w = a.admittance(1e-6);
        assert!((w.re - 0.01).abs() < 1e-7 && w.im.abs() < 1e-7);
    }

    #[test]
    fn propagation_constant_has_nonnegative_real_part() {
        let m = Medium::Linear(Linear::new("s", 10.0, 1.0, 0.01));
        for f in [1.0, 1e3, 1e6, 1e8] {
            let g = m.propagation_constant(2.0 * PI * f);
            assert!(g.re >= 0.0);
        }
    }

    #[test]
    fn propagation_constant_lossless_air_is_omega_over_c() {
        let m = Medium::Linear(Linear::new("air", 1.0, 1.0, 0.0));
        let omega = 2.0 * PI * 1e6;
        let g = m.propagation_constant(omega);
        assert!((g.im - omega / crate::ctes::C).abs() / g.im < 1e-12);
        assert!(g.re.abs() < 1e-12);
    }
}
