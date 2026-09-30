# Electrical material models (`mMaterial`): conductors and soil media.
#
# Every medium exposes the complex immittance W(ω) = σ(ω) + jωε(ω)
# (theory.md §7); the propagation constant γ = √(jωμW) with Re γ ≥ 0
# (theory.md §2) is shared.

"A homogeneous medium: conductor, air or soil half-space."
abstract type Medium end

"Constant-parameter isotropic medium (conductors, air, simple soil)."
struct Linear <: Medium
    id::String
    "Relative permittivity εr"
    epsilonr::Float64
    "Relative permeability μr"
    mur::Float64
    "Conductivity σ (S/m)"
    sigma::Float64
end

"Lima–Portela minimum-phase power-law soil (ADR 0007), ω₀ = 2π·1 MHz."
struct PortelaSoil <: Medium
    id::String
    "Relative permeability μr"
    mur::Float64
    "Low-frequency conductivity σ₀ (S/m)"
    sigma0::Float64
    "Power-law exponent α₀"
    alpha0::Float64
    "Dispersion magnitude at ω₀ (S/m)"
    kr::Float64
end

"Alipio–Visacro causal soil, *mean* parameter set (theory.md §7)."
struct AlipioVisacroSoil <: Medium
    id::String
    "Relative permeability μr"
    mur::Float64
    "Low-frequency (100 Hz) conductivity σ₀ (S/m)"
    sigma0::Float64
end

"`W(ω) = σ + jωεrε₀`"
admittance(m::Linear, omega::Real) = complex(m.sigma, omega * m.epsilonr * EPSILON0)

"`W(ω) = σ₀ + kr·[cot(πα₀/2) + j]·(ω/ω₀)^α₀` (ADR 0007)"
function admittance(m::PortelaSoil, omega::Real)
    omega0 = 2.0 * π * 1.0e6
    return complex(m.sigma0, 0.0) +
           m.kr * complex(1.0 / tan(0.5 * π * m.alpha0), 1.0) * (omega / omega0)^m.alpha0
end

"`W(ω) = σ₀ + Δσ(f)[1 + j tan(πξ/2)] + jωε₀ε∞` (theory.md §7, mean set)"
function admittance(m::AlipioVisacroSoil, omega::Real)
    f0, xi, epsr_inf = 1.0e6, 0.54, 12.0
    f = omega / (2.0 * π)
    h = 1.26 * (1.0e3 * m.sigma0)^(-0.73)
    dsigma = m.sigma0 * h * (f / f0)^xi
    return complex(m.sigma0 + dsigma, omega * EPSILON0 * epsr_inf + dsigma * tan(0.5 * π * xi))
end

"Relative permeability μr"
permeability(m::Medium) = m.mur

"`γ = √(jωμ₀μrW(ω))`, `Re γ ≥ 0`"
propagation_constant(m::Medium, omega::Real) =
    sqrt(complex(0.0, omega) * m.mur * MU0 * admittance(m, omega))

"One-line human-readable description."
report(::Linear) = "linear material"
report(m::PortelaSoil) =
    @sprintf("Lima-Portela dispersive soil %s: sigma0=%.3e S/m, alpha0=%.4f, kr=%.3e S/m, mur=%.3f",
             m.id, m.sigma0, m.alpha0, m.kr, m.mur)
report(m::AlipioVisacroSoil) =
    @sprintf("Alipio-Visacro dispersive soil %s: sigma0=%.3e S/m (mean parameter set), mur=%.3f",
             m.id, m.sigma0, m.mur)
