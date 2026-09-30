#!/usr/bin/env julia

include(joinpath(@__DIR__, "tupa_matched_common.jl"))

const FREQUENCIES = 10.0 .^ range(2, 7, length=401)
const RESISTIVITIES = (30.0, 300.0, 3000.0)
const LENGTH = 10.0

geometry = straight_geometry([0.0, 0.0, -0.5], [LENGTH, 0.0, -0.5], 7e-3, 40)
cases = [
    (label="ρ = $(Int(rho)) Ω·m", length_m=LENGTH, rho=rho,
     frequencies=FREQUENCIES,
     impedance=harmonic_impedance(geometry, FREQUENCIES; rho=rho, epsr=10.0))
    for rho in RESISTIVITIES
]

output_root = joinpath(@__DIR__, "tupa_matched_grcev_l10_mhem")
write_comparison_csv(output_root * ".csv", cases)
plot_comparison_cases(output_root * ".png", cases;
    title="mHEM: Tupa-matched 10 m horizontal electrode")
