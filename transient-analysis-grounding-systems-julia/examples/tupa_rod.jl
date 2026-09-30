#!/usr/bin/env julia

include(joinpath(@__DIR__, "tupa_matched_common.jl"))

const FREQUENCIES = 10.0 .^ range(1, 6, length=41)
const LENGTH = 3.0
const RHO = 100.0

geometry = straight_geometry([0.0, 0.0, -0.5], [0.0, 0.0, -3.5], 1e-2, 6)
impedance = harmonic_impedance(geometry, FREQUENCIES; rho=RHO, epsr=10.0)
cases = [(label="3 m vertical rod", length_m=LENGTH, rho=RHO,
          frequencies=FREQUENCIES, impedance=impedance)]

output_root = joinpath(@__DIR__, "tupa_matched_rod_mhem")
write_comparison_csv(output_root * ".csv", cases)
plot_comparison_cases(output_root * ".png", cases;
    title="mHEM: Tupa-matched vertical rod")
