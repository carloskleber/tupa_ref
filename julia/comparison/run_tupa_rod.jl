#!/usr/bin/env julia

using Printf
using Tupa

length(ARGS) == 2 || error("usage: julia run_tupa_rod.jl <repository-root> <output.csv>")

const FREQUENCIES = 10.0 .^ range(1, 6, length=41)
study, _ = load_study(joinpath(abspath(ARGS[1]), "common", "rod.json"))
sweep = run_sweep(study, FREQUENCIES, ["Node_1"], [1 + 0im])
impedance = vec(sweep.voltage[1, :])

open(ARGS[2], "w") do io
    println(io, "solver,length_m,rho_ohm_m,frequency_hz,z_real_ohm,z_imag_ohm,z_magnitude_ohm,z_phase_deg")
    for (frequency, z) in zip(FREQUENCIES, impedance)
        @printf(io, "Tupa,3.0,100.0,%.16e,%.16e,%.16e,%.16e,%.16e\n",
                frequency, real(z), imag(z), abs(z), rad2deg(angle(z)))
    end
end
