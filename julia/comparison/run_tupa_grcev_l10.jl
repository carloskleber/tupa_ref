#!/usr/bin/env julia
# Grcev et al. 2018 Fig. 12, ℓ = 10 m: |Z(f)| of the Julia port on a dense
# 401-point axis (100 Hz–10 MHz) for docs/validation/tupa-vs-mhem.md.

using Printf
using Tupa

length(ARGS) == 2 || error("usage: julia run_tupa_grcev_l10.jl <repository-root> <output.csv>")
const FREQUENCIES = 10.0 .^ range(2, 7, length=401)
const CASES = ((30.0, "grcev_fig12_l10_rho30.json"),
               (300.0, "grcev_fig12_l10_rho300.json"),
               (3000.0, "grcev_fig12_l10_rho3000.json"))

set_verbosity(VERB_QUIET)
open(ARGS[2], "w") do io
    println(io, "solver,length_m,rho_ohm_m,frequency_hz,z_real_ohm,z_imag_ohm,z_magnitude_ohm,z_phase_deg")
    for (rho, filename) in CASES
        case = validate_study_references!(load_study(joinpath(abspath(ARGS[1]), "common", filename)))
        elapsed = @elapsed run_sweep!(case.study, FREQUENCIES, [Source("Node_1", 1.0)])
        @printf("Tupa rho=%.0f: sweep %.3f s\n", rho, elapsed)
        for (frequency, z) in zip(FREQUENCIES, input_impedance(case.study, "Node_1"))
            @printf(io, "Tupa,10.0,%.1f,%.16e,%.16e,%.16e,%.16e,%.16e\n",
                    rho, frequency, real(z), imag(z), abs(z), rad2deg(angle(z)))
        end
    end
end
