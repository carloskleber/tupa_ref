#!/usr/bin/env julia
# Benchmark driver for the Julia port: runs ONE case, writes the same result
# files as the CLI (`<case>_results.{csv,json}` for a sweep,
# `<case>_transient_results.{csv,json}` for a transient) and prints a
# machine-readable timing line.
#
#   julia --project=<repo>/julia run_julia.jl <repo-root> <out-dir> <case-name>
#
# Timings (seconds, in-process):  load = JSON read + assembly + reference
# validation, cold = first solve (includes JIT compilation), warm = second
# solve of a freshly loaded study (geometry preparation included, no JIT).

using Printf
using Tupa

length(ARGS) == 3 || error("usage: run_julia.jl <repo-root> <out-dir> <case-name>")
root, outdir, name = abspath(ARGS[1]), abspath(ARGS[2]), ARGS[3]
mkpath(outdir)
path = joinpath(root, "common", name * ".json")
set_verbosity(VERB_QUIET)
Tupa.default_blas_threads!()   # as the CLI: one LAPACK thread unless OPENBLAS_NUM_THREADS is set

# `@elapsed` would hide assignments in a `let` scope, so time explicitly.
function timed(f)
    t0 = time_ns()
    v = f()
    return v, (time_ns() - t0) / 1e9
end

load() = validate_study_references!(load_study(path))

function solve(case)
    case.sources !== nothing && case.freq_hz !== nothing &&
        run_sweep!(case.study, case.freq_hz, case.sources)
    return case.transient === nothing ? nothing : transient_response(case.study, case.transient)
end

case, t_load = timed(load)
transient, t_cold = timed(() -> solve(case))
_, t_warm = timed(() -> solve(load()))

out(suffix) = joinpath(outdir, name * suffix)
if case.sources !== nothing && case.freq_hz !== nothing
    o = case.outputs
    write(out("_results.csv"), results_csv(case.study; nodes = o.nodes, electrodes = o.electrodes,
                                           quantities = o.quantities))
    write(out("_results.json"), results_json(case.study; nodes = o.nodes, electrodes = o.electrodes,
                                             quantities = o.quantities))
end
if transient !== nothing
    write(out("_transient_results.csv"), transient_csv(case.transient, transient))
    write(out("_transient_results.json"), transient_json(case.study.title, case.transient, transient))
end

@printf("TIMING load=%.6f cold=%.6f warm=%.6f\n", t_load, t_cold, t_warm)
