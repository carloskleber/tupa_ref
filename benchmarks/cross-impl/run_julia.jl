#!/usr/bin/env julia
# Benchmark driver for the Julia port: runs ONE case, writes its results in the
# the layout the plotting code reads: harmonic -> <case>_results.json
# (frequencies + derived.inputImpedance), transient -> tidy CSV (first observed node) and prints a machine-readable timing line.
#
#   julia --project=<repo>/julia run_julia.jl <repo-root> <out-dir> <case-name>
#
# Timings (seconds, in-process):  load = JSON read + assembly,
# cold = first solve (includes JIT compilation), warm = second solve.

using Printf
using Tupa

length(ARGS) == 3 || error("usage: run_julia.jl <repo-root> <out-dir> <case-name>")
root, outdir, name = abspath(ARGS[1]), abspath(ARGS[2]), ARGS[3]
mkpath(outdir)
path = joinpath(root, "common", name * ".json")

# `@elapsed` would hide assignments in a `let` scope, so time explicitly.
function timed(f)
    t0 = time_ns()
    v = f()
    return v, (time_ns() - t0) / 1e9
end

(study, input), t_load = timed(() -> load_study(path))
transient = haskey(input, :signal)

function harmonic(study, input)
    f = input[:frequencies]
    n = round(Int, Float64(f[:pointsPerDecade]) * log10(Float64(f[:max]) / Float64(f[:min]))) + 1
    freqs = 10 .^ range(log10(Float64(f[:min])), log10(Float64(f[:max])), length=max(n, 2))
    ids = String[x[:node] for x in input[:sources]]
    vals = ComplexF64[complex(Float64(x[:current][:re]), Float64(x[:current][:im])) for x in input[:sources]]
    return run_sweep(study, freqs, ids, vals), ids
end

if transient
    sig = input[:signal]
    r, t_cold = timed(() -> transient_response(study, sig))
    study2, input2 = load_study(path)
    _, t_warm = timed(() -> transient_response(study2, input2[:signal]))
    open(joinpath(outdir, name * "_transient_results.csv"), "w") do io
        println(io, "time_s,quantity,id,value")
        id = r.node_ids[1]
        for k in eachindex(r.time)
            @printf(io, "%.8e,voltage,%s,%.8e\n", r.time[k], id, r.voltage[1, k])
        end
    end
else
    (sw, ids), t_cold = timed(() -> harmonic(study, input))
    study2, input2 = load_study(path)
    _, t_warm = timed(() -> harmonic(study2, input2))
    idx = findfirst(p -> p.first == ids[1], study.nodes)
    i_src = complex(Float64(input[:sources][1][:current][:re]), Float64(input[:sources][1][:current][:im]))
    z = [sw.voltage[idx, k] / i_src for k in eachindex(sw.frequencies)]
    fmt(x) = @sprintf("%.8e", x)
    open(joinpath(outdir, name * "_results.json"), "w") do io
        println(io, "{\"frequencies\": [", join(fmt.(sw.frequencies), ", "), "],")
        println(io, "\"derived\": {\"inputImpedance\": [",
                join(["{\"re\": $(fmt(real(v))), \"im\": $(fmt(imag(v)))}" for v in z], ", "), "]}}")
    end
end

# "warm" re-loads the study so geometry preparation is included, like a fresh
# run of the compiled codes, but without JIT cost.
@printf("TIMING load=%.6f cold=%.6f warm=%.6f\n", t_load, t_cold, t_warm)
