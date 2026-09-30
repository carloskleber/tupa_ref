#!/usr/bin/env julia

using Plots
using Plots.PlotMeasures

length(ARGS) >= 3 || error("usage: julia plot_tupa_mhem_grcev_l10.jl <solver-results.csv> <reference-results.csv> <output> [<output> ...]")

function read_numeric_csv(path)
    lines = readlines(path)
    header = split(first(lines), ',')
    columns = Dict(name => Float64[] for name in header)
    for line in Iterators.drop(lines, 1)
        values = split(line, ',')
        for (name, value) in zip(header, values)
            push!(columns[name], parse(Float64, value))
        end
    end
    columns
end

function read_solver_csv(path)
    lines = readlines(path)
    header = split(first(lines), ',')
    rows = NamedTuple[]
    for line in Iterators.drop(lines, 1)
        values = split(line, ',')
        row = Dict(zip(header, values))
        push!(rows, (solver=row["solver"], rho=parse(Float64, row["rho_ohm_m"]),
                     frequency=parse(Float64, row["frequency_hz"]),
                     magnitude=parse(Float64, row["z_magnitude_ohm"])))
    end
    rows
end

solver_rows = read_solver_csv(ARGS[1])
reference = read_numeric_csv(ARGS[2])
rhos = (30.0, 300.0, 3000.0)
panels = Any[]
for rho in rhos
    p = plot(xscale=:log10, yscale=:log10, xlims=(1e2, 1e7), grid=true, legend=:best,
             title="ρ = $(Int(rho)) Ω·m", xlabel="Frequency (Hz)", ylabel="|Zₕ| (Ω)")
    for (solver, color) in (("Tupa", "#0072B2"), ("mHEM", "#D55E00"))
        rows = filter(row -> row.solver == solver && row.rho == rho, solver_rows)
        plot!(p, getproperty.(rows, :frequency), getproperty.(rows, :magnitude),
              label=solver, color=color, linewidth=2)
    end
    indices = findall(==(rho), reference["rho_ohm_m"])
    scatter!(p, reference["frequency_hz"][indices], reference["reference_magnitude_ohm"][indices],
             label="Full-wave reference", markercolor=:white, markerstrokecolor=:black,
             markersize=3.5)
    push!(panels, p)

    e = plot(xscale=:log10, xlims=(1e2, 1e7), grid=true, legend=:best, xlabel="Frequency (Hz)",
             ylabel="Error vs reference (%)")
    plot!(e, reference["frequency_hz"][indices], reference["tupa_error_percent"][indices],
          label="Tupa", color="#0072B2", marker=:circle, markersize=2.5)
    plot!(e, reference["frequency_hz"][indices], reference["mhem_error_percent"][indices],
          label="mHEM", color="#D55E00", marker=:circle, markersize=2.5)
    hline!(e, [0.0], label="", color=:black, linewidth=0.8)
    push!(panels, e)
end

figure = plot(panels[1], panels[3], panels[5], panels[2], panels[4], panels[6];
              layout=(2, 3), size=(1500, 1000),
              left_margin=5mm, bottom_margin=7mm,
              plot_title="10 m horizontal electrode: Tupa and mHEM vs Grcev full-wave reference")
for path in ARGS[3:end]
    savefig(figure, path)
end
