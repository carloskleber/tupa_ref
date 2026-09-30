#!/usr/bin/env julia

using Plots
using Plots.PlotMeasures

length(ARGS) >= 5 || error(
    "usage: julia plot_tupa_mhem_aligned_cases.jl <grcev.csv> <tupa-rod.csv> <mhem-rod.csv> <mom-reference.csv> <output> [<output> ...]",
)

function read_solver_csv(path)
    lines = readlines(path)
    header = split(first(lines), ',')
    rows = NamedTuple[]
    for line in Iterators.drop(lines, 1)
        row = Dict(zip(header, split(line, ',')))
        push!(rows, (
            solver=row["solver"],
            length_m=parse(Float64, row["length_m"]),
            rho=parse(Float64, row["rho_ohm_m"]),
            frequency=parse(Float64, row["frequency_hz"]),
            magnitude=parse(Float64, row["z_magnitude_ohm"]),
            phase=parse(Float64, row["z_phase_deg"]),
        ))
    end
    rows
end

function read_numeric_csv(path)
    lines = readlines(path)
    header = split(first(lines), ',')
    columns = Dict(name => Float64[] for name in header)
    for line in Iterators.drop(lines, 1)
        for (name, value) in zip(header, split(line, ','))
            push!(columns[name], parse(Float64, value))
        end
    end
    columns
end

rows = vcat(read_solver_csv(ARGS[1]), read_solver_csv(ARGS[2]), read_solver_csv(ARGS[3]))
mom = read_numeric_csv(ARGS[4])
cases = (
    (length_m=10.0, rho=30.0, title="10 m horizontal, ρ = 30 Ω·m", has_mom=true),
    (length_m=10.0, rho=300.0, title="10 m horizontal, ρ = 300 Ω·m", has_mom=true),
    (length_m=10.0, rho=3000.0, title="10 m horizontal, ρ = 3000 Ω·m", has_mom=true),
    (length_m=3.0, rho=100.0, title="3 m vertical rod, ρ = 100 Ω·m", has_mom=false),
)

magnitude_panels = Any[]
difference_panels = Any[]
for case in cases
    tupa = filter(row -> row.solver == "Tupa" && row.length_m == case.length_m && row.rho == case.rho, rows)
    mhem = filter(row -> row.solver == "mHEM" && row.length_m == case.length_m && row.rho == case.rho, rows)
    isempty(tupa) && error("missing Tupa results for $(case.title)")
    length(tupa) == length(mhem) || error("frequency count mismatch for $(case.title)")

    frequencies = getproperty.(tupa, :frequency)
    tupa_magnitude = getproperty.(tupa, :magnitude)
    mhem_magnitude = getproperty.(mhem, :magnitude)
    frequency_limits = extrema(frequencies)

    magnitude = plot(
        frequencies, tupa_magnitude;
        label="Tupa", color="#0072B2", linewidth=2,
        xscale=:log10, yscale=:log10, xlims=frequency_limits,
        title=case.title, xlabel="Frequency (Hz)", ylabel="|Zₕ| (Ω)",
        grid=true, legend=:best,
    )
    plot!(magnitude, frequencies, mhem_magnitude;
          label="mHEM", color="#D55E00", linewidth=2, linestyle=:dash)
    if case.has_mom
        indices = findall(==(case.rho), mom["rho_ohm_m"])
        scatter!(magnitude, mom["frequency_hz"][indices],
                 mom["reference_magnitude_ohm"][indices];
                 label="Full-wave MoM", markercolor=:white,
                 markerstrokecolor=:black, markersize=3.5)
    end
    push!(magnitude_panels, magnitude)

    if case.has_mom
        indices = findall(==(case.rho), mom["rho_ohm_m"])
        difference = plot(
            mom["frequency_hz"][indices], mom["tupa_error_percent"][indices];
            label="Tupa vs MoM", color="#0072B2", marker=:circle,
            markersize=2.5, linewidth=1.5, xscale=:log10,
            xlims=frequency_limits, xlabel="Frequency (Hz)",
            ylabel="Error vs MoM (%)", grid=true, legend=:best,
        )
        plot!(difference, mom["frequency_hz"][indices], mom["mhem_error_percent"][indices];
              label="mHEM vs MoM", color="#D55E00", marker=:circle,
              markersize=2.5, linewidth=1.5)
    else
        relative_difference = 100 .* (tupa_magnitude .- mhem_magnitude) ./ mhem_magnitude
        difference = plot(
            frequencies, relative_difference;
            label="(Tupa − mHEM) / mHEM", color="#009E73", linewidth=2,
            xscale=:log10, xlims=frequency_limits,
            xlabel="Frequency (Hz)", ylabel="Magnitude difference (%)",
            grid=true, legend=:best,
        )
    end
    hline!(difference, [0.0]; label="", color=:black, linewidth=0.8)
    push!(difference_panels, difference)
end

figure = plot(
    magnitude_panels..., difference_panels...;
    layout=(2, 4), size=(2000, 1000), left_margin=8mm, bottom_margin=7mm,
    plot_title="Tupa and mHEM compared with full-wave MoM results",
)
for path in ARGS[5:end]
    savefig(figure, path)
end
