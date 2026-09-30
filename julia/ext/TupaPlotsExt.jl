# Optional transient plot (Julia-only extra, `--plot`): loaded automatically
# when Plots.jl is loaded alongside Tupa. The reference outputs are the
# CSV/JSON files; the GUI (ADR 0011) plots those for every implementation.
module TupaPlotsExt

using Plots
using Tupa: Tupa, TransientSpec, TransientResult

function Tupa.write_transient_plot(path::AbstractString, spec::TransientSpec, r::TransientResult)
    time_us = r.t .* 1e6
    current_plot = plot(time_us, r.injected_current ./ 1e3; xlabel = "Time (μs)",
                        ylabel = "Current (kA)", label = "Injected current", linewidth = 2,
                        grid = true, legend = :topright)
    voltage_plot = plot(; xlabel = "Time (μs)", ylabel = "Voltage (kV)", grid = true,
                        legend = :topright)
    for (i, id) in enumerate(spec.observe_nodes)
        plot!(voltage_plot, time_us, r.node_responses[i, :] ./ 1e3; label = "Voltage $id",
              linewidth = 2)
    end
    figure = plot(current_plot, voltage_plot; layout = (2, 1), size = (900, 700),
                  plot_title = "TUPÃ transient response")
    savefig(figure, path)
    return path
end

end # module
