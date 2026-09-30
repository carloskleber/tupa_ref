#!/usr/bin/env julia
# Command-line driver: same arguments and output files as the Fortran
# executable (see julia/README.md). Works from any directory without
# `--project`: the package project is the parent of this `bin` directory.
pushfirst!(LOAD_PATH, normpath(joinpath(@__DIR__, "..")))

using Tupa

# `--plot` (Julia-only extra) needs Plots.jl from the default environment;
# loading it activates the TupaPlotsExt extension.
if "--plot" in ARGS && Base.find_package("Plots") !== nothing
    using Plots
end

exit(Tupa.main(ARGS))
