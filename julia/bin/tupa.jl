#!/usr/bin/env julia
# Make this launcher work from any directory without requiring callers to pass
# `--project`. The package project is the parent of this `bin` directory.
pushfirst!(LOAD_PATH, normpath(joinpath(@__DIR__, "..")))

using Tupa
isempty(ARGS) && error("usage: julia bin/tupa.jl <study.json>")
Tupa.run_file(ARGS[end])
