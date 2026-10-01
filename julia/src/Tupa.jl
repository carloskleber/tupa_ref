"""
    Tupa

TUPÃ — Julia implementation of the Hybrid Electromagnetic Model (HEM).

An implementation of the public contract (JSON schema v1 + `common/` cases,
ADR 0002/0018) that mirrors the Fortran reference module by module, like the
Rust port (ROADMAP Phase 8), so a reviewer can audit kernel against kernel.
`Float64` and `ComplexF64` throughout.

File ↔ Fortran module: `Ctes`, `Error`, `Verbosity`, `Material`, `Node`,
`Electrode`, `element/{Element,Line,Mesh}`, `Structure`, `Geometry`,
`GeometryCache`, `Impedance`, `Mesh`, `Result`, `Study`, `Signal`, `Fft`,
`Transient`, `ResultsWriter`, `JsonParser` (`mJsonParser` + the `mTupa`
loader); the driver below is `mTupa`/`app/main.f90`.
"""
module Tupa

using LinearAlgebra
using Printf
import JSON
import SpecialFunctions
using PrecompileTools: @setup_workload, @compile_workload

include("Ctes.jl")
include("Error.jl")
include("Verbosity.jl")
include("Material.jl")
include("Node.jl")
include("Electrode.jl")
include("element/Element.jl")
include("Structure.jl")
include("element/Line.jl")
include("element/Mesh.jl")
include("element/Catenary.jl")
include("GeometryCache.jl")
include("Impedance.jl")
include("Geometry.jl")
include("Mesh.jl")
include("Result.jl")
include("Study.jl")
include("Signal.jl")
include("Fft.jl")
include("Transient.jl")
include("ResultsWriter.jl")
include("JsonParser.jl")

# errors, verbosity
export TupaError, set_verbosity, VERB_QUIET, VERB_NORMAL, VERB_VERBOSE
# object model
export Linear, PortelaSoil, AlipioVisacroSoil, admittance, propagation_constant,
       Node, Electrode, Line, MeshElement, Catenary, Structure, add_node!, add_material!,
       add_element!, assemble!, find_node_index, find_electrode_index
# numerics
export GeometryOptions, build_geometry_matrices, dqag_k15, internal_impedance
# study, sweep, results
export Study, Source, prepare!, solve_frequency!, run_sweep!, input_impedance,
       max_voltage_magnitude, log_frequency_axis, dump_structure
export results_csv, results_json, transient_csv, transient_json, transient_signals_csv,
       transient_signals_json, fmt_real
# time domain
export heidler_signal, heidler_signal_terms, double_exp_signal, portela_signal, waveform,
       tail_taper, fft_forward!, fft_inverse!, sample_time_axis,
       one_sided_frequency_axis, tukey_antialias_filter, TransientSpec,
       transient_response, transient_signals, write_transient_plot
# JSON loader and driver
export load_study, load_study_string, validate_study_references!,
       RunOptions, run_from_file, run_study_from_file

"""
    write_transient_plot(path, spec, result)

Save the injected current and observed node voltages of a transient result
as an image. Julia-only extra, implemented by the `TupaPlotsExt` package
extension: available once `Plots` is loaded (`using Plots`).
"""
function write_transient_plot end

"""
Options of the command-line driver (`fortran/app/main.f90` plus the Rust
port's `--dump-structure`/`--output-dir` and the Julia-only `--plot`).
"""
Base.@kwdef struct RunOptions
    "Geometry/quadrature options (`--epsrel`, `--no-cache`, `--kernel`)"
    geometry::GeometryOptions = GeometryOptions()
    "Image model of studies that do not state one (`--image-model`): `:frequency_dependent` or `:ideal`"
    image_model::Symbol = :frequency_dependent
    "Print the assembled nodes/electrodes and stop before any physics"
    dump_structure::Bool = false
    "Directory the result files are written to (`nothing`: current directory)"
    output_dir::Union{Nothing,String} = nothing
    "Also write `<case>_transient_plot.png` (needs `Plots`, see `write_transient_plot`)"
    plot::Bool = false
end

"""
    run_from_file(filename; options = RunOptions())

Load a case, run the sweep and/or transient it asks for and write
`<base>_results.{csv,json}` / `<base>_transient_results.{csv,json}` (same
names and layout as the Fortran executable). Returns the loaded case and the
transient result (`nothing` when the case has no `signal` block; for a list of
independent signals, ADR 0026, the first signal's — the files hold them all).
"""
function run_from_file(filename::AbstractString; options::RunOptions = RunOptions())
    start = time()
    verbosity_level() == VERB_VERBOSE && println("\n Loading study ", filename)
    case = load_study(filename)
    case.study.options = options.geometry
    case.study.default_image_model = options.image_model
    validate_study_references!(case)

    if options.dump_structure
        print(dump_structure(case.study))
        return (case = case, transient = nothing)
    end

    base = splitext(basename(filename))[1]
    out_path(suffix) = options.output_dir === nothing ? base * suffix :
                       joinpath(options.output_dir, base * suffix)

    do_sweep = case.sources !== nothing && case.freq_hz !== nothing
    do_transient = case.transient !== nothing
    transient = nothing

    if do_sweep
        run_sweep!(case.study, case.freq_hz, case.sources)
        report(case.study)
        o = case.outputs
        csv_file, json_file = out_path("_results.csv"), out_path("_results.json")
        write(csv_file, results_csv(case.study; nodes = o.nodes, electrodes = o.electrodes,
                                    quantities = o.quantities))
        write(json_file, results_json(case.study; nodes = o.nodes, electrodes = o.electrodes,
                                      quantities = o.quantities))
        verbose(VERB_NORMAL, "Wrote $csv_file and $json_file")
    end

    if do_transient
        spec = case.transient
        csv_file = out_path("_transient_results.csv")
        json_file = out_path("_transient_results.json")
        if !isempty(spec.signals)
            # A list of independent signals sharing one transfer function (ADR 0026)
            results = transient_signals(case.study, spec)
            transient = first(results)
            do_sweep || report(case.study)
            write(csv_file, transient_signals_csv(spec, results))
            write(json_file, transient_signals_json(case.study.title, spec, results))
        else
            transient = transient_response(case.study, spec)
            do_sweep || report(case.study)
            write(csv_file, transient_csv(spec, transient))
            write(json_file, transient_json(case.study.title, spec, transient))
        end
        verbose(VERB_NORMAL, "Wrote $csv_file and $json_file")
        if options.plot
            png = out_path("_transient_plot.png")
            write_transient_plot(png, spec, transient)
            verbose(VERB_NORMAL, "Wrote $png")
        end
    end

    if !(do_sweep || do_transient)
        report(case.study)
        verbose(VERB_NORMAL,
                "(structure-only case: no sources/frequencies/signal block -- nothing to solve)")
    end

    verbose(VERB_NORMAL, "Simulation duration: " * format_duration(time() - start))
    return (case = case, transient = transient)
end

"""
    run_study_from_file(filename) -> Study

Load a case and run only its harmonic sweep (`runStudyFromFile`).
"""
function run_study_from_file(filename::AbstractString)
    case = load_study(filename)
    validate_study_references!(case)
    (case.sources === nothing || case.freq_hz === nothing) &&
        raise_error("runStudyFromFile: '$filename' has no 'sources'/'frequencies' block " *
                    "to sweep (ADR 0013)")
    run_sweep!(case.study, case.freq_hz, case.sources)
    return case.study
end

function format_duration(seconds::Real)
    hours = floor(Int, seconds / 3600)
    remainder = seconds - hours * 3600
    minutes = floor(Int, remainder / 60)
    secs = remainder - minutes * 60
    s = hours > 0 ? "$hours h " : ""
    (hours > 0 || minutes > 0) && (s *= "$minutes min ")
    return s * @sprintf("%.4f s", secs)
end

"""
    default_blas_threads!()

Run LAPACK single-threaded unless `OPENBLAS_NUM_THREADS` is set: the solves
are small enough that threading rarely pays and oversubscribes badly next to
other processes, and one thread keeps results reproducible run to run (the
Rust port and the Fortran build with `OMP_NUM_THREADS=1` are single-threaded
too).
"""
default_blas_threads!() = (haskey(ENV, "OPENBLAS_NUM_THREADS") || BLAS.set_num_threads(1); nothing)

const USAGE ="Usage: tupa.jl [-v|--verbose] [-q|--quiet] [--epsrel <value>] [--no-cache] " *
              "[--kernel single|double] [--image-model frequency-dependent|ideal] " *
              "[--dump-structure] [--output-dir <dir>] [--plot] <study.json>"

"""
    main(args) -> Int

Command-line driver: same arguments and output files as the Fortran
executable, plus `--dump-structure`, `--output-dir` (as the Rust port) and
`--plot`. Returns the process exit code.
"""
function main(args::AbstractVector{<:AbstractString})
    filename = nothing
    eps_rel = DEFAULT_QUAD_EPS_REL
    use_cache = true
    kernel = :single
    image_model = :frequency_dependent
    dump = false
    output_dir = nothing
    plot = false
    k = 1
    while k <= length(args)
        arg = args[k]
        if arg in ("-v", "--verbose")
            set_verbosity(VERB_VERBOSE)
        elseif arg in ("-q", "--quiet")
            set_verbosity(VERB_QUIET)
        elseif arg == "--no-cache"
            use_cache = false
        elseif arg == "--dump-structure"
            dump = true
        elseif arg == "--plot"
            plot = true
        elseif arg == "--epsrel"
            if k == length(args)
                println(stderr, "error: --epsrel requires a value, e.g. --epsrel 1.0e-6")
                return 1
            end
            k += 1
            x = tryparse(Float64, args[k])
            if x === nothing || !(x > 0) || !isfinite(x)
                println(stderr, "error: --epsrel: invalid value '$(args[k])' (must be a positive real)")
                return 1
            end
            eps_rel = x
        elseif arg == "--kernel"
            if k == length(args)
                println(stderr, "error: --kernel requires a value (single or double)")
                return 1
            end
            k += 1
            if !(args[k] in ("single", "double"))
                println(stderr, "error: --kernel: invalid value '$(args[k])' (expected single or double)")
                return 1
            end
            kernel = Symbol(args[k])
        elseif arg == "--image-model"
            if k == length(args)
                println(stderr, "error: --image-model requires a value (frequency-dependent or ideal)")
                return 1
            end
            k += 1
            if args[k] == "frequency-dependent"
                image_model = :frequency_dependent
            elseif args[k] == "ideal"
                image_model = :ideal
            else
                println(stderr, "error: --image-model: invalid value '$(args[k])' " *
                                "(expected frequency-dependent or ideal)")
                return 1
            end
        elseif arg == "--output-dir"
            if k == length(args)
                println(stderr, "error: --output-dir requires a directory")
                return 1
            end
            k += 1
            output_dir = args[k]
        else
            filename = arg
        end
        k += 1
    end

    if filename === nothing
        println(" ", USAGE)
        println(stderr, "error: missing study file argument")
        return 1
    end
    if plot && !hasmethod(write_transient_plot, Tuple{AbstractString,TransientSpec,TransientResult})
        println(stderr, "error: --plot needs Plots.jl loaded (bin/tupa.jl loads it when it is " *
                        "installed in the default environment: julia -e 'using Pkg; Pkg.add(\"Plots\")')")
        return 1
    end

    options = RunOptions(geometry = GeometryOptions(kernel = kernel, eps_rel = eps_rel,
                                                    use_cache = use_cache),
                         image_model = image_model, dump_structure = dump, output_dir = output_dir, plot = plot)
    default_blas_threads!()
    try
        run_from_file(filename; options = options)
    catch e
        e isa TupaError || e isa SystemError || e isa Base.IOError || rethrow()
        println(stderr, "error: ", sprint(showerror, e))
        return 1
    end
    return 0
end

# Compile the whole pipeline at package precompile time, so a CLI run does
# not pay seconds of JIT: a two-element case with a non-parallel pair
# (adaptive quadrature), a voltage source, a sweep, a transient and all
# writers.
@setup_workload begin
    text = """{
      "title": "precompile",
      "soil": { "conductivity": 0.01, "permittivity": 10.0, "permeability": 1.0 },
      "nodes": [ { "id": "A", "position": [0, 0, -0.5] }, { "id": "B", "position": [2, 0, -0.5] },
                 { "id": "C", "position": [2, 2, -0.5] } ],
      "materials": [ { "id": "cu", "epsilonr": 1.0, "mur": 1.0, "sigma": 5.96e7 } ],
      "elements": [ { "type": "line", "id": "L1", "from": "A", "to": "B", "radius": 0.007,
                      "segments": 2, "material": "cu" },
                    { "type": "line", "id": "L2", "from": "B", "to": "C", "radius": 0.007,
                      "segments": 1, "material": "cu" } ],
      "sources": [ { "node": "A", "voltage": { "re": 1.0, "im": 0.0 } },
                   { "node": "C", "current": { "re": 1.0, "im": 0.0 } } ],
      "frequencies": { "min": 100.0, "max": 1000.0, "pointsPerDecade": 1 },
      "signal": { "waveform": "doubleExp", "imax": 1.0, "front": "f1_2_50", "sourceNode": "A",
                  "observeNodes": ["A"], "observeElectrodes": ["L1_e1"], "nyquistHz": 1e5,
                  "fftPoints": 8, "antialiasStart": 0.9 }
    }"""
    @compile_workload begin
        level = verbosity_level()
        set_verbosity(VERB_QUIET)
        case = validate_study_references!(load_study_string(text))
        run_sweep!(case.study, case.freq_hz, case.sources)
        results_csv(case.study)
        results_json(case.study; quantities = ["voltage", "inputImpedance"])
        r = transient_response(case.study, case.transient)
        transient_csv(case.transient, r)
        transient_json(case.study.title, case.transient, r)
        dump_structure(case.study)
        set_verbosity(level)
    end
end

end # module
