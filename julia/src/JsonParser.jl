# JSON schema v1 reader (`mJsonParser` + the `mTupa` loader).
#
# Reproduces the Fortran reader's leniency, like the Rust port: a missing
# number is 0, a missing string is empty, unknown keys are ignored, unknown
# element types are skipped with a warning. A *present* value of the wrong
# JSON type is an error here (the Fortran reader silently yields 0), as in
# the Rust port.

"Output selection (`outputs` block, ADR 0013). `nothing` = everything."
Base.@kwdef struct Outputs
    nodes::Union{Nothing,Vector{String}} = nothing
    electrodes::Union{Nothing,Vector{String}} = nothing
    quantities::Union{Nothing,Vector{String}} = nothing
end

"A fully loaded case file."
struct LoadedCase
    "The study (structure not yet assembled)"
    study::Study
    "`sources` block, if present"
    sources::Union{Nothing,Vector{Source}}
    "Frequency axis from the `frequencies` block, if present (Hz)"
    freq_hz::Union{Nothing,Vector{Float64}}
    "`outputs` block (all `nothing` when absent)"
    outputs::Outputs
    "`signal` block, if present"
    transient::Union{Nothing,TransientSpec}
end

# --- typed field access ------------------------------------------------------

field(obj, key::AbstractString) = obj isa AbstractDict ? get(obj, key, nothing) : nothing

function json_real(obj, key::AbstractString, default = 0.0)
    v = field(obj, key)
    v === nothing && return default
    (v isa Real && !(v isa Bool)) || raise_error("mTupa: '$key' must be a number")
    return Float64(v)
end

function json_str(obj, key::AbstractString, default = "")
    v = field(obj, key)
    v === nothing && return default
    v isa AbstractString || raise_error("mTupa: '$key' must be a string")
    return String(v)
end

function json_bool(obj, key::AbstractString, default = false)
    v = field(obj, key)
    v === nothing && return default
    v isa Bool || raise_error("mTupa: '$key' must be true or false")
    return v
end

function json_array(obj, key::AbstractString)
    v = field(obj, key)
    v === nothing && return nothing
    v isa AbstractVector || raise_error("mTupa: '$key' must be an array")
    return v
end

json_strings(obj, key::AbstractString) =
    (a = json_array(obj, key); a === nothing ? nothing :
     [x isa AbstractString ? String(x) : raise_error("mTupa: '$key' must hold strings") for x in a])

function json_vec3(obj, key::AbstractString)
    a = json_array(obj, key)
    v = a === nothing ? Float64[] : Float64[x isa Real ? x : raise_error("mTupa: '$key' must hold numbers") for x in a]
    return (get(v, 1, 0.0), get(v, 2, 0.0), get(v, 3, 0.0))
end

# Truncate a JSON number to an integer count like Fortran's int(real);
# negatives clamp to 0 (rejected later by the element checks).
count_of(x::Float64) = x <= 0.0 ? 0 : trunc(Int, x)

# --- loader ------------------------------------------------------------------

"""
    load_study(path) -> LoadedCase

Parse a case file (`loadStudy`). The structure is not assembled yet; call
`validate_study_references!` (which assembles) before solving.
"""
function load_study(path::AbstractString)
    text = try
        read(path, String)
    catch e
        raise_error("cannot read '$path': $(sprint(showerror, e))")
    end
    return load_study_string(text)
end

"Parse a case from a JSON string."
function load_study_string(text::AbstractString)
    root = try
        JSON.parse(text)
    catch e
        raise_error("JSON error: $(sprint(showerror, e))")
    end
    root isa AbstractDict || raise_error("JSON error: the case file must be a JSON object")

    soil_spec = field(root, "soil")
    soil_type = json_str(soil_spec, "type", "linear")
    soil = if soil_type == "linear"
        Linear("soil", json_real(soil_spec, "permittivity"), json_real(soil_spec, "permeability"),
               json_real(soil_spec, "conductivity"))
    elseif soil_type == "portela"
        PortelaSoil("soil", json_real(soil_spec, "permeability"), json_real(soil_spec, "sigma0"),
                    json_real(soil_spec, "alpha0"), json_real(soil_spec, "kr"))
    elseif soil_type == "alipio-visacro"
        AlipioVisacroSoil("soil", json_real(soil_spec, "permeability"), json_real(soil_spec, "sigma0"))
    else
        raise_error("mTupa: unknown soil.type '$soil_type' (expected linear, portela or alipio-visacro)")
    end

    st = Structure(soil)
    for n in something(json_array(root, "nodes"), ())
        add_node!(st, Node(json_str(n, "id"), json_vec3(n, "position")))
    end
    for m in something(json_array(root, "materials"), ())
        add_material!(st, Linear(json_str(m, "id"), json_real(m, "epsilonr"), json_real(m, "mur"),
                                 json_real(m, "sigma")))
    end
    for e in something(json_array(root, "elements"), ())
        kind = json_str(e, "type")
        if kind == "line"
            add_element!(st, Line(json_str(e, "id"), json_str(e, "from"), json_str(e, "to"),
                                  json_real(e, "radius"), count_of(json_real(e, "segments")),
                                  json_str(e, "material")))
        elseif kind == "mesh"
            add_element!(st, MeshElement(json_str(e, "id"), json_vec3(e, "position"),
                                         json_real(e, "lengthX"), json_real(e, "lengthY"),
                                         count_of(json_real(e, "rowsX")), count_of(json_real(e, "rowsY")),
                                         json_real(e, "radius"), count_of(json_real(e, "segments")),
                                         json_str(e, "material")))
        else
            println(stderr, " mTupa: unknown element type '$kind' — skipped")
        end
    end
    study = Study(json_str(root, "title"), st)

    sources = let list = json_array(root, "sources")
        list === nothing ? nothing : [load_source(s) for s in list]
    end

    freq_hz = let f = field(root, "frequencies")
        if f === nothing
            nothing
        else
            fmin, fmax = json_real(f, "min"), json_real(f, "max")
            # ADR 0013: nPoints = round(pointsPerDecade * log10(max/min)) + 1,
            # rounding half away from zero like Fortran nint
            raw = round(json_real(f, "pointsPerDecade") * log10(fmax / fmin), RoundNearestTiesAway) + 1.0
            log_frequency_axis(fmin, fmax, isfinite(raw) && raw > 2.0 ? Int(raw) : 2)
        end
    end

    o = field(root, "outputs")
    outputs = Outputs(nodes = json_strings(o, "nodes"), electrodes = json_strings(o, "electrodes"),
                      quantities = json_strings(o, "quantities"))

    sig = field(root, "signal")
    transient = sig === nothing ? nothing : load_signal(sig)

    return LoadedCase(study, sources, freq_hz, outputs, transient)
end

# "voltage" wins if both are present; neither → zero current
function load_source(s)
    node = json_str(s, "node")
    v, c = field(s, "voltage"), field(s, "current")
    v !== nothing && return Source(node, complex(json_real(v, "re"), json_real(v, "im")), true)
    c !== nothing && return Source(node, complex(json_real(c, "re"), json_real(c, "im")), false)
    return Source(node, 0.0im, false)
end

function load_signal(s)
    imax = field(s, "imax") === nothing ? nothing : json_real(s, "imax")
    wf = json_str(s, "waveform")
    signal = if wf == "heidler"
        terms = json_array(s, "terms")
        if terms === nothing
            heidler_signal(something(imax, 0.0))
        else
            heidler_signal_terms([json_real(t, "i0") for t in terms], [json_real(t, "n") for t in terms],
                                 [json_real(t, "tau1") for t in terms], [json_real(t, "tau2") for t in terms];
                                 imax = imax)
        end
    elseif wf == "doubleExp"
        double_exp_signal(something(imax, 0.0), json_str(s, "front"); jones = json_bool(s, "jones"))
    else
        raise_error("mTupa: unknown signal.waveform '$wf' (expected heidler or doubleExp)")
    end
    antialias_start = json_real(s, "antialiasStart", 1.0)
    (antialias_start <= 0.0 || antialias_start > 1.0) &&
        raise_error("mTupa: signal.antialiasStart must be in (0, 1]")
    return TransientSpec(signal = signal, source_node = json_str(s, "sourceNode"),
                         observe_nodes = something(json_strings(s, "observeNodes"), String[]),
                         observe_electrodes = something(json_strings(s, "observeElectrodes"), String[]),
                         nyquist_hz = json_real(s, "nyquistHz"),
                         fft_points = count_of(json_real(s, "fftPoints")),
                         freq_zero_hz = json_real(s, "freqZeroHz", 1.0e-6),
                         antialias_start = antialias_start)
end

# --- reference validation ----------------------------------------------------

function require_node(study::Study, id::AbstractString, fieldname::AbstractString)
    find_node_index(study.structure, id) === nothing &&
        raise_error("mTupa: $fieldname references unknown node '$id'")
    return nothing
end

function require_electrode(study::Study, id::AbstractString, fieldname::AbstractString)
    find_electrode_index(study.structure, id) === nothing &&
        raise_error("mTupa: $fieldname references unknown electrode '$id' (discretised segment " *
                    "IDs look like '<element id>_e<n>', not the input element/boundary-node ID " *
                    "— see common/README.md)")
    return nothing
end

"""
    validate_study_references!(case) -> LoadedCase

Upfront ID cross-reference validation (`validateStudyReferences`): assembles
the structure, then checks every node/electrode referenced by `sources`,
`signal` and `outputs` — before any geometry-factor or solve work runs.
"""
function validate_study_references!(case::LoadedCase)
    study = case.study
    assemble!(study.structure)
    if case.sources !== nothing
        for s in case.sources
            require_node(study, s.node, "sources[].node")
        end
    end
    if case.transient !== nothing
        t = case.transient
        require_node(study, t.source_node, "signal.sourceNode")
        foreach(id -> require_node(study, id, "signal.observeNodes"), t.observe_nodes)
        foreach(id -> require_electrode(study, id, "signal.observeElectrodes"), t.observe_electrodes)
    end
    o = case.outputs
    o.nodes === nothing || foreach(id -> require_node(study, id, "outputs.nodes"), o.nodes)
    o.electrodes === nothing ||
        foreach(id -> require_electrode(study, id, "outputs.electrodes"), o.electrodes)
    return case
end
