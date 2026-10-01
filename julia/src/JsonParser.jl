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

    kernel, image_model, max_segment_length = load_numerics(field(root, "numerics"))

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
    # Segment count: `segments` (default 1), raised to ceil(length / target) when the
    # study carries a target — the target only ever refines (ROADMAP Phase 10 item 3)
    segments_for(e, len) = begin
        n = field(e, "segments") === nothing ? 1 : count_of(json_real(e, "segments"))
        max_segment_length > 0.0 && len > 0.0 ?
            max(n, ceil(Int, len / max_segment_length - 1.0e-9)) : n
    end
    chord(from, to) = begin
        a, b = find_node_index(st, from), find_node_index(st, to)
        a === nothing || b === nothing ? 0.0 : vnorm(vsub(st.nodes[a].p, st.nodes[b].p))
    end
    for e in something(json_array(root, "elements"), ())
        kind = json_str(e, "type")
        if kind == "line"
            n = segments_for(e, chord(json_str(e, "from"), json_str(e, "to")))
            add_element!(st, Line(json_str(e, "id"), json_str(e, "from"), json_str(e, "to"),
                                  json_real(e, "radius"), n, json_str(e, "material")))
        elseif kind == "catenary"
            c = chord(json_str(e, "from"), json_str(e, "to"))
            sag = json_real(e, "sag")
            # parabolic arc length: c + 8 s² / (3 c)
            n = segments_for(e, c > 0.0 ? c + 8.0 * sag * sag / (3.0 * c) : c)
            add_element!(st, Catenary(Line(json_str(e, "id"), json_str(e, "from"), json_str(e, "to"),
                                           json_real(e, "radius"), n, json_str(e, "material")), sag))
        elseif kind == "mesh"
            rows_x, rows_y = count_of(json_real(e, "rowsX")), count_of(json_real(e, "rowsY"))
            # one count serves every bar: the longest bar sets the target
            bar = max(json_real(e, "lengthX") / max(rows_x, 1), json_real(e, "lengthY") / max(rows_y, 1))
            add_element!(st, MeshElement(json_str(e, "id"), json_vec3(e, "position"),
                                         json_real(e, "lengthX"), json_real(e, "lengthY"),
                                         rows_x, rows_y, json_real(e, "radius"),
                                         segments_for(e, bar), json_str(e, "material")))
        elseif kind == "channel"
            # ROADMAP Phase 10b (ADR 0025): rejected rather than skipped, so a
            # channel case never runs without its channel by mistake
            raise_error("mTupa: the channel element (ROADMAP Phase 10b) is not implemented in the Julia " *
                        "port yet (follow-along lag, see julia/README.md)")
        else
            println(stderr, " mTupa: unknown element type '$kind' — skipped")
        end
    end
    study = Study(json_str(root, "title"), st)
    study.kernel = kernel
    study.image_model = image_model
    study.max_segment_length = max_segment_length

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

"""
`numerics` block (ADR 0024, ROADMAP Phase 10): `(kernel, image_model,
max_segment_length)`; each field is independently optional (`nothing` / `nothing`
/ 0.0 when absent) and an unknown value is an error.
"""
function load_numerics(num)
    num === nothing && return nothing, nothing, 0.0
    kernel = nothing
    kind = field(num, "kernel") === nothing ? nothing : json_str(num, "kernel")
    kind === nothing || kind in ("single", "double") ||
        raise_error("mTupa: unknown numerics.kernel '$kind' (expected single or double)")
    kind === nothing || (kernel = Symbol(kind))
    image_model = nothing
    model = field(num, "imageModel") === nothing ? nothing : json_str(num, "imageModel")
    if model !== nothing
        model == "frequency-dependent" ? (image_model = :frequency_dependent) :
        model == "ideal" ? (image_model = :ideal) :
        raise_error("mTupa: unknown numerics.imageModel '$model' (expected frequency-dependent or ideal)")
    end
    max_len = 0.0
    if field(num, "maxSegmentLength") !== nothing
        max_len = json_real(num, "maxSegmentLength")
        max_len > 0.0 || raise_error("mTupa: numerics.maxSegmentLength must be positive")
    end
    return kernel, image_model, max_len
end

# "voltage" wins if both are present; neither → zero current
function load_source(s)
    field(s, "returnNode") === nothing ||
        raise_error("mTupa: sources[].returnNode (ROADMAP Phase 10b) is not implemented in the Julia port yet " *
                    "(follow-along lag, see julia/README.md)")
    node = json_str(s, "node")
    v, c = field(s, "voltage"), field(s, "current")
    v !== nothing && return Source(node, complex(json_real(v, "re"), json_real(v, "im")), true)
    c !== nothing && return Source(node, complex(json_real(c, "re"), json_real(c, "im")), false)
    return Source(node, 0.0im, false)
end

# ROADMAP Phase 9 signal fields (ADR 0015 amendment 2026-09-30) that this
# port does not implement yet: rejected rather than silently ignored, so a
# case never runs as a plain-FFT transient by mistake (julia/README.md
# conformance table).
const PHASE9_SIGNAL_FIELDS = ("sources", "window", "transform", "nltDamping", "transferFunction")
# ... and the two-node-source fields of ROADMAP Phase 10b (ADR 0025)
const PHASE10B_SIGNAL_FIELDS = ("returnNode", "quantity")

function load_signal(s)
    for key in PHASE9_SIGNAL_FIELDS
        field(s, key) === nothing ||
            raise_error("mTupa: signal.$key (ROADMAP Phase 9) is not implemented in the Julia port yet " *
                        "(follow-along lag, see julia/README.md)")
    end
    for key in PHASE10B_SIGNAL_FIELDS
        field(s, key) === nothing ||
            raise_error("mTupa: signal.$key (ROADMAP Phase 10b) is not implemented in the Julia port yet " *
                        "(follow-along lag, see julia/README.md)")
    end
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
    elseif wf == "portela"
        portela_signal(something(imax, 0.0), json_real(s, "alpha"), json_real(s, "tFront"),
                       json_real(s, "tTopEnd"), json_real(s, "tTailEnd"))
    else
        raise_error("mTupa: unknown signal.waveform '$wf' (expected heidler, doubleExp or portela)")
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
