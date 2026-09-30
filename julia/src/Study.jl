# Study orchestration (`mStudy`): assembly, frequency-independent geometry
# (computed once), per-frequency impedance fill, solve (current and ideal
# voltage sources, ADR 0010/0016), frequency sweep and convenience queries.

"Frequency-independent data prepared once per study."
struct Prepared
    "Geometry matrices (factors, distances, cosines)"
    geom::GeometryMatrices
    "Segment lengths (m)"
    length::Vector{Float64}
    "Segment radii (m)"
    radius::Vector{Float64}
    "Segment medium code: 1 = air, 2 = soil"
    pos::Vector{Int}
    "Mesh with topology"
    mesh::Mesh
end

"A source injected at a named node: current (A), or ideal voltage (V, ADR 0016)."
struct Source
    node::String
    value::ComplexF64
    is_voltage::Bool
end
Source(node::AbstractString, value::Number; voltage::Bool = false) =
    Source(String(node), ComplexF64(value), voltage)

"One study: structure, numerical options, optional preparation and sweep results."
mutable struct Study
    title::String
    structure::Structure
    "Quadrature/cache options"
    options::GeometryOptions
    "Filled by `prepare!`"
    prepared::Union{Nothing,Prepared}
    "Node voltages per frequency"
    voltage_results::ResultSet
    "Electrode end currents `i1` (\"longitudinal\" store)"
    long_current_results::ResultSet
    "Electrode end currents `i2` (\"transversal\" store)"
    trans_current_results::ResultSet
    "Last sweep frequency axis (Hz)"
    sweep_freq_hz::Vector{Float64}
    "Last sweep source node ids"
    sweep_source_ids::Vector{String}
    "Effective source currents, source × frequency"
    sweep_source_currents::Matrix{ComplexF64}
end

"New study over an existing structure."
Study(title::AbstractString, structure::Structure) =
    Study(String(title), structure, GeometryOptions(), nothing, ResultSet(), ResultSet(),
          ResultSet(), Float64[], String[], zeros(ComplexF64, 0, 0))

"Solution of one frequency point with the effective injected currents."
struct RunOutput
    voltage::Vector{ComplexF64}
    current1::Vector{ComplexF64}
    current2::Vector{ComplexF64}
    "Effective injected current per source (A)"
    source_currents::Vector{ComplexF64}
end

"One-time preparation: assembly + geometry matrices + topology (`prepareStudy`)."
function prepare!(study::Study)
    study.prepared === nothing || return study
    verbosity_level() == VERB_VERBOSE &&
        println(" Assembling structure and computing geometry factors...")
    st = study.structure
    assemble!(st)

    nseg = length(st.electrodes)
    n1, n2 = Vector{Int}(undef, nseg), Vector{Int}(undef, nseg)
    p1, p2 = Vector{Vec3}(undef, nseg), Vector{Vec3}(undef, nseg)
    radius, len = Vector{Float64}(undef, nseg), Vector{Float64}(undef, nseg)
    pos = Vector{Int}(undef, nseg)
    for (i, e) in enumerate(st.electrodes)
        n1[i], n2[i] = e.nodes
        p1[i], p2[i] = st.nodes[n1[i]].p, st.nodes[n2[i]].p
        radius[i] = e.radius
        len[i] = vnorm(vsub(p2[i], p1[i]))
        pos[i] = segment_medium(p1[i], p2[i])
    end

    geom = build_geometry_matrices(p1, p2, radius, pos, study.options)
    verbosity_level() == VERB_VERBOSE &&
        println(" Geometry-factor quadrature cache: $(geom.cache_stats.hits) hits, ",
                "$(geom.cache_stats.misses) misses ($(geom.cache_stats.entries) entries)")
    study.prepared = Prepared(geom, len, radius, pos, Mesh(length(st.nodes), n1, n2))
    return study
end

"Internal impedance of electrode `e` (`segmentInternalImpedance`)."
segment_internal_impedance(e::Electrode, radius, length, omega) =
    internal_impedance(radius, length, omega, e.material.sigma, e.material.mur)

"Fill the impedance matrices at `omega` (`setZ`)."
function fill_impedance!(study::Study, omega::Float64)
    st = study.structure
    prep = study.prepared
    mesh, gm = prep.mesh, prep.geom
    calc_param_w!(mesh, omega, st.air.mur * MU0, admittance(st.air, omega),
                  permeability(st.soil) * MU0, admittance(st.soil, omega))
    n = mesh.nseg
    for i in 1:n, j in i:n
        if i == j
            zint = segment_internal_impedance(st.electrodes[i], prep.radius[i], prep.length[i], omega)
            calc_z_self!(mesh, i, prep.pos[i], gm.rbar[i, i], gm.rbari[i, i], prep.length[i], zint,
                         gm.g[i, i], gm.gi[i, i], gm.cos_theta_i[i, i])
        else
            calc_z_mutual!(mesh, i, j, prep.pos[i], prep.pos[j], gm.rbar[i, j], gm.rbari[i, j],
                           prep.length[i], prep.length[j], gm.g[i, j], gm.gi[i, j],
                           gm.cos_theta[i, j], gm.cos_theta_i[i, j])
        end
    end
    return study
end

"""
    solve_frequency!(study, omega, sources) -> RunOutput

Solve one frequency point (`tStudy%run`). Sources are current injections
unless `is_voltage`, which adds ideal voltage sources by unit-injection
superposition (ADR 0016).
"""
function solve_frequency!(study::Study, omega::Real, sources::AbstractVector{Source})
    prepare!(study)
    fill_impedance!(study, Float64(omega))
    pos = Vector{Int}(undef, length(sources))
    for (k, s) in enumerate(sources)
        idx = find_node_index(study.structure, s.node)
        idx === nothing && raise_error("tStudy%run: source node '$(s.node)' not found")
        pos[k] = idx
    end
    mesh = study.prepared.mesh
    any(s -> s.is_voltage, sources) && return solve_with_voltage_sources(mesh, pos, sources)
    currents = ComplexF64[s.value for s in sources]
    sol = inject_signal(mesh, pos, currents)
    return RunOutput(sol.voltage, sol.current1, sol.current2, currents)
end

"""
    run_sweep!(study, freq_hz, sources) -> Study

Sweep over `freq_hz` (`runSweep`), storing all node voltages and electrode
end currents in the study's result sets.
"""
function run_sweep!(study::Study, freq_hz::AbstractVector{<:Real}, sources::AbstractVector{Source})
    prepare!(study)
    st = study.structure
    omega = [2.0 * π * f for f in freq_hz]
    electrode_ids = [e.id for e in st.electrodes]
    volt = ResultSet([n.id for n in st.nodes], omega)
    i1 = ResultSet(electrode_ids, omega)
    i2 = ResultSet(copy(electrode_ids), omega)
    src = zeros(ComplexF64, length(sources), length(omega))

    for (k, w) in enumerate(omega)
        verbosity_level() == VERB_VERBOSE && println(@sprintf(" f = %.3e Hz", freq_hz[k]))
        out = solve_frequency!(study, w, sources)
        volt.data[:, k] = out.voltage
        i1.data[:, k] = out.current1
        i2.data[:, k] = out.current2
        src[:, k] = out.source_currents
    end

    study.voltage_results = volt
    study.long_current_results = i1
    study.trans_current_results = i2
    study.sweep_freq_hz = collect(Float64, freq_hz)
    study.sweep_source_ids = [s.node for s in sources]
    study.sweep_source_currents = src
    return study
end

"""
Input impedance `V(node)/I_source(node)` per swept frequency
(`inputImpedance`); the node must have been a source of the last sweep.
"""
function input_impedance(study::Study, node_id::AbstractString)
    i_src = findfirst(==(node_id), study.sweep_source_ids)
    i_src === nothing &&
        raise_error("tStudy%inputImpedance: '$node_id' was not a runSweep source node")
    i_node = find_node_index(study.structure, node_id)
    i_node === nothing && raise_error("tStudy%inputImpedance: node '$node_id' not found")
    return [study.voltage_results[i_node, k] / study.sweep_source_currents[i_src, k]
            for k in 1:frequency_count(study.voltage_results)]
end

"`max_i |V_i|` per swept frequency (`maxVoltageMagnitude`)."
max_voltage_magnitude(study::Study) =
    vec(maximum(abs, study.voltage_results.data; dims = 1, init = 0.0))

"Print the study summary (nodes, materials, elements) at NORMAL verbosity or above."
function report(study::Study)
    verbosity_level() < VERB_NORMAL && return nothing
    st = study.structure
    io = IOBuffer()
    println(io, "=========================================")
    println(io, "Example Study Initialization")
    println(io, "=========================================")
    println(io, "Study Title: ", study.title)
    println(io, "Number of Nodes: ", length(st.nodes))
    println(io, "Number of Materials: ", length(st.materials))
    println(io, "Number of Elements: ", length(st.elements))
    println(io, "Nodes:")
    for n in st.nodes
        @printf(io, "  %s at (%.2f, %.2f, %.2f)\n", n.id, n.p[1], n.p[2], n.p[3])
    end
    println(io, "Materials:")
    for m in Iterators.reverse(st.materials)
        println(io, report(m))
    end
    println(io, "Elements:")
    for e in st.elements
        print(io, report(e))
    end
    println(io, "=========================================")
    println(String(take!(io)))
    return nothing
end

"""
Structure dump for assembly comparison (ROADMAP Phase 8 item 4): nodes and
electrodes with IDs, coordinates, radii and media — same format as the Rust
`--dump-structure`.
"""
function dump_structure(study::Study)
    st = study.structure
    io = IOBuffer()
    println(io, "# nodes: index,id,x,y,z")
    for (i, n) in enumerate(st.nodes)
        println(io, "node,", i, ",", n.id, ",", fmt_e12(n.p[1]), ",", fmt_e12(n.p[2]), ",",
                fmt_e12(n.p[3]))
    end
    println(io, "# electrodes: index,id,node1,node2,radius,medium")
    for (i, e) in enumerate(st.electrodes)
        medium = segment_medium(st.nodes[e.nodes[1]].p, st.nodes[e.nodes[2]].p) == AIR ? "air" : "soil"
        println(io, "electrode,", i, ",", e.id, ",", e.nodes[1], ",", e.nodes[2], ",",
                fmt_e12(e.radius), ",", medium)
    end
    return String(take!(io))
end

# `{:.12e}` as Rust prints it (shortest exponent, no '+'), for byte-identical dumps
function fmt_e12(x::Float64)
    mant, ex = split(@sprintf("%.12e", x), 'e')
    return string(mant, "e", parse(Int, ex))
end

"Ideal voltage sources by unit-injection superposition (ADR 0016)."
function solve_with_voltage_sources(mesh::Mesh, pos::Vector{Int}, sources::AbstractVector{Source})
    ns = length(pos)
    unit = [ComplexF64[j == k ? 1.0 : 0.0 for j in 1:ns] for k in 1:ns]
    units = try
        inject_signals(mesh, pos, unit)
    catch e
        e isa TupaError || rethrow()
        raise_error("tStudy%run: unit-injection solve failed ($(e.msg))")
    end

    v_idx = [k for k in 1:ns if sources[k].is_voltage]
    nv = length(v_idx)
    a = zeros(ComplexF64, nv, nv)
    rhs = zeros(ComplexF64, nv)
    for j in 1:nv
        for l in 1:nv
            a[j, l] = units[v_idx[l]].voltage[pos[v_idx[j]]]
        end
        r = sources[v_idx[j]].value
        for k in 1:ns
            sources[k].is_voltage || (r -= sources[k].value * units[k].voltage[pos[v_idx[j]]])
        end
        rhs[j] = r
    end
    f = lu!(a; check = false)
    issuccess(f) || raise_error("tStudy%run: voltage-source constraint solve failed (singular matrix)")
    ldiv!(f, rhs)

    ieff = ComplexF64[s.value for s in sources]
    ieff[v_idx] = rhs
    combine(field, len) = begin
        out = zeros(ComplexF64, len)
        for (k, sol) in enumerate(units)
            out .+= getfield(sol, field) .* ieff[k]
        end
        out
    end
    return RunOutput(combine(:voltage, mesh.nno), combine(:current1, mesh.nseg),
                     combine(:current2, mesh.nseg), ieff)
end

"Log-spaced frequency axis `fmin … fmax` with `n_points` samples (`logFrequencyAxis`)."
function log_frequency_axis(f_min_hz::Real, f_max_hz::Real, n_points::Integer)
    n_points < 2 && raise_error("logFrequencyAxis: nPoints must be >= 2")
    log_min, log_max = log10(f_min_hz), log10(f_max_hz)
    return [10.0^(log_min + (log_max - log_min) * k / (n_points - 1)) for k in 0:n_points-1]
end
