# Container for nodes, elements, materials, electrodes and the soil medium
# (`mStructure`). Elements are kept in JSON declaration order; later elements
# may reference nodes created by earlier ones (e.g. a `line` attached to a
# `mesh`'s main nodes), so `assemble!` visits them in order (ADR 0020).

"Geometric structure of a study."
mutable struct Structure
    "Soil half-space (z < 0)"
    soil::Medium
    "Air half-space (z > 0): hardcoded vacuum, ADR 0019"
    air::Linear
    "Boundary and internal nodes"
    nodes::Vector{Node}
    "Discretised segments (populated by `assemble!`)"
    electrodes::Vector{Electrode}
    "Elements awaiting assembly, declaration order"
    elements::Vector{AbstractElement}
    "Conductor materials in declaration order"
    materials::Vector{Linear}
    assembled::Bool
end

"Empty structure over the given soil."
Structure(soil::Medium) =
    Structure(soil, Linear("air", 1.0, 1.0, 0.0), Node[], Electrode[], AbstractElement[], Linear[], false)

"Append a node; returns its index."
add_node!(s::Structure, n::Node) = (push!(s.nodes, n); length(s.nodes))

"Append an electrode."
add_electrode!(s::Structure, e::Electrode) = (push!(s.electrodes, e); s)

"Append an element (assembled later, in this order)."
add_element!(s::Structure, e::AbstractElement) = (push!(s.elements, e); s)

"Append a conductor material."
add_material!(s::Structure, m::Linear) = (push!(s.materials, m); s)

"Index of the node with the given id, or `nothing`."
find_node_index(s::Structure, id::AbstractString) = findfirst(n -> n.id == id, s.nodes)

"Index of the electrode with the given id, or `nothing`."
find_electrode_index(s::Structure, id::AbstractString) = findfirst(e -> e.id == id, s.electrodes)

"""
Material by id, or `nothing`; the most recently added of a duplicated id wins
(the Fortran list prepends).
"""
function find_material(s::Structure, id::AbstractString)
    i = findlast(m -> m.id == id, s.materials)
    return i === nothing ? nothing : s.materials[i]
end

"Discretise every element into nodes and electrodes (idempotent)."
function assemble!(s::Structure)
    s.assembled && return s
    for e in s.elements
        assemble!(e, s)
    end
    s.assembled = true
    return s
end

"Medium code of a segment from its end points: 1 = air, 2 = soil (midpoint z)."
segment_medium(pa::Vec3, pb::Vec3) = 0.5 * (pa[3] + pb[3]) > 0.0 ? AIR : SOIL
