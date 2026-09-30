# Straight line element (`mElementLine`): discretised into `segments` equal
# electrodes joined by internal nodes `<id>_n<k>`; electrodes are `<id>_e<k>`.

"A straight conductor between two named nodes."
struct Line <: AbstractElement
    "Element id (prefix of generated `_n<k>` nodes and `_e<k>` electrodes)"
    id::String
    id_node_start::String
    id_node_end::String
    "Conductor radius (m)"
    radius::Float64
    "Number of electrodes (segments)"
    n_electrodes::Int
    "Conductor material id"
    id_material::String
end

"""
Add `n-1` internal nodes and `n` electrodes to `s`; returns the number of
internal nodes created (`assembleLine`).
"""
function assemble!(l::Line, s::Structure)
    idx_start = find_node_index(s, l.id_node_start)
    idx_start === nothing && raise_error("tLine '$(l.id)': start node '$(l.id_node_start)' not found")
    idx_end = find_node_index(s, l.id_node_end)
    idx_end === nothing && raise_error("tLine '$(l.id)': end node '$(l.id_node_end)' not found")
    material = find_material(s, l.id_material)
    material === nothing && raise_error("tLine '$(l.id)': material '$(l.id_material)' not found")
    l.n_electrodes < 1 && raise_error("tLine '$(l.id)': segments must be >= 1")

    n = l.n_electrodes
    p_start = s.nodes[idx_start].p
    p_end = s.nodes[idx_end].p
    inc = ((p_end[1] - p_start[1]) / n, (p_end[2] - p_start[2]) / n, (p_end[3] - p_start[3]) / n)

    node_idx = Vector{Int}(undef, n + 1)
    node_idx[1] = idx_start
    for k in 1:n-1
        p = (p_start[1] + k * inc[1], p_start[2] + k * inc[2], p_start[3] + k * inc[3])
        node_idx[k+1] = add_node!(s, Node("$(l.id)_n$k", p))
    end
    node_idx[n+1] = idx_end

    for k in 1:n
        add_electrode!(s, Electrode("$(l.id)_e$k", (node_idx[k], node_idx[k+1]), l.radius, material))
    end
    return n - 1
end

report(l::Line) = @sprintf("Element ID: %s, Material: %s, Nodes: %d, Radius: %.3f m\n",
                           l.id, l.id_material, l.n_electrodes + 1, l.radius)
