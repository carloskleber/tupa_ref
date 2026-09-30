# Catenary element (`mElementCatenary`, ADR 0023): a sagging span between two
# nodes, discretised into straight segments like `Line` but with the chain
# nodes on the legacy parabolic profile (theory.md §4.4).

"A parabolic span: the `Line` fields plus the midspan sag (m; negative bows upward)."
struct Catenary <: AbstractElement
    line::Line
    sag::Float64
end

"Reject a span that leaves its end nodes' half-space, then assemble it as a sagging `Line`."
function assemble!(c::Catenary, s::Structure)
    l = c.line
    a, b = find_node_index(s, l.id_node_start), find_node_index(s, l.id_node_end)
    if a !== nothing && b !== nothing
        pa, pb = s.nodes[a].p, s.nodes[b].p
        crosses = any(1:l.n_electrodes-1) do k
            z = profile_point(pa, pb, k, l.n_electrodes, c.sag)[3]
            (min(pa[3], pb[3]) >= 0.0 && z < 0.0) || (max(pa[3], pb[3]) <= 0.0 && z > 0.0)
        end
        crosses && raise_error("tCatenary '$(l.id)': the sag profile crosses the air-soil interface (z = 0)")
    end
    assemble_sagging!(l, s, c.sag)
    return nothing
end

report(c::Catenary) = @sprintf("Catenary, sag %.3f m\n", c.sag) * report(c.line)
