# Geometric point of the conductor network (`mNode`).

"A 3-vector (m)."
const Vec3 = NTuple{3,Float64}

"""
Discretisation node: identifier + position `(x, y, z)` in metres (z up,
air–soil interface at z = 0). Voltages live in the result store, not here.
"""
struct Node
    id::String
    p::Vec3
end

Node(id::AbstractString, p::AbstractVector{<:Real}) = Node(String(id), (Float64(p[1]), Float64(p[2]), Float64(p[3])))
