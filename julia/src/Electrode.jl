# Cylindrical conductor segment between two nodes (`mElectrode`).

"One discretised conductor segment."
struct Electrode
    "Identifier, `<element id>_e<k>` for line-generated segments"
    id::String
    "Indices of the end nodes (n1, n2) in `Structure.nodes`"
    nodes::NTuple{2,Int}
    "Cylinder radius (m)"
    radius::Float64
    "Conductor material (copy of the owning element's material)"
    material::Linear
end
