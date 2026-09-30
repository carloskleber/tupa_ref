# Frequency-domain result storage (`mResult`): one complex value per
# (entity, frequency) pair, where an entity is a node (voltages) or an
# electrode (longitudinal or transversal end currents).

"Results of one quantity over a frequency axis."
struct ResultSet
    entity_ids::Vector{String}
    "Angular-frequency axis (rad/s)"
    omega::Vector{Float64}
    "Values, entity × frequency"
    data::Matrix{ComplexF64}
end

ResultSet() = ResultSet(String[], Float64[], zeros(ComplexF64, 0, 0))
"Zero-filled storage for the given entities and ω axis (rad/s)."
ResultSet(ids::Vector{String}, omega::Vector{Float64}) =
    ResultSet(ids, omega, zeros(ComplexF64, length(ids), length(omega)))

entity_count(r::ResultSet) = length(r.entity_ids)
frequency_count(r::ResultSet) = length(r.omega)
entity_id(r::ResultSet, i::Integer) = r.entity_ids[i]
Base.getindex(r::ResultSet, i::Integer, k::Integer) = r.data[i, k]
Base.setindex!(r::ResultSet, v, i::Integer, k::Integer) = (r.data[i, k] = v)
