# Memoisation of quadrature-computed mutual geometry factors
# (`mGeometryCache`).
#
# A segment pair is determined up to congruence by the two lengths and the
# four cross endpoint distances; each is rounded to `SIG_DIGITS` significant
# digits and the key canonicalised over the 8 labelling symmetries of g(a,b).
# First stored value wins.

const SIG_DIGITS = 10

"Canonical congruence key of a segment pair."
const CacheKey = NTuple{6,Float64}

"Hit/miss statistics."
struct CacheStats
    hits::Int
    misses::Int
    entries::Int
end
CacheStats() = CacheStats(0, 0, 0)

"The memo table. A disabled cache always misses and stores nothing."
mutable struct GeometryCache
    enabled::Bool
    table::Dict{CacheKey,Float64}
    hits::Int
    misses::Int
end
GeometryCache(enabled::Bool) = GeometryCache(enabled, Dict{CacheKey,Float64}(), 0, 0)

@inline distance(a::Vec3, b::Vec3) = sqrt((b[1] - a[1])^2 + (b[2] - a[2])^2 + (b[3] - a[3])^2)

"Round `v` to `SIG_DIGITS` significant digits; values < 1e-30 collapse to 0."
function quantize(v::Float64)
    v < 1.0e-30 && return 0.0
    s = 10.0^(SIG_DIGITS - 1 - floor(Int, log10(v)))
    return round(v * s, RoundNearestTiesAway) / s
end

"Canonical key for the pair `(a1-a2, b1-b2)` with lengths `la`, `lb`."
function geom_cache_key(a1::Vec3, a2::Vec3, la::Float64, b1::Vec3, b2::Vec3, lb::Float64)
    d11, d12 = quantize(distance(a1, b1)), quantize(distance(a1, b2))
    d21, d22 = quantize(distance(a2, b1)), quantize(distance(a2, b2))
    laq, lbq = quantize(la), quantize(lb)
    key = (Inf, Inf, Inf, Inf, Inf, Inf)
    for sw in 0:1
        # swapping the roles of a and b transposes the distance table
        m = sw == 0 ? ((d11, d12), (d21, d22)) : ((d11, d21), (d12, d22))
        l1, l2 = sw == 0 ? (laq, lbq) : (lbq, laq)
        for ra in 1:2, rb in 1:2
            r1, r2 = ra, 3 - ra
            c1, c2 = rb, 3 - rb
            cand = (l1, l2, m[r1][c1], m[r1][c2], m[r2][c1], m[r2][c2])
            cand < key && (key = cand)
        end
    end
    return key
end

"Look up a key, updating statistics; `nothing` on a miss."
function cache_get!(c::GeometryCache, key::CacheKey)
    c.enabled || return nothing
    g = get(c.table, key, nothing)
    g === nothing ? (c.misses += 1) : (c.hits += 1)
    return g
end

"Insert; an existing key keeps its first value."
cache_put!(c::GeometryCache, key::CacheKey, g::Float64) =
    (c.enabled && get!(c.table, key, g); nothing)

"Statistics since construction."
cache_stats(c::GeometryCache) = CacheStats(c.hits, c.misses, length(c.table))
