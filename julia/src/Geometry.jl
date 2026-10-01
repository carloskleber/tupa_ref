# Geometry-factor layer (`mGeometry`): mean/image distances, real geometry
# factors and direction cosines for pairs of straight cylindrical segments
# (theory.md §4–5, ADR 0004). Frequency-independent; computed once per
# geometry. No dependency on the object model — plain endpoints and radii.

"Threshold on `|va × vb|` below which two unit vectors count as parallel (Matlab `barraquad`'s `cruz < 1e-20`)."
const PARALLEL_TOL = 1.0e-20
"Touching/collinearity tolerance of the closed-form parallel formula (Matlab `NUMP`)."
const NUMP = 1.0e-6

@inline vsub(a::Vec3, b::Vec3) = (a[1] - b[1], a[2] - b[2], a[3] - b[3])
@inline vdot(a::Vec3, b::Vec3) = a[1] * b[1] + a[2] * b[2] + a[3] * b[3]
@inline vnorm(a::Vec3) = sqrt(vdot(a, a))
@inline vcross(u::Vec3, v::Vec3) =
    (u[2] * v[3] - u[3] * v[2], u[3] * v[1] - u[1] * v[3], u[1] * v[2] - u[2] * v[1])

# Natural log that returns NaN (like Fortran and Rust) instead of throwing
# for a negative argument, so a degenerate closed form falls back to
# quadrature.
@inline safe_log(x::Float64) = x < 0.0 ? NaN : log(x)

"Decompose a segment into its unit direction and length."
function segment_vector(p1::Vec3, p2::Vec3)
    d = vsub(p2, p1)
    len = vnorm(d)
    return (d[1] / len, d[2] / len, d[3] / len), len
end

"""
Coincident (self) geometry factor, axis-to-surface (theory.md §4.2):
`g_self = 2 [ l ln((l+h)/r0) − h + r0 ]`, `h = √(l² + r0²)`.
"""
function self_geometry_factor(l::Float64, r0::Float64)
    h = sqrt(l * l + r0 * r0)
    return 2.0 * (l * log((l + h) / r0) - h + r0)
end

"Mirror a position or direction through the z = 0 air–soil interface."
image_vector(d::Vec3) = (d[1], d[2], -d[3])

"Distance between two points (segment midpoints), theory.md §4.1."
mean_distance(p1::Vec3, p2::Vec3) = vnorm(vsub(p2, p1))

"`cos θ` between two direction vectors; 0 if either has zero length."
function direction_cosine(d1::Vec3, d2::Vec3)
    n1, n2 = vnorm(d1), vnorm(d2)
    (n1 <= 0.0 || n2 <= 0.0) && return 0.0
    return vdot(d1, d2) / (n1 * n2)
end

"""
Closed-form mutual geometry factor of two PARALLEL segments (theory.md §4.2;
Matlab `barraquad.m` `posparal`). `nothing` signals a degenerate (NaN/Inf)
result: the caller falls back to quadrature.
"""
function parallel_geometry_factor(a1::Vec3, a2::Vec3, la::Float64, va::Vec3,
                                  b1::Vec3, b2::Vec3, lb::Float64, vb::Vec3)
    # g = ∫∫ dla dlb / R does not depend on the segments' orientation
    # (theory.md §4.2; the sign lives in cos θ), so an opposite-direction b is
    # traversed backwards and only the same-direction branches are needed. The
    # legacy `posparal` opposite-direction branches were wrong whenever
    # la != lb (ADR 0017 finding 8).
    c1, c2, vc = vdot(va, vb) > 0.0 ? (b1, b2, vb) : (b2, b1, (-vb[1], -vb[2], -vb[3]))
    da1b1 = vnorm(vsub(a1, c1))
    da1b2 = vnorm(vsub(a1, c2))
    da2b1 = vnorm(vsub(a2, c1))
    da2b2 = vnorm(vsub(a2, c2))
    x2 = la

    if da1b2 > da2b1
        xi1 = vdot(vsub(c1, a1), va)
        xi2 = xi1 + lb
        d11, d12, d21, d22 = da1b1, da1b2, da2b1, da2b2
    else
        x2 = lb
        xi1 = vdot(vsub(a1, c1), vc)
        xi2 = xi1 + la
        d11, d12, d21, d22 = da1b1, da2b1, da1b2, da2b2
    end

    l11 = xi1
    l21 = xi1 - x2
    l22 = xi2 - x2
    y = sqrt(max(d11 * d11 - l11 * l11, 0.0)) / la

    g = if y < NUMP
        if abs(xi1) < NUMP || abs(xi1 - x2) < NUMP || abs(xi2) < NUMP || abs(xi2 - x2) < NUMP
            # touching, non-overlapping collinear segments: exact for both
            # orientations (see the Fortran comment)
            (la + lb) * log(la + lb) - la * log(la) - lb * log(lb)
        else
            x2 * safe_log((x2 - xi2) / (x2 - xi1)) +
            xi1 * safe_log(-(x2 - xi1) / xi1) +
            xi2 * safe_log(-xi2 / (x2 - xi2))
        end
    else
        d11 - d12 - d21 + d22 +
        x2 * safe_log((d22 + l22) / (d21 + l21)) +
        xi1 * safe_log((d11 - xi1) / (d21 - l21)) +
        xi2 * safe_log((d22 - l22) / (d12 - xi2))
    end
    return isfinite(g) ? g : nothing
end

"""
Numerical options of the geometry build. `kernel` selects the quadrature of
the pairs that have no closed form (ROADMAP Phase 10 item 1): `:single`, the
mHEM single integral (theory.md §4.2, the default), or `:double`, the nested
2-D Gauss–Kronrod quadrature (the pre-Phase-10 path and the test oracle;
`numerics.kernel: "double"`).
"""
Base.@kwdef struct GeometryOptions
    "Quadrature kernel for pairs without a closed form: `:single` or `:double`"
    kernel::Symbol = :single
    "Relative-error factor of the quadrature (CLI `--epsrel`)"
    eps_rel::Float64 = DEFAULT_QUAD_EPS_REL
    "Memoise quadrature results (CLI `--no-cache` disables)"
    use_cache::Bool = true
    "Always use quadrature, even for parallel pairs (testing oracle)"
    force_numeric::Bool = false
end

"""
General mutual geometry factor `g(a,b) = ∫ dl_a dl_b / R_ab` (theory.md §4.2)
for non-coincident, non-identical segments: closed form for parallel pairs,
memoised adaptive quadrature (`opts.kernel`) otherwise.
"""
function mutual_geometry_factor(a1::Vec3, a2::Vec3, b1::Vec3, b2::Vec3,
                                opts::GeometryOptions, cache::GeometryCache)
    va, la = segment_vector(a1, a2)
    vb, lb = segment_vector(b1, b2)

    if !opts.force_numeric && vnorm(vcross(va, vb)) < PARALLEL_TOL
        g = parallel_geometry_factor(a1, a2, la, va, b1, b2, lb, vb)
        g === nothing || return g
    end

    use_cache = cache.enabled && opts.use_cache
    if use_cache
        key = geom_cache_key(a1, a2, la, b1, b2, lb)
        hit = cache_get!(cache, key)
        hit === nothing || return hit
    end
    g = opts.kernel === :double ? geometry_factor_2d(a1, va, la, b1, vb, lb, opts.eps_rel) :
                                  geometry_factor_1d(a1, va, la, b1, vb, lb, opts.eps_rel)
    use_cache && cache_put!(cache, key, g)
    return g
end

"Full `n × n` geometry matrices of a set of segments (symmetric)."
struct GeometryMatrices
    "Direct geometry factor"
    g::Matrix{Float64}
    "Image geometry factor"
    gi::Matrix{Float64}
    "Mean distance (diagonal = radius)"
    rbar::Matrix{Float64}
    "Image mean distance"
    rbari::Matrix{Float64}
    "Direction cosine (diagonal = 1)"
    cos_theta::Matrix{Float64}
    "Image direction cosine"
    cos_theta_i::Matrix{Float64}
    "Cache statistics of this build"
    cache_stats::CacheStats
end

"""
    build_geometry_matrices(p1, p2, radius, pos, opts) -> GeometryMatrices

Build the geometry matrices (theory.md §4–5, `buildGeometryMatrices`). Self
entries: closed-form `g_self`, `Rbar = r0`, `cosθ = 1`. Image entries (also
the diagonal, a segment against its own image) always go through
`mutual_geometry_factor`. Mutual pairs in different media (`pos[i] != pos[j]`,
1 = air, 2 = soil) are skipped and zeroed, exactly as `calcZMutual` would
discard them (ADR 0005). Pass `pos = nothing` to compute every pair.
"""
function build_geometry_matrices(p1::AbstractVector{Vec3}, p2::AbstractVector{Vec3},
                                 radius::AbstractVector{Float64},
                                 pos::Union{Nothing,AbstractVector{<:Integer}},
                                 opts::GeometryOptions = GeometryOptions())
    n = length(p1)
    cache = GeometryCache(opts.use_cache)

    dir = Vector{Vec3}(undef, n)
    len = Vector{Float64}(undef, n)
    mid = Vector{Vec3}(undef, n)
    for i in 1:n
        dir[i], len[i] = segment_vector(p1[i], p2[i])
        mid[i] = (0.5 * (p1[i][1] + p2[i][1]), 0.5 * (p1[i][2] + p2[i][2]),
                  0.5 * (p1[i][3] + p2[i][3]))
    end

    g, gi = zeros(n, n), zeros(n, n)
    rbar, rbari = zeros(n, n), zeros(n, n)
    ct, cti = zeros(n, n), zeros(n, n)

    for i in 1:n, j in i:n
        mixed = i != j && pos !== nothing && pos[i] != pos[j]
        if i == j
            g[i, i] = self_geometry_factor(len[i], radius[i])
            rbar[i, i] = radius[i]
            ct[i, i] = 1.0
        elseif !mixed
            g[i, j] = g[j, i] = mutual_geometry_factor(p1[i], p2[i], p1[j], p2[j], opts, cache)
            rbar[i, j] = rbar[j, i] = mean_distance(mid[i], mid[j])
            ct[i, j] = ct[j, i] = direction_cosine(dir[i], dir[j])
        end
        mixed && continue

        # image term: segment i against the mirror image of segment j
        gi[i, j] = gi[j, i] = mutual_geometry_factor(p1[i], p2[i], image_vector(p1[j]),
                                                     image_vector(p2[j]), opts, cache)
        rbari[i, j] = rbari[j, i] = mean_distance(mid[i], image_vector(mid[j]))
        cti[i, j] = cti[j, i] = direction_cosine(dir[i], image_vector(dir[j]))
    end
    return GeometryMatrices(g, gi, rbar, rbari, ct, cti, cache_stats(cache))
end
