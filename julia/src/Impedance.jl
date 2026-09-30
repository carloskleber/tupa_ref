# Adaptive quadrature and solid-conductor internal impedance (`mImpedance`).
#
# The mutual geometry factor g(a,b) = ∬ ds_a ds_b / r is evaluated by two
# nested calls of a line-by-line port of `dqag_k15` (adaptive Gauss–Kronrod
# 7/15, at most `MAXINT` subintervals, same tolerances and subdivision
# strategy as the Fortran code). A different rule (the prototype's fixed
# 64×64 midpoint grid) sits ~0.09 % away — three orders above the golden
# 1e-6 tolerance (ROADMAP Phase 8, "Quadrature").

"Maximum number of subintervals in the adaptive quadrature."
const MAXINT = 500

"Default relative-error factor of `geometry_factor_2d` (scaled there by the shorter segment length)."
const DEFAULT_QUAD_EPS_REL = 1.0e-6

const XGK = (-0.9914553711208126, -0.9491079123427585, -0.8648644233597691,
             -0.7415311855993944, -0.5860872354676911, -0.4058451513773972,
             -0.2077849550078985, 0.0, 0.2077849550078985, 0.4058451513773972,
             0.5860872354676911, 0.7415311855993944, 0.8648644233597691,
             0.9491079123427585, 0.9914553711208126)

const WGK = (0.02293532201052922, 0.06309209262997855, 0.1047900103222502,
             0.1406532597155259, 0.1690047266392679, 0.1903505780647854,
             0.2044329400752989, 0.2094821410847278, 0.2044329400752989,
             0.1903505780647854, 0.1690047266392679, 0.1406532597155259,
             0.1047900103222502, 0.06309209262997855, 0.02293532201052922)

# Gauss–Legendre weights of the nested 7-point rule (sum to 2) and their
# positions in XGK/WGK.
const WG = (0.1294849661688697, 0.2797053914892767, 0.3818300505051189,
            0.4179591836734694, 0.3818300505051189, 0.2797053914892767,
            0.1294849661688697)
const IGAUSS = (2, 4, 6, 8, 10, 12, 14)

"15-point Gauss–Kronrod rule on `[a, b]`: `(result, abserr)`."
@inline function qk15(f::F, a::Float64, b::Float64) where {F}
    center = 0.5 * (a + b)
    hlgth = 0.5 * (b - a)
    fv = ntuple(j -> f(center + hlgth * XGK[j]), Val(15))
    resk = 0.0
    for j in 1:15
        resk += WGK[j] * fv[j]
    end
    resg = 0.0
    for j in 1:7
        resg += WG[j] * fv[IGAUSS[j]]
    end
    return resk * hlgth, abs(resk - resg) * hlgth
end

"Interval lists of one `dqag_k15` call, reusable across calls."
struct QuadWorkspace
    alist::Vector{Float64}
    blist::Vector{Float64}
    rlist::Vector{Float64}
    elist::Vector{Float64}
end
QuadWorkspace() = QuadWorkspace(Float64[], Float64[], Float64[], Float64[])

"""
    dqag_k15(f, a, b, epsabs, epsrel[, ws]) -> (result, abserr)

Adaptive 1-D integration with the 15-point Gauss–Kronrod rule. Always
bisects the subinterval with the largest error until
`err <= max(epsabs, epsrel*|result|)` or `MAXINT` intervals exist.
"""
function dqag_k15(f::F, a::Float64, b::Float64, epsabs::Float64, epsrel::Float64,
                  ws::QuadWorkspace = QuadWorkspace()) where {F}
    alist, blist, rlist, elist = ws.alist, ws.blist, ws.rlist, ws.elist
    empty!(alist); empty!(blist); empty!(rlist); empty!(elist)

    r0, e0 = qk15(f, a, b)
    push!(alist, a); push!(blist, b); push!(rlist, r0); push!(elist, e0)
    total_result = r0
    total_error = e0
    converged = total_error <= max(epsabs, epsrel * abs(total_result))

    @inbounds while !converged && length(alist) < MAXINT
        maxind = 1
        for i in 2:length(alist)
            elist[i] > elist[maxind] && (maxind = i)
        end
        a1 = alist[maxind]
        b1 = blist[maxind]
        c = 0.5 * (a1 + b1)

        area1, err1 = qk15(f, a1, c)
        area2, err2 = qk15(f, c, b1)

        total_result -= rlist[maxind]
        total_error -= elist[maxind]

        alist[maxind] = a1
        blist[maxind] = c
        rlist[maxind] = area1
        elist[maxind] = err1

        push!(alist, c); push!(blist, b1); push!(rlist, area2); push!(elist, err2)

        total_result = total_result + area1 + area2
        total_error = total_error + err1 + err2

        converged = total_error <= max(epsabs, epsrel * abs(total_result))
    end
    return total_result, total_error
end

"""
    twodq(f, a, b, glo, hhi, errabs, errrel) -> (result, abserr)

Double integral of `f(x, y)` over `x ∈ [a, b]`, `y ∈ [glo(x), hhi(x)]`
(`TWODQ`): nested adaptive quadrature, inner absolute tolerance
`0.5*errabs/max(1, b-a)`.
"""
function twodq(f::F, a::Float64, b::Float64, glo::G, hhi::H, errabs::Float64,
               errrel::Float64) where {F,G,H}
    inner_ws = QuadWorkspace()
    inner_epsabs = 0.5 * errabs / max(1.0, b - a)
    outer(x) = first(dqag_k15(y -> f(x, y), glo(x), hhi(x), inner_epsabs, errrel, inner_ws))
    return dqag_k15(outer, a, b, errabs, errrel)
end

"""
    geometry_factor_2d(a1, va, la, b1, vb, lb, eps_rel) -> Float64

Geometry factor g(a,b) of two line segments by 2-D adaptive quadrature
(theory.md §4.2, "general position"). `a1`/`va`/`la` and `b1`/`vb`/`lb` are
start point, unit direction and length of each segment.
"""
function geometry_factor_2d(a1::Vec3, va::Vec3, la::Float64, b1::Vec3, vb::Vec3, lb::Float64,
                            eps_rel::Float64)
    integrand(x, y) = begin
        z = 0.0
        for j in 1:3
            c = (b1[j] + vb[j] * y) - (a1[j] + va[j] * x)
            z += c * c
        end
        1.0 / sqrt(z)
    end
    errrel = min(la, lb) * eps_rel
    return first(twodq(integrand, 0.0, la, _ -> 0.0, _ -> lb, 0.0, errrel))
end

"""
    internal_impedance(radius, length, omega, sigma, mur) -> ComplexF64

Internal impedance of a solid cylindrical conductor (theory.md §4.3):
`z_int = √(jωμ/σ)/(2πr₀) · I₀(ρ)/I₁(ρ)`, `ρ = r₀√(jωμσ)`, `Zint = z_int·l`.
The Bessel functions are AMOS `zbesi` (SpecialFunctions), the same routine
as SLATEC `ZBESI` in the Fortran code. For `|ρ| > 500` the ratio is taken as
1 (asymptotic limit), as in the original implementation.
"""
function internal_impedance(radius::Float64, length::Float64, omega::Float64, sigma::Float64,
                            mur::Float64)
    jw = complex(0.0, omega)
    rho = radius * sqrt(jw * mur * MU0 * sigma)
    ratio = abs(rho) > 500.0 ? complex(1.0, 0.0) :
            SpecialFunctions.besseli(0, rho) / SpecialFunctions.besseli(1, rho)
    per_length = sqrt(jw * mur * MU0 / sigma) / (2.0 * π * radius) * ratio
    return per_length * length
end
