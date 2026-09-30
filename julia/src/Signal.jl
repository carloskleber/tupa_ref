# Time-domain excitation waveforms (`mSignal`, ADR 0015): Heidler (legacy
# fixed 6-term set [38] and the standard parametrised form [37, 39]), double
# exponential (± Jones front) and Portela's concave-front surge [1, 67], plus
# the pre-transform tail taper.

"A source waveform."
abstract type Signal end

"""
Heidler sum: `i(t) = Σ_k (I0_k/η_k) ratio/(1+ratio) e^{-t/τ2_k}`,
`ratio = (t/τ1_k)^{n_k}`; peak-normalised to `imax` when `rescale`.
"""
struct HeidlerSignal <: Signal
    imax::Float64
    i0::Vector{Float64}
    n::Vector{Float64}
    tau1::Vector{Float64}
    tau2::Vector{Float64}
    rescale::Bool
end

"""
Double exponential `imax/(k(α−β)) (e^{-βt} − e^{-αt})`, optionally with the
Jones `e^{-(αt)²}` front.
"""
struct DoubleExpSignal <: Signal
    imax::Float64
    alpha::Float64
    beta::Float64
    t_front::Float64
    jones::Bool
end

"""
Portela's piecewise surge (theory.md §8): front `imax·expm1(α t/t₁)/expm1(α)` on
(0, t₁) (linear ramp for α = 0), flat top to t₂, linear decay to zero at t₃.
"""
struct PortelaSignal <: Signal
    imax::Float64
    alpha::Float64
    t_front::Float64
    t_top_end::Float64
    t_tail_end::Float64
end

"Portela concave-front surge (`newPortelaSignal`); requires `0 < t_front <= t_top_end < t_tail_end`."
function portela_signal(imax::Real, alpha::Real, t_front::Real, t_top_end::Real, t_tail_end::Real)
    0 < t_front <= t_top_end < t_tail_end ||
        raise_error("newPortelaSignal: times must satisfy 0 < tFront <= tTopEnd < tTailEnd")
    return PortelaSignal(imax, alpha, t_front, t_top_end, t_tail_end)
end

"Legacy 6-term Heidler set of De Conti & Visacro [38] (MCS_FST#1), rescaled to `imax` (`newHeidlerSignal`)."
heidler_signal(imax::Real) =
    HeidlerSignal(imax, [6.0, 5.0, 5.0, 8.0, 22.0, 20.0], [2.0, 3.0, 5.0, 9.0, 21.0, 2.0],
                  [3.0, 3.5, 4.8, 6.0, 7.0, 70.0] .* 1.0e-6,
                  [76.0, 10.0, 30.0, 26.0, 23.2, 200.0] .* 1.0e-6, true)

"""
Standard parametrised Heidler sum (`newHeidlerSignalTerms`); `imax = nothing`
keeps physical amplitudes, a number rescales the peak.
"""
function heidler_signal_terms(i0, n, tau1, tau2; imax::Union{Nothing,Real} = nothing)
    length(n) == length(tau1) == length(tau2) == length(i0) ||
        raise_error("newHeidlerSignalTerms: i0/n/tau1/tau2 must all have one entry per term")
    isempty(i0) && raise_error("newHeidlerSignalTerms: at least one term is required")
    (any(<=(0), tau1) || any(<=(0), tau2) || any(<(1), n)) &&
        raise_error("newHeidlerSignalTerms: tau1/tau2 must be > 0 and n >= 1")
    return HeidlerSignal(imax === nothing ? 0.0 : imax, collect(Float64, i0), collect(Float64, n),
                         collect(Float64, tau1), collect(Float64, tau2), imax !== nothing)
end

"Double-exponential surge by standard name: `f1_2_5`, `f1_2_50`, `f1_2_200`, `f250_2500` (`newDoubleExpSignal`)."
function double_exp_signal(imax::Real, name::AbstractString; jones::Bool = false)
    params = name == "f1_2_5"    ? (1.2e-6, 1.25e6, 2.8736e5) :
             name == "f1_2_50"   ? (1.2e-6, 2.4691e6, 1.4663e4) :
             name == "f1_2_200"  ? (1.2e-6, 2.6247e6, 3521.1) :
             name == "f250_2500" ? (250.0e-6, 9615.4, 347.58) :
             raise_error("newDoubleExpSignal: unknown waveform '$name' " *
                         "(expected f1_2_5, f1_2_50, f1_2_200 or f250_2500)")
    t_front, alpha, beta = params
    return DoubleExpSignal(imax, alpha, beta, t_front, jones)
end

"Waveform samples at times `t` (s); zero for `t ≤ 0`."
function waveform(s::HeidlerSignal, t::AbstractVector{<:Real})
    eta = [exp(-(s.tau1[k] / s.tau2[k]) * (s.n[k] * s.tau2[k] / s.tau1[k])^(1.0 / s.n[k]))
           for k in eachindex(s.i0)]
    i = zeros(length(t))
    for (idx, tv) in enumerate(t)
        tp = max(tv, 0.0)
        for k in eachindex(eta)
            ratio = (tp / s.tau1[k])^s.n[k]
            i[idx] += tv > 0.0 ? (s.i0[k] / eta[k]) * ratio / (1.0 + ratio) * exp(-tp / s.tau2[k]) : 0.0
        end
    end
    if s.rescale
        peak = maximum(abs, i; init = 0.0)
        peak > 0.0 && (i .= i ./ peak .* s.imax)
    end
    return i
end

function waveform(s::DoubleExpSignal, t::AbstractVector{<:Real})
    front(x) = s.jones ? exp(-(s.alpha * x)^2) : exp(-s.alpha * x)
    k = (exp(-s.beta * s.t_front) - front(s.t_front)) / (s.alpha - s.beta)
    return [tv > 0.0 ? s.imax / (k * (s.alpha - s.beta)) * (exp(-s.beta * tv) - front(tv)) : 0.0
            for tv in t]
end

function waveform(s::PortelaSignal, t::AbstractVector{<:Real})
    return map(t) do tv
        if 0.0 < tv < s.t_front
            s.alpha == 0.0 ? s.imax * tv / s.t_front :
                             s.imax * expm1(s.alpha * tv / s.t_front) / expm1(s.alpha)
        elseif s.t_front <= tv < s.t_top_end
            s.imax
        elseif s.t_top_end <= tv < s.t_tail_end
            s.imax * (s.t_tail_end - tv) / (s.t_tail_end - s.t_top_end)
        else
            0.0
        end
    end
end

"Smooth pre-transform tail taper `0.5 erfc((k − 0.8 n)/(n/20))`, `k = 1..n`."
function tail_taper(n::Integer)
    pos = 0.8 * n
    deltan = n / 20.0
    return [0.5 * SpecialFunctions.erfc((k - pos) / deltan) for k in 1:n]
end
