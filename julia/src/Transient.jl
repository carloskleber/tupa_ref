# Transfer-function transient driver (`mTransient`): one unit-current sweep
# over the one-sided FFT frequency axis gives the transfer functions of every
# observed node/electrode; the tail-tapered excitation spectrum (optionally
# Tukey anti-alias filtered, ADR 0021) is multiplied in and transformed back
# (ADR 0014/0015/0019).

"Time axis `t_k = k/(2 f_nyq)`, `k = 0..n−1`."
function sample_time_axis(nyquist_hz::Real, n_samples::Integer)
    dt = 1.0 / (2.0 * nyquist_hz)
    return [k * dt for k in 0:n_samples-1]
end

"One-sided frequency axis (`n/2 + 1` bins, 0…Nyquist); the DC bin is replaced by `freq_zero_hz` (ADR 0019)."
function one_sided_frequency_axis(nyquist_hz::Real, n_samples::Integer, freq_zero_hz::Real = 1.0e-6)
    n_bins = n_samples ÷ 2 + 1
    f = [k * nyquist_hz / (n_bins - 1) for k in 0:n_bins-1]
    f[1] = freq_zero_hz
    return f
end

"""
    tukey_antialias_filter(n_bins, taper_start) -> Vector{Float64}

Tukey raised-cosine roll-off (ADR 0021, `tukeyAntialiasFilter`): 1 up to
`taper_start` (fraction of Nyquist), cosine to 0 at Nyquist. `taper_start = 1`
is the identity.
"""
function tukey_antialias_filter(n_bins::Integer, taper_start::Real)
    n_bins < 2 && raise_error("tukeyAntialiasFilter: nBins must be at least 2")
    (taper_start <= 0.0 || taper_start > 1.0) &&
        raise_error("tukeyAntialiasFilter: taperStart must be in (0, 1]")
    return map(0:n_bins-1) do k
        x = k / (n_bins - 1)
        x <= taper_start ? 1.0 : 0.5 * (1.0 + cos(π * (x - taper_start) / (1.0 - taper_start)))
    end
end

"Transient request (mirrors the JSON `signal` block, ADR 0015)."
Base.@kwdef struct TransientSpec
    "Excitation waveform"
    signal::Signal
    "Node where the current is injected"
    source_node::String
    "Nodes whose voltage is returned"
    observe_nodes::Vector{String} = String[]
    "Electrodes whose `i1(t)`, `i2(t)` are returned"
    observe_electrodes::Vector{String} = String[]
    "Nyquist frequency (Hz)"
    nyquist_hz::Float64
    "Number of time/FFT samples (power of two)"
    fft_points::Int
    "Replacement frequency for the DC bin (Hz)"
    freq_zero_hz::Float64 = 1.0e-6
    "Anti-alias roll-off start as fraction of Nyquist, in (0, 1]; 1 = off"
    antialias_start::Float64 = 1.0
end

"Time-domain results; response matrices are observed entity × sample."
struct TransientResult
    "Time axis (s)"
    t::Vector{Float64}
    "Injected current `i(t)` after the tail taper (A)"
    injected_current::Vector{Float64}
    "Observed node voltages (V)"
    node_responses::Matrix{Float64}
    "`i1(t)` of observed electrodes (A)"
    i1_responses::Matrix{Float64}
    "`i2(t)` of observed electrodes (A)"
    i2_responses::Matrix{Float64}
end

function spectrum_to_time_series(results::ResultSet, entity::Int, excitation::Vector{ComplexF64},
                                 antialias::Vector{Float64}, n_bins::Int, n_samples::Int)
    full = zeros(ComplexF64, n_samples)
    for k in 1:n_bins-1
        full[k] = results[entity, k] * excitation[k] * antialias[k]
    end
    full[1] = complex(real(full[1]), 0.0)
    # mirror the conjugate half: bin k (1-based) = nBins..nSamples ← 2 nBins − k
    for k in n_bins:n_samples
        sb = 2 * n_bins - k
        full[k] = conj(results[entity, sb] * excitation[sb]) * antialias[sb]
    end
    fft_inverse!(full)
    return real.(full)
end

"""
    transient_response(study, spec) -> TransientResult

Run the transient pipeline (`transientResponse`): the unit-current sweep on
the one-sided frequency axis, then synthesis of every observed node voltage
and electrode end current.
"""
function transient_response(study::Study, spec::TransientSpec)
    n = spec.fft_points
    is_power_of_two(n) || raise_error("transientResponse: nSamples must be a power of two")

    t = sample_time_axis(spec.nyquist_hz, n)
    injected = waveform(spec.signal, t) .* tail_taper(n)
    excitation = fft_forward!(complex.(injected))

    n_bins = n ÷ 2 + 1
    antialias = tukey_antialias_filter(n_bins, spec.antialias_start)
    freq_hz = one_sided_frequency_axis(spec.nyquist_hz, n, spec.freq_zero_hz)
    run_sweep!(study, freq_hz, [Source(spec.source_node, 1.0 + 0.0im, false)])

    st = study.structure
    synth(results, idx) = spectrum_to_time_series(results, idx, excitation, antialias, n_bins, n)
    nodes = zeros(length(spec.observe_nodes), n)
    for (r, id) in enumerate(spec.observe_nodes)
        idx = find_node_index(st, id)
        idx === nothing && raise_error("transientResponse: node '$id' not found")
        nodes[r, :] = synth(study.voltage_results, idx)
    end
    i1 = zeros(length(spec.observe_electrodes), n)
    i2 = zeros(length(spec.observe_electrodes), n)
    for (r, id) in enumerate(spec.observe_electrodes)
        idx = find_electrode_index(st, id)
        idx === nothing && raise_error("transientResponse: electrode '$id' not found")
        i1[r, :] = synth(study.long_current_results, idx)
        i2[r, :] = synth(study.trans_current_results, idx)
    end
    return TransientResult(t, injected, nodes, i1, i2)
end
