using Tupa: self_geometry_factor, mutual_geometry_factor, geometry_factor_2d, twodq,
            GeometryCache, geom_cache_key, cache_get!, cache_put!, cache_stats, MU0, C0,
            permeability, is_power_of_two, next_power_of_two, fmt_real, wanted

@testset "materials" begin
    lin = Linear("s", 10.0, 1.0, 0.01)
    @test abs(real(admittance(lin, 1e-9)) - 0.01) < 1e-15
    p = PortelaSoil("s", 1.0, 0.01, 0.7, 1e-3)
    w = admittance(p, 1e-6)
    @test abs(real(w) - 0.01) < 1e-9 && abs(imag(w)) < 1e-9
    a = AlipioVisacroSoil("s", 1.0, 0.01)
    w = admittance(a, 1e-6)
    @test abs(real(w) - 0.01) < 1e-7 && abs(imag(w)) < 1e-7
    @test all(real(propagation_constant(lin, 2π * f)) >= 0 for f in (1.0, 1e3, 1e6, 1e8))
    air = Linear("air", 1.0, 1.0, 0.0)
    g = propagation_constant(air, 2π * 1e6)
    @test abs(imag(g) - 2π * 1e6 / C0) / imag(g) < 1e-12 && abs(real(g)) < 1e-12
    @test permeability(p) == 1.0
end

@testset "geometry" begin
    # g_self = 2∫0^l (l-u)/√(u²+r0²) du, checked by quadrature
    l, r0 = 0.5, 0.007
    num, _ = dqag_k15(u -> 2 * (l - u) / sqrt(u^2 + r0^2), 0.0, l, 0.0, 1e-12)
    @test abs(self_geometry_factor(l, r0) - num) / num < 1e-9

    a1, a2, b1, b2 = (0.0, 0.0, -1.0), (2.0, 0.0, -1.0), (0.5, 1.5, -1.0), (3.5, 1.5, -1.0)
    closed = mutual_geometry_factor(a1, a2, b1, b2, GeometryOptions(), GeometryCache(false))
    numeric = mutual_geometry_factor(a1, a2, b1, b2,
                                     GeometryOptions(force_numeric = true, eps_rel = 1e-9),
                                     GeometryCache(false))
    @test abs(closed - numeric) / closed < 1e-6

    # opposite directions, unequal lengths (a 2 m air segment against the image
    # of a 4 m one, legacy torre2): the legacy branches gave -173 instead of
    # +0.092 (ADR 0017 finding 8)
    a1, a2, b1, b2 = (0.0, 0.0, 50.0), (0.0, 0.0, 48.0), (0.0, 5.0, -40.0), (0.0, 5.0, -36.0)
    closed = mutual_geometry_factor(a1, a2, b1, b2, GeometryOptions(), GeometryCache(false))
    numeric = mutual_geometry_factor(a1, a2, b1, b2,
                                     GeometryOptions(force_numeric = true, eps_rel = 1e-9),
                                     GeometryCache(false))
    @test abs(closed - numeric) / numeric < 1e-6
    @test closed ≈ mutual_geometry_factor(a1, a2, b2, b1, GeometryOptions(), GeometryCache(false)) rtol = 1e-12

    # touching collinear segments: log formula
    g = mutual_geometry_factor((0.0, 0.0, -1.0), (1.0, 0.0, -1.0), (1.0, 0.0, -1.0),
                               (3.0, 0.0, -1.0), GeometryOptions(), GeometryCache(false))
    @test abs(g - (3 * log(3.0) - 2 * log(2.0))) < 1e-12

    # parallel unit segments 1 m apart: 2(asinh(1) − √2 + 1)
    g = geometry_factor_2d((0.0, 0.0, 0.0), (1.0, 0.0, 0.0), 1.0, (0.0, 1.0, 0.0),
                           (1.0, 0.0, 0.0), 1.0, 1e-8)
    @test abs(g - 2 * (asinh(1.0) - sqrt(2.0) + 1)) < 1e-7

    # image of a horizontal segment at depth h is at distance 2h
    m = build_geometry_matrices([(0.0, 0.0, -0.5)], [(1.0, 0.0, -0.5)], [0.01], nothing)
    @test m.rbari[1, 1] ≈ 1.0 && m.cos_theta_i[1, 1] ≈ 1.0 && m.cos_theta[1, 1] == 1.0
    m = build_geometry_matrices([(0.0, 0.0, -0.5)], [(0.0, 0.0, -1.5)], [0.01], nothing)
    @test m.cos_theta_i[1, 1] ≈ -1.0

    # mixed-media pairs are zeroed
    m = build_geometry_matrices([(0.0, 0.0, 1.0), (0.0, 0.0, -1.0)],
                                [(0.0, 0.0, 2.0), (0.0, 0.0, -2.0)], [0.01, 0.01], [1, 2])
    @test m.g[1, 2] == 0 && m.gi[1, 2] == 0 && m.rbar[1, 2] == 0 && m.g[1, 1] > 0 && m.g[2, 2] > 0

    p1 = [(0.0, 0.0, -0.5), (1.0, 0.0, -0.5), (1.0, 1.0, -0.5)]
    p2 = [(1.0, 0.0, -0.5), (1.0, 1.0, -0.5), (0.0, 1.0, -0.5)]
    m = build_geometry_matrices(p1, p2, fill(0.01, 3), nothing)
    @test m.g == m.g' && m.gi == m.gi'
end

@testset "geometry cache" begin
    a1, a2, b1, b2 = (0.0, 0.0, -1.0), (1.0, 0.0, -1.0), (0.0, 2.0, -1.0), (0.0, 3.0, -1.0)
    k = geom_cache_key(a1, a2, 1.0, b1, b2, 1.0)
    @test k == geom_cache_key(a2, a1, 1.0, b1, b2, 1.0)
    @test k == geom_cache_key(b1, b2, 1.0, a1, a2, 1.0)
    @test k == geom_cache_key(a1, a2, 1.0, b2, b1, 1.0)
    o, q = (0.0, 0.0, 0.0), (0.0, 1.0, 0.0)
    @test geom_cache_key(o, (1.0, 0.0, 0.0), 1.0, q, (1.0, 1.0, 0.0), 1.0) !=
          geom_cache_key(o, (2.0, 0.0, 0.0), 2.0, q, (2.0, 1.0, 0.0), 2.0)
    c = GeometryCache(false)
    cache_put!(c, k, 1.0)
    @test cache_get!(c, k) === nothing && cache_stats(c).entries == 0
    c = GeometryCache(true)
    cache_put!(c, k, 1.0); cache_put!(c, k, 2.0)
    @test cache_get!(c, k) == 1.0
    @test cache_stats(c) == Tupa.CacheStats(1, 0, 1)
end

@testset "quadrature and internal impedance" begin
    r, e = dqag_k15(x -> x^2, 0.0, 3.0, 0.0, 1e-12)
    @test abs(r - 9) < 1e-13 && e < 1e-12
    r, _ = twodq((x, y) -> 1.0, 0.0, 2.0, _ -> 0.0, _ -> 3.0, 0.0, 1e-10)
    @test abs(r - 6) < 1e-12
    r0, l, sigma = 0.007, 1.0, 5.96e7
    z = internal_impedance(r0, l, 2π * 1.0, sigma, 1.0)
    rdc = l / (sigma * π * r0^2)
    @test abs(real(z) - rdc) / rdc < 1e-4
    omega = 2π * 1e8     # |ρ| > 500 branch: 45° skin-effect limit
    z = internal_impedance(r0, l, omega, sigma, 1.0)
    expect = sqrt(im * omega * MU0 / sigma) / (2π * r0) * l
    @test abs(z - expect) / abs(expect) < 1e-12
end

@testset "FFT" begin
    x = [complex(sin(0.7k), cos(0.3k)) for k in 0:15]
    dft = [sum(x[j+1] * cis(-2π * j * k / 16) for j in 0:15) for k in 0:15]
    y = fft_forward!(copy(x))
    @test maximum(abs, y - dft) < 1e-12
    @test maximum(abs, fft_inverse!(fft_forward!([complex(k, -k / 3) for k in 0.0:63.0])) -
                       [complex(k, -k / 3) for k in 0.0:63.0]) < 1e-11
    @test_throws TupaError fft_forward!(ones(ComplexF64, 6))
    @test is_power_of_two(1024) && !is_power_of_two(1000) && next_power_of_two(1000) == 1024
    imp = zeros(ComplexF64, 8); imp[1] = 1
    @test all(≈(1), fft_forward!(imp))
end

@testset "signals" begin
    t = [k * 1e-8 for k in 0:19_999]
    w = waveform(double_exp_signal(30e3, "f1_2_50"), t)
    @test 0.9 * 30e3 < maximum(w) < 1.3 * 30e3 && w[1] == 0
    @test waveform(heidler_signal(30e3), [-1e-6, 0.0]) == [0.0, 0.0]
    w = waveform(heidler_signal(30e3), [k * 5e-8 for k in 0:3999])
    @test abs(maximum(abs, w) - 30e3) < 1e-6
    w = waveform(heidler_signal_terms([10e3], [2.0], [1.9e-6], [485e-6]), [k * 5e-8 for k in 0:19_999])
    @test 0.8 * 10e3 < maximum(w) < 1.1 * 10e3
    @test_throws TupaError heidler_signal_terms([1.0], [0.5], [1e-6], [1e-5])
    @test_throws TupaError heidler_signal_terms(Float64[], Float64[], Float64[], Float64[])
    @test_throws TupaError double_exp_signal(1.0, "nope")
    w = waveform(portela_signal(1000.0, 2.0, 2e-6, 20e-6, 100e-6), [0.0, 1e-6, 2e-6, 10e-6, 60e-6, 100e-6])
    @test w[1] == 0 && isapprox(w[2], 1000 * expm1(1.0) / expm1(2.0); atol = 1e-9)
    @test w[3] == w[4] == 1000 && isapprox(w[5], 500; atol = 1e-9) && w[6] == 0
    @test waveform(portela_signal(1.0, 0.0, 2e-6, 20e-6, 100e-6), [0.5e-6]) == [0.25]
    @test isapprox(waveform(portela_signal(1.0, 1e-12, 2e-6, 20e-6, 100e-6), [0.5e-6])[1], 0.25; atol = 1e-12)
    @test_throws TupaError portela_signal(1.0, 2.0, 2e-6, 1e-6, 100e-6)
    taper = tail_taper(1024)
    @test abs(taper[1] - 1) < 1e-12 && taper[end] < 1e-3 && abs(taper[819] - 0.5) < 0.05
    @test all(diff(taper) .<= 1e-15)
end

@testset "transient axes and Tukey filter (ADR 0021)" begin
    @test sample_time_axis(1e6, 8)[2] ≈ 0.5e-6
    f = one_sided_frequency_axis(1e6, 8, 1e-6)
    @test length(f) == 5 && f[1] == 1e-6 && f[5] ≈ 1e6
    w = tukey_antialias_filter(101, 0.5)
    @test w[1] == 1 && w[51] == 1 && abs(w[101]) < 1e-12 && all(diff(w) .<= 0)
    @test all(==(1), tukey_antialias_filter(11, 1.0))
    @test sum(tukey_antialias_filter(101, 0.85)[[93, 94]]) ≈ 1
    @test_throws TupaError tukey_antialias_filter(1, 0.5)
    @test_throws TupaError tukey_antialias_filter(10, 0.0)
    @test_throws TupaError tukey_antialias_filter(10, 1.1)
end

@testset "results writer" begin
    @test fmt_real(30.8173006) == "3.08173006E+01"
    @test fmt_real(-0.00982484349) == "-9.82484349E-03"
    @test fmt_real(0.0) == "0.00000000E+00"
    @test fmt_real(10.0) == "1.00000000E+01"
    @test fmt_real(1.0e100) == "1.00000000+100"
    @test fmt_real(1.0e-100) == "1.00000000-100"
    @test fmt_real(NaN) == "NaN"
    @test wanted("a", ["a"]) && !wanted("b", ["a"]) && wanted("b", nothing)
end

const CASE = """{
  "title": "t",
  "soil": { "conductivity": 0.01, "permittivity": 10.0, "permeability": 1.0 },
  "nodes": [ { "id": "A", "position": [0,0,-0.5] }, { "id": "B", "position": [10,0,-0.5] } ],
  "materials": [ { "id": "cu", "epsilonr": 1.0, "mur": 1.0, "sigma": 5.96e7 } ],
  "elements": [ { "type": "line", "id": "L", "from": "A", "to": "B",
                  "radius": 0.007, "segments": 4, "material": "cu" } ],
  "sources": [ { "node": "A", "current": { "re": 1.0, "im": 0.0 } } ],
  "frequencies": { "min": 10.0, "max": 1.0e6, "pointsPerDecade": 1 },
  "outputs": { "quantities": ["voltage"] }
}"""

with_signal(extra) = replace(CASE, "\"outputs\"" => "\"signal\": { \"waveform\": \"doubleExp\", " *
    "\"imax\": 30000, \"front\": \"f1_2_50\", \"sourceNode\": \"A\", \"observeNodes\": [\"A\"], " *
    "\"nyquistHz\": 1e6, \"fftPoints\": 1024$extra }, \"outputs\"")

@testset "JSON loader and validation" begin
    c = load_study_string(CASE)
    @test length(c.sources) == 1 && c.sources[1].node == "A" && !c.sources[1].is_voltage
    @test length(c.freq_hz) == 6 && c.freq_hz[1] ≈ 10 && abs(c.freq_hz[6] - 1e6) < 1e-3
    @test c.outputs.quantities == ["voltage"] && c.outputs.nodes === nothing
    # ADR 0013: round(ppd·log10(max/min)) + 1
    @test length(load_study_string(replace(CASE, "\"pointsPerDecade\": 1" => "\"pointsPerDecade\": 8")).freq_hz) == 41
    @test_throws TupaError load_study_string(replace(CASE, "\"conductivity\"" => "\"type\": \"bogus\", \"conductivity\""))
    @test_throws TupaError load_study_string(replace(CASE, "\"radius\": 0.007" => "\"radius\": \"thick\""))
    both = replace(CASE, "\"current\": { \"re\": 1.0, \"im\": 0.0 }" =>
                         "\"current\": { \"re\": 1.0, \"im\": 0.0 }, \"voltage\": { \"re\": 5.0, \"im\": 0.0 }")
    s = load_study_string(both).sources[1]
    @test s.is_voltage && s.value == 5.0

    bad = load_study_string(replace(CASE, "\"node\": \"A\"" => "\"node\": \"Z\""))
    err = try validate_study_references!(bad); nothing catch e; e end
    @test err isa TupaError && occursin("sources[].node references unknown node 'Z'", err.msg)
    bad = load_study_string(replace(CASE, "\"outputs\": { \"quantities\": [\"voltage\"] }" =>
                                          "\"outputs\": { \"electrodes\": [\"L\"] }"))
    err = try validate_study_references!(bad); nothing catch e; e end
    @test err isa TupaError && occursin("unknown electrode 'L'", err.msg) && occursin("_e<n>", err.msg)

    c = validate_study_references!(load_study_string(CASE))
    @test length(c.study.structure.nodes) == 5 && length(c.study.structure.electrodes) == 4
    @test find_electrode_index(c.study.structure, "L_e4") !== nothing

    c = redirect_stderr(devnull) do
        load_study_string(replace(CASE, "\"type\": \"line\"" => "\"type\": \"circumference\""))
    end
    @test isempty(c.study.structure.elements)

    t = load_study_string(with_signal("")).transient
    @test t.fft_points == 1024 && t.freq_zero_hz == 1e-6 && t.antialias_start == 1.0
    @test isempty(t.observe_electrodes)
    @test load_study_string(with_signal(", \"antialiasStart\": 0.85")).transient.antialias_start == 0.85
    @test_throws TupaError load_study_string(with_signal(", \"antialiasStart\": 1.5"))
end
