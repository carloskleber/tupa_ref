# Conformance harness: every common/*_expected.csv, run and diffed at 1e-6
# relative, plus the independent passivity check (same rules as
# fortran/test/test_common_cases.f90 and rust/tests/conformance.rs).

# Rows are matched by (frequency_hz, quantity, id) — the text fields must
# agree exactly — and re/im must agree within `reltol` of the row scale
# max(1e-6, |re|, |im|). Matching by key makes the harness independent of
# electrode order: grid_expected.csv predates ADR 0020's FIFO order.
function diff_csv(fresh::AbstractString, expected::AbstractString, reltol::Float64)
    fl, el = split(chomp(fresh), '\n'), split(chomp(expected), '\n')
    fl[1] == el[1] || return "header mismatch: $(fl[1]) vs $(el[1])"
    parse_row(line) = (f = split(line, ','); (join(f[1:3], ','), parse(Float64, f[4]), parse(Float64, f[5])))
    rows = Dict{String,NTuple{2,Float64}}()
    for line in fl[2:end]
        k, re, im = parse_row(line)
        haskey(rows, k) && return "duplicate fresh row $k"
        rows[k] = (re, im)
    end
    for line in el[2:end]
        k, ere, eim = parse_row(line)
        haskey(rows, k) || return "expected row missing from fresh run: $k"
        fre, fim = rows[k]
        scale = max(1e-6, abs(ere), abs(eim))
        (abs(fre - ere) >= reltol * scale || abs(fim - eim) >= reltol * scale) &&
            return "$k: fresh ($fre, $fim) vs expected ($ere, $eim)"
    end
    length(el) == length(fl) || return "row count differs: $(length(fl) - 1) fresh vs $(length(el) - 1) expected"
    return nothing
end

const SOURCE_NODE = Dict("grid" => "Node_A")

# The harmonic fixtures carry the Phase 10 defaults (single-integral kernel, Γ(ω) images,
# ADR 0024); `portela1997_ideal` pins `numerics.imageModel: "ideal"`.
@testset "golden fixture $name" for name in ("portela1997", "portela1997_ideal", "rod", "grid")
    study = run_study_from_file(joinpath(COMMON, name * ".json"))
    @test diff_csv(results_csv(study), read(joinpath(COMMON, name * "_expected.csv"), String), 1e-6) === nothing
    zin = input_impedance(study, get(SOURCE_NODE, name, "Node_1"))
    @test all(real(z) >= -1e-9 * max(abs(z), 1.0) for z in zin)
end

# The 200-electrode grid: its harmonic sweep is Phase 10 (maxSegmentLength-free, filtered
# `outputs`); the scan-fed transient (Phase 9 `transferFunction`) is not ported, so the
# case is loaded from the text without that field. Its transient fixture stays lag.
@testset "golden fixture portelaMesh (harmonic)" begin
    text = replace(read(joinpath(COMMON, "portelaMesh.json"), String),
                   r"\"transferFunction\"\s*:\s*\"interpolated\"\s*,\s*" => "")
    c = validate_study_references!(load_study_string(text))
    run_sweep!(c.study, c.freq_hz, c.sources)
    o = c.outputs
    fresh = results_csv(c.study; nodes = o.nodes, electrodes = o.electrodes, quantities = o.quantities)
    @test diff_csv(fresh, read(joinpath(COMMON, "portelaMesh_expected.csv"), String), 1e-6) === nothing
end

# Transient fixtures carry (time, [signal,] quantity, id, value); the independent-signals
# fixture of ADR 0026 is run here (the other Phase 9 transient fixtures are lag). Same rule as
# the Fortran/Rust harnesses: |fresh - expected| <= reltol * max(|expected|, 1e-3 * series peak).
function diff_transient_csv(fresh::AbstractString, expected::AbstractString, reltol::Float64)
    fl, el = split(chomp(fresh), '\n'), split(chomp(expected), '\n')
    fl[1] == el[1] || return "header mismatch: $(fl[1]) vs $(el[1])"
    length(fl) == length(el) || return "row count differs: $(length(fl) - 1) fresh vs $(length(el) - 1) expected"
    series(line) = (f = split(line, ','); join(f[2:end-1], ','))
    peak = Dict{String,Float64}()
    for line in el[2:end]
        k = series(line)
        peak[k] = max(get(peak, k, 0.0), abs(parse(Float64, split(line, ',')[end])))
    end
    for (a, b) in zip(fl[2:end], el[2:end])
        ka, kb = rsplit(a, ',', limit = 2), rsplit(b, ',', limit = 2)
        ka[1] == kb[1] || return "row key differs: $(ka[1]) vs $(kb[1])"
        fv, ev = parse(Float64, ka[2]), parse(Float64, kb[2])
        abs(fv - ev) > reltol * max(abs(ev), 1e-3 * peak[series(b)]) &&
            return "$(kb[1]): fresh $fv vs expected $ev"
    end
    return nothing
end

@testset "golden fixture portela1997_transient_signals (ADR 0026)" begin
    c = validate_study_references!(load_study(joinpath(COMMON, "portela1997_transient_signals.json")))
    rs = transient_signals(c.study, c.transient)
    fresh = transient_signals_csv(c.transient, rs)
    @test diff_transient_csv(fresh, read(joinpath(COMMON, "portela1997_transient_signals_expected.csv"), String), 1e-6) === nothing
end

@testset "every golden fixture has a test" begin
    names = sort([replace(f, "_expected.csv" => "") for f in readdir(COMMON) if endswith(f, "_expected.csv")])
    @test names == sort(["grid", "portela1997", "portela1997_ideal", "rod", PHASE9_LAG..., PHASE10B_LAG_FIXTURES..., "portela1997_transient_signals"])
end
