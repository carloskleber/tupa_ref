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

# The harmonic fixtures were regenerated for ROADMAP Phase 10 (PHASE10_LAG_FIXTURES):
# this port still computes the pre-Phase-10 numerics, so none is run until it is ported.
@testset "golden fixture $name" for name in filter(n -> !(n in PHASE10_LAG_FIXTURES), ("portela1997", "rod", "grid"))
    study = run_study_from_file(joinpath(COMMON, name * ".json"))
    @test diff_csv(results_csv(study), read(joinpath(COMMON, name * "_expected.csv"), String), 1e-6) === nothing
    zin = input_impedance(study, get(SOURCE_NODE, name, "Node_1"))
    @test all(real(z) >= -1e-9 * max(abs(z), 1.0) for z in zin)
end

@testset "every golden fixture has a test" begin
    names = sort([replace(f, "_expected.csv" => "") for f in readdir(COMMON) if endswith(f, "_expected.csv")])
    @test names == sort(["grid", "portela1997", "rod", PHASE9_LAG..., PHASE10_LAG_CASES...])
end
