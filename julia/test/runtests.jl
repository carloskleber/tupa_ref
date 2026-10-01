# Test suite of the Julia port — ports of the Fortran/Rust tests:
#   unit.jl        kernels and I/O (test_geometry, test_impedance, test_fft,
#                  test_signal, test_json_parser, results writer)
#   physics.jl     test_solve, test_sweep, test_transient, test_mesh_element,
#                  test_validation, every common/*.json case
#   conformance.jl golden fixtures common/*_expected.csv at 1e-6
#                  (test_common_cases, rust/tests/conformance.rs)
using Test, Tupa

const COMMON = normpath(joinpath(@__DIR__, "..", "..", "common"))
# ROADMAP Phase 9 cases/fixtures not yet ported (follow-along lag, julia/README.md)
const PHASE9_LAG = ["portela1997_transient_hann", "portela1997_transient_hann_time",
                    "portela1997_transient_interpolated", "portela1997_transient_multi",
                    "portela1997_transient_nlt", "portelaMesh", "portelaMesh_transient"]
# ROADMAP Phase 10 (ADR 0024), not ported: the case uses the `numerics` block, which
# the loader refuses; and the three harmonic golden fixtures were regenerated with
# the Phase 10 defaults (single-integral kernel, Γ(ω) images) while this port still
# computes the pre-Phase-10 numerics, so they are not run here (julia/README.md)
const PHASE10_LAG_CASES = ["portela1997_ideal"]
const PHASE10_LAG_FIXTURES = ["grid", "portela1997", "rod"]

set_verbosity(VERB_QUIET)

@testset "Tupa" begin
    include("unit.jl")
    include("physics.jl")
    include("conformance.jl")
end
