# TUPÃ — Coding and project conventions

Rules for contributing code to any TUPÃ implementation. Physics/sign
conventions are in [theory.md §2](theory.md) (normative, enforced by
[ADR 0008](adr/0008-sign-conventions-and-validation.md)); this file covers
everything else. This document absorbs the former `CLAUDE.md` project
instructions (retired 2026-07-05).

## Fortran

- **No implicit typing** — every file has `implicit none`; fpm enforces
  `implicit-typing = false`.
- **Free-form source**, `.f90` extension, no column limits.
- **Modern Fortran (2008+) only**: allocatable arrays, polymorphism, abstract
  interfaces. No new `COMMON` blocks or `EQUIVALENCE` (the one legacy
  `COMMON` in `mImpedance` is tracked debt — [ARCHITECTURE.md §7](ARCHITECTURE.md)).
- **Precision**: use the `dp` kind from `mCtes` (`real(dp)`, `complex(dp)`)
  in new code; legacy `kind=8` declarations migrate gradually. Imaginary
  unit: `IMAG_I` from `mCtes` or `(0.0_dp, 1.0_dp)`; complex construction
  via the intrinsic `cmplx(..., kind=dp)`.
- **Error handling**: never `stop`/`error stop` — raise through
  `mError%raiseError` (fortran-error-handler). Numerical routine failures
  (e.g. LAPACK `INFO`) return codes; the caller decides whether to raise.
- **Documentation**: FORD (`!!`) comments on every public type and routine,
  citing the theory.md section a physics routine implements.
- **Comments**: only for non-obvious physics derivations or numerical
  workarounds — never restating what the code plainly does.
- **English only**: all identifiers, comments, and documentation are in
  English. Legacy Portuguese identifiers inherited from the original MATLAB
  (e.g. `tensao`, `corrente1/2`, `IMPMUTUA`) were translated in bulk
  2026-07-09; do not reintroduce Portuguese names in new code.
- **Compilers**: latest gfortran is the development compiler; keep the code
  ifx-compatible (no gfortran-only extensions).

## Project rules

- **theory.md is normative**: where code and theory.md disagree, one of them
  has a bug, resolved explicitly (usually toward theory.md) — never by
  silently matching whichever paper or legacy file was read last.
- **Correctness before performance**: every physics routine lands with a
  test against an analytical value, an independent numerical oracle, or a
  published curve. No premature optimisation.
- **New features land in order**: theory.md update → JSON schema / `common/`
  case → per-language implementation (ADR 0002).
- **No new abstraction layers** unless explicitly agreed (reference-quality
  code prefers the simple, auditable path — ADR 0003).
- **Language separation**: no Python/Rust code inside `fortran/`; future
  implementations get sibling top-level folders sharing `common/`.
- **Public repo hygiene**: private legacy-code locations and personal
  reference-library paths must never appear in committed files.

## Defaults (carried over from the retired CLAUDE.md hooks)

| Topic | Default |
| --- | --- |
| Implementation language for new features | Fortran (until the Python port starts, ROADMAP Phase 8) |
| Soil dispersion model | `tPortelaSoil`, Lima–Portela parametrisation (ADR 0007) |
| JSON library escape hatch | json-fortran (ADR 0006) |
| Frequency axis | Logarithmic spacing for harmonic sweeps; linear grid for transients (theory.md §8) |
| Parallelism | OpenMP on the geometry/impedance fill loop — pencilled in; frequency-loop parallelism under evaluation (ROADMAP P6) |
| Output formats | CSV (primary) + JSON (structured) |
| Element priority | `tLine` (done); next per ROADMAP Phase 7 |
| Reference validation case | Buried straight conductor, 10 m, 0.5 m depth, σ = 0.01 S/m, εr ≈ 10 (theory.md §9) |

## Testing

- `fpm test` locally is the merge gate — there is **no hosted CI** (author
  decision, [ROADMAP §9](ROADMAP.md)).
- Fast suites (`test_mesh`, `test_assemble`) run in seconds in any profile.
  The quadrature-heavy suites (`test_geometry`, `test_impedance`) are only
  practical under `--profile release` — see [ROADMAP §5](ROADMAP.md).
- Every sign-sensitive rule stays pinned by a test with an analytically
  known answer (ADR 0008).
