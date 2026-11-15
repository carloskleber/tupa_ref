# TUPÃ — Coding and project conventions

Rules for contributing code to any TUPÃ implementation. Physics/sign
conventions are in [theory.md §2](theory.md) (normative, enforced by
[ADR 0008](adr/0008-sign-conventions-and-validation.md)); this file covers
everything else. This document absorbs the former `CLAUDE.md` project
instructions (retired; ADR 0018).

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
  Code that reads input must `return` right after raising, and loops that
  continue past a call that can raise check `errorRaised()`: a program
  embedding the solver runs with `mError`'s library mode, where a raise does not halt
  ([ARCHITECTURE.md](ARCHITECTURE.md) §5). Console output goes through
  `mVerbosity`'s `emitMessage`/`verbose`, not a bare `print`.
- **Documentation**: FORD (`!!`) comments on every public type and routine,
  citing the theory.md section a physics routine implements.
- **Comments**: only for non-obvious physics derivations or numerical
  workarounds — never restating what the code plainly does.
- **English only**: all identifiers, comments, and documentation are in
  English. Legacy Portuguese identifiers inherited from the original MATLAB
  (e.g. `tensao`, `corrente1/2`, `IMPMUTUA`) were translated in bulk;
  do not reintroduce Portuguese names in new code.
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
- **Language separation**: no Python/Rust code inside `fortran/`;
  implementations get sibling top-level folders (`rust/`, `julia/`, `gui/`)
  sharing `common/`.
- **Public repo hygiene**: private legacy-code locations and personal
  reference-library paths must never appear in committed files.

## Documentation rules

- **One home per kind of information** ([docs/README.md](README.md) has the
  table): the model in the theory, state and plan in the roadmap, history
  and lessons in the engineering log, decisions in the ADRs, measured
  comparisons in `validation/`. Link instead of repeating.
- **Dates and history only in the [engineering log](engineering_log.md).**
  Every other document states what is true now, in the present tense, and
  cites a log entry ID (`P10`, `INV-2026-11-02`) when the past matters. Do
  not write "done on", "reviewed on … settled on …", "not yet" or "as of" in
  the theory, the roadmap or a README. Exceptions: the `Date` line and dated
  amendment headings of an ADR, the release headings of the CHANGELOG, and
  the run date of a `validation/` report.
- **Closing an item**: remove its text from the roadmap, add a log entry
  (template in the log §2), and update the theory if the model changed.
- **Theory chapters** follow the editing rules at the end of
  [theory.md](theory.md): stable section numbers, a summary at the top,
  "check yourself" items at the end, legacy-code behaviour in ADR 0017 and never in the theory (no routine, variable or file names of the private codes; cite the dissertation instead).
- **References**: `references.md` numbers are permanent. A new reference is
  appended in a consecutive block at the end, never inserted into an older
  section, because the renderer renumbers each ordered list from its first
  item. Only electromagnetic, lightning, grounding and related numerical
  sources belong there. Text from a source is paraphrased and cited, never
  copied.
- **Language**: plain technical English for readers who may not be native
  speakers; short sentences, one idea each.

## Defaults (carried over from the retired CLAUDE.md hooks)

| Topic | Default |
| --- | --- |
| Implementation language for new features | Fortran first; the Rust port (`rust/`, ROADMAP Phase 8) follows per its item 10 follow-along rule; Rust gate: `cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test --release` |
| Soil dispersion model | `tPortelaSoil`, Lima–Portela parametrisation (ADR 0007) |
| JSON library escape hatch | json-fortran (ADR 0006) |
| Frequency axis | Logarithmic spacing for harmonic sweeps; linear grid for transients (theory.md §8) |
| FFT implementation | In-repo double-precision radix-2 (`mFft`) — ADR 0014 |
| Parallelism | OpenMP over the frequency loop (`runSweep`, thread-private meshes; ADR 0024 §4). Results are bit-identical for any thread count; `build.sh` enables `-fopenmp`, `--threads`/`OMP_NUM_THREADS` set the count, one BLAS thread per solve |
| Default numerics | Single-integral geometry kernel and Γ(ω) images since 0.6.0 (ADR 0024); `numerics: { "kernel": "double", "imageModel": "ideal" }` reproduces the earlier results |
| Output formats | CSV (primary) + JSON (structured) |
| Element priority | per ROADMAP Phases 12–13 |
| Reference validation case | Buried straight conductor, 10 m, 0.5 m depth, σ = 0.01 S/m, εr ≈ 10 (theory.md §9) |

## Testing

- `fpm test` locally is the merge gate — there is **no hosted CI**
  ([ADR 0018](adr/0018-author-interview-decisions-2026-07.md)). Validate
  solver changes with `-O2 -fopenmp` as well as the release flags
  (engineering log L-02, L-14).
- Fast suites (`test_mesh`, `test_assemble`) run in seconds in any profile.
  The quadrature-heavy suites (`test_geometry`, `test_impedance`) are only
  practical under `--profile release` — see [ROADMAP §5](ROADMAP.md).
- Every sign-sensitive rule stays pinned by a test with an analytically
  known answer (ADR 0008).
