# ADR 0022 — Rust implementation (`rust/`): design decisions and conformance status

- **Status**: Accepted
- **Date**: 2026-09-30
- **Context**: ROADMAP Phase 8 (second implementation), ADR 0002
  (language-agnostic object model), ADR 0018 (precision, no hosted CI).

## Context

ADR 0002 commits the project to several implementations sharing one object
model, one JSON contract and the `common/` cases. Phase 8 of the roadmap
proposed a Rust port as the conformance cross-check of the Fortran reference
and as a Fortran-toolchain-free distribution route (`cargo build` instead of
fpm + LAPACK/BLAS + the SLATEC clone). The Phase 8 design table was a
*proposal*; this ADR records what was built and where it deliberately
deviates from that table.

## Decision

A top-level Cargo package `rust/` (library `tupa` + binary `tupa`), sibling
of `fortran/`, `julia/` and `gui/`, edition 2024, MSRV 1.85,
`#![forbid(unsafe_code)]`, GPL-3.0-or-later. Modules map 1:1 onto the Fortran
modules so a reviewer can audit kernel against kernel:

| Fortran | Rust | Notes |
| --- | --- | --- |
| `mCtes`, `mError`, `mVerbosity` | `ctes`, `error`, `verbosity` | `TupaError`/`Result`; no panics on user input |
| `mMaterial` | `material` | `Linear`, `PortelaSoil`, `AlipioVisacroSoil`; `Medium` enum instead of dynamic dispatch |
| `mNode`, `mElectrode` | `node`, `electrode` | indices are 0-based |
| `mElement`, `mElementLine`, `mElementMesh` | `element::{Line, MeshElement}` | `Element` enum; declaration (FIFO) order, ADR 0020 |
| `mStructure` | `structure` | |
| `mGeometry`, `mGeometryCache` | `geometry`, `geometry_cache` | same congruence key, same closed-form parallel path |
| `mImpedance` | `impedance`, `bessel` | line-by-line `dqag_k15`/`qk15`/`TWODQ`; `internal_impedance` |
| `mMesh` | `mesh`, `linalg` | topology, `calcZSelf`/`calcZMutual`, augmented `Zeq`, LU |
| `mStudy`, `mResult` | `study`, `result` | sweep, voltage sources by superposition (ADR 0016) |
| `mResultsWriter` | `results_writer` | `ES16.8`-format numbers, same CSV/JSON layouts |
| `mSignal`, `mFft`, `mTransient` | `signal`, `fft`, `transient`, `special` | tail taper, radix-2 FFT, Tukey filter (ADR 0021) |
| `mJsonParser`, `mTupa` | `json` | typed `serde` schema, `validate_study_references` |

Kept from the Phase 8 proposal: `f64`/`Complex64` throughout; the adaptive
Gauss–Kronrod 7/15 quadrature and the radix-2 FFT are ported rather than
taken from crates (same tolerances, `maxint = 500`, same subdivision
order — only the same rule can reproduce the Fortran values to 1e-6 on the
quadrature-dominated cases); `serde`/`serde_json` for the reader; errors as
`Result<_, TupaError>`; CLI parity (same arguments, same output file
names) plus `--output-dir` and `--dump-structure`.

### Deviations from the Phase 8 proposal

1. **Linear solve: in-repo LU instead of `faer`.** A dense complex LU with
   partial pivoting and multiple right-hand sides (`linalg::solve_in_place`)
   — about 80 lines, pivot choice by `|re|+|im|` exactly like LAPACK's
   `izamax`, so the elimination order matches `ZGESV`. The systems are
   small (`nno + 2·nseg`), ADR 0003 prefers the simple auditable path, and
   the dependency list stays at `serde`, `serde_json`, `num-complex`. A
   `faer` (or LAPACK) backend remains a drop-in replacement behind a cargo
   feature if a large grid ever needs it; it is *not* implemented.
2. **Bessel functions: series + Hankel asymptotics instead of an AMOS
   port.** Internal impedance needs only the ratio `I₀(ρ)/I₁(ρ)` at
   `ρ = r₀√(jωμσ)` (phase exactly 45°). `bessel::i0_over_i1` uses the
   ascending series for `|ρ| < 20` and the truncated Hankel expansion
   otherwise (error ≲ 1e-13 relative over the range, and the dropped
   `e^{−ρ}` term is below `e^{−2 Re ρ} < 1e-12`). The Fortran branch
   `|ρ| > 500 → ratio = 1` is kept verbatim. The routine assumes
   the 45° phase of a solid conductor in the asymptotic regime; Phase 12
   item 2 (tubular conductor, needs `K₀`/`K₁` and general phase) will need
   the full AMOS subset after all, pinned against SLATEC tables.
3. **`erfc` in-repo** (`special::erfc`: series for `x < 2.5`, Lentz
   continued fraction beyond), needed by the tail taper; `std` has no
   `erfc`. Accurate to ≈ 1e-15 absolute.
4. **No optional SLATEC/LAPACK FFI features yet.** The bit-level comparison
   builds of the proposal are not implemented; conformance is measured
   against the golden CSVs instead.
5. **Stricter JSON typing.** A *present* value of the wrong JSON type
   (e.g. a string where a number is expected) is an error in Rust, where
   `json_real` in Fortran silently yields 0. Missing keys keep the Fortran
   defaults (0, empty string, empty list), unknown keys are ignored and
   unknown element types are skipped with a warning.
6. **Explicit `segments >= 1` check for `line`** (Fortran divides by zero
   and produces garbage); the `mesh` element already had this check.

### Conformance harness

`rust/tests/conformance.rs` walks `common/*_expected.csv`, runs the matching
case and compares rows **keyed by `(frequency_hz, quantity, id)`** at
1e-6 relative (same row-scale floor of `1e-6` as
`fortran/test/test_common_cases.f90`), plus the independent passivity
check. A guard test fails when a new golden fixture appears without a
matching Rust test.

Keyed rather than positional matching is deliberate:
`common/grid_expected.csv` was generated **before** the FIFO element-order
fix of ADR 0020 and lists the four `Line_k_e1` electrodes in reverse
declaration order, while the current Fortran assembler (and the Rust one)
emits them in declaration order. The physics is permutation-invariant, so
the values agree; the positional row-by-row comparison of the Fortran test
program would flag this fixture. This was **not verified on the Fortran
side** (no `gfortran`/`fpm` in the session that produced the Rust port) —
see "Follow-ups".

## Consequences

- Milestone 8a (harmonic conformance) is met on all three existing golden
  fixtures (`portela1997`, `rod`, `grid`) at 1e-6, and the Rust |Z| for the
  three Grcev ℓ = 10 m cases reproduces the published Fortran–Julia
  and Fortran–mHEM differences of
  [docs/validation/tupa-vs-mhem.md](../validation/tupa-vs-mhem.md) to the
  quoted precision (see `rust/README.md`).
- The transient path is implemented in full (Heidler legacy/parametrised,
  double exponential ± Jones, tail taper, FFT, anti-alias filter, DC-bin
  substitution, observed electrodes) and passes the ported internal
  consistency checks, **but no golden transient fixture exists**, so
  Milestone 8b ("full conformance") is not formally closed — it waits for
  Phase 8 item 1 (Fortran-side fixture widening).
- Phase 8 item 1 (contract freeze, widened fixtures, conformance tag) is a
  Fortran-side task and remains open. Until it lands, "passes every
  `common/` case" means: every case loads, validates and assembles; the
  three golden cases match at 1e-6; the remaining cases run end to end.

## Follow-ups

- Run `fpm test` for `test_common_cases` and regenerate or re-order
  `common/grid_expected.csv` if the positional comparison indeed fails
  (Fortran side).
- Phase 8 item 1: golden fixtures for a transient, a voltage-source, a
  `mesh`, a `portela` and an `alipio-visacro` case, `rod_air`, the
  structure-dump format, and the conformance tag.
- Phase 8 item 9: `docs/validation/fortran-vs-rust.md` (needs Fortran
  outputs for the non-golden cases).
- Phase 10 items 1–2 carry Rust counterparts (follow-along rule, item 10).
