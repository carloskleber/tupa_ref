# TUPÃ — Rust implementation

Independent Rust implementation of the TUPÃ Hybrid Electromagnetic Model
(HEM), ROADMAP Phase 8 / [ADR 0022](../docs/adr/0022-rust-implementation.md).
It implements the public contract — JSON schema v1 and the
[`common/`](../common/README.md) cases — and mirrors the Fortran reference
module by module. Fortran stays the implementation of record; Rust is the
conformance cross-check and a Fortran-toolchain-free route: **no LAPACK,
BLAS, SLATEC or fpm**, only `cargo`.

Dependencies: `serde`, `serde_json`, `num-complex`. `#![forbid(unsafe_code)]`.

## Build, run, test

```bash
cd rust
cargo build --release
./target/release/tupa ../common/portela1997.json          # writes portela1997_results.{csv,json}
./target/release/tupa ../common/portela1997_transient.json # writes *_transient_results.{csv,json}
./target/release/tupa -v --epsrel 1e-8 --no-cache ../common/grid.json
./target/release/tupa --dump-structure ../common/portelaMesh.json   # nodes/electrodes, no physics
```

Options (same as the Fortran executable, plus the last two):
`-v|--verbose`, `-q|--quiet`, `--epsrel <x>` (quadrature relative-error
factor, default `1e-6`), `--kernel single|double` (geometry quadrature: the
mHEM single integral, default, or the nested 2-D oracle), `--image-model
frequency-dependent|ideal` (image reflection coefficient: `Γ(ω)`, default, or
the ideal ±1 limit) — ROADMAP Phase 10, [ADR 0024](../docs/adr/0024-phase10-numerics.md) —
`--no-cache` (disable the geometry-factor memo table), `--dump-structure`,
`--output-dir <dir>`. A case's own `numerics` block overrides the two
Phase 10 flags. Result files are named
like the Fortran ones (`<case>_results.csv|json`,
`<case>_transient_results.csv|json`) and are read unchanged by the GUI.

Local merge gate (no hosted CI, ADR 0018):

```bash
cargo fmt --check && cargo clippy --all-targets -- -D warnings && cargo test --release
```

## Layout (Rust module ↔ Fortran module)

| `src/` | Fortran | Role |
| --- | --- | --- |
| `ctes`, `error`, `verbosity` | `Ctes`, `Error`, `Verbosity` | constants, `TupaError`, CLI levels |
| `material` | `Material` | linear / Portela / Alipio–Visacro media, `W(ω)`, `γ` |
| `node`, `electrode`, `structure`, `element/{line,mesh}` | `Node`, `Electrode`, `Structure`, `element/*` | object model and assembly (identical discretised IDs) |
| `geometry`, `geometry_cache` | `Geometry`, `GeometryCache` | `g(a,b)`, image terms, distances, cosines |
| `impedance`, `bessel` | `Impedance` | adaptive GK 7/15, single-integral `geometry_factor_1d` (default) and nested 2-D `geometry_factor_2d` quadrature, internal impedance |
| `mesh`, `linalg` | `Mesh` | topology, `Zeq` assembly (image coefficients `Γ(ω)`/ideal, `ImageModel`), LU with multiple RHS |
| `study`, `result` | `Study`, `Result` | preparation, fill, sweep, voltage sources (ADR 0016) |
| `results_writer` | `ResultsWriter` | CSV/JSON, `ES16.8` number format |
| `signal`, `fft`, `transient`, `special` | `Signal`, `Fft`, `Transient` | waveforms, radix-2 FFT, transient driver, `erfc` |
| `json` | `JsonParser`, `Tupa` | typed schema reader, `validate_study_references` |
| `main.rs` | `app/main.f90` | CLI |

## Conformance status

Measured on 2026-10-01 against the fixtures regenerated for ROADMAP Phase 10
(Linux, `rustc 1.97`; every fixture below, including the ones of earlier
phases, was re-checked then).
Tolerance and comparison rule: 1e-6 relative on the row scale
`max(1e-6, |re|, |im|)`, rows keyed by `(frequency_hz, quantity, id)`
(`tests/conformance.rs`). Transient fixtures (ADR 0015 amendment
2026-09-30): rows by position, values within
`1e-6 · max(|expected|, 1e-3 · series peak)`.

| Case | Fixture | Result |
| --- | --- | --- |
| `portela1997` | `portela1997_expected.csv` | **match** (worst 2e-17) |
| `rod` | `rod_expected.csv` | **match** (3e-17) |
| `grid` | `grid_expected.csv` | **match** (1.3e-10); regenerated in Phase 10, now in declaration order (the old file listed electrodes in reverse; the keyed comparison is kept) |
| `portela1997_ideal` (Phase 10 item 2: `numerics.imageModel: "ideal"`) | `portela1997_ideal_expected.csv` | **match** (2e-17) |
| `portelaMesh` (Phase 10 item 6: 185 nodes / 200 electrodes, harmonic with `outputs` filter, and scan-fed transient) | `portelaMesh_expected.csv`, `portelaMesh_transient_expected.csv` | **match** (1e-6; `portela_mesh_*` tests) |
| `portela1997_transient_{interpolated,hann,hann_time,multi,nlt}` (ROADMAP Phase 9) | `*_expected.csv` (transient shape, Fortran output) | **match** — worst 1.5e-8 (`nlt`), 3e-12 the others, under the transient rule (below); `tests/conformance.rs::check_transient_case` |
| `portela1997_transient`, `silva2025_*_transient` | none | vs fresh Fortran runs (2026-09-30, after Phase 9): 3e-12 and 2e-9 (`silva2025_rho100_transient`) under the transient rule |
| `silva2025_*`, `grcev_*`, `lima_*`, `poljak_fig4`, `rod_air`, … | none | load, validate, assemble; sweeps run |
| `linha*`, `torre*` (ADR 0023: `catenary` element, `portela` waveform) | none | load, validate, assemble; vs fresh Fortran runs: `linha1` 3e-10, `linha4` 1e-5 (quadrature-tolerance level, same with zero sag) — see `common/README.md` |

**Cross-code check on Grcev ℓ = 10 m** (`grcev_fig12_l10_rho{30,300,3000}`,
81 frequencies ≤ 1 MHz, |Z|): Rust vs the contributed Julia port differs
0.09 % / 0.09 % / 0.08 % (mean), Rust vs mHEM 0.04 % / 0.04 % / 0.40 %
(mean; max 0.12 / 0.38 / 4.08 %) — identical to the Fortran-vs-Julia and
Fortran-vs-mHEM figures in
[`docs/validation/tupa-vs-mhem.md`](../docs/validation/tupa-vs-mhem.md).
A direct Fortran-vs-Rust run (`docs/validation/fortran-vs-rust.md`, Phase 8
item 9) still needs Fortran outputs for the non-golden cases.

**Not yet covered**

- No golden fixture exists yet for `portela1997_transient` itself, or for
  voltage-source, `mesh`, `portela` or `alipio-visacro` cases — Phase 8
  item 1 (Fortran side); the Phase 9 transient fixtures above are the first
  transient ones. The Rust code
  paths are exercised by ported unit/consistency tests (`tests/physics.rs`:
  DC limit vs Sunde, passivity, sweep vs manual loop, voltage-source
  superposition, transient vs low-frequency impedance, mesh topology) but
  not compared against Fortran numbers.
- The Fortran `test_common_cases` compares rows by position; the former
  failure on `grid_expected.csv` (reverse electrode order) is gone since the
  fixture was regenerated in Phase 10.
- Bessel `I₀/I₁` is series + Hankel asymptotics, valid for the 45° arguments
  of the solid conductor; the tubular conductor (Phase 12 item 2) needs a
  general-phase implementation (ADR 0022).
- No parallelism (the Fortran sweep is threaded since Phase 10 item 4; the Rust one stays serial — Phase 8 scope), no LAPACK/`faer` backend, no SLATEC FFI feature.

## Phase 8 item 10 (follow-along rule)

Each later contract change (schema, `common/` case, default numerics)
carries a Rust item; lags are recorded in the conformance table above.

- **ROADMAP Phase 10** ([ADR 0024](../docs/adr/0024-phase10-numerics.md)) —
  **implemented, no lag**: `impedance::geometry_factor_1d` (line-by-line the
  Fortran `geometryFactor1D`, the default kernel; `GeometryKernel::{Single,
  Double}`), `mesh::ImageModel` and `MediumConstants::{gamma_air,gamma_soil}`
  (`Γ(ω)` images, ideal selectable), the `numerics` block in `json.rs` with
  `Study::{kernel, image_model, max_segment_length}` and the optional
  `segments`, the CLI flags, and the item 6 fixtures. The Rust and Fortran
  results agree to 1.3e-10 or better on every harmonic fixture and to 3e-12 on
  the transient ones except the NLT (1.5e-8), although both quadratures are
  adaptive: the subdivision decisions are identical operation by operation.

- **ROADMAP Phase 9** (transient pipeline completion, ADR 0015 amendment
  2026-09-30) — **implemented, no lag**: `signal.sources` and the `sine`
  waveform, `window` (half-Hann, spectral/time), `transferFunction:
  "interpolated"` (`transient::pchip_interpolate`, Matlab `pchip` port),
  `transform: "nlt"` (`Study::run_sweep_damped`, `admittance_laplace`,
  `calc_param_laplace`, `internal_impedance_laplace`). With `Re s > 0` the
  internal-impedance Bessel argument has phase below 45°, where the
  dropped `e^{-z}` companion of the asymptotic branch is even smaller
  (ADR 0022 note still applies to the tubular conductor).
