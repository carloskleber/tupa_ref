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
factor, default `1e-6`), `--no-cache` (disable the geometry-factor memo
table), `--dump-structure`, `--output-dir <dir>`. Result files are named
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
| `impedance`, `bessel` | `Impedance` | adaptive GK 7/15, nested 2-D quadrature, internal impedance |
| `mesh`, `linalg` | `Mesh` | topology, `Zeq` assembly, LU with multiple RHS |
| `study`, `result` | `Study`, `Result` | preparation, fill, sweep, voltage sources (ADR 0016) |
| `results_writer` | `ResultsWriter` | CSV/JSON, `ES16.8` number format |
| `signal`, `fft`, `transient`, `special` | `Signal`, `Fft`, `Transient` | waveforms, radix-2 FFT, transient driver, `erfc` |
| `json` | `JsonParser`, `Tupa` | typed schema reader, `validate_study_references` |
| `main.rs` | `app/main.f90` | CLI |

## Conformance status

Measured on 2026-09-30 (Linux, `rustc 1.94`). Tolerance and comparison rule:
1e-6 relative on the row scale `max(1e-6, |re|, |im|)`, rows keyed by
`(frequency_hz, quantity, id)` (`tests/conformance.rs`).

| Case | Fixture | Result |
| --- | --- | --- |
| `portela1997` | `portela1997_expected.csv` | **match** (1e-6) |
| `rod` | `rod_expected.csv` | **match** (1e-6) |
| `grid` | `grid_expected.csv` | **match** (1e-6); fixture predates ADR 0020's FIFO order and lists electrodes in reverse declaration order — see below |
| `portela1997_transient`, `silva2025_*_transient` | none | runs; internal consistency only |
| `silva2025_*`, `grcev_*`, `lima_*`, `poljak_fig4`, `rod_air`, … | none | load, validate, assemble; sweeps run |
| `portelaMesh` | none (structure-only) | 185 nodes / 200 electrodes, as pinned in `test_mesh_element.f90` |
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

- No golden fixture exists for transient, voltage-source, `mesh`, `portela`
  or `alipio-visacro` cases — Phase 8 item 1 (Fortran side). The Rust code
  paths are exercised by ported unit/consistency tests (`tests/physics.rs`:
  DC limit vs Sunde, passivity, sweep vs manual loop, voltage-source
  superposition, transient vs low-frequency impedance, mesh topology) but
  not compared against Fortran numbers.
- The Fortran `test_common_cases` compares rows by position. If
  `grid_expected.csv` indeed is in reverse electrode order (it is, in the
  checked-in file) that test needs the fixture regenerated — not checked
  here, no Fortran toolchain in the session that wrote this port.
- Bessel `I₀/I₁` is series + Hankel asymptotics, valid for the 45° arguments
  of the solid conductor; the tubular conductor (Phase 12 item 2) needs a
  general-phase implementation (ADR 0022).
- No parallelism, no LAPACK/`faer` backend, no SLATEC FFI feature.

## Phase 8 item 10 (follow-along rule)

Each later contract change (schema, `common/` case, default numerics)
carries a Rust item; lags are recorded in the conformance table above.
