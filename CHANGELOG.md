# Changelog

Semantic versioning, annotated git tags (ADR 0018). The roadmap
([docs/ROADMAP.md](docs/ROADMAP.md)) carries the phase-by-phase history;
this file records what changes **for users of the solver** — above all default
numerics, which move result files.

## 0.6.0 — 2026-10-01 (ROADMAP Phase 10)

### Changed — default numerics (results move)

- **Image reflection coefficient.** The image parcels of `Z_t` and `Z_ℓ` now
  carry the frequency-dependent `Γ(ω) = (W_own − W_other)/(W_own + W_other)`
  instead of the ideal `±1` ([ADR 0024](docs/adr/0024-phase10-numerics.md) §2;
  the original Matlab's default mode). The change grows as f²: 2.5e-8 at 10 Hz,
  2.3e-4 at 100 kHz and 1.8e-3 at 1 MHz on the Portela conductor's input
  voltage; under 2 points of mean error on every published-curve comparison
  ([validation/phase10-image-model.md](docs/validation/phase10-image-model.md)).
  To reproduce 0.5.0 results: `"numerics": { "imageModel": "ideal" }` (or
  `--image-model ideal`).
- **Geometry kernel.** Pairs of segments without a closed form are integrated
  by the mHEM single-integral form (8× faster on non-parallel pairs, 7.9e-8
  from the previous nested 2-D quadrature) — ADR 0024 §1. To reproduce 0.5.0:
  `"numerics": { "kernel": "double" }` (or `--kernel double`).
- All golden fixtures in `common/` were regenerated once for the two changes
  above; `grid_expected.csv` is now in declaration order (the Fortran
  `test_common_cases` no longer fails on it).

### Added

- Optional top-level `numerics` block in the case file: `kernel`,
  `imageModel`, `maxSegmentLength` (a per-study segment-length target; element
  `segments` is now optional). CLI: `--kernel`, `--image-model`, `--threads`.
- Frequency-level OpenMP in `runSweep` (and so in the transient driver):
  thread-private meshes, results bit-identical for any thread count;
  3.4× on 4 threads for a 200-electrode grid. Needs a `-fopenmp` build
  (`fortran/build.sh`).
- New cases/fixtures: `portela1997_ideal`; `portelaMesh` (32 × 32 m grid,
  185 nodes / 200 electrodes) with harmonic and scan-fed transient fixtures.
- Cross-code validation against TAGS (`benchmarks/tags-xval/`,
  [docs/validation/tags-xval.md](docs/validation/tags-xval.md)): ≤ 0.3 % below
  1 MHz on six cases.
- Rust implementation of the same changes; every fixture matched (worst
  1.3e-10 harmonic, 1.5e-8 NLT).

### Fixed

- SLATEC `D1MACH`'s lazily initialised constant table is initialised before
  any threaded region (it raised its flag before filling the table; a threaded
  first `ZBESI` call could fail with `IERR = 2`).

### Known

- The Julia port has not followed Phases 9–10 (no Julia toolchain was
  reachable): it still computes the 0.5.0 numerics and refuses the `numerics`
  block — `julia/README.md`.
- The NLT remains opt-in (`signal.transform: "nlt"`); the default is unchanged
  (ADR 0024 §7).

## 0.5.0 — 2026-07-31

First public release (ROADMAP §4, ADR 0018 postscript).
