# ADR 0024 — Phase 10 numerics: single-integral kernel, Γ(ω) images, segment-length target, threaded sweep (`numerics` block)

- **Status**: Accepted
- **Date**: 2026-10-01

## Context

ROADMAP Phase 10 groups the proposals that change default numerics or unblock
larger cases (§7 P1, P2, P3, P6, P8), so that the golden fixtures are
regenerated **once** (prioritisation rule 5). The decisions below were taken
while implementing it; the earlier ADRs they amend are 0004 (geometry
factors), 0005 (images) and 0013 (input schema).

## Decision

### 1. Geometry-factor quadrature: single integral by default (item 1)

`g(a,b)` of pairs with no closed form is computed by the mHEM single-integral
form of theory.md §4.2. The inner integral over segment `b` is taken in
closed form, `ln((r1 + r2 + lb)/(r1 + r2 − lb))`, leaving one adaptive
Gauss–Kronrod integral over `a` (`mImpedance::geometryFactor1D`, with the
same `dqag_k15` as before). The nested 2-D quadrature (`geometryFactor2D`)
stays as the test oracle and as a selectable kernel.

- **Tolerance.** `epsabs = 0`, `epsrel = quadEpsRel` (default 1e-6, CLI
  `--epsrel`). The 2-D path scales `epsrel` by the shorter segment length to
  compensate nested-integration error; the 1-D path has no nesting and needs
  no such scaling. The 2-D oracle keeps its old behaviour.
- **Conditioning.** For a field point alongside `b` the sum `r1 + r2 − lb`
  is written as `ρ²(1/(r1 + s) + 1/(r2 + lb − s))` (`s` the axial coordinate,
  `ρ` the distance to the axis), which removes the cancellation that makes
  the naive form lose digits for close segments.
- **End-point singularity.** Touching segments make the integrand
  logarithmically singular at the shared end point (integrable). A quadrature
  node can land exactly on it (the centre node of the first Gauss–Kronrod
  panel does for a T junction). The guard is `max(diff, 1e-300)`, written
  without NaN-dependent comparisons because the release build uses
  `-ffast-math`; the first formulation (`.not. (diff > tiny)`) returned NaN
  there. The floor is `1e-300`, not `tiny`: `4/tiny` overflows to `+Inf`.
- **Re-entrancy.** All state is local (an internal procedure), so the kernel
  is safe to call from threads, unlike `geometryFactor2D` (module variables
  and a COMMON block).
- **Selection.** `numerics.kernel: "single" | "double"` per study (default
  `"single"`); `--kernel single|double` is the process default. A study's own
  choice applies only to its geometry build.
- **Measured.** 120 random non-parallel segments (14 400 pairs, direct and
  image, cache off): 41 ms vs 328 ms (8×); largest relative difference of
  `G` against the 2-D path 7.9e-8. On cases dominated by closed-form
  parallel pairs the saving is small (`portelaMesh`: 1.9 s either way).

### 2. Frequency-dependent image reflection coefficient: default (item 2)

The image parcels of `Z_t` **and** `Z_ℓ` carry
`Γ(ω) = (W_own − W_other)/(W_own + W_other)`, with `W_own` the immittance of
the medium holding the real segment and `W_other` that of the other medium:
`Γ_soil = (W_s − jωε0)/(W_s + jωε0)` for buried segments, and its negative
for segments in air. This is the original Matlab's default mode (ADR 0017
finding 1), applied to both parcels as it does (and as PRTL-mHEM does; TAGS
applies it to `Z_t` only, `Γ_ℓ = 1`). In terms of the stored constants
`cE = 1/(4πW)` it is `(cE_other − cE_own)/(cE_other + cE_own)`, so
`calcParamW`/`calcParamLaplace` compute it from the constants they already
set — real ω and the NLT's `s = c + jω` alike — and `calcZSelf`/`calcZMutual`
replace the real ±1 by this complex factor (`tMesh%gammaAir`/`gammaSoil`).

- **Ideal images stay selectable** — `numerics.imageModel: "ideal"`, CLI
  `--image-model ideal` — as the `|W_own| ≫ |W_other|` limit (Γ = +1 in
  soil, −1 in air; the Matlab `SOLO_IDEAL`), the low-frequency pin and the
  pre-Phase-10 behaviour. Default `"frequency-dependent"`.
- **Size of the change.** It grows as f²: for the `Node_1` voltage of
  `portela1997` the relative difference between the two models is 2.5e-8 at
  10 Hz, 2.5e-6 at 1 kHz, 2.3e-4 at 100 kHz and 1.8e-3 at 1 MHz (`rod`:
  4.5e-4 at 1 MHz). The transient fixtures move by 2e-3 to 1.7e-2 of the
  series peak (the band edge, where Γ departs most from 1). Against the
  published curves it is within ±1.5 points of MAPE
  ([validation/phase10-image-model.md](../validation/phase10-image-model.md)).
- The mixed-media (air↔soil) coupling stays neglected (ADR 0005).

### 3. Per-study segment-length target (item 3)

`numerics.maxSegmentLength` (metres): every `line`, `catenary` and `mesh`
element gets `max(segments, ceil(length/target))` segments: an explicit
`segments` larger than the bound is kept (the target never coarsens an
element that states its own count), and `segments` becomes optional
(default 1) so a case can state only the target — which is how a coarse
Schroeder-style mesh (target ~ 1000·r₀) or a fine λ/10 one is requested. Lengths:
the chord for a `line`, the parabolic arc `c + 8s²/(3c)` for a `catenary`,
and the longest bar for a `mesh` (one count serves every bar). Absent, every
element keeps its own `segments` and nothing changes. The λ/10 rule remains
the user's to apply (theory.md §4.1); the knob exposes the accuracy/cost
trade-off of Schroeder et al. [19] with a coarse-vs-fine convergence test
(`test_segmentation.f90`).

### 4. Frequency-level parallelism (item 4)

`tStudy%runSweep` solves the frequencies concurrently (OpenMP `parallel do`,
dynamic schedule), each thread on a **private copy of the mesh**. The
per-frequency body is `solveAtFrequency(this, mesh, …)`: the study is
read-only there, writes go to the thread's mesh and the result arrays at
disjoint columns `k`, and the geometry factors were built before the loop.
Every frequency runs the identical serial operations, so results are
**bit-identical for any thread count** (`test_parallel.f90`, current- and
voltage-source paths). Without `-fopenmp` the directives are comments.
`run` is unchanged for callers and still leaves the last frequency in
`this%mesh`; `runSweep` re-solves the last point once for that. The
transient driver reaches the same loop through `runSweep`.

- **The fill-loop OpenMP of Phase 3 item 4 is superseded** and will not be
  written: for the 585-unknown `portelaMesh` system the LU factorisation is
  ≈ 100 % of a frequency's cost (41 `ZGESV` solves of random matrices of
  that size take 5.25 s; the whole sweep 5.21 s), while the fill is O(n²)
  evaluations.
- **A race found on the way.** The SLATEC fork's `D1MACH` sets its
  `IFLAG` before it fills `DMACH`; threads whose first `ZBESI` calls
  overlapped could read a zero table and fail with `IERR = 2` (intermittently: seen once
  in about 50 runs of `test_common_cases` before the fix, in none of 40 after).
  `warmUpMachineConstants` calls it once before the parallel region.
- **Scaling** (`portelaMesh`, 200 electrodes, 41 frequencies, 4 cores):
  5.21 s, 2.65 s, 1.53 s for 1, 2, 4 threads (3.4×). CLI `--threads <n>`
  (needs a `-fopenmp` build; `OMP_NUM_THREADS` otherwise). With a threaded
  BLAS set one BLAS thread per solve (`OPENBLAS_NUM_THREADS=1`), as TAGS
  does. `-fopenmp` also puts automatic arrays on the stack: keep large
  scratch arrays allocatable.
- Rust and Julia keep serial sweeps (Phase 8 scope excludes parallelism).

### 5. Schema

One optional top-level block, parsed before the elements:

```json
"numerics": {
  "kernel": "single" | "double",
  "imageModel": "frequency-dependent" | "ideal",
  "maxSegmentLength": 2.5
}
```

Each field is independently optional; unknown values are rejected at load
time. Precedence: the study's field, else the CLI default, else the
built-in default. A `numerics` block never alters another study's state.
JSON `"segments"` is optional on `line`/`catenary`/`mesh` elements.

### 6. Fixtures, tolerance, versions

All golden fixtures were regenerated once under items 1 and 2: the three
harmonic and the five transient ones. `grid_expected.csv` thereby leaves its
pre-ADR-0020 electrode order, closing the grid-order finding of Phase 8 item 1
(the rest of that item stays open).
New fixtures: `portela1997_ideal` (the ideal-image pin) and, for item 6,
`portelaMesh` (harmonic and scan-fed transient — the case file now carries a
filtered `outputs` block, 21 frequencies, a 512-sample scan-fed signal).
Tolerances are unchanged (1e-6 harmonic; transient rule of the ADR 0015
amendment). The Rust port reproduces every fixture (worst 1.3e-10 harmonic,
1.5e-8 NLT, 3e-12 other transients). The Fortran package version goes to
0.6.0, the release carrying the changed defaults; this is the conformance
freeze Phase 8 item 1 proposed, taken on the Phase 10 fixture set.

### 7. NLT default: not flipped

Phase 9 item 5 left the NLT default open until the TAGS cross-validation
(item 5). That comparison ([validation/tags-xval.md](../validation/tags-xval.md))
is harmonic: it confirms the frequency-domain kernels the NLT reuses at
`s = c + jω` (TAGS' own NLT is a separate implementation, and no TAGS
transient case was run). It says nothing about what is specific to the NLT:
the `e^{ct}` growth of the error towards the end of the record
(validation/phase9-transient-options.md). Changing the default would alter
every transient result for a gain the evidence does not show, so
`signal.transform` stays `"fft"`. Revisit with a TAGS time-domain case.

### 8. Julia follows later

No Julia toolchain was reachable in the session (egress policy denied
`julialang.org`), as in Phase 9. The Julia port keeps the pre-Phase-10
numerics (2-D kernel, ideal images), so its loader refuses a `numerics`
block and its tests skip the regenerated harmonic fixtures
(`julia/README.md`).

## Consequences

- Default results change at the MHz end (Γ) and at the 1e-7 level
  everywhere else (kernel); old outputs are reproducible with
  `numerics: { "kernel": "double", "imageModel": "ideal" }`.
- Sweeps scale with cores up to the number of frequencies; a single
  frequency is as before.
- ADR 0004's quadrature description and ADR 0005's "ideal images" are
  amended (notes added there); theory.md §4.2 and §5 describe the new
  defaults.
