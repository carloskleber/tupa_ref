# Changelog

Semantic versioning, annotated git tags (ADR 0018). The roadmap
([docs/ROADMAP.md](docs/ROADMAP.md)) carries the phase-by-phase history;
this file records what changes **for users of the solver** — above all default
numerics, which move result files.

## Unreleased

### Added

- **Hooks for embedding the solver in another program** (a GUI or language
  binding that cannot be halted by an input error; no change for the CLI): `mError` library mode — `setLibraryMode(.true.)` makes
  `raiseError` record the message and return instead of halting, and the input
  readers (`loadStudy`, `validateStudyReferences`, `assembleStructure`,
  `calibrateChannels`) unwind with it; `errorRaised`, `errorCount`,
  `errorMessage`, `clearError` read it back. `mVerbosity` message sink —
  `setMessageSink` / `emitMessage` with `MSG_INFO`/`MSG_WARNING`/`MSG_ERROR`
  — through which all library output now goes (standard output when no sink is
  installed, as before). Test `test_library_mode`.
- **GUI: `plane` element** — the viewer and element tree load `plane` elements,
  drawing the same wire grid the solver builds (bars shared between adjacent
  planes drawn once) plus a translucent sheet over each rectangle.

- **`plane` element and 2 MV laboratory case** ([docs/marx_generator.md](docs/marx_generator.md)):
  a wire-grid rectangle of any orientation that reuses shared nodes and bars
  (walls, ceiling), `tools/gen_marx_lab.py` generating
  `common/marx_lab_2MV*.json` (Marx generator, divider, tower mockup with
  500 kV string, 50 m phase cable, 40 m hall) and `tools/impulse_params.py`
  (IEC 60060-1 peak, T1, T2).

- **Marx impulse generator support** ([docs/marx_generator.md](docs/marx_generator.md)):
  `step` and `branchNorton` signal waveforms (a capacitor charged to V0
  discharging through a series R-L-C `branch`, as its Norton current source),
  a `lumped` R-L-C embedded in one segment of a `line` element (keeps the
  segment's geometry and coupling), and branch-current output in
  `transientResponseSignals` (API only). Case `common/marx_single_stage.json`;
  tests `test_marx`, `test_lumped_segment`.

### Documentation

- **Validation writeups for the newer features** ([docs/validation/](docs/validation/README.md)):
  `sources-validation.md` (ideal voltage sources and two-node sources against
  superposition of unit-current runs), `gpr-potentials.md` (surface potential,
  GPR, touch and step voltage against closed forms) and a windowing section in
  `phase9-transient-options.md`. The scripts behind them
  (`sources_checks.py`, `phase11_checks.py`, `phase9_transient_checks.py`) run on
  any solver executable through `run_checks.sh`. Root README: repository layout
  and the full case list.
- **Induced-voltage literature, second pass** (references.md [82]–[106]): the
  original Agrawal derivation, field data (Rubinstein, Barker), the Rachidi
  review, and corona, dispersive and lossy ground, inclined channels,
  grounding coupling, sag and terrain. theory.md §4.4, §4.5, §4.6, §7, §9 and
  §10.1 gain the matching notes and errata. The 1985 Florida line is now
  taken as 450 m. ROADMAP Phase 10d lists the candidate follow-up cases.

### Added

- **Air–soil cross-medium coupling** (`numerics.crossMedium: "transmission"`,
  default `"none"`; ADR 0029, theory.md §5.1, ROADMAP Phase 14). Segments in air
  and in soil couple through the transmitted field (`1/(2π(Wₐ+Wₛ))`, no image term,
  straight-ray retardation) in `Zt`, `Zℓ` and the observation potentials, so a
  lightning channel or tower now acts on buried electrodes. Off by default: no
  existing result moves. Fortran only; the Rust and Julia loaders refuse
  the switch. New cases `rod_air_cross`, `channel_tower_cross`.
- **Total voltage along an overhead line** (`signal.totalVoltage`, ADR 0028
  amendment, theory.md §4.6.3): the observed node voltages of a transient run can be
  reported as the total voltage `φ + s∫A_z^i dz` of the lightning channel's incident
  field, the quantity the induced-voltage literature plots, instead of the scalar
  potential `φ` (default; goldens unchanged). Nodes at or below the ground are
  unchanged. For the Nucci base case (variant B) the voltage 250 m from the end
  goes from 58.9 to 68.1 kV and the centre from 51.3 to 81.8 kV against about 74
  and 100 kV in the paper's Fig. 9. New module `mIncident`
  (`segmentPotentialFactor` moved there), `test_incident.f90`; `induced_nucci1993_risers.json`
  sets the option. Fortran only; harmonic output has no such option yet.
- **Lumped series-RLC `branch` element and the lightning-induced voltage anchor**
  ([ADR 0028](docs/adr/0028-induced-voltages-and-lumped-branch.md), ROADMAP
  Phase 10d, theory.md §4.6): `{"type": "branch", "from", "to"?, "resistance",
  "inductance", "capacitance"}` puts a lumped `R + sL + 1/(sC)` between two nodes,
  or from one node to remote earth (no `to`), on the harmonic and the
  Laplace path; it adds no node and no segment. With it a matched overhead
  line can be terminated, and the Nucci et al. (1993) base case — a 1 km, 10 m
  high line, a subsequent stroke 50 m away — runs as `common/induced_nucci1993.json`
  (resistor at the wire end) and `common/induced_nucci1993_risers.json`
  (vertical risers, the reference's boundary condition); results against the
  paper's tables in [docs/validation/induced-validation.md](docs/validation/induced-validation.md).
  Fortran only: the Rust and Julia loaders refuse the element, the GUI skips it.
  theory.md also gains §4.6 (fields of the channel, the Taylor/Agrawal/Rusck
  coupling forms, lossy ground) and references.md [75]–[81].
- **Grounding-safety outputs** (ROADMAP Phase 11,
  [ADR 0027](docs/adr/0027-observation-potentials-and-safety-outputs.md),
  theory.md §3.1): an optional `observation` input block asks for the potential
  ψ at explicit points and on a rectangular surface grid, the ground potential
  rise and the touch voltage (legacy geometric definition: maximum
  `|ψ − u_node|` over a 1 m, 36-point circle at the surface) at designated
  nodes, step pairs and a step-voltage map. Evaluated from the solved sweep, no
  new unknowns; written to `<case>_potentials.csv`/`.json`, leaving the
  existing result files unchanged. Fortran (`mObservation`, `mPotentials`,
  `tPotentials`/`tMagnitudes`) and Rust (`observation`, `potentials`) —
  matching at 2e-8 on the new fixtures `grid_safety*`; Julia refuses the block.
  Harmonic only: transient maps, the field vector and path voltages are open.
- **Independent transient signals** (`signal.signals`,
  [ADR 0026](docs/adr/0026-independent-transient-signals.md)): the legacy
  `sinal` list. Several waveforms, optionally each at its own node, are applied
  one at a time over the same observed nodes and electrodes, with one response
  set per signal; the transfer function is solved once, so the extra signals
  are close to free (the legacy line case `linha5a`: 24 s for three fronts
  instead of 3 × 25 s). Multi-signal results JSON hold `time` and a `signals`
  array (the top-level response members are absent, so a single-signal reader
  fails visibly), the CSV a `signal` column; single-signal files are unchanged.
  Fortran, Rust and Julia (shared or per-entry nodes). Case and fixture
  `portela1997_transient_signals`.
- `tStudy%runSweepUnits` / `Study::run_sweep_units`: transfer functions of
  several unit terminals from one factorisation per frequency, keeping only the
  observed rows.
- GUI: the `channel` element (ADR 0025) loads instead of being skipped as an unknown
  type: the tree lists it, and the 3D view draws a glowing line from `<id>-base`
  (coincident with the strike node) to `<id>-top`, selectable like any element.
- GUI: the signal model covers every `signal` form (single, `sources`,
  `signals`) and option, so every `common/` case loads (the loader used to raise
  `KeyError` on the `sources` form, Heidler `terms` and the sine); the
  Transient tab overlays the checked signals of a multi-signal results file.
- `tools/legacy_import.py` keeps every `sinal` entry; the seven `linha*` cases
  are regenerated with their three fronts (1, 2 and 10 µs).

### Changed

- The transient driver solves each distinct (node, return node, quantity)
  terminal once, so `signal.sources` entries sharing a node no longer repeat
  the sweep, and a large structure no longer keeps its whole sweep per source.
  `transientResponse*` no longer leaves a sweep in the study
  (`voltageResults`, `inputImpedance`); results are unchanged.
- Julia port: ROADMAP Phase 10 numerics (single-integral kernel, `Γ(ω)` images,
  `numerics` block, `maxSegmentLength`; `--kernel`, `--image-model`). The
  regenerated harmonic fixtures and `portela1997_ideal` now run there at 1e-6.
  No solver (Fortran/Rust) behaviour changes.

### Fixed

- Malformed input that dereferenced a null JSON handle or built an undefined
  frequency axis is now an input error with a message (Fortran): a study with
  no `soil` block, a node, `mesh` or `channel` with a missing or short
  `position` (anything but three numbers), a `signal` block without
  `observeNodes`, and a `frequencies` block with `min <= 0`, `max < min` or no
  `pointsPerDecade`.

## 0.7.0 — 2026-10-01 (ROADMAP Phase 10b)

Additive: studies without a channel or a two-node source produce the same
results as 0.6.0.

### Added

- **Lightning channel element** (`"type": "channel"`,
  [ADR 0025](docs/adr/0025-lightning-channel-and-two-node-sources.md)): a
  chain of air segments, uniform or graded from the foot, with a series
  loading `R' + jωL'` that slows the current wave to a prescribed
  return-stroke speed. `"calibrate": true` solves for the loading scale
  against the 10–90 % tangent speed metric of Baba & Rakov at load time (a
  few seconds); the calibrated scale and the measured speed are recorded in a
  new additive top-level `channels` array of both results JSON files.
- **Two-node sources**: `returnNode` on `sources[]`, `signal` and
  `signal.sources[]` — a current dipole (`+I` at the node, `−I` at the return
  node) or a delta-gap voltage source (constraint on the node-pair voltage);
  `quantity: "voltage"` on transient sources. A tower strike is now one
  `channel` element plus one source.
- Cases and fixtures: `channel_unloaded`, `channel_loaded`, `channel_tower`,
  `channel_tower_gap` (harmonic and transient).
- Validation ([docs/validation/channel-validation.md](docs/validation/channel-validation.md),
  `docs/validation/channel_checks.py`): the unloaded channel follows Chen's
  analytic current to 1.5 % of the peak; loaded-wire speeds are within 0.03c
  of Baba & Rakov's Table 3.
- Rust implementation of all of the above; every fixture matched, the
  calibrated one included.

### Fixed

- Sources sharing a node are merged into one right-hand-side vector over the
  distinct nodes (the previous vector-subscript assignment of the injection
  was undefined for repeated nodes).

### Known

- The Julia port refuses the `channel` element, `returnNode` and `quantity`
  (it follows Phase 10 but not yet Phase 9, so no channel fixture runs there)
  — `julia/README.md`.
- The channel couples to air segments only; buried-electrode outputs see it
  through the source current (ROADMAP Phase 14).
- The Ishii reduced-scale induced-voltage validation is not done (needs the
  measured waveform).
- Calibration refuses segmentations for which the speed metric is unreliable
  (a calibrated scale outside 0.25–4, e.g. 0.23 m radius with 20 m segments).

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

- The Julia port had not followed Phase 10 when this was released (no Julia
  toolchain was reachable); it was ported afterwards (2026-10-01, see the
  Unreleased entry) — `julia/README.md`.
- `numerics.maxSegmentLength` undershoots the target on a `mesh` (bars are
  sized by `length/rows`, not `length/(rows − 1)`); `line` and `catenary` are
  exact. No fixture is affected (ADR 0024 §8).
- The NLT remains opt-in (`signal.transform: "nlt"`); the default is unchanged
  (ADR 0024 §7).

## 0.5.0 — 2026-07-31

First public release (ROADMAP §4, ADR 0018 postscript).
