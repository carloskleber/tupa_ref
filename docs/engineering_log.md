# TUPÃ — Engineering log and lessons learned

Integrated record of what was **done** in each phase of TUPÃ, the decisions
taken on the way, and the lessons that apply to all later work. Created on
2026-11-02 from the history that lived inside [ROADMAP.md](ROADMAP.md); the
format follows the pyelektra engineering log. Prose is English
([CONVENTIONS.md](CONVENTIONS.md)).

## 1. Role of this document

| Question | Where to answer |
| --- | --- |
| What was delivered, with what evidence, and what did we learn? | **this log** |
| What is left to do, and in what order? | [ROADMAP.md](ROADMAP.md) (future only; the past is one line per phase) |
| Why was it decided that way? | [adr/](adr/) and §5 below (decision register) |
| How does the physics work? | [theory.md](theory.md) |
| Current state at a glance? | [ROADMAP.md §1](ROADMAP.md) and the root [README](../README.md) |

Golden rule: **state and plan are not duplicated here.** The log records the
completed past and the lessons; open items stay in the roadmap, which points
back to the matching entry. The phase and item numbers are stable — code
comments cite them ("ROADMAP Phase 3 item 4") — so each entry keeps the
original numbering.

The format is inspired by lessons-learned practice in engineering projects
(event → lesson → recommendation), by decision records (ADR) and by *Keep a
Changelog* (dated entries). The tone is blameless: it describes the system and
the evidence, not people.

## 2. How to record

1. **When**: on closing a phase or item (and whenever an investigation ends),
   add an entry in §6 and, if something reusable was learned, a lesson `L-nn`
   (or add the origin to an existing one).
2. **Entry ID**: `P<phase>[letter]` for roadmap phases (`P10b`), `M1` for the
   maintenance track, `GUI-n`, `INV-yyyy-mm-dd` for investigations outside a
   phase.
3. **Entry template** (fixed order, omit what does not apply):

   ```
   #### Pnn — Short title (YYYY-MM-DD or range)
   - **Delivered**: what now exists (files/symbols).
   - **Evidence**: tests, numbers, comparison with the reference.
   - **Decisions**: what was chosen, by whom, and what was rejected.
   - **Lesson**: `L-nn` or a short sentence.
   - **Refs**: ADR n, theory.md §n.
   ```
4. **Lesson `L-nn`**: *Rule* (imperative, verifiable) · *Origin* (entries) ·
   *Applies when*. Lessons are never deleted; a superseded one is marked
   `[SUPERSEDED by L-nn]`.
5. Entries are append-only; a correction of fact is a new entry that
   references the old one.
6. Absolute dates (YYYY-MM-DD).

## 3. Index

| Section | Content |
| --- | --- |
| [§4](#4-lessons-learned) | Lessons `L-01 … L-17` |
| [§5](#5-decision-register) | Decisions, in the order they were taken |
| [§6](#6-record-by-phase) | Record by phase: P0–P7, P8, P8J, P9, P10, P10b–e, P11–P15, M1, GUI, investigations |

---

## 4. Lessons learned

### Numerics and compilers

#### L-01 — Fast-math removes NaN/Inf semantics; do not rely on them

- **Rule**: code that detects NaN or Inf (`g == g`, `ieee_is_nan`, a `huge()`
  bound) is defeated by `-ffinite-math-only`, which `-ffast-math` implies.
  Keep `-fno-finite-math-only` in the optimised flags, and write numerical
  guards so that they do not need a NaN to exist.
- **Origin**: INV-2026-11-02 (all outputs of `labuat_impulse` NaN under
  `build.sh`; finite with the flag removed, `ieee_is_nan` did **not** rescue
  it); P10 item 1 (the clamp of the single-integral kernel was already written
  without NaN comparisons for the same reason); the `test_impedance`
  coincident-segment check, defeated by `-ffast-math`.
- **Applies when**: touching `build.sh` flags, adding a degenerate-case
  fallback, or reading a result file whose outputs are all NaN.

#### L-02 — Test with several compiler configurations, not only the release one

- **Rule**: a change to the solver is validated at least with `-O2 -fopenmp`
  (no fast-math) and with the release flags; a different compiler (ifx) is a
  cheap independent oracle, and `-fpe0 -traceback` turns the first invalid
  operation into a line number.
- **Origin**: INV-2026-11-02 (ifx fast-math with traps ran clean and matched
  gfortran `-O2` to 8 digits, which exonerated the physics and located the
  fault in gfortran `-ffinite-math-only`); the golden failures of
  `channel_tower` under fast-math (reassociation drifts past 1e-6 on the
  two-node harmonic sweeps).
- **Applies when**: any numerical regression, and before blaming the model for
  a NaN or a huge value.

#### L-03 — Bisect flags on a reduced study

- **Rule**: shrink the case until it reproduces in seconds (here
  `fftPoints` 1024 → 32: 10 s instead of 10 min), then change one flag at a
  time. Build variants into a scratch prefix, never over `bin/Tupa`.
- **Origin**: INV-2026-11-02 (four builds, one answer).
- **Applies when**: a long-running case misbehaves.

#### L-04 — "Finite" is not "correct": check plausibility

- **Rule**: after a NaN is fixed, compare magnitudes with a hand estimate
  (source open-circuit voltage, energy). A result that two compilers agree on
  to 8 digits and is still physically absurd points at the model or the input —
  or at the transform (L-05).
- **Origin**: INV-2026-11-02 (peak node voltages of 1.6e10 V from a 6 MV Marx
  generator, identical in gfortran and ifx; traced to the NLT record end).
- **Applies when**: a new laboratory or line case is first run.

#### L-05 — Look at the whole time series before judging a transient

- **Rule**: plot or sample the full record. A maximum over the record can be an
  end-of-window artefact of the transform while the physical part is fine; test
  by changing the record length and checking that the early part does not move.
- **Origin**: INV-2026-11-02 (peak 1.6e10 V was entirely in the last ~25 % of the
  record; 256 vs 1024 samples agree before the end and each diverges at its own
  end, with amplitude ∝ N²).
- **Applies when**: NLT cases with slowly decaying responses; always report the
  usable portion of the record.

#### L-06 — A performance anomaly is a correctness signal

- **Rule**: when a computation is orders of magnitude slower than its
  structure predicts, look for a wrong result that merely stays "plausible".
- **Origin**: the Gauss–Kronrod weights bug of 2026-07-14 (wrong Gauss weights
  made every adaptive integral reach its subdivision limit — 1000× slow and
  still correct to tolerance; gprof signature `qk15`/`dqag` ≈ 999 calls); the
  `0·Inf = NaN` misdiagnosed as a degenerate pair and sent to quadrature for a
  rod crossing the interface (≈ 700 M `inverseDistanceIntegrand` calls,
  `Geometry.f90` comment in `parallelGeometryFactor`).
- **Applies when**: a profile is dominated by one routine.

#### L-07 — Measure before parallelising; keep the shared state out of kernels

- **Rule**: profile the cost split first; parallelise the loop that dominates.
  Library routines with lazy-initialised module state race under threads.
- **Origin**: P3 item 4 / P10 item 4 (the per-frequency LU is ≈ 100 % of the
  cost, so the fill-loop OpenMP was dropped and the frequency loop threaded);
  SLATEC `D1MACH` lazy init set its flag before filling the table
  (`warmUpMachineConstants`, later removed with SLATEC, M1); module-level
  procedure pointers and `COMMON /params/` blocked reentrancy.
- **Applies when**: adding OpenMP, or any module-level mutable state.

### Reference, validation and ports

#### L-08 — The reference implementation has bugs; derive from the model, not the code

- **Rule**: port the model, not the implementation; test each formula against
  an independent integral or published value before inheriting it.
- **Origin**: P1 (self geometry factor wrong in **both** legacies, ADR 0017
  finding 2); P13 item 1 follow-up (parallel-segment closed form wrong for
  opposite-direction pairs of unequal length, inherited by all three ports,
  found 2026-09-30 while importing the tower cases); P11 (`potsolonovo.m`
  wrong by a factor of the segment length, finding 9); `tStructure%air` never
  populated → NaN for any electrode in air (ADR 0019); the C++ call-site bug
  (finding 3).
- **Applies when**: porting any legacy routine.

#### L-09 — Another code's conventions gate cross-validation

- **Rule**: compare physical outputs only, and account for convention
  differences before reading a gap as an error.
- **Origin**: P10 item 5 (TAGS uses `|cos θ|`, which moves a loop oriented
  around itself by 4 % at 100 kHz and the image of a vertical electrode by
  ~1.6 % at 1 MHz; TAGS keeps Γ_ℓ = 1, ≤ 0.2 % below 1 MHz); ADR 0017
  finding 5.
- **Applies when**: comparing against TAGS, PRTL or the legacy codes.

#### L-10 — Same algorithm, same quadrature, or the tolerance moves

- **Rule**: a port that must meet 1e-6 reproduces the quadrature rule, not
  just the integral. A different rule alone cost ~0.09 %.
- **Origin**: Julia prototype (fixed 64×64 midpoint rule) → Phase 8 decision
  to port `dqag_k15` line by line; the adaptive branching can still flip near a
  threshold, so any case outside 1e-6 is investigated before touching the
  tolerance, and a tolerance change needs an ADR.
- **Applies when**: a port or a refactor changes summation order.

#### L-11 — Near-zero rows need an absolute floor

- **Rule**: a relative 1e-6 comparison on a quantity that is zero by symmetry
  tests round-off. Give the keyed comparison an absolute floor or filter those
  rows.
- **Origin**: P8J item 4 (`linha4`/`torre2` zero-by-symmetry current rows at
  the 1e-12 floor).
- **Applies when**: widening fixtures.

#### L-12 — A frozen contract needs a conformance tag and an explicit lag record

- **Rule**: a later phase that changes the contract (schema, case, default
  numerics) carries an item per implementation; if an implementation lags it
  must say so — in a conformance table, and by **rejecting** unknown input
  with an explicit error rather than ignoring it.
- **Origin**: P8 item 10 and P8J item 9 (Julia refused the Phase 9 `signal`
  fields and the `numerics` block while lagging); GUI-3 below (the 3-D view
  skipped `ring`/`branch` as unknown and then crashed on the first node they
  create — silently skipping is worse than refusing).
- **Applies when**: adding an element type or a schema field.

#### L-13 — Change the golden fixtures once

- **Rule**: group every change of default numerics into one phase and
  regenerate the fixtures in one step; a fixture older than the case it
  describes fails for the wrong reason.
- **Origin**: P10 (kernel + Γ(ω) regenerated all eight fixtures once);
  `grid_expected.csv` predated the FIFO fix of ADR 0020 (fixed 2026-10-01);
  `channel_loaded` predates the later 2048-sample change (fails in Fortran and
  Rust until regenerated).
- **Applies when**: planning a numerical default change.

### Process and environment

#### L-14 — Local gate, release profile, warm cache

- **Rule**: with no hosted CI (ADR 0018), the gate is a local
  `fpm build && fpm test` under `--profile release`, plus `cargo`/`Pkg.test()`
  for the ports; debug-profile quadrature suites are not runnable.
- **Origin**: P0–P3 (`test_geometry`'s 10×10 fill exceeded 9 minutes in debug,
  2026-07-05); a cold `fpm test` pays minutes of stdlib compilation; the
  release suite takes about 6 s (2026-10-01).
- **Applies when**: running the tests.

#### L-15 — Pick the binary by mtime; fpm build dirs accumulate

- **Rule**: after a build, check `ls -l bin/Tupa` for a fresh mtime, and use a
  scratch `--prefix` for experiment builds. `build.sh` is not executable —
  run it as `bash build.sh`.
- **Origin**: INV-2026-11-02 (a "rebuilt" binary dated 10:15 was still the old
  one); an `--output-dir` given as a relative path from the wrong directory
  left a stray directory in the repo root.
- **Applies when**: comparing builds.

#### L-16 — Background shell wrappers: do not nest `&`

- **Rule**: start a long run with the tool's own background option, not with a
  trailing `&` inside the command; otherwise the wrapper "completes" at once.
- **Origin**: INV-2026-11-02 (the first O2 run never executed).
- **Applies when**: launching long solver runs.

#### L-17 — A second toolchain exposes portability debts

- **Rule**: keep source portable between gfortran and ifx where cheap.
- **Origin**: INV-2026-11-02 (ifx's preprocessor rejects `__has_include` in
  `Version.F90`; built from a patched scratch copy). M1 already noted that
  steps 1–4 (SLATEC removal) unblock the `ifx` build.
- **Applies when**: touching the preprocessor-dependent files.

---

## 5. Decision register

Decisions of the author (Q&As of 2026-07-05, 2026-07-17, 2026-08-02,
2026-09-30, 2026-10-01, 2026-10-29) and the rules the roadmap used. The
authoritative text is in the ADR; this is the quick index.

| Date | Decision | Where |
| --- | --- | --- |
| 2026-07-05 | Scope, project role (citable reference implementation; usability secondary), convention authority, public contract, compilers, **no hosted CI**, release process, benchmarks, precision, error handling | ADR 0018 |
| 2026-07-05 | MVP = tower-footing grounding under lightning; lines and substations are the application tier | ADR 0018 |
| 2026-07 | Original Matlab code is the **model reference of record** | ADR 0017 |
| 2026-07 | ω₀ = 2π·1 MHz in the Portela soil model | ADR 0007 |
| 2026-07-17 | Heidler standard parametrised form (IEC 62305-1 sets); ideal voltage sources by unit-injection superposition | P7 items 1–2, ADR 0016 |
| 2026-07-17 | Channel: antenna-model route, series impedance calibrated to a prescribed return-stroke speed | P10b, ADR 0025 |
| 2026-07-17 | Touch/step: legacy geometric definitions (1 m, 36-point circle); IEEE Std 80 as context, no derating in the solver | P11, ADR 0027 |
| 2026-07-17 | Cross-medium coupling strictly after the Γ(ω) images | P14 |
| 2026-07-17 | Generic internal impedance: frequency-tabulated R(f), X(f) | P13 item 2 (amended by ADR 0030) |
| 2026-08-02 | Scan grid = the existing `frequencies` block for the interpolated transient; default stays `full` | P9 item 1 |
| 2026-08-02 | Windowing: both placements (spectral and time), selectable in the file | P9 item 2 |
| 2026-08-02 | NLT opt-in first; default flipped only after cross-validation | P9 item 5 → **not flipped** (ADR 0024 §7) |
| 2026-08-02 | The digitised-curve campaign substitutes the "0.1.0" oracle | ADR 0018 postscript |
| 2026-09-30 | Prioritisation rules 1–6 for Phases 8–15 (second implementation first; decided-and-small before open-ended; MVP before tier; dependencies; change fixtures once; new theory last) | ROADMAP, P8 |
| 2026-09-30 | Rust: port the kernels (in-repo GK 7/15, FFT, LU) rather than use crates; 1e-6 kept | ADR 0022 |
| 2026-10-01 | Lightning channel pulled ahead of Phase 11 (numbered 10b) | P10b |
| 2026-10-01 | Fixtures regenerated once for Phase 10 | P10, L-13 |
| 2026-10-11 | Induced voltages and the `branch` element (Phase 12 item 3 pulled forward) | P10d, ADR 0028 |
| 2026-10-29 | A tube is a cross-section entry, not a new element; μr family for steel | P12 item 2, ADR 0030 |
| 2026-10-29 | Coaxial encasement (insulation, concrete, semiconductor) as one thin-layer mechanism; finite volumes later (Phase 15 item 2) | P13 item 3 |
| 2026-10-29 | Armoured cables: armour is an ordinary HEM electrode, interior stamped like `branch`; per-segment co-located interior | P13 item 4 |
| 2026-10-29 | `plane` bars as strips of a continuous sheet (`sheet` kind) | P13 item 5 |
| 2026-11-02 | Release flags keep `-fno-finite-math-only` | INV-2026-11-02, L-01 |

---

## 6. Record by phase

### Foundation (Phases 0–7)

#### P0 — Convention audit and cleanup (done)

- **Delivered**: signs audited against theory.md §2/§5/§6 (propagation
  constant `√(jωμ(σ+jωε))`, decaying `e^{−γR}`, air-image longitudinal sign
  "−"); mesh module naturalised (1-based indices, `initMesh`, dead C-interop
  deleted); test-runner bugs fixed (`check.f90` counter accumulation, bogus
  `TWODQ` argument).
- **Evidence**: signs pinned by `test_mesh.f90`.
- **Refs**: ADR 0008.

#### P1 — Geometry layer (done)

- **Delivered**: `tLine%assemble`; `Geometry.f90` (mean/image distances,
  `g(a,b)` by Gauss–Kronrod, closed-form `g_self`, direction cosines);
  mixed-media pairs skipped as a performance hint only (the zeroing decision
  stays in `Mesh.f90`); solid-conductor internal impedance via SLATEC `ZBESI`.
- **Evidence**: geometry matrices match independent numerical integration to
  1e-6 (`test_geometry.f90`).
- **Lesson**: L-08 (the legacy self-factor formula was wrong).
- **Refs**: ADR 0005, 0017 finding 2.

#### P2 — End-to-end single-frequency solve (done, DC-limit scope)

- **Delivered**: `tStudy%run` (assemble → topology → geometry cached once →
  per-frequency fill → `calcFreq2` → inject → solve); current injection at
  named nodes; fill interface (ADR 0009, motivated by the C++ call-site bug).
- **Evidence**: DC limit against Sunde/Dwight (15 %), low-frequency plateau,
  passivity over 10 Hz–1 MHz (`test_solve.f90`). The Portela-1997 curve match
  was not attempted for lack of tabulated data (theory.md §9.2); the TAGS
  comparison (P10 item 5) later gave the oracle: 0.03 % on this conductor.
- **Refs**: ADR 0003, 0009, 0010.

#### P3 — Frequency sweep, results, output (done, items 1–3)

- **Delivered**: `logFrequencyAxis`; `runSweep` and `tResult` queries
  (`inputImpedance`, `maxVoltageMagnitude`); tidy CSV and JSON writers;
  `example4.f90`.
- **Decisions**: item 4 (OpenMP on the fill loop) **superseded** — blocked on
  `mImpedance` reentrancy, and later shown unnecessary because the LU is
  ≈ 100 % of the cost; replaced by frequency-level parallelism (P10 item 4).
- **Lesson**: L-07.

#### P4 — Dispersive soil (done)

- **Delivered**: `tPortelaSoil` (ω₀ = 2π·1 MHz); `tMaterial%admittance` deferred
  function; shared `calcPropagationConstant`; `calcParamW`.
- **Evidence**: DC-limit convergence to `tLinear`, passivity, formula pinned at
  ω₀ (`test_material.f90`). Curve match: same data gap as P2.
- **Refs**: ADR 0007.

#### P5 — JSON I/O and common cases (done)

- **Delivered**: input schema v1 frozen (`sources`/`frequencies`/`outputs`);
  Fortran reader (`loadStudy`, `runStudyFromFile`); write-time output filtering;
  `common/` cases `portela1997`, `rod`, `grid` (+ later `rod_air`,
  `silva2025_rho*`) with golden `_expected.csv` diffed at 1e-6 by
  `test_common_cases.f90`, with an independent passivity check.
- **Decisions**: the grid was kept to one mesh (non-parallel pairs cost ~1–2 s
  each in 2-D quadrature at the time); the parser stayed within the ADR 0006
  minimal subset. The discretised-ID gotcha is documented in `common/README.md`.
- **Refs**: ADR 0006, 0013.

#### P6 — Sources and time domain (done)

- **Delivered**: `mSignal` (Heidler legacy 6-term set; double exponential
  `f1_2_5`/`f1_2_50`/`f1_2_200`/`f250_2500`, optional Jones front);
  `mTransient` (tail taper, one-sided spectrum, unit-current transfer function,
  conjugate-symmetric IFFT, DC bin replaced by `freqZeroHz`); in-repo
  double-precision radix-2 FFT; `portela1997_transient.json`.
- **Evidence**: internal consistency only (slow-surge GPR tracks the validated
  low-frequency |Zin| within 25 %, `test_transient.f90`).
- **Refs**: ADR 0014, 0015, 0019.

#### P7 — Sources, signals and composite elements (done 2026-09-30)

- **Delivered**: (1) parametrised Heidler (2026-07-17); (2) voltage sources by
  unit-injection superposition (2026-07-17, ADR 0016); (3) `mesh` composite
  element (2026-07-31, ADR 0020); (4) optional anti-alias filter, a Tukey
  roll-off, off by default (2026-09-29, ADR 0021, contributed by acslima).
- **Decisions**: the open-ended Phase 7 backlog was split into Phases 9–15 on
  2026-09-30. Where the former items went: transfer-function-fed transient,
  windowing, Portela front, multiple injections, NLT → Phase 9 items 1–5;
  GPR/touch/step → Phase 11; `tCircumference` → Phase 12 item 1; tubular
  conductor → Phase 12 item 2 (re-scoped 2026-10-29 as cross-section entries);
  series RLC → Phase 12 item 3 (pulled into 10d); `tCatenary` → Phase 13 item 1;
  generic internal impedance (OPGW) → 13 item 2; insulated conductor → 13 item 3;
  multipolar cables → 13 item 4; cross-medium mutual impedance → Phase 14 item 1;
  lightning channel → Phase 10b (air) and Phase 14 item 2 (coupling to buried
  electrodes); multi-layer soil → Phase 15 item 1.
- **Evidence**: a real-sized sweep was impractical before P10 item 1;
  `portelaMesh.json` shipped structure-only until then.

### Second and third implementations

#### P8 — Rust implementation (items 2–8 implemented; items 1 and 9 open)

- **Delivered**: `rust/` package (library `tupa` + binary), modules 1:1 with the
  Fortran ones; conformance harness (`tests/conformance.rs`: keyed rows, 1e-6,
  passivity, guard for new fixtures); schema reader (`json.rs`, all `common/`
  cases load); object model with identical discretised IDs and
  `--dump-structure`; numerical kernels (line-by-line `dqag_k15`, series +
  Hankel Bessel subset); in-repo LU with `izamax` pivoting; sweep, writers, CLI
  parity (+ `--output-dir`); time domain.
- **Evidence**: Milestone 8a (harmonic conformance) met 2026-09-30 on
  `portela1997`, `rod`, `grid`; Phase 9 (worst 1.5e-8) and Phase 10
  (1.3e-10 harmonic, 1.5e-8 NLT) fixtures matched on the day each landed.
  Milestone 8b (full conformance) implemented but not closed.
- **Decisions**: Rust, not Python (Python is the GUI's language, ADR 0011);
  deviations from the proposal recorded in ADR 0022 (in-repo LU instead of
  `faer`, Bessel subset instead of an AMOS port, no FFI); a present value of the
  wrong JSON type is an error rather than 0; no hosted CI; GPL-3.0-or-later.
- **Lesson**: L-10, L-11, L-12. A finding for item 1: `grid_expected.csv`
  predated the ADR 0020 FIFO order and failed positional comparison (2026-09-30,
  gfortran 13), fixed 2026-10-01 by the Phase 10 regeneration (L-13).
- **Refs**: ADR 0022.

#### P8J — Julia port (items 1–4 done; 5–8 open)

- **Delivered**: contributed prototype merged 2026-09-29
  ([PR #1](https://github.com/carloskleber/tupa_ref/pull/1), the first external
  code contribution); realigned module by module 2026-09-30 (`dqag_k15`/`TWODQ`
  ports, closed-form parallel pairs, congruence cache, radix-2 FFT, voltage
  sources, `validateStudyReferences`, writers with `outputs` filtering, CLI
  options). `JSON3` → `JSON`; `FFTW` dropped; `Plots` an optional extension; a
  precompile workload brings a small CLI run to ~0.8 s.
- **Evidence**: harmonic conformance (Milestone 8J-a) met 2026-09-30 —
  `portela1997`/`rod` ~1e-17, `grid` 6e-8; 166 ported checks in ~3 s; cross-check
  of all 47 runnable `common/` outputs (32 sweeps, 15 transients) against fresh
  Fortran and Rust runs: harmonic within 1e-6 except `linha3` (1e-3, sensitive to
  the quadrature tolerance), `linha4`/`torre2` (round-off floor), `linha5`/
  `linha5a` (1e-4); transients within 7e-6 of peak. Phase 10 ported 2026-10-01
  on Julia 1.13 (252 checks).
- **Decisions**: first contribution as shipped was **not conforming** (fixed
  midpoint quadrature, no voltage sources, no `outputs` filtering, no results
  files) — it established the contract ports cleanly (ADR 0002) and fixed the
  quadrature decision for Rust (L-10). Phases 9 and 10 first **lagged** for lack
  of a Julia toolchain / egress policy in the implementing session (the loader
  rejects the new fields explicitly, `PHASE9_LAG`).
- **Lesson**: L-10, L-12. The mesh segment target undershoots (ADR 0024 §8) in
  all three implementations (found on the way, still open).

### Time domain and numerics

#### P9 — Transient pipeline completion (done 2026-09-30)

- **Delivered**: (1) scan-fed transfer function (`signal.transferFunction:
  "interpolated"`, `pchipInterpolate` = Matlab `pchip`); (2) windows
  (`signal.window`, Hann, spectral or time placement); (3) Portela concave-front
  signal (`waveform: "portela"`, α < 0 accepted for subsequent strokes); (4)
  multiple injections (`signal.sources[]`, `sine` waveform, differential ±1
  pattern); (5) Numerical Laplace Transform (`signal.transform: "nlt"`,
  `nltDamping` default ln(N²)/T, Laplace-domain `admittanceLaplace`/
  `calcParamLaplace`/`internalImpedanceLaplace`).
- **Evidence**: scan-fed vs full on `silva2025_*_transient` within 1.7e-5
  (driving point) / 2.7e-5 (remote node) of peak, stated tolerance 1e-4, 12–15×
  faster; NLT 6–10× closer than the FFT to a long-record reference over the
  first half of a short record, degrading as e^{ct}; five new fixtures; existing
  transient outputs byte-identical bar last-digit round-off. Details:
  [validation/phase9-transient-options.md](validation/phase9-transient-options.md).
- **Decisions**: the interpolated mode rejects a scan axis that does not span
  [`freqZeroHz`, `nyquistHz`] (the legacy extrapolates); the erfc tail taper and
  the ADR 0021 anti-alias filter stay separate options; `interpolated` and NLT
  are mutually exclusive; defaults unchanged so no fixture moved. Single
  exponential and impulse/step waveforms are ported only when a case needs them.
- **Refs**: ADR 0015 amendment 2026-09-30; theory.md §8.

#### P10 — Reference kernel, image model and performance (done 2026-10-01)

- **Delivered**: (1) single-integral kernel (`geometryFactor1D`, `numerics.kernel`,
  `--kernel double` keeps the 2-D oracle); (2) frequency-dependent image
  coefficient `Γ(ω)` on both image parcels (`numerics.imageModel: "ideal"` keeps
  ±1); (3) `numerics.maxSegmentLength`; (4) OpenMP over frequencies on
  thread-private meshes (`--threads`); (5) TAGS cross-validation; (6)
  `portelaMesh.json` 32 × 32 m grid. Package version 0.6.0.
- **Evidence**: kernel 8× faster than 2-D on random non-parallel pairs, 7.9e-8
  apart; T junction to 2e-7; Γ(ω) changes the Portela conductor by 1.8e-3 at
  1 MHz (f²-scaling), under 2 points of MAPE against every published curve;
  threaded sweep bit-identical for any thread count, 3.4× on 4 threads (41
  frequencies of `portelaMesh`, 5.2 s → 1.5 s); TAGS agreement ≤ 0.3 % below
  1 MHz on six cases ([validation/tags-xval.md](validation/tags-xval.md)); the
  coarse-vs-fine segmentation test is **not monotone** below 1 m (0.5 m worse than
  1 m), so no convergence-sign claim is made.
- **Decisions**: P1 and P2 grouped so fixtures regenerate once (rule 5); the Γ
  on both parcels follows the Matlab and PRTL-mHEM (TAGS keeps Γ_ℓ = 1); the
  NLT default was **not** flipped (the comparison was harmonic; a TAGS
  time-domain case is the open follow-up); the λ/10 default stays the user's to
  apply; quadrature tolerances revisited (`epsrel = 1e-6` unscaled, `epsabs = 0`,
  `maxint = 500` for the single integral; the 2-D oracle keeps
  `min(la,lb)·1e-6`).
- **Lesson**: L-01 (clamp without NaN comparisons), L-07, L-09, L-13.
- **Refs**: ADR 0024; [validation/phase10-image-model.md](validation/phase10-image-model.md).

#### P10c — Independent transient signals (done 2026-10-01)

- **Delivered**: `signal.signals` (one waveform each, one result set per
  signal, CSV `signal` column) in Fortran, Rust, Julia and the GUI;
  `runSweepUnits` — distinct (node, return node, quantity) terminals share one
  LU per frequency, which also removes the repeated sweep of `sources` entries
  on a shared node; `tools/legacy_import.py` keeps every `sinal`.
- **Evidence**: `portela1997_transient_signals` (Rust 2.4e-13, Julia 1e-6).
- **Refs**: ADR 0026.

### Lightning, induced voltages and laboratory

#### P10b — Lightning channel in air (done 2026-10-01)

- **Delivered**: `channel` element (strike node or free position, incidence and
  azimuth, `speed` or explicit `inductance`, `R'`, graded segments from the
  foot, `calibrate`); two-node sources (`returnNode`; current dipole or
  delta-gap voltage; transient `quantity: "voltage"`); `mChannelCalibration`.
- **Evidence**: Chen's unloaded current on a vertical cylinder over perfect
  ground within 1.5 % of peak; loaded-channel speeds 0.35–0.58c against 0.37–0.60c
  of Table 3 of [44]; the calibrated `L'` within 18 % of the closed form (scale
  0.82–1.05); fixtures `channel_unloaded`, `channel_loaded`, `channel_tower`,
  `channel_tower_gap`. Package 0.7.0.
- **Decisions**: antenna-model route (2026-07-17); `canal.m`'s spacing is not
  ported; no cloud capacitance at the top (no case needed it); channel cases run
  on the NLT path. Julia lags (loader refuses element, `returnNode`, `quantity`).
- **Lesson**: the `channel_loaded` fixture later went stale (L-13).
- **Refs**: ADR 0025; [validation/channel-validation.md](validation/channel-validation.md).

#### P10d — Induced voltages on nearby lines (Fortran done 2026-10-12)

- **Delivered**: lumped series-RLC `branch` element (nodal-admittance stamp,
  harmonic and Laplace; this is P12 item 3, pulled forward because a matched
  line needs a termination resistor); the Nucci et al. 1993 validation anchor
  (base case, Table III trends, Fig. 14 formulation test, sensitivity study,
  lossy-ground variant); `signal.totalVoltage` (adds `s ∫ A_z^i dz` of the
  channel to observed node voltages, `mIncident`).
- **Evidence**: `test_branch.f90` pins the DC cases of theory.md §6 to 1e-9;
  [validation/induced-validation.md](validation/induced-validation.md).
- **Decisions**: Cooray–Rubinstein is the first target for the lossy-ground
  correction (Tossani et al. [89]: adequate for statistical performance,
  Sommerfeld only for very poor ground). Rust, Julia and the GUI lag (loaders
  refuse `branch`; the GUI got it 2026-11-02, GUI-3).
- **Refs**: ADR 0028; theory.md §4.6.

#### P10e — Impulse-laboratory support (Fortran done 2026-10-27)

- **Delivered**: `step` and `branchNorton` waveforms (a charged capacitor behind
  a series R-L-C `branch`, as its Norton current); `lumped` R-L-C embedded in a
  `line` segment (added to the segment's internal impedance, keeps its coupling);
  `plane` wire-grid rectangle (Richmond [118], Ludwig [119]); the 2 MV laboratory
  case (`tools/gen_marx_lab.py`, `tools/impulse_params.py`, IEC 60060-1).
- **Evidence**: `test_marx`, `test_lumped_segment`, `test_plane`.
- **Decisions**: bars of a plane are round wires of an input radius; metal-wall
  surface impedance deferred to P13 item 5; no golden fixtures for the
  laboratory cases; Rust and Julia refuse `plane`, `lumped` and the waveforms.
- **Refs**: [marx_generator.md](marx_generator.md),
  [marx_generator_requirements.md](marx_generator_requirements.md).

#### P12 item 1 — `ring` element, LabUAT (done 2026-10-30)

- **Delivered**: `mElementRing` (legacy `Anel.m`: circle in an arbitrary plane,
  ≥ 3 chords, closed loop, single-medium constraint); `common/labuat_impulse*.json`
  generated by `tools/gen_labuat.py` (32-stage 6 MV Marx, five-unit divider, two
  220 m poles on three porticos with ring yokes, tower mockup; arc closed from
  t = 0, or not).
- **Evidence**: `test_ring`.
- **Open**: see INV-2026-11-02 for the first full run of this case.

### Grounding outputs and coupling

#### P11 — Grounding-safety outputs (done 2026-10-01, harmonic)

- **Delivered**: `mPotentials::potentialsAt` (potential of the solved `i1 + i2`
  at arbitrary points, with the solver's `Γ(ω)` images and only the segments of
  the point's medium); `observation` block (`points[]`, `grid`, `touch`, `steps[]`,
  GPR); `<case>_potentials.csv/.json`; `grid_safety` case.
- **Evidence**: Simpson quadrature of the factor, the far-field point-source
  value of a rod's current, and the rod's own node voltages (surface potential
  within 0.3 % at 10 Hz); `grid_safety` ≈ 25 ms; Rust 2e-8.
- **Decisions**: both input forms from the start; legacy-geometric definitions;
  IEEE Std 80 [42] as context, no derating in the solver; an `observation` block
  beside a transient-only case raises an error. The GUI does not show potentials
  yet; Julia refuses the block.
- **Refs**: ADR 0027.

#### P13 item 1 — `catenary` element (done 2026-09-30)

- **Delivered**: JSON `"catenary"` (theory.md §4.4) in Fortran, Rust and Julia;
  the legacy "catenary" (`Catenaria.m`) is a **parabolic** sag profile, so the
  Matlab-faithful port and the parabolic approximation coincide; pulled forward
  for the legacy `linha4` case.
- **Decisions**: the 3-node variant (`catenaria3`) waits for a case that needs it.
- **Refs**: ADR 0023.

#### P13 item 2 — Cross-section internal impedance (Fortran done 2026-10-29)

- **Delivered**: `crossSections` block (inline or from files); `poleResidue` and
  `table` kinds, each carrying an equivalent radius `r_eq`, scaled by
  conductivity at fixed μr; fixture `common/cross_sections/wire_Cu_r10mm.json`,
  `test_cross_section`.
- **Decisions**: the July table-only decision was **amended** — the NLT evaluates
  `Z_int` at complex `s`, so a pole-residue kind sits beside the table.
- **Refs**: ADR 0030.

#### P14 — Air–soil coupling (Fortran done 2026-10-19, opt-in)

- **Delivered**: mutual impedance between segments in different media
  (`numerics.crossMedium`, theory.md §5.1; candidate route per [35] and Salari
  [66] App. B); channel coupling to buried electrodes (follows from item 1 plus
  the transmitted term in the observation potentials; `channel_tower_cross`).
- **Evidence**: analytic limits and continuity only — no literature curve yet.
- **Decisions**: strictly after Γ(ω) (2026-07-17); opt-in, default not flipped.
- **Refs**: ADR 0029.

### Maintenance

#### M1 — Remove SLATEC (steps 1–7 and 9 done 2026-10-29)

- **Delivered**: native `mBessel` (≤ 3e-14 against a stored AMOS table);
  `warmUpMachineConstants` and the SLATEC path dependency removed; the
  commercial `vendor/slatec_zbesi` dropped.
- **Decisions**: the only SLATEC routine in use was `ZBESI` (the `I₀/I₁` ratio);
  Rust had already replaced it. Steps 1–4 unblock the `ifx` build.
- **Lesson**: L-07, L-17.
- **Refs**: ADR 0031.

### Legacy and project decisions

#### Legacy re-inspection and author interview (July 2026)

- **Delivered**: findings moved to ADR 0017 (numbers preserved): Γ(ω) images are
  original Matlab behaviour; self-factor bug lineage; C++-only call-site bug;
  three solver layouts in the Matlab; convention mixing gates cross-validation;
  feature inventory; legacy input formats. Author decisions of 2026-07-05 moved
  to ADR 0018.
- **Legacy element inventory** (for the record): straight lines (three
  variants), ring, grid, cable, catenary, lightning-channel element, helicoidal
  rod, tube, conduit, lattice and crossarm placeholders, lumped series-RLC
  "impedance" elements (extra unknowns that do not couple electromagnetically —
  their `Zt` row is zeroed), insulated buried cables (leakage through jωε only,
  placeholder theory flagged TODO in the legacy code). The C++ adds bundle and
  L-profile internal impedances and a shielded-wire segment.
- **Lesson**: L-08.

#### Milestones and releases

- **v0.5.0** (tagged 2026-07-31, "first release"): the digitised-curve campaign
  (Grcev, Lima ×2, Poljak, Silva ×2) accepted as the executable-oracle
  substitute for the 0.1.0 bar (decision 2026-08-02).
- **Julia contributed prototype** 2026-09-29; **8a, 8J-a** 2026-09-30.
- **0.6.0** — Phase 10 (2026-10-01). **0.7.0** — Phase 10b.

### GUI

#### GUI-1 — View-only module (G0/G1)

- **Delivered**: Python/PySide6/Qt3D study tree, 3-D view, results and transient
  plots (ADR 0011); overlay plot for independent signals (P10c).

#### GUI-2 — Single virtual environment (2026-11-02)

- **Delivered**: the duplicate `gui/.venv` was removed; the repo-root `.venv` is
  the only environment (`UV_PROJECT_ENVIRONMENT=../.venv uv sync --inexact`,
  documented in `gui/README.md`).
- **Lesson**: `--inexact` keeps the packages the validation scripts already
  had in that environment.

#### GUI-3 — `ring`/`branch` support and NaN warning (2026-11-02)

- **Delivered**: `RingElement` (vertices as in `Ring.f90`; vertex 0 is the node
  `<id>`, others `<id>-01…`) and `BranchElement` in the model, loader, tree and 3-D
  view; a line that references an unknown node is skipped instead of crashing; a
  red banner above both plot panels when the selected series has NaN/Inf samples.
- **Evidence**: `labuat_impulse.json` loads and opens; 37 GUI tests pass, one
  known baseline failure (`portelaMesh` title).
- **Lesson**: L-12 — before the change the GUI skipped `ring` as unknown and then
  failed with `KeyError: 'torgi'` on the line that referred to the ring's node.

### Investigations

#### INV-2026-11-02 — All-NaN outputs in `labuat_impulse` (found and fixed)

- **Symptom**: opening `labuat_impulse_transient_results.json` in the GUI showed
  only the injected-current plot; every node voltage (9) and electrode current
  (3 × 2) was `nan` at all 1024 samples.
- **Cause**: `-ffinite-math-only` (implied by `-ffast-math` in `build.sh`). A
  bisection on a reduced study (`fftPoints` 32): `-O3` finite; `-O3 -ffast-math
  -fno-finite-math-only` finite and equal to the strict build (1.9470616e7);
  `-ffast-math -fno-associative-math` all NaN; `-ffast-math` all NaN. ifx
  (`-O3 -fp-model=fast -fpe0 -traceback`) ran clean and gave the same node
  voltages as gfortran `-O2` (`gif` peak 1.5913243e10, 8 digits), full study in
  5.4 min against 9.6 min (gfortran `-O2`).
- **Where**: the NaN-based fallback of `parallelGeometryFactor`
  (`Geometry.f90`, `ok = (g == g) .and. (abs(g) < huge(1.0d0))`) is the likely
  site, as the closed-form log terms give `0·Inf` for segments touching their
  mirror image; replacing `g == g` by `ieee_is_nan(g)` did **not** cure it, so
  other NaN-dependent code may also be affected. The deliberate NaN of
  `besselI0OverI1` at z = 0 is not reached.
- **Fix**: `-fno-finite-math-only` added to the `-ffast-math` line of
  `fortran/build.sh`; the reduced study is finite with the rebuilt binary.
- **The 1e10 V amplitudes (explained)**: with the corrected binary, the full
  `labuat_impulse_noarc` run (5 min, 12 threads) is finite and **physical for the
  first ~60 % of the record** — a peak near 4.8 MV at `gif`/`divf`/`p1m` (about
  80 % of the 6 MV open-circuit sum, slowly decaying) — and then diverges in the
  last ~25 %, reaching ±1e10 V at the final sample. The arc and no-arc cases give
  the same end values and both compilers agree, so it is not the model, the
  input or the toolchain. A 256-sample run of the same case (window 25.6 µs)
  reproduces samples 50 and 100 of the 1024 run exactly (4.1e6 and 4.8e6 V) and
  blows up at **its own** last samples (up to 8.5e8 V). The artefact scales as
  e^{cT} = N² (16× smaller for N = 256, 12× observed): it is the documented
  end-of-record degradation of the Numerical Laplace Transform (theory.md §8,
  log P9), here severe because the Marx response decays slowly and has
  high-frequency content that the band limit (5 MHz) truncates.
- **Consequence**: results of the LabUAT cases are usable only before roughly
  half to two thirds of the window; read the tail as an artefact. Options to
  try (roadmap near-term item 1): the spectral Hann window (`signal.window`),
  a smaller `nltDamping`, a longer window with the usable part cropped.
- **Lesson**: L-01 to L-05, L-15 to L-17.
