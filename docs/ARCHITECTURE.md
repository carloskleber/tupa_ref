# TUPÃ — Architecture

Architectural vision for the reference implementation: components, layers,
flows, data management and state. The physics behind every component is
specified in [theory.md](theory.md) (normative); forward plans live in
[ROADMAP.md](ROADMAP.md); individual decisions in [adr/](adr/). Terms are
defined in [GLOSSARY.md](GLOSSARY.md).

Everything below describes the **Fortran** implementation (package version
0.7.0) as of 2026-10-01. Statements about intent (rather than code) are marked
*(intent)*; the object model itself is language-agnostic by decision
([ADR 0002](adr/0002-language-agnostic-object-model.md)) and the Rust and
Julia ports (§8) map onto the same components.

---

## 1. Architectural style

Two-layer design: an **object-model layer** (derived types, type-bound
procedures, abstract interfaces) for domain modelling and orchestration, over
**procedural numerical kernels** (plain modules operating on arrays) for the
physics. The split is deliberate:

- the object model is what ports to other languages (ADR 0002);
- the kernels are what gets validated against theory and legacy code, and
  they must stay auditable — simple loops, explicit formulas, one LAPACK
  call per solve ([ADR 0003](adr/0003-augmented-zeq-system.md));
- the geometry kernel (`mGeometry`) intentionally has **no dependency on the
  object model** — it takes plain endpoint/radius arrays, so it can be
  tested and reasoned about in isolation.

The time domain is **not a second solver**. It is a thin pipeline on top of
the frequency-domain one: the transient driver (`mTransient`) calls
`tStudy%runSweep`/`runSweepUnits` for unit-excitation transfer functions
H(f) and does only spectrum arithmetic and FFTs itself
([ADR 0015](adr/0015-time-domain-signal-schema.md)).

Correctness is preferred over performance, except where a speed-up is
bit-identical or oracle-checked (the single-integral kernel with the 2-D
oracle kept, the geometry cache, the threaded sweep — [ADR 0024](adr/0024-phase10-numerics.md)).

## 2. Components and layers

```
 I/O boundary      orchestration           domain model                  numerical kernels
┌───────────┐    ┌────────────────┐    ┌──────────────────────┐
│ app/main  │───►│ tupa (module)  │───►│ tStudy               │    ┌─────────────────────────┐
│ CLI flags │    │ loadStudy      │    │  ├ tStructure        │    │ mGeometry (+ Cache)     │
└───────────┘    │ validate…Refs  │    │  │  ├ tNode[]        │    │  G, Gi, R̄, R̄i, cosθ     │
                 │ runFromFile    │    │  │  ├ tElectrode[]   │    ├─────────────────────────┤
 ┌──────────┐    └───┬────────┬───┘    │  │  ├ tElement LL    │    │ mMesh                   │
 │mJson     │◄───────┘        │        │  │  │  tLine          │    │  A/B/C/D topology       │
 │Parser    │                 │        │  │  │  tCatenary      │    │  calcParamW/Laplace     │
 │(json-    │                 │        │  │  │  tMeshElement   │    │  calcZSelf/Mutual       │
 │ fortran) │                 │        │  │  │  tChannel       │    │  calcFreq2 → ZGESV      │
 └──────────┘                 │        │  │  └ tMaterial LL    │    ├─────────────────────────┤
                              │        │  │    tLinear         │    │ mImpedance              │
        ┌─────────────────────▼──┐     │  │    tPortelaSoil    │    │  GK 7/15 quadrature     │
        │ mTransient             │     │  │    tVisacroAlipio… │    │  ZBESI internal Z       │
        │  signal → FFT → H·X →  │────►│  ├ tMesh (per ω)      │    └─────────────────────────┘
        │  IFFT  (NLT optional)  │     │  └ tResult[]          │
        │ mSignal  mFft          │     │     V / I_long / I_tr │    ┌─────────────────────────┐
        │ mChannelCalibration    │     └──────────────────────┘    │ mResultsWriter          │
        └────────────────────────┘                                  │  CSV / JSON (ADR 0012,  │
 support: mCtes (constants, dp kind)  mError (feh)  mVerbosity      │  0015)                  │
                                                                    └─────────────────────────┘
```

| Component | File | Role | Status |
| --- | --- | --- | --- |
| `main` | `fortran/app/main.f90` | CLI entry: parses `-v/-q`, `--epsrel`, `--kernel`, `--image-model`, `--threads`, `--no-cache`, then hands the JSON path to `runFromFile` | working |
| `tupa` | `fortran/src/Tupa.f90` | JSON → object model mapping (`loadStudy`), pre-run reference validation (`validateStudyReferences`), the end-to-end driver `runFromFile` (sweep and/or transient, writes result files) and `runStudyFromFile` (sweep only, library use) | working; `loadStudy` is long (§7) |
| `mJsonParser` | `fortran/src/JsonParser.f90` | Thin wrapper over json-fortran (ADR 0006) | working, full JSON grammar |
| `tStudy` | `fortran/src/Study.f90` | Top container: structure, mesh, geometry cache, results. `prepareStudy` (once), `run` (one ω), `runSweep` (frequency axis, OpenMP), `runSweepUnits` (multi-terminal transfer functions), `inputImpedance`, `maxVoltageMagnitude`, `report` | working |
| `tStructure` | `fortran/src/Structure.f90` | Owns nodes/electrodes (dynamic arrays), elements/materials (linked lists), air and soil media; `assembleStructure` | working |
| `tElement` family | `fortran/src/element/` | Self-discretising geometric generators: `tLine`, `tCatenary` (extends `tLine`, parabolic sag, ADR 0023), `tMeshElement` (composite rectangular grid built from `tLine` bars, ADR 0020), `tChannel` (loaded lightning channel, ADR 0025) | all working |
| `tMaterial` family | `fortran/src/Material.f90` | Medium immittance per model: `tLinear`, `tPortelaSoil` (ADR 0007), `tVisacroAlipioSoil` (theory.md §7); `admittance(ω)` and `admittanceLaplace(s)` deferred bindings | working |
| `tNode`, `tElectrode` | `fortran/src/Node.f90`, `Electrode.f90` | Mesh primitives; an electrode may be series-`loaded` (R′, L′ replace the skin-effect impedance) | working |
| `mGeometry` | `fortran/src/Geometry.f90` | Frequency-independent geometry matrices (ADR 0004); kernel selectable (`setGeometryKernel`) | working, tested |
| `mGeometryCache` | `fortran/src/GeometryCache.f90` | Memo table for quadrature geometry factors, keyed by segment-pair congruence (6 rounded distances, 8-fold canonicalised) | working; not thread-safe (§7) |
| `mMesh` | `fortran/src/Mesh.f90` | Topology A–D, medium constants (real ω and Laplace s), image reflection coefficients Γ, impedance entries (ADR 0009), `Zeq` assembly, `ZGESV` solve with one or several right-hand sides (ADR 0003, 0016) | working, tested |
| `mImpedance` | `fortran/src/Impedance.f90` | Adaptive Gauss–Kronrod quadrature (mHEM single integral `geometryFactor1D`; nested 2-D `geometryFactor2D` as oracle); Bessel internal impedance (SLATEC `ZBESI`) | working; 2-D path non-reentrant (§7) |
| `mSignal` | `fortran/src/Signal.f90` | Excitation waveforms `tSignal` → `tHeidlerSignal`, `tDoubleExpSignal`, `tPortelaSignal`, `tSineSignal`; `tSignalSlot` (terminal, return node, quantity, waveform); tail taper | working |
| `mFft` | `fortran/src/Fft.f90` | Double-precision radix-2 FFT (ADR 0014) | working |
| `mTransient` | `fortran/src/Transient.f90` | Transfer-function transient driver: `transientResponse` (one source), `transientResponseSources` (superposed), `transientResponseSignals` (independent signals, ADR 0026); scan-fed pchip H(f), Hann window, anti-alias filter (ADR 0021), NLT | working |
| `mChannelCalibration` | `fortran/src/ChannelCalibration.f90` | Scales a channel's closed-form L′(z) so its simulated return-stroke speed hits the target (Baba–Rakov metric, NLT ramp response on a throw-away `tStudy`) | working |
| `tResult` family | `fortran/src/Result.f90` | `tVoltages`, `tLongCurrents`, `tTransCurrents`: own copies of entity IDs and ω axis, `get`/`set`/`entityId` accessors | working, filled by `runSweep` |
| `mResultsWriter` | `fortran/src/ResultsWriter.f90` | Harmonic CSV/JSON (ADR 0012) and transient CSV/JSON (single/multi-source, and the independent-signals form, ADR 0015/0026) | working, tested |
| `mCtes`, `mError`, `mVerbosity` | `Ctes.f90`, `Error.f90`, `Verbosity.f90` | Constants (`dp`, μ₀, ε₀, …); feh error boundary; global quiet/normal/verbose level | working |

## 3. Execution flow

`runFromFile` is the single end-to-end path. A case may carry a `sources` +
`frequencies` block (harmonic sweep), a `signal` block (transient), both, or
neither (structure-only: assemble and report). The two analyses are
independent and each writes its own files to the working directory.

```
load JSON ──► loadStudy ──► tStudy (structure: nodes, materials, elements;
                              numerics: kernel / imageModel / maxSegmentLength)
                │  calibrateChannels(study)   channels with "calibrate": true
                ▼
     validateStudyReferences: sources[].node/returnNode, signal source/observe
     nodes and electrodes, outputs.* resolved against the structure — a bad ID
     raises here, before any expensive step
                ▼
  ┌── harmonic: study%runSweep(freqHz, sources…) ─────────────────────────┐
  │     prepareStudy (once, first call):                                   │
  │        structure%assembleStructure()   elements discretise themselves  │
  │        buildGeometryMatrices(p1,p2,r)  G, Gi, R̄, R̄i, cosθ  [cache]    │
  │        initMesh, calcTopology          A, B, C, D                      │
  │     per ω (OpenMP, one thread-private tMesh per thread):               │
  │        calcParamW / calcParamLaplace   cE, cM, γ, Γ                    │
  │        fillAtFrequency                 calcZSelf/calcZMutual, calcFreq2│
  │        injectSignal / voltage-source   ZGESV → u, i1, i2               │
  │     copy into tVoltages / tLongCurrents / tTransCurrents               │
  │  study%report;  writeResultsCsv / writeResultsJson   (ADR 0012)        │
  └────────────────────────────────────────────────────────────────────────┘
  ┌── transient: transientResponseSources | transientResponseSignals ─────┐
  │     sample each tSignal on the time axis, taper, forward FFT           │
  │     study%runSweepUnits: unit excitation of every distinct terminal,   │
  │        one factorisation per ω (or per scan point, then pchip) → H_k   │
  │     Σ_k H_k·X_k  (or one set per signal), window / anti-alias filter,  │
  │        conjugate-symmetric rebuild, inverse FFT  (NLT: damped by       │
  │        e^{-ct}, solved at s = c + jω, undamped)                        │
  │  writeTransientResults* / writeTransientSignals*   (ADR 0015, 0026)    │
  └────────────────────────────────────────────────────────────────────────┘
```

Order matters in one place: `runFromFile` writes the harmonic results
**before** the transient run, because the transient driver calls the sweep
internally (its own unit-current, FFT-bin frequency axis) and overwrites the
study's stored sweep results.

Assembly uses inversion of control: `tStructure` iterates its element list
and each element calls back into the structure (`addNode`, `addElectrode`)
to register what it creates. Elements receive the structure as `class(*)`
and downcast with `select type` — a workaround for Fortran's circular-module
restriction between `mElement` and `mStructure`. Composite elements reuse
the simple ones: `tMeshElement` plants its main nodes and calls the `tLine`
bar-assembly routine per bar; `tCatenary` is a `tLine` with a displaced node
placement.

## 4. Data management

**Sources.** One JSON study file per run — the only configuration
mechanism. The schema (structure; `sources`/`frequencies`/`outputs`,
ADR 0013; `signal`, ADR 0015/0026; `numerics`, ADR 0024) is documented in
[../common/README.md](../common/README.md). CLI flags set process-wide
defaults for the numerics (kernel, image model, quadrature tolerance, threads,
cache); a study's own `numerics` block overrides them for that study. No
environment variables beyond `OMP_NUM_THREADS`, no config files, no network.

**Modeling.** The JSON maps 1:1 onto the object model: user *boundary* nodes
and elements are inputs; assembly derives the flat arrays the solver
consumes (`tStructure%nodes`, `%electrodes` with `n1`/`n2` connectivity).
Derived state has three lifetimes:

- *once per study* (`tStudy%prepared`): assembled structure and the geometry
  matrices `geomG/Gi/Rbar/Rbari/CosTheta/CosThetaI`, per-electrode length,
  radius and medium (`geomPos`: air if the midpoint has z > 0), and the
  topology matrices inside `tMesh`;
- *per frequency*: impedance matrices, `Zeq` and the solution vectors, held
  by a `tMesh`. `runSweep` gives each thread its own `tMesh` and copies
  voltages/currents out into `tVoltages`/`tLongCurrents`/`tTransCurrents` as
  each frequency completes — the accumulation across the sweep lives in
  `tStudy`, not in a mesh;
- *per run*: the stored sweep metadata (`sweepFreqHz`, `sweepSourceIds`,
  `sweepReturnIds`, `sweepSourceCurrentsFreq`, `sweepDamping`) — what
  `inputImpedance` needs to interpret the stored solution.

**State & ownership.**

- `tStructure` owns everything geometric. Nodes/electrodes: growable arrays
  (doubling). Elements/materials: linked lists filled by `move_alloc` (the
  structure takes ownership), released by a `final` destructor. The air
  medium is hardcoded vacuum ([ADR 0019](adr/0019-air-medium-hardcoded-vacuum.md));
  soil is the case's `soil` material.
- `tElectrode%material` is a pointer into the owning element's allocatable
  material copy; elements outlive electrodes within a study, so the alias is
  safe for the current lifecycle. Elements also keep private copies of the
  nodes/electrodes they created, for reporting only — the structure's arrays
  are the solver's single source of truth.
- `tMesh` owns all matrices as allocatables sized by `initMesh(nn, ns)`.
  `ZGESV` factorises `Zeq` **in place** — after a solve, `Zeq` holds the LU
  factors and `fillAtFrequency` must be re-run before another solve
  (`runSweepUnits` instead factorises once and solves several unit
  right-hand sides against the same factors).
- `mGeometryCache` is module-level state (a hash table); it is cleared per
  `buildGeometryMatrices` and whenever the quadrature tolerance changes.

**Persistence.** Input is read once. Results are written by
`runFromFile` (or by any caller of `mResultsWriter`) to the working
directory, named from the case basename:

| Analysis | Files | Schema |
| --- | --- | --- |
| harmonic | `<case>_results.csv`, `<case>_results.json` | tidy/long CSV; JSON per [ADR 0012](adr/0012-results-json-schema.md) (also lists channels) |
| transient | `<case>_transient_results.csv`, `…json` | ADR 0015; independent-signal form per ADR 0026 |

Both honour the case's `outputs` selection (nodes, electrodes, quantities).
Nothing is written as a side effect of `runSweep` itself. The GUI reads these
files, never solver internals (§8).

## 5. Concurrency, precision, errors, logging

- **Threading**: `tStudy%runSweep` and `runSweepUnits` solve the frequencies
  concurrently (OpenMP, ROADMAP Phase 10 item 4,
  [ADR 0024](adr/0024-phase10-numerics.md) §4) — one thread-private `tMesh`
  per thread, the study read-only inside `solveAtFrequency`/`fillAtFrequency`,
  results written at disjoint columns, so a sweep is bit-identical for any
  thread count. `build.sh` passes `-fopenmp`; a plain `fpm build` is serial
  (the directives are comments). The geometry build stays serial (it runs
  once; the single-integral kernel is re-entrant, the 2-D oracle and the
  cache are not). The transient driver and channel calibration reach the same
  loops through `runSweep`/`runSweepUnits`. CLI `--threads <n>`;
  `OMP_NUM_THREADS` otherwise.
- **Precision**: uniform double precision. `mCtes` exports `dp`
  (`kind(1.0d0)`); new code uses `real(dp)`/`complex(dp)`, legacy `kind=8`
  declarations migrate gradually ([CONVENTIONS.md](CONVENTIONS.md)).
- **Error handling**: all fatal conditions route through
  `mError%raiseError`, which triggers a critical feh `ErrorInstance`
  (reports and halts). No `stop`/`error stop` remains in project code.
  Solver-level numerical failure (ZGESV `INFO ≠ 0`) is returned as a code to
  the caller; inside the threaded loops it is recorded under an OpenMP
  `critical` section and raised after the parallel region. Conductor-material
  checks (`checkConductorMaterials`) run before the loop for the same reason.
- **Logging**: `print`-based, with ANSI colours from `mCtes`; `tStudy%report`
  builds human-readable summaries. No logging framework, none planned.
  `mVerbosity` holds a global `VERB_QUIET`/`VERB_NORMAL`/`VERB_VERBOSE` level
  (`verbose(level, msg)`), set from the `-v`/`--verbose`/`-q`/`--quiet`
  flags; it gates routine output (report, sweep summary, geometry-cache
  statistics, elapsed time) but never errors/warnings.

## 6. Extension mechanisms

| Axis | How | Guard rails |
| --- | --- | --- |
| New geometry (ring, tower, …) | Extend `tElement`, implement `assemble` + `report`; add a case to the `elements` dispatch in `loadStudy` | Priority order in ROADMAP Phases 12–13 (ring next; grid done — Phase 7 item 3; catenary done — ADR 0023; lightning channel done — Phase 10b, ADR 0025) |
| New soil/conductor model | Extend `tMaterial`, implement `admittance`, `admittanceLaplace`, `report` (`calcPropagationConstant` is inherited) | One subtype per literature reference, named after it (ADR 0007). Conductor internal impedance is still `tLinear`-only (§7) |
| New excitation waveform | Extend `tSignal`, implement `waveform(t)`; add a constructor and a `signal.waveform` case in `parseSignalWaveform` | Must be sampled on the transient time axis and tail-tapered like the others |
| New output | Extend `tResult`, implement `alloc`/`get`/`set`; wire into `runSweep` and `mResultsWriter` | Use the legacy output-class inventory to prioritise (ROADMAP P7, Phase 11) |
| Alternate geometry-factor kernel | `mGeometry%setGeometryKernel`; the mHEM 1-D form is the default (`mImpedance%geometryFactor1D`), the 2-D quadrature stays as test oracle and is selectable (`numerics.kernel`, `--kernel`) | ROADMAP P1 (done, Phase 10 item 1); ADR 0004, 0024 |
| Image reflection model | `mMesh%calcImageCoefficients` fills `gammaAir`/`gammaSoil` (Γ(ω) default, ideal ±1 selectable via `numerics.imageModel`, `--image-model`); `calcZSelf`/`calcZMutual` multiply the image parcel by them | ROADMAP P2 (done, Phase 10 item 2); ADR 0009 keeps call sites untouched, ADR 0024 |
| Series loading of a segment | `tElectrode%loaded` + `loadResistance`/`loadInductance` replace the skin-effect impedance in `mStudy%segmentInternalImpedance` (used by the lightning channel; also the slot for generic internal-impedance models, ROADMAP Phase 13 item 2) | ADR 0025 |
| Transform / transfer-function mode | `tTransientOptions` (`transform` FFT or NLT, `transferFunction` full or interpolated, `window`, `windowPlacement`, anti-alias start) | ADR 0015 amendment 2026-09-30, ADR 0021 |
| Several transient excitations on one structure | `tStudy%runSweepUnits` solves every distinct unit terminal from one factorisation per frequency (multi-RHS, observed rows only); `transientResponseSignals` multiplies each signal's spectrum into the shared H (`independent = .true.`: one response set per signal, ADR 0026; otherwise superposed, ADR 0015) | |
| Two-node sources | `returnNodeIds` of `tStudy%run`/`runSweep`: the unit pattern becomes the ±1 dipole (`injectionPatterns`); the solver kernel still sees only nodal current injections | ADR 0025 (extends ADR 0010, 0016) |
| Other languages | Re-implement the object model; must pass `common/` cases | ADR 0002; JSON schema is the public contract (see §8) |

The **public interface** of the project is the JSON schema plus the
`common/` reference cases — nothing else. All Fortran module APIs are
internal and may change without notice (author decision, ROADMAP §9).

## 7. Known architectural debts

Tracked, deliberate, and safe at the current scale:

- `mImpedance` keeps a legacy `COMMON /params/` block and module-level
  function pointers for the nested 2-D quadrature (`geometryFactor2D`, now
  the test oracle) — not reentrant. The default `geometryFactor1D` has no
  such state; the geometry build is nevertheless left serial (it runs once).
- `mGeometryCache` shares one hash table across all callers and mutates it on
  lookup/insert: not thread-safe, also the reason the geometry build is
  serial.
- SLATEC's `D1MACH` fills its constant table lazily and not race-free (it
  raises its flag before filling the table): `warmUpMachineConstants` calls
  it once before the threaded sweep. Any new threaded entry point that can
  reach `ZBESI` first must do the same.
- `mJsonParser` (a json-fortran wrapper, ADR 0006) keeps one module-level
  `json_core` instance — non-reentrant, fine for this project's
  one-file-at-a-time usage but would need revisiting for a hypothetical
  concurrent/multi-file caller.
- `tStudy%mesh`'s per-frequency state (see §4) makes the frequency loop
  sequential over one mesh instance; the sweeps therefore give every thread
  its own copy of the mesh (memory ≈ one `Zeq` of $(n_n + 2n_s)^2$ complex
  numbers per thread). `-fopenmp` also puts automatic arrays on the stack:
  large scratch arrays must be `allocatable`.
- Dense augmented solve scales as $(n_n + 2n_s)^3$ — fine for the reference
  scale (hundreds of segments), by design (ADR 0003).
- `tupa%loadStudy` is a single ~650-line routine with a long optional-argument
  interface (sources, signal, outputs, numerics each returned through its own
  argument); `runFromFile` mirrors that with a large set of locals. A
  "parsed case" derived type bundling the blocks would shrink both — not done
  because the JSON schema, not this interface, is the public contract.
- Conductor internal impedance (skin effect) supports only `tLinear`
  materials; other conductor models, and bundle/cable internal impedance,
  need the generic internal-impedance slot of ROADMAP Phase 13 item 2.
- `tChannel` calibration (`calibrateChannels`) runs inside `loadStudy` — a
  load step that performs full NLT simulations (bounded to 2.5 km channel
  length, ≤ 20 iterations). It is opt-in per channel (`calibrate`), but it
  makes loading a case non-trivial in cost.

## 8. Other implementations and tools

The Fortran code is the implementation of record. Three siblings consume the
same contract (JSON schema + `common/` cases + result-file formats) and share
no code with it:

| Module | Role | Mapping to this document |
| --- | --- | --- |
| `rust/` | Second conforming implementation, no LAPACK/SLATEC (own linear algebra, Bessel, FFT); [ADR 0022](adr/0022-rust-implementation.md), [README](../rust/README.md) | Module-by-module mirror of §2 (`study.rs`, `mesh.rs`, `transient.rs`, `channel_calibration.rs`, `element/{line,catenary,mesh,channel}.rs`, …); matches every regenerated fixture |
| `julia/` | Third conforming implementation, scripting/REPL route; [README](../julia/README.md) | Same mirror (`src/*.jl`, `src/element/`); lags on parts of Phase 9 and 10b — status table in ROADMAP §1 and Phase 8J |
| `gui/` | View-only PySide6 module: study tree, 3-D view, harmonic/transient plots ([ADR 0011](adr/0011-gui-module-technology-and-scope.md), [GUI_SDD.md](GUI_SDD.md)) | Reads input and result JSON only (`data/loader.py`), never solver internals; works with any of the three solvers |
| `tools/legacy_import.py` | Converts legacy Matlab case files to schema-v1 JSON (ADR 0023) | Offline, outside the run pipeline |

Cross-implementation agreement is checked by the golden fixtures in
`common/` (`*_expected.csv`) at 1e-6 relative; each implementation has its
own conformance harness. Cross-code validation against the TAGS program and
digitised published curves lives in [validation/](validation/README.md) and
`benchmarks/`.
