# TUPÃ — Architecture

Architectural vision for the reference implementation: components, layers,
flows, data management and state. The physics behind every component is
specified in [theory.md](theory.md) (normative); forward plans live in
[ROADMAP.md](ROADMAP.md); individual decisions in [adr/](adr/). Terms are
defined in [GLOSSARY.md](GLOSSARY.md).

Everything below describes the **Fortran** implementation as of 2026-07-10.
Statements about intent (rather than code) are marked *(intent)*; the object
model itself is language-agnostic by decision
([ADR 0002](adr/0002-language-agnostic-object-model.md)) and future
Python/Rust implementations must map onto the same components.

---

## 1. Architectural style

Two-layer design: an **object-model layer** (derived types, type-bound
procedures, abstract interfaces) for domain modelling and orchestration, over
**procedural numerical kernels** (plain modules operating on arrays) for the
physics. The split is deliberate:

- the object model is what ports to other languages (ADR 0002);
- the kernels are what gets validated against theory and legacy code, and
  they must stay auditable — simple loops, explicit formulas, one LAPACK
  call ([ADR 0003](adr/0003-augmented-zeq-system.md));
- the geometry kernel (`mGeometry`) intentionally has **no dependency on the
  object model** — it takes plain endpoint/radius arrays, so it can be
  tested and reasoned about in isolation.

Correctness is preferred over performance everywhere until validation exists
(reference-implementation role, [ROADMAP §9](ROADMAP.md)).

## 2. Components and layers

```
 I/O boundary          orchestration            domain model                 numerical kernels
┌─────────────┐      ┌──────────────┐      ┌───────────────────────┐      ┌──────────────────────┐
│ app/main    │─────►│ tupa (module)│─────►│ tStudy                │      │ mGeometry            │
│ (CLI)       │      │ loadStudy    │      │  ├ tStructure         │      │  buildGeometryMatrices│
└─────────────┘      │ runFromFile  │      │  │  ├ tNode[]         │      │  g, gi, R̄, R̄i, cosθ  │
                     └──────┬───────┘      │  │  ├ tElectrode[]    │      └──────────┬───────────┘
                            │              │  │  ├ tElement LL ────┼── assemble ──┐  │
                     ┌──────▼───────┐      │  │  │  (tLine, …)     │              │  ▼
                     │ mJsonParser  │      │  │  └ tMaterial LL    │      ┌──────────────────────┐
                     │ (hand-rolled │      │  │     (tLinear,      │      │ mMesh                │
                     │  subset,     │      │  │      tPortelaSoil) │      │  A/B/C/D topology    │
                     │  ADR 0006)   │      │  ├ tMesh ◄────────────┼──────│  calcParam (γ, cE,cM)│
                     └──────────────┘      │  └ tResult[] (declared│      │  calcZSelf/Mutual     │
                                           │     not filled yet)   │      │  Zeq → ZGESV → u,i1,i2│
 support: mCtes (constants, dp kind)       └───────────────────────┘      ├──────────────────────┤
          mError (feh raiseError)                                         │ mImpedance           │
                                                                          │  GK 7/15 quadrature  │
                                                                          │  ZBESI internal Z    │
                                                                          └──────────────────────┘
```

| Component | File | Role | Status |
| --- | --- | --- | --- |
| `main` | `fortran/app/main.f90` | CLI entry: JSON path → `runFromFile` → assemble/report, then (only if the case carries `sources`/`frequencies`) `runSweep` + write `<basename>_results.csv`/`.json` to the working directory | working |
| `tupa` | `fortran/src/Tupa.f90` | JSON → object model mapping | working for the current schema |
| `mJsonParser` | `fortran/src/JsonParser.f90` | Thin wrapper over json-fortran (ADR 0006) | working, full JSON grammar |
| `tStudy` | `fortran/src/Study.f90` | Top container; owns structure, mesh, results; `run` solves one ω, `runSweep` drives the full frequency axis | working (Phase 2 `run`, Phase 3 `runSweep`) |
| `tStructure` | `fortran/src/Structure.f90` | Owns nodes/electrodes (dynamic arrays) and elements/materials (linked lists); `assembleStructure` | working |
| `tElement`/`tLine` | `fortran/src/element/` | Geometric generators; self-discretising | `tLine` only |
| `tMaterial` family | `fortran/src/Material.f90` | γ(ω) per medium; `tLinear` working, `tPortelaSoil` placeholder (ADR 0007) | partial |
| `tNode`, `tElectrode` | `fortran/src/Node.f90`, `Electrode.f90` | Mesh primitives | working |
| `mGeometry` | `fortran/src/Geometry.f90` | Frequency-independent geometry matrices (ADR 0004) | working, tested |
| `mMesh` | `fortran/src/Mesh.f90` | Topology, medium constants, impedance entries (ADR 0009), Zeq assembly, ZGESV solve (ADR 0003) | working, tested; fill loop pending |
| `mImpedance` | `fortran/src/Impedance.f90` | Adaptive Gauss–Kronrod quadrature; Bessel internal impedance | working, tested; non-reentrant (§7) |
| `tResult` family | `fortran/src/Result.f90` | Output containers (voltages, currents vs ω): own copies of entity IDs + ω axis, `get`/`set`/`entityId` accessors | working, filled by `tStudy%runSweep` |
| `mResultsWriter` | `fortran/src/ResultsWriter.f90` | CSV (tidy/long) and JSON (ADR 0012 v0) results writers | working, tested |
| `mCtes`, `mError` | `fortran/src/Ctes.f90`, `Error.f90` | Constants (`dp` kind, μ₀, ε₀, …); feh error boundary | working |

## 3. Execution flow

Intended pipeline (theory.md §1; steps marked ✗ are not yet wired — see
ROADMAP Phase 6):

```
load JSON ──► build tStudy ──► structure%assembleStructure()
                                    │  elements discretise themselves:
                                    │  resolve IDs → internal nodes → electrodes
                                    ▼
              validateStudyReferences: sources[].node, signal.sourceNode/
              observeNodes/observeElectrodes, outputs.nodes/electrodes
              resolved against the assembled structure — bad ID raises here,
              before any of the (expensive) steps below run
                                    ▼
              buildGeometryMatrices(p1, p2, radius)          [once per geometry]
                    G, Gi, R̄, R̄i, cosθ, cosθi
                                    ▼
              calcTopology(n1, n2)   →  A, B, C, D           [once per topology]
                                    ▼
        ┌── runSweep: for each ω in freqHz (logFrequencyAxis or custom) ──┐
        │   calcParam(ω)         medium constants cE, cM, γ              │
        │   calcZSelf/Mutual     Zlong, Ztrans entries                   │
        │   calcFreq2()          assemble augmented Zeq                  │
        │   injectSignal(...)    RHS = current injections                │
        │                        ZGESV → u, i1, i2                       │
        │   store into tVoltages/tLongCurrents/tTransCurrents            │
        └──────────────────────────────────────────────────────────────┘
                                    ▼
              inputImpedance(nodeId) / maxVoltageMagnitude() queries
                                    ▼
              writeResultsCsv / writeResultsJson (ADR 0012)
                                    ▼
              mTransient: FFT or NLT ↔ time domain (Phase 6, Phase 9;
              scan-fed H(f), windows, multi-source — ADR 0015)
```

Assembly uses inversion of control: `tStructure` iterates its element list
and each element calls back into the structure (`addNode`, `addElectrode`)
to register what it creates. Elements receive the structure as `class(*)`
and downcast with `select type` — a workaround for Fortran's circular-module
restriction between `mElement` and `mStructure`.

## 4. Data management

**Sources.** One JSON study file per run (title, soil, nodes, materials,
elements) — the only configuration mechanism; schema and subset limits are
documented in [../common/README.md](../common/README.md). No environment
variables, no config files, no network access.

**Modeling.** The JSON maps 1:1 onto the object model: user *boundary* nodes
and elements are inputs; assembly derives the flat arrays the solver
consumes (`tStructure%nodes`, `%electrodes` with `n1`/`n2` connectivity).
Geometry matrices and topology matrices are derived state, computed once;
impedance matrices and the solution vectors are per-frequency state inside
`tMesh` (each frequency overwrites the previous one — `tStudy%runSweep`
copies `tMesh%voltage`/`current1`/`current2` out into
`tVoltages`/`tLongCurrents`/`tTransCurrents` after every `run` call, so the
accumulation across the sweep lives in `tStudy`, not `tMesh`).

**State & ownership.**

- `tStructure` owns everything geometric. Nodes/electrodes: growable arrays
  (doubling). Elements/materials: linked lists filled by `move_alloc` (the
  structure takes ownership), released by a `final` destructor.
- `tElectrode%material` is a pointer into the owning element's allocatable
  material copy; elements outlive electrodes within a study, so the alias is
  safe for the current lifecycle. Elements also keep private copies of the
  nodes/electrodes they created, for reporting only — the structure's arrays
  are the solver's single source of truth.
- `tMesh` owns all matrices as allocatables sized by `initMesh(nn, ns)`.
  `ZGESV` factorises `Zeq` **in place** — after a solve, `Zeq` holds the LU
  factors and `calcFreq2` must be re-run before another solve.

**Persistence.** Input is read once (no environment/config/network). Results
are written out explicitly by the caller via `mResultsWriter`'s
`writeResultsCsv`/`writeResultsJson` (ROADMAP Phase 3) — nothing is written
automatically as a side effect of `runSweep`.

## 5. Concurrency, precision, errors, logging

- **Threading**: `tStudy%runSweep` solves the frequencies concurrently
  (OpenMP, ROADMAP Phase 10 item 4, [ADR 0024](adr/0024-phase10-numerics.md)
  §4) — one thread-private `tMesh` per thread, the study read-only inside
  `solveAtFrequency`, results written at disjoint columns, so a sweep is
  bit-identical for any thread count. `build.sh` passes `-fopenmp`; a plain
  `fpm build` is serial (the directives are comments). The geometry build
  stays serial (it runs once; the single-integral kernel is re-entrant, the
  2-D oracle is not). The transient driver reaches the same loop through
  `runSweep`. CLI `--threads <n>`; `OMP_NUM_THREADS` otherwise.
- **Precision**: uniform double precision. `mCtes` exports `dp`
  (`kind(1.0d0)`); new code uses `real(dp)`/`complex(dp)`, legacy `kind=8`
  declarations migrate gradually ([CONVENTIONS.md](CONVENTIONS.md)).
- **Error handling**: all fatal conditions route through
  `mError%raiseError`, which triggers a critical feh `ErrorInstance`
  (reports and halts). No `stop`/`error stop` remains in project code.
  Solver-level numerical failure (ZGESV `INFO ≠ 0`) is returned as a code to
  the caller, not raised — the study layer decides.
- **Logging**: `print`-based, with ANSI colours from `mCtes`; `tStudy%report`
  builds human-readable summaries. No logging framework, none planned.
  `mVerbosity` holds a global `VERB_QUIET`/`VERB_NORMAL`/`VERB_VERBOSE` level,
  set from `app/main.f90`'s `-v`/`--verbose`/`-q`/`--quiet` flags; it gates
  routine output (`tStudy%report`, sweep summary lines) but never
  errors/warnings.

## 6. Extension mechanisms

| Axis | How | Guard rails |
| --- | --- | --- |
| New geometry (ring, catenary, tower, grid…) | Extend `tElement`, implement `assemble` + `report` | Priority order in ROADMAP Phases 12–13 (ring next; grid done — Phase 7 item 3; catenary done — ADR 0023; lightning channel done — Phase 10b, ADR 0025) |
| New soil/conductor model | Extend `tMaterial`, implement `calcPropagationConstant` | One subtype per literature reference, named after it (ADR 0007) |
| New output | Extend `tResult`, implement `alloc`/`get`/`set`; wire into `runSweep` and `mResultsWriter` | Use the legacy output-class inventory to prioritise (ROADMAP P7) |
| Alternate geometry-factor kernel | `mGeometry%setGeometryKernel`; the mHEM 1-D form is the default (`mImpedance%geometryFactor1D`), the 2-D quadrature stays as test oracle and is selectable (`numerics.kernel`) | ROADMAP P1 (done, Phase 10 item 1); ADR 0004, 0024 |
| Image reflection model | `mMesh%calcImageCoefficients` fills `gammaAir`/`gammaSoil` (Γ(ω) default, ideal ±1 selectable via `numerics.imageModel`); `calcZSelf`/`calcZMutual` multiply the image parcel by them | ROADMAP P2 (done, Phase 10 item 2); ADR 0009 keeps call sites untouched, ADR 0024 |
| Series loading of a segment | `tElectrode%loaded` + `loadResistance`/`loadInductance` replace the skin-effect impedance in `mStudy%segmentInternalImpedance` (used by the lightning channel; also the slot for generic internal-impedance models, ROADMAP Phase 13 item 2) | ADR 0025 |
| Several transient excitations on one structure | `tStudy%runSweepUnits` solves every distinct unit terminal from one factorisation per frequency (multi-RHS, observed rows only); `mTransient%transientResponseSignals` multiplies each signal's spectrum into the shared H (`independent = .true.`: one response set per signal, ADR 0026; otherwise superposed, ADR 0015) |
| Two-node sources | `returnNodeIds` of `tStudy%run`/`runSweep`: the unit pattern becomes the ±1 dipole (`injectionPatterns`); the solver kernel still sees only nodal current injections | ADR 0025 (extends ADR 0010, 0016) |
| Other languages | Re-implement the object model; must pass `common/` cases | ADR 0002; JSON schema is the public contract |

The **public interface** of the project is the JSON schema plus the
`common/` reference cases — nothing else. All Fortran module APIs are
internal and may change without notice (author decision, ROADMAP §9).

## 7. Known architectural debts

Tracked, deliberate, and safe at the current scale:

- `mImpedance` keeps a legacy `COMMON /params/` block and module-level
  function pointers for the nested 2-D quadrature (`geometryFactor2D`, now
  the test oracle) — not reentrant. The default `geometryFactor1D` has no
  such state; the geometry build is nevertheless left serial (it runs once).
- SLATEC's `D1MACH` fills its constant table lazily and not race-free (it
  raises its flag before filling the table): `warmUpMachineConstants` calls
  it once before the threaded sweep. Any new threaded entry point that can
  reach `ZBESI` first must do the same.
- `mJsonParser` (now a json-fortran wrapper, ADR 0006) keeps one
  module-level `json_core` instance — non-reentrant, fine for this
  project's one-file-at-a-time usage but would need revisiting for a
  hypothetical concurrent/multi-file caller. The old hard subset limits
  (64 items/container, no string escapes) are gone.
- `tMesh` per-frequency state (see §4) makes the frequency loop sequential
  over one mesh instance; `runSweep` therefore gives every thread its own
  copy of the mesh (memory ≈ one `Zeq` of $(n_n + 2n_s)^2$ complex numbers
  per thread). `-fopenmp` also puts automatic arrays on the stack: large
  scratch arrays must be `allocatable`.
- Dense augmented solve scales as $(n_n + 2n_s)^3$ — fine for the reference
  scale (hundreds of segments), by design (ADR 0003).
