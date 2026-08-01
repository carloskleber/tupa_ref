# TUPÃ — Roadmap

Reference electromagnetic transient solver (HEM / Method of Moments).
Fortran implementation first; object model and test cases shared with future
Rust/Python implementations (see [ADR 0002](adr/0002-language-agnostic-object-model.md)).

This roadmap supersedes the earlier `implementation-plan.md` /
`IMPLEMENTATION_PLAN.md`. It is based on a side-by-side analysis of this
repository against the legacy (private) implementations and the theory
consolidated in [theory.md](theory.md). Since July 2026 the **original
Matlab code** (the dissertation implementation) is the **model reference of
record** — the re-inspection findings live in
[ADR 0017](adr/0017-legacy-reinspection-findings.md) (§8 below is a stub),
and the author-interview decisions of 2026-07-05 in
[ADR 0018](adr/0018-author-interview-decisions-2026-07.md) (§9 stub).

**MVP scope**: tower-footing grounding under lightning. Full transmission
lines and substation grids are the follow-on application tier (ADR 0018).
The project's primary role is a **scientifically citable reference
implementation**; usability as an engineering tool is secondary.

---

## 1. Current state

| Area | State |
| --- | --- |
| Object model | `tStudy → tStructure → tElement/tMaterial → tNode/tElectrode`, `tMesh`; FORD-documented |
| Geometry layer | `Geometry.f90`: mean/image distances, direct+image geometry factors (`g`, closed-form `g_self`), direction cosines; quadrature cache |
| Impedance fill | ADR 0009 interface: `calcZSelf`/`calcZMutual` apply all theory factors internally; solid-conductor Bessel internal impedance (SLATEC `ZBESI`) |
| Solver | Augmented `Zeq` assembly + `ZGESV` (ADR 0003); multi-RHS variant for superposition (ADR 0016) |
| Sources | Current injections at named nodes (ADR 0010); ideal voltage sources via unit-injection superposition, mixable with current sources (ADR 0016) |
| Materials | `tLinear`, `tPortelaSoil` (ADR 0007), `tVisacroAlipioSoil` (mean set, theory.md §7); air hardcoded to vacuum (ADR 0019) |
| Sweep & results | `runSweep` + `tResult` storage, `inputImpedance`/`maxVoltageMagnitude`; CSV/JSON writers (ADR 0012) with `outputs` filtering |
| Time domain | `mSignal` (Heidler — legacy 6-term [38] and standard parametrised form [37, 39]; double-exp ± Jones), tail taper, in-repo FFT (ADR 0014), transfer-function transient driver (`mTransient`) |
| JSON I/O | Minimal parser (ADR 0006); schema v1: structure + `sources`/`frequencies`/`outputs` (ADR 0013) + `signal` (ADR 0015) + voltage sources/Heidler terms (ADR 0016/0015 amendment) |
| Cases & tests | `common/` regression fixtures (golden, not an oracle — §7 P3), 12 test programs, all green under `fpm test --profile release` |
| GUI | Python/PySide6 view-only module (`gui/`, ADR 0011): study tree, 3-D view, results/transient plots |

The original gap analysis (nine numbered gaps between this repository and
the legacy pipeline) is fully resolved as of Phases 0–6; the historically
notable items are recorded where they now belong:

- self-geometry-factor bug in *both* legacies (was item 8) —
  [ADR 0017](adr/0017-legacy-reinspection-findings.md) finding 2;
- `tStructure%air` never populated → NaN for any electrode in air (was
  item 9) — [ADR 0019](adr/0019-air-medium-hardcoded-vacuum.md);
- C-interop leftovers, sign conventions, stub `assemble`, missing
  sweep/sources/outputs — closed by Phases 0–3 below.

---

## 2. Guiding principles

- **Theory doc is normative** — code conforms to [theory.md](theory.md);
  papers are mapped through its §2 conventions table (ADR 0008).
- **Correctness before performance** — every physics routine lands with a test
  against an analytical value or a published curve.
- **Reference quality** — prefer the simple, auditable path (full `Zeq` solve,
  ADR 0003) over optimisations until validated.
- **Language-agnostic contracts** — JSON schema + `common/` cases are the
  cross-implementation interface (ADR 0002, 0006).

---

## 3. Phases

Item numbers are stable — code comments cite them ("ROADMAP Phase 3
item 4").

### Phase 0 — Convention audit and cleanup — **done**

1. Sign conventions audited against theory.md §2/§5/§6: propagation constant
   `√(jωμ(σ+jωε))`, decaying `e^{−γR}`, air-image longitudinal sign "−";
   pinned by `test_mesh.f90` (ADR 0008).
2. Mesh module naturalised: 1-based indices, `initMesh` (no pointer), dead
   C-interop code deleted.
3. Test-runner bugs fixed along the way (`check.f90` counter accumulation,
   bogus `TWODQ` argument).

### Phase 1 — Geometry layer — **done**

1. `tLine%assemble`: node/material resolution, internal nodes, electrode
   registration (`test_assemble.f90`).
2. `Geometry.f90`: mean/image distances, `g(a,b)` by Gauss–Kronrod
   quadrature, closed-form `g_self` (legacy formula confirmed wrong —
   ADR 0017 finding 2), direction cosines; mixed-media pairs skipped as a
   perf hint only (the zeroing decision stays in `Mesh.f90`, ADR 0005).
3. Solid-conductor internal impedance (SLATEC `ZBESI`); tubular deferred to
   Phase 7.
4. Exit criterion met: geometry matrices match independent numerical
   integration to 1e−6 relative (`test_geometry.f90`).

### Phase 2 — End-to-end single-frequency solve — **done (DC-limit scope)**

1. `tStudy%run` wired: assemble → topology → geometry (cached once) →
   per-frequency fill → `calcFreq2` → inject → solve. Fill interface per
   ADR 0009 (motivated by the C++ call-site bug, ADR 0017 finding 3).
2. Current-source injection at named nodes (ADR 0010).
3. Validation: DC limit vs Sunde/Dwight (15 %), low-frequency plateau,
   passivity across 10 Hz–1 MHz (`test_solve.f90`). **Full Portela-1997
   curve match not attempted** — no tabulated data exists (theory.md §9.2);
   needs §7 P3 as an executable oracle.

### Phase 3 — Frequency sweep, results, output — **done (items 1–3)**

1. `logFrequencyAxis` default log-spaced axis.
2. `runSweep` result storage + `inputImpedance`/`maxVoltageMagnitude`
   queries; `tResult` simplified to plain-copy accessors.
3. CSV (tidy/long) and JSON (ADR 0012) writers; `example4.f90` end to end.
4. **Deferred**: OpenMP on the geometry fill loop — blocked on `mImpedance`
   reentrancy (module-level procedure pointers + `COMMON /params/`,
   ARCHITECTURE.md §7); a determinism test pins the write pattern
   (`test_geometry.f90`). Expected to be superseded by frequency-level
   parallelism (§7 P6).

### Phase 4 — Dispersive soil — **done**

1. `tPortelaSoil` per ADR 0007 (ω₀ = 2π·1 MHz); `tMaterial%admittance`
   deferred function, shared `calcPropagationConstant`; `calcParamW`.
2. DC-limit convergence to `tLinear`, passivity, formula pinned at ω₀
   (`test_material.f90`). Curve match: same data gap as Phase 2.

### Phase 5 — JSON I/O and common cases — **done**

1. Input schema v1 frozen (ADR 0013: `sources`/`frequencies`/`outputs`);
   Fortran reader (`loadStudy` optional arguments, `runStudyFromFile`);
   write-time output filtering. Discretised-ID gotcha documented in
   `common/README.md`.
2. `common/` cases: `portela1997`, `rod`, `grid` (+ later `rod_air`,
   `silva2025_rho*`) with golden `_expected.csv` fixtures diffed by
   `test_common_cases.f90` (1e-6 relative; independent passivity check).
   Grid kept to one mesh — non-parallel pairs cost ~1–2 s each in 2-D
   quadrature until §7 P1.
3. Parser stayed within the ADR 0006 minimal subset.

### Phase 6 — Sources and time domain — **done**

1. `mSignal`: Heidler (legacy 6-term set) and double exponential
   (`f1_2_5`/`f1_2_50`/`f1_2_200`/`f250_2500`, optional Jones front).
   Remaining legacy waveforms (single exp, impulse/step, Portela concave,
   sine) ported as needed.
2. FFT transient driver (`mTransient`): tail taper, one-sided spectrum,
   unit-current transfer function, conjugate-symmetric IFFT; DC bin
   replaced by `freqZeroHz` (ADR 0019 singularity). In-repo
   double-precision radix-2 FFT (ADR 0014). Schema + transient results
   shape in ADR 0015; `portela1997_transient.json`.
3. Validation is internal-consistency only (slow-surge GPR tracks the
   validated low-frequency `|Zin|` within 25 %, `test_transient.f90`) —
   same data gap as Phases 2/4.

### Phase 7 — More elements, input functions and outputs — **in progress**

Done (2026-07-17):

- **Heidler function** — standard parametrised form (Heidler 1985 [37];
  IEC 62305-1 [39] parameter sets): `newHeidlerSignalTerms` (arbitrary
  terms, analytic η peak correction, optional legacy-style `imax`
  rescale); JSON `signal.terms` (ADR 0015 amendment).
- **Voltage source** — ideal voltage sources converted to equivalent
  current injections by unit-injection superposition in the study layer
  (ADR 0016, implementing ADR 0010); mixed voltage+current source sets
  supported; JSON `sources[].voltage`; `inputImpedance` uses per-frequency
  effective currents.

Remaining, in order of preference. Scoping decisions from the 2026-07-17
author Q&A in *italics*; effort rated S/M/L (S ≈ days, M ≈ a focused
week-scale task, L = new theory/object-model work) from the 2026-07-17
legacy survey (registered findings in theory.md §3.1, §4.3, §5, §6):

- `tCatenary` — **S**. *Matlab-faithful port, discretised into straight
  segments like `tLine`*. Survey finding: the legacy "catenary"
  (`Catenaria.m`) is actually a **parabolic** sag profile (z ∝ x², sag
  parameter at midspan, uniform plan spacing), plus a 3-node variant — so
  Matlab-faithful and parabolic approximation coincide. Pure
  element-assembly work, no new physics.
- Numerical Laplace Transform (NLT) option (§7 P4) — **M**. *Opt-in first:
  `signal.transform: "fft"` (default) `| "nlt"`; flip the default only
  after P3 cross-validation, keeping golden fixtures stable*. Driver-only
  change (theory.md §8); refs: Gómez & Uribe [17], TAGS as executable
  reference.
- Hanning (and other) windowing — **S**, rides on NLT (the window filter
  before the inverse transform, theory.md §8); the plain-FFT path keeps
  the existing erfc tail taper (different role: record truncation).
- Ground potential rise (GPR), touch and step voltage — single-frequency
  study (§7 P7) — **M**. *Both input forms from the start: explicit
  observation-points array plus an optional auto surface-grid block,
  mirroring the Matlab output-class inventory (ADR 0017 finding 6)*.
  Post-processing of the solved I_t — formula and legacy/TAGS correlation
  registered in theory.md §3.1; needs an ADR 0012 results-schema
  extension. *Decided (2026-07-17 Q&A): legacy-geometric definitions
  (touch = max |ψ − u_node| on a 1 m circle, step = ψ difference at 1 m
  spacing), citing IEEE Std 80 [42] as normative context; body-circuit /
  surface-layer derating factors stay out of the solver.*
- Series RLC element — **M**. *Series form only first (R + jωL + 1/(jωC)
  two-terminal, Matlab-style non-coupling lumped element); parallel later
  if a case needs it*. Legacy mechanism (extra non-electromagnetic branch,
  Z(ω) on the Z_ℓ diagonal, zeroed Z_t row) registered in theory.md §6,
  including the nonsingularity check and DC pin a port must add.
- Tubular conductor (extrapolation: simulation of metallic pipes) — **S**.
  Schelkunoff I/K formula now stated explicitly in theory.md §4.3 [40];
  element = `tLine` + wall thickness (legacy `Tubo.m`); extends
  `mImpedance` with SLATEC `ZBESK` alongside `ZBESI` (scaled variants for
  large arguments).
- Insulated conductor — **M–L**. Survey finding (theory.md §4.3): the
  legacy branch is an acknowledged placeholder (drops soil conduction,
  ignores the coating entirely; flagged TODO in the legacy code) — do
  **not** port it; implement Sunde's coating admittance in series with the
  bare-conductor soil leakage [41].
- Generic internal impedance models (e.g. OPGW), specified in a JSON
  database; alternative use from the material property in elements —
  **M**. *Decided (2026-07-17 Q&A): database entries carry
  frequency-tabulated R(f), X(f) (measured/datasheet data), interpolated
  at solve time* — captures stranding/steel-core effects the equivalent
  tube misses; extrapolation limits must be validated and flagged. (The
  legacy repo's ACSR spreadsheet is candidate seed data; the C++
  bundle/L-profile models remain a possible catalog kind later.)
- Lightning discharge channel — **M–L**. Survey finding: the legacy
  element class is an **empty placeholder**; a helper (`canal.m`) only
  generates channel geometry (log-spaced segments from cloud height down
  to the strike point, incidence/azimuth angles) — the channel is then
  ordinary HEM segments in air, i.e. an "electromagnetic return-stroke
  model" in Baba & Rakov's classification [34,44]. *Decided (2026-07-17
  Q&A): antenna-model route, with added distributed series impedance
  calibrated so the channel propagation matches an intended return-stroke
  speed prescribed in the JSON* — the loading technique surveyed in [44].
  A working channel also opens the induced-voltage application route
  ([45,46]; lossy-ground coupling via Norton's approximation) and covers
  the LEMP term plain EMT analysis misses [52] — both post-MVP.
- Mutual impedance between segments in different media — **L**. No legacy
  implementation exists to port (unfinished body, ADR 0017); the
  candidate quasi-static transmission-coefficient route is registered in
  theory.md §5 [35]; validate on `rod_air`-class cases. *Decided
  (2026-07-17 Q&A): sequenced strictly after §7 P2 (Γ(ω) images) — both
  touch the same interface machinery and P2 restores reference behaviour
  first.*
- `tCircumference` (grounding rings) — **S**. Legacy `Anel.m`: circle in
  an arbitrary plane (centre + normal vector + rotation), ≥ 3 straight
  segments, closed loop (exercises loop topology); single-medium
  constraint enforced.
- ~~Grid/mesh generator element~~ — **done** (ADR 0020): JSON `"type":
  "mesh"`, `mElementMesh`/`tMeshElement`, composite element emitting
  `tLine` bars on a rectangular pattern. Running an actual frequency sweep
  over a real-sized grid stays impractical until §7 P1 (quadrature cost,
  §5) lands — `common/portelaMesh.json` ships structure-only for that
  reason.
- Multipolar cables (internal representation by impedance/admittance
  matrix) — **L**. Object-model change (multi-conductor element); refs:
  Ametani cable constants [43], Schelkunoff [40]; PRTL-mHEM's tubular
  bundles are a partial analogue.
- Option of multiple injections (e.g. three-phase sine emulating line
  voltage, plus impulse injection) — **S–M**. The harmonic side already
  handles simultaneous mixed sources (ADR 0016); remaining work is the
  transient pipeline (per-source spectra × per-source transfer functions,
  superposed — linear) and schema. Survey note: the legacy also supports
  a *differential* (±1 two-node) injection pattern worth carrying along.
- Multi-layer soil and reflection-coefficient images — **L**. Lifts the
  §5 single-interface premise; route: layered-earth Green's functions via
  quasi-static complex images (Li, Chen & Wang [33]; Sunde [41]
  background). Explicitly out of MVP scope (theory.md §5, §10.1) — keep
  last.

The Matlab reference's full element inventory, for the record: straight
lines (three variants), ring, grid, cable, catenary, lightning-channel
element, helicoidal rod, tube, conduit, lattice and crossarm placeholders
(block/cube/pyramid/tetrahedron), lumped series-RLC "impedance" elements
(extra unknowns that do not couple electromagnetically — their `Zt` row is
zeroed), and insulated buried cables (leakage through jωε only;
placeholder theory, flagged TODO in the legacy code itself). The C++ adds
bundle and L-profile (lattice-member) internal impedances and a
shielded-wire segment.

### Phase 8 — Second implementation (Rust)

Rust port following the object model; must pass every `common/` case.
Originally Python was proposed, but for now Python is dedicated to the GUI
side (ADR 0011).

---

## 4. Milestones

- **Phases 0–6**: met (see §3) — end-to-end harmonic sweep and FFT
  transient from a JSON case file, dispersive soils, regression fixtures.
- **0.1.0 release bar** (ADR 0018): the Portela-curve case within
  tolerance. Blocked on an executable oracle — §7 P3 (TAGS
  cross-validation) or real tabulated data.
- **Next engineering steps**: §7 P1 (mHEM 1-D kernel — unblocks larger
  `common/` grids) and §7 P2 (Γ(ω) images — restores reference behaviour).

---

## 5. Testing strategy

| Layer | Where | What |
| --- | --- | --- |
| Unit | `fortran/test/` | quadrature vs closed forms; sign/decay pins; Bessel `Z_int` vs tables; dispersion DC limit; waveforms |
| Integration | `fortran/test/` | end-to-end DC resistance; sweep/transient consistency; reciprocity, passivity; voltage-source superposition |
| Reference | `common/` | JSON in → CSV out, golden diff at 1e-6; shared across languages |
| Benchmarks | `benchmarks/` (proposed) | TAGS and PRTL-mHEM as git submodules; cross-code runs per [BENCHMARKS.md](BENCHMARKS.md) |

There is **no hosted CI** (ADR 0018): the gate is a local
`fpm build && fpm test` before merging. Practical caveats:

- **Run the slow suites under `--profile release`** (as `build.sh` builds):
  in the debug profile the quadrature-heavy suites are effectively
  un-runnable (`test_geometry`'s 10×10 fill exceeded 9 minutes, measured
  2026-07-05).
- A cold `fpm test` pays several minutes of stdlib compilation; keep the
  build cache.
- Every non-parallel segment pair costs ~1–2 s in `geometryFactor2D`'s 2-D
  adaptive quadrature at today's tolerances (measured 2026-07-10) — keep
  new `common/` cases' electrode counts small until §7 P1 lands.

---

## 6. Open decisions

| Decision | Status |
| --- | --- |
| Soil dispersion model | **Implemented** — ADR 0007 (`tPortelaSoil`, ω₀ = 2π·1 MHz), Phase 4; `tVisacroAlipioSoil` mean set (§7 P5) |
| Voltage-source handling | **Implemented** — current-injection equivalents (ADR 0010) by unit-injection superposition (ADR 0016), Phase 7 |
| Impedance-fill interface | **Implemented** — theory factors inside `calcZ*` (ADR 0009) |
| FFT dependency | **Implemented** — in-repo double-precision radix-2 FFT (ADR 0014); NLT proposed on top (§7 P4) |
| JSON schema v1 | **Implemented (Fortran, GUI)** — ADR 0013 + 0015 (+ 0016 additions); Rust reader pending Phase 8 |
| Reduced `Z_g` solver | Deferred optimisation (ADR 0003) |
| GUI module | **Decided** — Python/PySide6/Qt3D, view-only v1 (ADR 0011) |
| Results JSON schema | **Frozen** — ADR 0012 (harmonic) and ADR 0015 (transient) |
| Quadrature tolerances | Dissertation-era values (`errrel = min(la,lb)·10⁻⁶`, `maxint = 500`), open to revision — revisit with the §7 P1 mHEM kernel (ADR 0018) |
| Binary results format | Deferred — stay on JSON/CSV (ADR 0012); HDF5 is the leading candidate vs ADR 0006's zero-dependency philosophy. Revisit once a real case is actually too large for JSON. |

---

## 7. Proposals from the open-source HEM comparison (July 2026)

Three companion open-source codes were inspected side by side with this
repository (see "Related open-source implementations" in
[references.md](references.md)): **TAGS** (pedrohnv, C99), **PRTL-mHEM**
(VitorLima1990, Python), **PRTL** (acslima, Wolfram/CDF).

Headline finding: TUPÃ's geometry-factor separation (theory.md §4.1, from
the 2003 dissertation) is the same optimisation published as **mHEM** by
Lima et al. [11] and used by TAGS/PRTL-mHEM — the core design is
independently validated in the literature. TAGS's closed-form self integral
is identical to theory.md's `g_self`, confirming the Phase 1 fix.

### P1 — mHEM single-integral kernel for `g(a,b)` (low effort)

Replace the default 2-D Gauss–Kronrod evaluation with the 1-D form of
theory.md §4.2 (inner integral in closed form). Same quantity, cheaper and
better conditioned for close segments; keep the 2-D path as the test
oracle. Unblocks larger `common/` grid cases (§5).

### P2 — Frequency-dependent image reflection coefficient (low effort)

`Γ_t(ω) = (W_soil − jωε₀)/(W_soil + jωε₀)`, `Γ_ℓ = 1` (theory.md §5) as
the default for buried conductors. **The original Matlab already does this
as its default mode** (ADR 0017 finding 1) — this restores reference
behaviour, it does not extend it. The ideal ±1 table remains as the
low-frequency limit and test pin. Grcev-grid MHz-range behaviour needs it.

### P3 — Cross-code validation against TAGS (medium effort)

Run the Phase 2 buried conductor (later the Grcev grid) through both codes
and compare input impedance over the sweep — the executable oracle the
0.1.0 bar needs (§4). Compare physical outputs only (conventions differ
internally — theory.md §9.6, ADR 0017 finding 5).

### P4 — Numerical Laplace Transform for the time domain (medium effort)

NLT (`s = c + jω`, damping `c ≈ ln(N²)/T`, window filter — Gómez & Uribe
[17]) as TAGS and PRTL use. Physics kernels untouched (already complex);
only the sweep driver and inverse transform change. Plain FFT is the
`c = 0` special case and remains for tests. Listed in Phase 7.

### P5 — Concretise the dispersive-soil subtypes

`tVisacroAlipioSoil` per Alipio & Visacro 2014 [14]: **done (2026-07-16,
mean parameter set)** — see ADR 0007's "Exercised" note and
`test_material.f90`; the JSON `soil.type` selector reaches all three soil
models. Open: *relatively conservative*/*conservative* parameter sets and
`tLongmireSmithSoil` (Longmire & Smith [15] per Cavka et al. [16]).

### P6 — Parallelise over frequencies, not the matrix fill

TAGS multithreads the frequency loop and pins BLAS to one thread;
frequencies are embarrassingly parallel and TUPÃ's loop has the same shape
once geometry factors are cached. Measure both, but expect this to
supersede the fill-loop OpenMP pencilled in Phase 3 item 4
([CONVENTIONS.md](CONVENTIONS.md) records the current default).

### P7 — Field/potential post-processing (new feature)

Scalar potential, electric field and path voltages at arbitrary points
from the solved `I_t`/`I_ℓ` (touch and step voltages, GPR profiles), as
TAGS and the Matlab reference both provide — use the Matlab output-class
inventory (ADR 0017 finding 6) to prioritise `tResult` subtypes. Overlaps
the Phase 7 "GPR, touch and step voltage" item.

### P8 — Criteria-based segmentation defaults (low effort)

Schroeder, Moura & Machado [19]: segments up to ~1000·r₀ stay within
engineering accuracy — far coarser than 10·r₀, >30× faster. Keep λ/10 as
the default `assemble` bound (theory.md §4.1) but expose the
segment-length target per study, validated by a coarse-vs-fine convergence
test.

### Explicitly *not* proposed

- TAGS's symmetric `(u, I_ℓ, I_t)` block system — equivalent to the ported
  `(u, i₁, i₂)` form; consistency-check option only (the Matlab carries it
  as "método 5", ADR 0017 finding 4).
- The PRTL transmission-line performance chain — out of scope for the
  grounding-solver milestone; revisit after Phase 8.
- Complex images (Kuhar et al. [20]) — only relevant above a few MHz and
  only after P2 is in and validated (theory.md §5/§10.1).
- Time-domain HEM (HEM-TD; origin [47], refined in [21]) — pays off only for
  nonlinear phenomena, which TUPÃ excludes by design (theory.md §8); the
  direct TL+FDTD time-domain route [48] confirms the same trade-off.
- Rational-model/FDNE export for EMT programs [26, 27] — a future *output
  format*, not solver work; revisit when users ask for EMT integration.

---

## 8. Findings from re-inspecting the legacy implementations (July 2026)

Moved to [ADR 0017](adr/0017-legacy-reinspection-findings.md) (finding
numbers preserved): Γ(ω) images are original Matlab behaviour (1); self
geometry factor bug lineage (2); C++-only call-site bug (3); all three
solver layouts in the Matlab (4); convention mixing gates cross-validation
(5); feature inventory (6); legacy input formats (7).

---

## 9. Author-interview decisions (2026-07-05)

Moved to [ADR 0018](adr/0018-author-interview-decisions-2026-07.md):
scope, project role, convention authority, sources, tolerances,
stable/fluid modules, public contract, compilers, no-hosted-CI, release
process (0.1.0 bar), SLATEC, benchmarks, validation-data status,
precision, error handling.
