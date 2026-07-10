# TUPÃ — Roadmap

Reference electromagnetic transient solver (HEM / Method of Moments).
Fortran implementation first; object model and test cases shared with future
Python/Rust implementations (see [ADR 0002](adr/0002-language-agnostic-object-model.md)).

This roadmap (formerly `implementation-plan.md`, which it supersedes together
with the earlier `IMPLEMENTATION_PLAN.md`) is based on a side-by-side analysis
of this repository against the legacy (private) implementations and the theory
consolidated in [theory.md](theory.md). Originally the comparison target was
the C++/Fortran hybrid; since July 2026 the **original Matlab code** (the
dissertation implementation) is available alongside it and is the **model
reference of record** — see §8 for what its re-inspection changed, and
"Related implementation notes" in [references.md](references.md) for the two
codes' contents. §9 records the author-interview decisions of 2026-07-05.

**MVP scope**: tower-footing grounding under lightning. Full transmission
lines and substation grids are the follow-on application tier (§9). The
project's primary role is a **scientifically citable reference
implementation**; usability as an engineering tool is secondary.

---

## 1. Current state

### Works today

| Area | State |
| --- | --- |
| Object model skeleton | `tStudy → tStructure → tElement/tMaterial → tNode/tElectrode`, `tMesh` — compiles, FORD-documented |
| Impedance quadrature | `Impedance.f90`: adaptive Gauss–Kronrod 7/15 double integration of `1/R`, tested (`test_impedance.f90`) |
| Mesh matrix code | `Mesh.f90`: allocation, topology A/B/C/D, medium constants, `Zeq` assembly, `ZGESV` solve, injection — ported from the original Fortran module |
| Materials | `tLinear` propagation constant; `tPortelaSoil` parameters declared |
| JSON parser | Minimal hand-rolled parser (`JsonParser.f90`), study loading started |
| Build/test | FPM + `build.sh` (LAPACK/BLAS/SLATEC, OpenMP flags), basic test framework |

### Missing or wrong — gap analysis vs. the original

The original implementation's pipeline is:

```text
Elements → Structure (nodes+segments) → geometry factors (G, Gi, R̄, R̄i, cosθ)
        → per frequency: medium constants → Zlong/Ztrans fill → Zeq → inject → solve
        → Collector/outputs → (FFT ↔ time domain)
```

Gaps in this repository, in dependency order (1–4, 7, 8 resolved by Phases
0–1; see §3 for what changed):

1. ~~**Geometry-factor layer is absent.**~~ **Resolved (Phase 1)** —
   `Geometry.f90` (`mGeometry`) computes mean/image distances, `g`/`g_self`
   (direct + image), and direction cosines; see `test/test_geometry.f90`.
2. ~~**Sign-convention divergences.**~~ **Resolved (Phase 0)** — `calcParam`,
   the propagation factor, and the air-image longitudinal sign now match
   theory.md exactly (see `test/test_mesh.f90`). The C/D topology entries
   already matched theory.md; no change was needed there.
3. ~~**`tLine%assemble` is a stub**~~ **Resolved (Phase 1)** — discretises,
   resolves node/material IDs, and populates `tStructure%electrodes`.
   `tStudy%run` still doesn't call `assembleStructure` (Phase 2).
4. ~~**Internal impedance missing**~~ **Resolved (Phase 1)**, solid conductor
   only (`mImpedance%internalImpedance`, SLATEC `ZBESI`); tubular is Phase 7.
5. ~~**No frequency sweep, no sources, no outputs**~~ **Fill loop and
   single-frequency solve resolved (Phase 2)** — `tStudy%run` wires
   assemble → topology → geometry (cached once) → per-frequency
   `calcParam`/fill/`calcFreq2`/`injectSignal`; current-source injection at
   named nodes works (ADR 0010). `tResult` types are still declared but
   never filled, and there is still no CSV/JSON writer or formal sweep
   storage — that remains Phase 3. The impedance-fill interface itself was
   fixed by [ADR 0009](adr/0009-impedance-fill-interface.md) —
   `calcZSelf`/`calcZMutual` take raw `mGeometry` outputs and apply every
   theory factor (propagation, direction cosines, length normalisation,
   including the direct-term `e^{−γr₀}` of theory.md §4.3) internally,
   pinned by hand-evaluated values in `test_mesh.f90`.
6. **`tPortelaSoil` is a placeholder** (returns zero) — see ADR 0007. It is
   the first of several planned dispersive-soil `tMaterial` subtypes
   (`tLongmireSmithSoil`, `tVisacroAlipioSoil`, ...), not the only one.
7. ~~**Leftover C-interop artifacts**~~ **Resolved (Phase 0)** — indices
   naturalised, `alocaMalha` replaced by `initMesh` (no pointer), `calcFreqF`
   and `Solver.f90` (`solMalha`) deleted.
8. ~~**Suspicious self geometry factor in the original**~~ **Resolved
   (Phase 1)** — confirmed wrong (missing factor of 2) by re-deriving the
   defining integral from scratch; theory.md's formula matches and is
   implemented as `mGeometry%selfGeometryFactor`, tested against quadrature.
   The July 2026 legacy re-inspection (§8) traced the bug to the Matlab
   original and found a second defect in the same expression: a literal `1`
   where `l` belongs in the log argument, carried verbatim into the C++.

Housekeeping: stray `*.mod` files at `fortran/` root and the `.history/`
folder are gitignored (no longer an issue in practice — stray `.mod` files
from old builds can still shadow fresh ones in gfortran's search path if not
removed before a rebuild). `test_impedance.f90`'s pending diff was already
resolved before Phase 0; found instead: `test_twodq_simple` passed a bogus
extra argument to `TWODQ`, and a test-runner bug (`check.f90`'s per-section
counters were never accumulated into the whole-program total, so `fpm test`
could report "ALL TESTS PASSED" / exit 0 even with failing assertions
earlier in the same file) — both fixed.

**Phase 0 status: done.** Items 1–3, 7 above are fixed in `Mesh.f90` (propagation
constant, propagation-factor exponent, air-image longitudinal sign, C-interop
index/pointer/dead-code cleanup), pinned by `fortran/test/test_mesh.f90`. Item 8
(self geometry factor) is unaffected — no self-factor code exists yet — and
still applies to Phase 1. Two pre-existing, unrelated issues were found while
getting `fpm test` green and are left for whoever picks up Phase 1/geometry
work: `test_impedance.f90`'s `check` module resets its pass/fail counters on
every `test_init`, so only the last block's failures affect the exit code —
earlier `[FAIL]` lines don't fail the build; and two of its assertions
currently fail (`inverseDistanceIntegrand` decreasing-with-distance, using a degenerate
all-zero direction vector in the test setup; and `geometryFactor2D` on coincident
segments, consistent with gap 8 — no self-term regularisation exists yet).

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

### Phase 0 — Convention audit and cleanup (prerequisite) — **done**

1. ~~Audit `Mesh.f90` sign conventions against theory.md §2/§5/§6; fix toward
   the doc. Unit-test: propagation factor decays; `Zlong`/`Ztrans` symmetric.~~
   Fixed: propagation constant now `sqrt(j*omega*mu*(sigma+j*omega*eps))`,
   propagation factor `exp(-gamma*R)`, air-image longitudinal sign corrected
   to "−". C/D topology signs already matched theory.md (no change needed).
   Pinned by `test/test_mesh.f90`.
2. ~~Naturalise the mesh module: 1-based indices, no pointer-returning
   allocator, delete `calcFreqF`/`solMalha` dead code, replace `error stop`
   with feh errors.~~ Done: `alocaMalha` → `initMesh` (intent(inout), no
   pointer), all `+1` C-interop index shifts removed, `calcFreqF` and
   `Solver.f90` (`solMalha`) deleted (no callers).
3. ~~Gitignore `.history/` and stray `.mod`; resolve the pending
   `test_impedance.f90` diff.~~ `.gitignore` already covered these; the
   pending diff had already been resolved before this pass. Found instead:
   `test_twodq_simple` passed a bogus extra argument to `TWODQ` (compile
   error) — fixed.

**Exit criterion met**: `fpm test` green; mesh module has no C-legacy artifacts.

### Phase 1 — Geometry layer (structure → matrices) — **done**

1. ~~`tLine%assemble`: resolve node/material IDs, create internal nodes and
   `nElectrodes` segments, register with `tStructure` (flat arrays + `n1`/`n2`
   connectivity).~~ Done, including new `tStructure%findNodeIndex` /
   `%findMaterial` lookups and the previously-unpopulated `tStructure%electrodes`
   array (`%addElectrode`). Tested in `test/test_assemble.f90`.
2. Geometry-factor computation (new module `Geometry.f90`, `mGeometry`):
   - ~~mean distances `R̄`, image distances `R̄ᵢ` (mirror through z = 0);~~ done.
   - ~~`g(a,b)` by Gauss–Kronrod quadrature; `g_self` closed formula (derived,
     tested against quadrature with axis-to-surface offset — item 8 above);~~
     done — **the original C++ formula was confirmed wrong** (missing a
     factor of 2; see gap 8 below). theory.md's formula was independently
     re-derived from the defining integral and verified against quadrature.
   - ~~direction cosines, incl. image direction (z-component flipped);~~ done.
   - same-medium test; mixed pairs skipped (ADR 0005): **not implemented in
     mGeometry** — this is medium/position information, not a geometric
     property (theory.md §5), so it correctly lives in `Mesh.f90`'s
     `calcZSelf`/`calcZMutual` (`pos1`/`pos2` args), not the geometry layer.
3. ~~Internal impedance `Z_int`: solid conductor Bessel formula (SLATEC or
   stdlib Bessel); tubular later.~~ Done for the solid conductor
   (`mImpedance%internalImpedance`, SLATEC `ZBESI`); tubular deferred (Phase 7).
   `fpm.toml`/`build.sh` updated to link SLATEC and export `LIBRARY_PATH` —
   `fpm build`/`fpm test` need `LIBRARY_PATH=$HOME/.local/lib:$LIBRARY_PATH`
   if not run via `build.sh` (fpm's `link` list alone doesn't add `-L`).
4. ~~Tests: closed-form parallel-segments factor vs quadrature; self factor vs
   quadrature; image distances for a buried horizontal wire.~~ Done in
   `test/test_geometry.f90`, plus internal-impedance DC/high-frequency limit
   checks.

**Exit criterion met**: for a 10 m line in 10 segments, geometry matrices
match independent (scripted) numerical integration to 1e−6 relative.

### Phase 2 — End-to-end single-frequency solve — **done (DC-limit scope)**

1. ~~Wire `tStudy%run`: assemble → topology → per-frequency
   (`calcParam` from `tMaterial`s → `Zlong`/`Ztrans` fill from geometry
   matrices → `calcFreq2` → inject → extract).~~ Done: `run(this, omega,
   sourceNodeIds, sourceCurrents)` assembles and computes the geometry-factor
   matrices once (cached on `tStudy`, guarded by a `prepared` flag), then
   repeats only the per-frequency block (medium constants, fill loop over
   `calcZSelf`/`calcZMutual`, `calcFreq2`, `injectSignal`) on every call —
   so a caller sweeping frequency does not redo the O(n²) quadrature
   (theory.md §4.1). The legacy trap here — the C++ self-term call passed
   its *longitudinal* image geometry factor in the *transversal*-image
   argument slot — motivated [ADR 0009](adr/0009-impedance-fill-interface.md):
   `calcZSelf`/`calcZMutual` consume the raw `mGeometry` matrices directly
   (no caller-side pre-scaling), removing that class of bug. Cross-checking
   the assembled fill against the Matlab reference on moduli is still open
   (no cross-code harness exists yet — see P3 below).
2. ~~Current-source injection at named nodes~~ Done (ADR 0010; voltage
   sources remain deferred) — `run`'s `sourceNodeIds`/`sourceCurrents`
   arguments, resolved via `tStructure%findNodeIndex`.
3. **Validation test — DC-limit scope done, curve-match deferred**: the
   Portela-1997-parameter buried horizontal conductor (10 m, r₀ = 7 mm,
   0.5 m depth, σ = 0.01 S/m, εr = 10) is pinned in
   `fortran/test/test_solve.f90`:
   - ~~DC limit vs Sunde/Dwight resistance formula (theory.md §9.1)~~ done,
     within 15 % at 10 Hz (loose because the formula itself drops
     higher-order terms and 10 Hz isn't literally DC);
   - ~~low-frequency input impedance ≈ DC resistance~~ done (10 Hz vs 100 Hz
     agree within 5 %, confirming the plateau) and passivity
     (`Re(Zin) ≥ 0`) holds across the whole 10 Hz–1 MHz sweep;
   - **full curve within 5 % of Portela 1997 [2] — not attempted**: no
     tabulated data exists for that curve (theory.md §9.2), so there is
     nothing to compare against yet. Left for ROADMAP §7 P3 (TAGS
     cross-validation), a separate, larger task that would supply an
     executable oracle.

**Exit criterion met (DC-limit scope)**: `fpm run --example example3`
prints the impedance-vs-frequency table for the Portela-1997-parameter
conductor over 100 Hz–1 MHz (the smoke cases `example1`/`example2` use
εr = 1 soil and are not the validation case, per `common/README.md`).

### Phase 3 — Frequency sweep, results, output — **done (items 1-3); item 4 deferred**

1. ~~Log-spaced frequency axis (default) with user override.~~ Done:
   `logFrequencyAxis(fMin, fMax, nPoints)` (Study.f90); pass any other array
   to `runSweep` directly to override.
2. ~~Fill `tVoltages`/`tLongCurrents`/`tTransCurrents` across the sweep;
   convenience queries (`inputImpedance`, `maxVoltage`).~~ Done:
   `tStudy%runSweep` calls `run` once per frequency and stores every node
   voltage / electrode current in `this%voltageResults`/
   `longCurrentResults`/`transCurrentResults`; `tStudy%inputImpedance(nodeId)`
   and `tStudy%maxVoltageMagnitude()` are the convenience queries. `tResult`
   itself was simplified along the way (Result.f90): concrete types now own
   a plain copy of entity IDs and the frequency axis instead of pointers
   into `tStructure`, with `get`/`set`/`entityId`/`entityCount`/
   `frequencyCount` accessors — the original pointer-based `alloc_interface`
   (with an unused `tElement` array parameter `tStructure` never populated)
   was dead scaffolding with zero callers, so this is a like-for-like
   simplification, not a behaviour change.
3. ~~CSV writer (primary) and JSON results writer, against the schema frozen
   in [ADR 0012](adr/0012-results-json-schema.md).~~ Done:
   `mResultsWriter` (`writeResultsCsv`/`writeResultsJson`). CSV is tidy/long
   form (`frequency_hz,quantity,id,re,im`, one row per frequency x entity x
   quantity) since ADR 0012 only froze the JSON shape; JSON matches the ADR
   exactly, with `derived.inputImpedance` computed from the sweep's first
   source node (single-port assumption — the ADR doesn't define a
   multi-port derived quantity). `example4.f90` demonstrates the full
   sweep -> query -> write pipeline end to end.
4. **Deferred, not just unattempted**: OpenMP on the geometry-factor fill
   loop. The fill loop's own write pattern is already race-free (each
   matrix entry is written by exactly one outer iteration — see the
   comment above `buildGeometryMatrices`'s loop, Geometry.f90) and a
   determinism test pins it (`test_geometry.f90`), but `mutualGeometryFactor`
   falls back to `geometryFactor2D`/`TWODQ` (Impedance.f90) for any
   non-parallel segment pair, and that quadrature keeps its integration
   state in module-level procedure pointers (`pF`/`pG`/`pH`) and a
   `COMMON /params/` block — non-reentrant (ARCHITECTURE.md §7). Annotating
   the fill loop with `!$omp parallel do` today would only be safe for
   geometries where every pair happens to be parallel (which is why the
   existing 10-segment collinear test wouldn't have caught it); any real
   grid with non-parallel elements would silently corrupt results under
   concurrent threads. Needs the mImpedance reentrancy fix first (its own
   task — likely nesting `inverseDistanceIntegrand`/`lowerLimit`/
   `upperLimit` and `outer_fcn` as internal procedures closing over their
   host's local variables instead of module/COMMON state — but
   `test_impedance.f90` currently unit-tests those three functions directly
   via a test-side `common /params/` alias, so the fix also touches that
   test's public-API assumptions), tracked as open work, not folded into
   this pass.

### Phase 4 — Dispersive soil

1. Implement `tPortelaSoil` per ADR 0007 (**accepted 2026-07-05**): the
   Lima–Portela parametrisation of references.md [31], reference frequency
   ω₀ = 2π·1 MHz. Requires adding a `sigma0` field to the type (it currently
   carries only `alpha0`/`kr`). Do **not** reuse legacy Matlab `kr` values
   unconverted — they are referenced to ω₀ = 1 rad/s (theory.md §7).
2. DC-limit convergence test against `tLinear`; repeat Phase 2 validation with
   dispersion on (Portela 1997 dispersive curves).

### Phase 5 — JSON I/O and common cases

1. Freeze the input schema v1 (title, media, materials, elements, sources,
   frequencies, outputs) — documented next to the cases.
2. `common/` folder: JSON inputs + expected CSV outputs for (at least) the
   Phase 2 conductor, a vertical rod, and a small grid; run locally as
   integration tests via `fpm test` (no hosted CI — decision §9).
3. Parser: stay within the minimal-parser subset; escape hatch per ADR 0006.

### Phase 6 — Sources and time domain

1. Signal waveforms: Heidler, double exponential (the Matlab reference ships
   Heidler, double exponential plus a Jones-parametrised variant, single
   exponential, impulse/step, Portela's concave model and sine — port as
   needed).
2. FFT driver: excitation spectrum → transfer function → IFFT (stdlib FFT or
   FFTW); Hanning taper / analytic-continuation notes in theory.md §8.
3. Validation: impulse response of the Phase 2 conductor vs published
   waveforms.

### Phase 7 — More elements and media (as needed)

Priority from the original's inventory: `tCircumference` (grounding rings),
`tCatenary`, grid/mesh generator element, tubular conductor, insulated
conductor, series RLC element; reflection-coefficient images and multi-layer
soil after that. The Matlab reference's full element inventory, for the
record: straight lines (three variants), ring, grid, cable, catenary,
lightning-channel element, helicoidal rod, tube, conduit, solid shapes
(block/cube/pyramid/tetrahedron), lumped series-RLC "impedance" elements
(extra unknowns that do not couple electromagnetically — their `Zt` row is
zeroed), and insulated buried cables (leakage through $j\omega\varepsilon$
only; placeholder theory, flagged TODO in the legacy code itself). The C++
adds bundle and L-profile (lattice-member) internal impedances and a
shielded-wire segment.

### Phase 8 — Second implementation (Python)

NumPy/SciPy port following the object model; must pass every `common/` case.
Rust later, same contract.

---

## 4. Near-term milestone

**Phases 0–2**: validated single-frequency solve for the Portela 1997 buried
conductor, with the convention audit done first. Priority order:

1. [x] Phase 0 audit + cleanup
2. [x] `tLine%assemble` + structure arrays
3. [x] Geometry-factor module (direct + image) + tests
4. [x] `Z_int` Bessel internal impedance
5. [x] `Zlong`/`Ztrans` fill *interface* (ADR 0009, theory factors internal)
       and fill loop over the geometry matrices (`tStudy%run`)
6. [x] `tStudy%run` wiring + current injection (ADR 0010)
7. [x] DC-limit test (`test_solve.f90`); Portela curve test deferred —
       no tabulated data exists (theory.md §9.2), needs P3 (TAGS
       cross-validation) as an executable oracle
8. [x] `example3` prints the result table (`example1`/`example2` stay as
       the εr = 1 smoke cases per `common/README.md`)

---

## 5. Testing strategy

| Layer | Where | What |
| --- | --- | --- |
| Unit | `fortran/test/` | quadrature vs closed forms; sign/decay pins; Bessel `Z_int` vs tables; dispersion DC limit |
| Integration | `fortran/test/` | end-to-end DC resistance; full vs reduced solve (when both exist); reciprocity, passivity |
| Reference | `common/` | JSON in → CSV out, compared with tolerance; shared across languages |
| Benchmarks | `benchmarks/` (proposed) | TAGS and PRTL-mHEM as git submodules; cross-code runs per [BENCHMARKS.md](BENCHMARKS.md) |

There is **no hosted CI** (decision §9): the gate is a local
`fpm build && fpm test` before merging. Practical caveats, measured 2026-07-05:

- In the default (debug) profile the quadrature-heavy suites are effectively
  un-runnable — `test_geometry`'s 10×10 fill exceeded 9 minutes without
  completing, dominated by the adaptive 2-D Gauss–Kronrod calls plus
  `-fcheck` array-temporary copies. **Run the slow suites under
  `--profile release`** (as `build.sh` builds), or split tests into fast
  (mesh/assemble: seconds) and slow (geometry/impedance) tiers and run the
  slow tier before merges only.
- A cold `fpm test` also pays several minutes of stdlib compilation; keep the
  build cache.

---

## 6. Open decisions

| Decision | Status |
| --- | --- |
| Soil dispersion model | **Decided** — ADR 0007 accepted 2026-07-05: `tPortelaSoil` first, Lima–Portela [31] parametrisation, ω₀ = 2π·1 MHz |
| Voltage-source handling | **Decided** — current-injection equivalent ([ADR 0010](adr/0010-sources-as-current-injections.md)) |
| Impedance-fill interface | **Decided** — theory factors inside `calcZ*` ([ADR 0009](adr/0009-impedance-fill-interface.md)) |
| FFT dependency | stdlib vs FFTW — decide in Phase 6; NLT proposed on top (P4 in §7) |
| JSON schema v1 | draft with Phase 5; freeze before Python port |
| Reduced `Z_g` solver | deferred optimisation (ADR 0003) |
| GUI module | **Decided** — Python/PySide6/Qt3D, view-only v1, own `gui/` folder ([ADR 0011](adr/0011-gui-module-technology-and-scope.md)) |
| Results JSON schema (output) | **Decided** — v0 frozen ahead of any writer ([ADR 0012](adr/0012-results-json-schema.md)); consumed by Phase 3 item 3 and GUI phase G2 |
| Quadrature tolerances | `errrel = min(la,lb)·10⁻⁶`, `maxint = 500` are dissertation-era values, open to revision (interview §9) — revisit with the P1 mHEM kernel |

---

## 7. Proposals from the open-source HEM comparison (July 2026)

Three companion open-source codes were inspected side by side with this
repository (see "Related open-source implementations" in
[references.md](references.md)):

- **TAGS** (pedrohnv, C99) — HEM/mHEM grounding solver, NLT time domain,
  field/potential post-processing;
- **PRTL-mHEM** (VitorLima1990, Python) — mHEM grounding inside a full line
  lightning-performance chain;
- **PRTL** (acslima, Wolfram/CDF) — the original open framework the Python
  port derives from.

Headline finding: TUPÃ's geometry-factor separation (theory.md §4.1, from the
2003 dissertation) is the same optimisation published as **mHEM** by Lima et
al. (references.md [11]) and used by TAGS/PRTL-mHEM — so the core design is
independently validated in the literature. TAGS's closed-form self integral
is identical to theory.md's `g_self`, confirming the gap-8 fix. The concrete
proposals, in priority order:

### P1 — mHEM single-integral kernel for `g(a,b)` (Phase 2, low effort)

Replace the default 2-D Gauss–Kronrod evaluation of the geometry factor with
the 1-D form now stated in theory.md §4.2: the inner integral over the sender
segment is the closed-form log term, leaving one adaptive quadrature. Same
quantity, cheaper and better conditioned for close segments. Keep the 2-D
path as the test oracle (already exists in `Impedance.f90`/`Geometry.f90`).

### P2 — Frequency-dependent image reflection coefficient (Phase 2/4, low effort)

Promote Γ(ω) from "planned refinement" to the default for buried conductors:
`Γ_t(ω) = (W_soil − jωε₀)/(W_soil + jωε₀)`, `Γ_ℓ = 1` (theory.md §5). Both
open-source companion codes use it even with constant soil parameters; it
needs only the medium constants already computed in `calcParam` and
multiplies the image parcel. **Strengthened by the July 2026 re-inspection
(§8): the original Matlab already implements this coefficient as its default
mode** (ideal images are its `SOLO_IDEAL` switch; the C++ port dropped it) —
so this proposal restores reference behaviour, it does not extend it. The
ideal ±1 table remains as its low-frequency limit and as a test pin.
Validation impact: Portela 1997 curves should still match; Grcev-grid
MHz-range behaviour will not without it.

### P3 — Cross-code validation against TAGS (Phase 2 milestone, medium effort)

TAGS builds locally (C99 + Cubature + LAPACK) and takes arbitrary electrode
lists. Add a validation step that runs the Phase 2 buried conductor (and later
the Grcev grid) through both codes and compares input impedance over the
sweep. This gives an executable oracle *now*, independent of digitised paper
curves. Caveats in theory.md §9.6: compare physical outputs only — TAGS uses
`|cosθ|` and different incidence/system conventions internally.

### P4 — Numerical Laplace Transform for the time domain (Phase 6, medium effort)

Adopt the NLT (complex frequency `s = c + jω`, damping `c ≈ ln(N²)/T`, window
filter before the inverse transform — Gómez & Uribe, references.md [17])
instead of the plain FFT drive, as TAGS and PRTL do. The physics kernels are
untouched (they already take complex medium constants); only the sweep driver
and the inverse-transform step change. Plain FFT is the `c = 0` special case
and remains for tests.

### P5 — Concretise the dispersive-soil subtypes (Phase 4, feeds ADR 0007)

The comparison supplies exact, citable formulas and parameter tables:
`tVisacroAlipioSoil` per Alipio & Visacro 2014 [14] (with the mean /
relatively conservative / conservative sets — the default in both reference
codes), `tLongmireSmithSoil` per Longmire & Smith [15] as parametrised by
Cavka et al. [16]. Keep `tPortelaSoil` first (matches the project validation
curves); implement `tVisacroAlipioSoil` second since it enables direct
comparison with TAGS/PRTL-mHEM outputs.

### P6 — Parallelise over frequencies, not the matrix fill (Phase 3, decision)

TAGS deliberately multithreads the frequency loop and pins BLAS to one thread
("this is important"), since frequencies are embarrassingly parallel and the
per-frequency work (fill + `ZGESV`) shares nothing. With geometry factors
precomputed once, TUPÃ's frequency loop has the same shape. Proposal: measure
both, but expect frequency-level OpenMP to supersede the fill-loop OpenMP
currently pencilled in ([CONVENTIONS.md](CONVENTIONS.md) records the current
default; update it if confirmed).

### P7 — Field/potential post-processing (Phase 7+, new feature)

TAGS computes scalar potential, electric field and path voltages at arbitrary
points from the solved `I_t`/`I_ℓ` (touch and step voltages, GPR profiles).
The Matlab reference has the same family of outputs (electric fields, 2-D/3-D
soil-surface potentials, touch potential, mesh potentials, transfer
functions) — use its output-class inventory to prioritise the `tResult`
subtypes. Not needed for the near-term milestone.

### P8 — Criteria-based segmentation defaults (Phase 2/3, low effort)

From the July 2026 literature batch rather than the code comparison:
Schroeder, Moura & Machado (references.md [19]) show tower-footing responses
stay within engineering accuracy (GPR peak +10 %, overvoltage peaks +5 %)
with segments up to ~1000·r₀ — vastly coarser than the traditional 10·r₀,
over 30× faster. Proposal: keep λ/10 as the default `Structure.assemble()`
bound (theory.md §4.1), but expose the segment-length target as a per-study
input validated by a coarse-vs-fine convergence test, so large grids don't
pay fine-mesh cost by default. Feeds the same meshing code Phase 2 already
touches.

### Explicitly *not* proposed

- Adopting TAGS's symmetric `(u, I_ℓ, I_t)` immittance block system — TUPÃ's
  `(u, i₁, i₂)` form is equivalent, already ported, and pinned by theory.md
  §6; noted in §9.6 as a consistency-check option only. (The Matlab original
  already carries this layout as its solver "método 5" — evidence it is a
  useful cross-check, still not the primary path.)
- The transmission-line performance chain of PRTL/PRTL-mHEM (towers, spans,
  flashover, outage rate) — out of scope for the grounding-solver milestone;
  revisit after Phase 8 (it is the dissertation's original application).
- Complex images (Kuhar et al., references.md [20]) — extends HEM validity
  above a few MHz; lightning studies don't need it, and it only makes sense
  after P2's Γ(ω) is in and validated. Documented in theory.md §5/§10.1.
- Time-domain HEM (HEM-TD, references.md [21]) — only pays off for nonlinear
  phenomena (soil ionisation, arresters, corona), which TUPÃ excludes by
  design (theory.md §8).
- Rational-model / FDNE export for EMT programs (references.md [26, 27]) — a
  natural future *output format* (fit `Z_g(ω)`, enforce passivity, emit an
  ATP/EMTP/PSCAD equivalent), not solver work; revisit when there are users
  asking for EMT integration.

---

## 8. Findings from re-inspecting the legacy implementations (July 2026)

The original repository was re-cloned with **both** legacy MoM
implementations side by side: the original Matlab code (the dissertation
implementation) and the C++/Fortran hybrid ported from it. The Matlab
version is now the **model reference of record**. What the re-inspection
established, beyond what is already folded into the sections above:

1. **Γ(ω) images are original behaviour.** The Matlab's default mode
   computes the frequency-dependent image reflection coefficient
   (equal-permeability Fresnel form, applied to both `Z_t` and `Z_ℓ` image
   parcels) and keeps ideal images behind a `SOLO_IDEAL` switch; the C++
   port kept only the ideal limits. Feeds P2 (theory.md §5 updated).
2. **Self geometry factor bug lineage.** The legacy expression
   `r − h + l·log((1 + h)/r)`, `h = hypot(l, r)`, originates in the Matlab
   and was ported verbatim to the C++: it is half the correct `g_self` *and*
   has a literal `1` where `l` belongs (dimensionally inconsistent; the two
   coincide only for 1 m segments). Gap 8's fix stands (theory.md §4.2
   updated).
3. **C++-only call-site bug.** The C++ self-term call passes the
   longitudinal image geometry factor in the transversal-image slot — do not
   use the C++ fill as an oracle for the diagonal terms (warning added to
   Phase 2).
4. **All three solver layouts exist in the Matlab** as switchable methods:
   reduced nodal (two variants), augmented (LU / GMRES fallback), and a
   TAGS-style symmetric `(u, I_ℓ, I_t)` system ("método 5") — plus
   commented-out "Portela convention" sign variants. Ready-made
   consistency-test material for theory.md §6/§9.4.
5. **Convention mixing is real and must gate cross-validation.** The Matlab
   uses `σ + jωε` immittance and a decaying `e^{−γR}` (theory conventions),
   but a `−jωμ/4π` longitudinal constant and a `D` incidence stored as `−1`
   (compensated in solver assembly). Compare against it on moduli and
   time-domain waveforms only (theory.md §2 caveat added; reinforces
   ADR 0008).
6. **Feature inventory is richer than previously documented**: tubular
   internal impedance (I/K Bessel) already implemented; two dispersive-soil
   routines (Portela power-law at ω₀ = 1 rad/s [30]; Lima–Portela at
   2π·1 MHz [31]); field/soil-potential/touch-voltage output classes (feeds
   P7); Heidler/double-exp/Jones/exponential/impulse/Portela-concave/sine
   signals (Phase 6); the element list in Phase 7; a direct inverse-Fourier
   quadrature over an interpolated spectrum besides the FFT driver
   (theory.md §8).
7. **Input formats**: the Matlab reads keyword-based text case files
   (`.caso`/`.est`); XML was a C++ addition. Neither constrains the JSON
   schema (ADR 0006), but the Matlab case files are the natural source when
   porting reference cases to `common/`.

---

## 9. Author-interview decisions (2026-07-05)

Recorded from the documentation-reconstruction interview; where a decision
changed a document or the code, the change is already applied and referenced.

| Topic | Decision |
| --- | --- |
| Application scope | MVP: tower-footing grounding. Full application tier: complete lines or substations. |
| Project role | Scientifically citable reference implementation (docs tone, validation rigour follow from this). |
| Convention authority | theory.md remains normative with the **engineering convention** (`e^{+jωt}`); Portela's papers (physics convention) are mapped through the §2 conjugation table, never followed directly. |
| Validated models | **None yet** — no end-to-end physics validation has run; only unit-level convention pins. |
| Soil dispersion | ADR 0007 accepted: `tPortelaSoil` first, Lima–Portela [31] parametrisation, ω₀ = 2π·1 MHz. |
| Proposals P1/P2/P4 | Confirmed as written (mHEM 1-D kernel; Γ(ω) images; NLT). |
| Sources | Current-injection equivalents (ADR 0010). |
| Fill interface | Theory factors inside `calcZSelf`/`calcZMutual` (ADR 0009); `current1/current2` documented as end currents `i₁`/`i₂`. |
| Quadrature tolerances | Dissertation-era values kept for now, open to revision. |
| Stable vs fluid modules | Stable: `mMesh` conventions, `mGeometry`, `mImpedance`. Fluid: `tResult`, `mJsonParser`, `tStudy%run`. |
| Public contract | JSON schema + `common/` cases only; all Fortran module APIs are internal and changeable. |
| Compilers | Latest gfortran; keep ifx-compatible. |
| CI | No hosted CI (no GitHub Actions); local `fpm build && fpm test` gate (§5). |
| Release process | Proposed (delegated): semver; **0.1.0 = validated Phase 2 milestone** (Portela-curve case within tolerance); annotated git tags + a CHANGELOG; no package-registry publishing planned. **Open as of the Phase 2 wiring pass**: only the DC-limit check is executable today (§3 Phase 2 item 3) — the Portela-curve tolerance check needs P3 (TAGS cross-validation) or real tabulated data first, so hitting this 0.1.0 bar needs one of those, not just the wiring done here. |
| SLATEC | The **cloned** `fortran/slatec/` checkout (from the author's fork, fetched by `build.sh`) is canonical and may be fine-tuned in place. |
| Benchmarks | TAGS and PRTL-mHEM to be added as git submodules under `benchmarks/` (see [BENCHMARKS.md](BENCHMARKS.md)); they are the executable validation oracles until curated reference datasets arrive. |
| Validation data | No tabulated data from Portela 1997 exists — only the equations; further validation references to be supplied by the author. Visacro & Soares 2005 [5] has no usable comparison data. No legacy-output fixtures for now. |
| Precision | `dp` kind added to `mCtes`; new code uses it, legacy `kind=8` migrates gradually. |
| Error handling | All `error stop` converted to feh (`raiseError`), including the JSON parser and the CLI entry point. |
| Housekeeping | `check.f90` stub test and `calcBase` placeholder deleted. |
