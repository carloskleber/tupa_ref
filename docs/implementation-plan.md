# TUPÃ — Implementation Plan

Reference electromagnetic transient solver (HEM / Method of Moments).
Fortran implementation first; object model and test cases shared with future
Python/Rust implementations (see [ADR 0002](adr/0002-language-agnostic-object-model.md)).

This plan supersedes the previous `IMPLEMENTATION_PLAN.md`. It is based on a
side-by-side analysis of this repository against the original (private) C++
implementation and the theory now consolidated in [theory.md](theory.md).

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
5. **No frequency sweep, no sources, no outputs** — `tResult` types are
   declared but never filled; no signal waveforms; no CSV/JSON writers.
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
currently fail (`FUNCBARRA` decreasing-with-distance, using a degenerate
all-zero direction vector in the test setup; and `IMPMUTUA` on coincident
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
     `calcZPropria`/`calcZMutua` (`pos1`/`pos2` args), not the geometry layer.
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

### Phase 2 — End-to-end single-frequency solve

1. Wire `tStudy%run`: assemble → topology → per-frequency
   (`calcParam` from `tMaterial`s → `Zlong`/`Ztrans` fill from geometry
   matrices → `calcFreq2` → inject → extract).
2. Current-source injection at named nodes (voltage sources deferred).
3. **Validation test (the milestone)**: buried horizontal conductor
   (10 m, r₀ = 5–7 mm, 0.5 m depth, σ = 0.01 S/m, εr = 10):
   - DC limit vs Sunde/Dwight resistance formula (theory.md §9.1);
   - low-frequency input impedance ≈ DC resistance;
   - full curve 100 Hz–1 MHz within 5 % of Portela 1997 [2].

**Exit criterion**: `fpm run --example example1` prints the validated
impedance-vs-frequency table.

### Phase 3 — Frequency sweep, results, output

1. Log-spaced frequency axis (default) with user override.
2. Fill `tVoltages`/`tLongCurrents`/`tTransCurrents` across the sweep;
   convenience queries (`inputImpedance`, `maxVoltage`).
3. CSV writer (primary) and JSON results writer.
4. OpenMP on the geometry-factor fill loop (already flagged in build.sh) —
   only after Phase 2 validation, with a determinism test.

### Phase 4 — Dispersive soil

1. Implement `tPortelaSoil` per ADR 0007 (Portela power-law proposed; confirm).
2. DC-limit convergence test against `tLinear`; repeat Phase 2 validation with
   dispersion on (Portela 1997 dispersive curves).

### Phase 5 — JSON I/O and common cases

1. Freeze the input schema v1 (title, media, materials, elements, sources,
   frequencies, outputs) — documented next to the cases.
2. `common/` folder: JSON inputs + expected CSV outputs for (at least) the
   Phase 2 conductor, a vertical rod, and a small grid; run as integration
   tests in CI (GitHub Actions + `fpm test`).
3. Parser: stay within the minimal-parser subset; escape hatch per ADR 0006.

### Phase 6 — Sources and time domain

1. Signal waveforms: Heidler, double exponential (the original also had
   Portela's concave model, sine, step — port as needed).
2. FFT driver: excitation spectrum → transfer function → IFFT (stdlib FFT or
   FFTW); Hanning taper / analytic-continuation notes in theory.md §8.
3. Validation: impulse response of the Phase 2 conductor vs published
   waveforms.

### Phase 7 — More elements and media (as needed)

Priority from the original's inventory: `tCircumference` (grounding rings),
`tCatenary`, grid/mesh generator element, tubular conductor, insulated
conductor, series RLC element; reflection-coefficient images and multi-layer
soil after that.

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
5. [ ] `Zlong`/`Ztrans` fill from geometry matrices
6. [ ] `tStudy%run` wiring + current injection
7. [ ] DC-limit test, then Portela curve test
8. [ ] `example1` prints the result table

---

## 5. Testing strategy

| Layer | Where | What |
| --- | --- | --- |
| Unit | `fortran/test/` | quadrature vs closed forms; sign/decay pins; Bessel `Z_int` vs tables; dispersion DC limit |
| Integration | `fortran/test/` | end-to-end DC resistance; full vs reduced solve (when both exist); reciprocity, passivity |
| Reference | `common/` | JSON in → CSV out, compared with tolerance; shared across languages |
| CI | GitHub Actions | `fpm build && fpm test` per push; reference cases on PR |

---

## 6. Open decisions

| Decision | Status |
| --- | --- |
| Soil dispersion model | ADR 0007 **proposed** (Portela power-law first) — confirm; see P5 in §7 |
| Voltage-source handling | current-injection equivalent vs constraint rows — decide in Phase 3 |
| FFT dependency | stdlib vs FFTW — decide in Phase 6; NLT proposed on top (P4 in §7) |
| JSON schema v1 | draft with Phase 5; freeze before Python port |
| Reduced `Z_g` solver | deferred optimisation (ADR 0003) |

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
reference codes use it even with constant soil parameters; it needs only the
medium constants already computed in `calcParam` and multiplies the image
parcel. The ideal ±1 table remains as its low-frequency limit and as a test
pin. Validation impact: Portela 1997 curves should still match; Grcev-grid
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
currently pencilled in (CLAUDE.md hook to be updated if confirmed).

### P7 — Field/potential post-processing (Phase 7+, new feature)

TAGS computes scalar potential, electric field and path voltages at arbitrary
points from the solved `I_t`/`I_ℓ` (touch and step voltages, GPR profiles).
Maps cleanly onto new `tResult` subtypes; useful the moment grids are
simulated. Not needed for the near-term milestone.

### Explicitly *not* proposed

- Adopting TAGS's symmetric `(u, I_ℓ, I_t)` immittance block system — TUPÃ's
  `(u, i₁, i₂)` form is equivalent, already ported, and pinned by theory.md
  §6; noted in §9.6 as a consistency-check option only.
- The transmission-line performance chain of PRTL/PRTL-mHEM (towers, spans,
  flashover, outage rate) — out of scope for the grounding-solver milestone;
  revisit after Phase 8 (it is the dissertation's original application).
