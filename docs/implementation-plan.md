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

Gaps in this repository, in dependency order:

1. **Geometry-factor layer is absent.** The original precomputes, per segment
   pair: mean distances (direct + image), geometry factors `g` (direct +
   image), direction cosines, and the self factor from a closed formula. Here
   only the raw quadrature exists; nothing computes or stores these matrices,
   and nothing computes image geometry. This is the biggest missing piece
   (theory.md §4–5, ADR 0004).
2. **Sign-convention divergences.** The port changed signs relative to the
   original in at least: `calcParam` propagation constant (imag part negated),
   propagation factor exponent (`exp(+j·d·γ)` vs `exp(−d·k)`), air-image
   longitudinal sign in `calcZPropria`/`calcZMutua`, and C/D topology entries
   (+1 here vs −1 original). Some may be equivalent under conjugation — they
   must be audited against theory.md, not eyeballed (ADR 0008).
3. **`tLine%assemble` is a stub** — no discretisation, no electrode creation,
   no node/material resolution; `tStructure%electrodes` is never populated and
   `assembleStructure` isn't reachable from `tStudy%run` (also a stub).
4. **Internal impedance missing** — Bessel-function `Z_int` (solid conductor)
   not ported (original had dedicated impedance-function classes).
5. **No frequency sweep, no sources, no outputs** — `tResult` types are
   declared but never filled; no signal waveforms; no CSV/JSON writers.
6. **`tPortelaSoil` is a placeholder** (returns zero) — see ADR 0007. It is
   the first of several planned dispersive-soil `tMaterial` subtypes
   (`tLongmireSmithSoil`, `tVisacroAlipioSoil`, ...), not the only one.
7. **Leftover C-interop artifacts** — 0-based `+1` index shifts in `Mesh.f90`,
   pointer-returning `alocaMalha`, dead `calcFreqF`/`solMalha` stubs with
   `error stop` (violates the feh rule).
8. **Suspicious self geometry factor in the original** — the C++
   `fatorGeometriaPropria()` returns `r − h + l·log((1+h)/r)`; theory gives
   `2[l·ln((l+√(l²+r₀²))/r₀) − √(l²+r₀²) + r₀]` (factor 2, and `l`, not `1`,
   in the log). Do **not** port it blindly; derive and test (theory.md §4.2).

Housekeeping: stray `*.mod` files at `fortran/` root and the `.history/`
folder should be gitignored; `fortran/test/test_impedance.f90` has uncommitted
changes to review.

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

### Phase 0 — Convention audit and cleanup (prerequisite)

1. Audit `Mesh.f90` sign conventions against theory.md §2/§5/§6; fix toward
   the doc. Unit-test: propagation factor decays; `Zlong`/`Ztrans` symmetric.
2. Naturalise the mesh module: 1-based indices, no pointer-returning
   allocator, delete `calcFreqF`/`solMalha` dead code, replace `error stop`
   with feh errors.
3. Gitignore `.history/` and stray `.mod`; resolve the pending
   `test_impedance.f90` diff.

**Exit criterion**: `fpm test` green; mesh module has no C-legacy artifacts.

### Phase 1 — Geometry layer (structure → matrices)

1. `tLine%assemble`: resolve node/material IDs, create internal nodes and
   `nElectrodes` segments, register with `tStructure` (flat arrays + `n1`/`n2`
   connectivity).
2. Geometry-factor computation (new module or extension of `Impedance.f90`):
   - mean distances `R̄`, image distances `R̄ᵢ` (mirror through z = 0);
   - `g(a,b)` by Gauss–Kronrod quadrature; `g_self` closed formula (derived,
     tested against quadrature with axis-to-surface offset — item 8 above);
   - direction cosines, incl. image direction (z-component flipped);
   - same-medium test; mixed pairs skipped (ADR 0005).
3. Internal impedance `Z_int`: solid conductor Bessel formula (SLATEC or
   stdlib Bessel); tubular later.
4. Tests: closed-form parallel-segments factor vs quadrature; self factor vs
   quadrature; image distances for a buried horizontal wire.

**Exit criterion**: for a 10 m line in 10 segments, geometry matrices match
independent (scripted) numerical integration to 1e−6 relative.

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

1. [ ] Phase 0 audit + cleanup
2. [ ] `tLine%assemble` + structure arrays
3. [ ] Geometry-factor module (direct + image) + tests
4. [ ] `Z_int` Bessel internal impedance
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
| Soil dispersion model | ADR 0007 **proposed** (Portela power-law first) — confirm |
| Voltage-source handling | current-injection equivalent vs constraint rows — decide in Phase 3 |
| FFT dependency | stdlib vs FFTW — decide in Phase 6 |
| JSON schema v1 | draft with Phase 5; freeze before Python port |
| Reduced `Z_g` solver | deferred optimisation (ADR 0003) |
