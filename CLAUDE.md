# CLAUDE.md — TUPÃ Project

---

## Project Identity

**TUPÃ** is a reference electromagnetic field transient solver based on the
Method of Moments (HEM — Hybrid Electromagnetic Model), used for lightning
and grounding system analysis.

Primary implementation: **Modern Fortran** (Fortran 2008+), built with FPM.
Planned future implementations: Python, Rust (same object model, shared test cases).

---

## Architecture

```
tStudy                  ← top-level container, handles I/O and orchestration
  tStructure            ← geometry + materials
    tElement (abstract) ← tLine, tCatenary, tCircumference, tTower, ...
    tMaterial (abstract)← tLinear, tPortelaSoil, tLongmireSmithSoil, ...
    tNode               ← 3D point with complex voltage
    tElectrode          ← line segment with longitudinal/transversal currents
  tMesh                 ← topology matrices + frequency-domain solution
  tResult (abstract)    ← tVoltages, tLongCurrents, tTransCurrents
```

Key physics flow:
1. `Structure.assemble()` → discretizes elements into nodes + electrodes
2. `Mesh.calcTopologia()` → builds A/B/C/D topology matrices
3. `Mesh.calcParam(omega)` → frequency-dependent medium constants (ε, μ, σ, k)
4. `Mesh.calcFreq2()` → assembles full impedance matrix Zeq
5. `Mesh.injetaSinalF()` → injects sources, calls ZGESV, stores voltages/currents

---

## Build and Test Commands

```bash
# Build and run main app
fpm build
fpm run

# Run examples
fpm run --example example1
fpm run --example example2

# Run tests
fpm test

# Full optimized build (downloads SLATEC, uses -O3 + OpenMP)
cd fortran && bash build.sh
```

Dependencies: LAPACK, BLAS, SLATEC (downloaded by build.sh), stdlib, fortran-error-handler.

---

## Coding Conventions

- **No implicit typing**: every file has `implicit none`
- **Free-form source**: `.f90` extension, no column limits
- **Modern Fortran only**: use allocatable arrays, polymorphism, abstract interfaces — no COMMON blocks, no EQUIVALENCE
- **No comments on obvious code**: only comment non-obvious physics derivations or numerical workarounds
- **Portuguese variable names are acceptable** — legacy from original MATLAB code; do not rename without asking
- **Complex arithmetic**: use Fortran intrinsic `cmplx()` and `(0.0_dp, 1.0_dp)` for imaginary unit; `dp` kind from `mCtes`
- **Error handling**: use `fortran-error-handler` (feh) — never `stop` directly
- **FORD convention**: write FORD style for automatic documentation of all types and routines

---

## Physics Context

- **Method of Moments**: integral equation formulation, conductors discretized into segments
- **Frequency domain**: all calculations at angular frequency ω; time-domain via IFFT
- **Media**: two-layer (air above z=0, soil below z=0); each has σ, εr, μr. Future implementations expect two-layer or multi-zone soil models.
- **Impedance types**:
  - `Zlong`: longitudinal (wire-to-wire, along conductor axis)
  - `Ztrans`: transversal (conductor-to-earth leakage)
- **ZGESV**: LAPACK routine for complex dense linear system A·x = b (LU factorization)
- **Image theory**: soil–air interface modeled by image conductors; sign rules matter

Key reference papers:
- Portela 1997 (IEEE EMC) — foundation of the HEM formulation
- Visacro & Soares 2005 (IEEE Trans. Power Del.) — HEM validation
- Author's dissertation, UFRJ 2003 (DOI:10.13140/RG.2.2.19894.56644)

---

## Customization Hooks

### Preferred implementation language for new features
<!-- LANG: fortran -->
Fortran

### Soil dispersion model to implement
<!-- SOIL_MODEL: longmire -->
Longmire-Smith (preferred for lightning studies; matches Portela reference)

### JSON library preference
<!-- JSON_LIB: json-fortran -->
json-fortran (FPM-compatible, mature)

### Frequency axis default
<!-- FREQ_AXIS: log -->
Logarithmic spacing (broadband analysis)

### Parallelism strategy
<!-- PARALLEL: openmp -->
OpenMP for impedance matrix fill loop (flags already in build.sh)

### Output formats to support
<!-- OUTPUT: csv,json -->
CSV (primary, gnuplot/Python compatible) + JSON (structured)

### Element types priority
<!-- ELEMENTS: line,catenary -->
tLine (done)

### Test validation reference
<!-- VALIDATION: portela1997 -->
Portela 1997 curves for buried straight conductor (10 m, 0.5 m depth, σ=0.01 S/m)

---

## What Claude Should NOT Do

- Do not rename Portuguese identifiers without explicit approval
- Do not introduce implicit typing or `implicit real` blocks
- Do not use `stop` — use `feh` error handler
- Do not use fixed-form Fortran or Hollerith constants
- Do not add Python/Rust code into the `fortran/` subtree
- Do not optimize prematurely — correctness first, then performance
- Do not create new abstraction layers unless explicitly requested

---

## Planned Features (do not implement unless asked)

- [ ] Time-domain IFFT driver
- [ ] Additional dispersive-soil `tMaterial` subtypes beyond `tPortelaSoil`
      (e.g. `tLongmireSmithSoil`, `tVisacroAlipioSoil`/Cole-Cole) — ADR 0007
- [ ] JSON input/output (Phase 5)
- [ ] Graphical output (gnuplot scripts or matplotlib)
