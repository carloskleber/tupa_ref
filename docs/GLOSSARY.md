# TUPÃ — Glossary

Main terms as used across the documentation and code. Symbols follow
[theory.md](theory.md); bracketed numbers are [references.md](references.md)
entries.

## Model and physics

- **HEM (Hybrid Electromagnetic Model)** — the power-community name for this
  Method-of-Moments application to thin-wire lightning/grounding problems:
  field coupling through $Z_\ell$/$Z_t$ matrices plus circuit-type (nodal)
  closure [1, 5]. "Hybrid" = field theory + circuit theory.
- **MoM (Method of Moments)** — general framework for solving integral
  equations by expanding the unknown in basis functions and testing the
  residual [6, 7]. TUPÃ fixes pulse basis functions and matching on segment
  averages.
- **mHEM (modified HEM)** — the HEM with frequency-independent geometry
  integrals precomputed once [11]; the same optimisation as TUPÃ's
  geometry-factor separation (ADR 0004), published independently.
- **Electrode / segment** — one straight cylindrical piece of discretised
  conductor (`tElectrode`); carries a longitudinal and a transversal
  current. "Segment" in theory text, "electrode" in the object model — same
  thing.
- **Node** — endpoint of one or more segments (`tNode`); carries the scalar
  potential (voltage to remote earth) $u$, stored in `tMesh%voltage`.
- **End currents $i_1, i_2$** — the currents entering a segment at its two
  end nodes (both positive *into* the segment, theory.md §2). Stored in
  `tMesh%current1/current2` as part of the solution $x = [u, i_1, i_2]$.
- **Longitudinal current $I_\ell$** — mean current along the segment axis,
  $I_\ell = (i_1 - i_2)/2$.
- **Transversal (leakage) current $I_t$** — total current leaking from the
  segment's lateral surface into the medium, $I_t = i_1 + i_2$.
- **$Z_\ell$ (Zlong)** — longitudinal impedance matrix: axial voltage drop
  on segment *a* per unit longitudinal current on segment *b* (vector
  potential coupling).
- **$Z_t$ (Ztrans)** — transversal impedance matrix: mean surface potential
  on *a* per unit leakage current from *b* (scalar potential coupling).
- **$Z_{eq}$ (augmented system)** — the $(n_n + 2n_s)$ block matrix stacking
  the voltage-drop, mean-potential and KCL equations (theory.md §6, ADR
  0003), solved by `ZGESV` per frequency.
- **Geometry factor $g(a,b)$** — the real, frequency-independent double
  integral $\iint dl_a\, dl_b / R$; precomputed once per geometry
  (theory.md §4.1-4.2, ADR 0004). $g_{self}$: its closed-form coincident
  (axis-to-surface) value.
- **Propagation constant $\gamma$** — $\sqrt{j\omega\mu(\sigma+j\omega\varepsilon)}$
  with $\mathrm{Re}\,\gamma \ge 0$; propagation factor $e^{-\gamma R}$
  (engineering convention $e^{+j\omega t}$, theory.md §2).
- **Immittance $W(\omega)$** — the medium's volumetric admittance density
  $\sigma + j\omega\varepsilon$; frequency-dependent for dispersive soil
  (theory.md §7).
- **Image method** — the air-soil interface ($z = 0$) represented by mirror
  segments; ideal limits give the ± sign rules of theory.md §5 (ADR 0005);
  the frequency-dependent coefficient $\Gamma(\omega)$ is the planned
  refinement (ROADMAP P2).
- **Internal impedance $Z_{int}$** — per-segment skin-effect impedance of
  the conductor itself, from the $I_0/I_1$ Bessel ratio (theory.md §4.3).
- **Dispersive soil** — soil whose σ and ε vary with frequency; modelled by
  `tMaterial` subtypes named after their references: `tPortelaSoil` [1, 30,
  31], `tLongmireSmithSoil` [15, 16] (not yet implemented),
  `tVisacroAlipioSoil` [14], mean parameter set (ADR 0007).
- **GPR (Ground Potential Rise)** — potential of the grounding structure vs
  remote earth under injected current; a primary engineering output.
- **NLT (Numerical Laplace Transform)** — time-domain route solving at
  damped complex frequencies $s = c + j\omega$ with data windows [17];
  planned replacement for the plain FFT drive (ROADMAP P4).
- **Thin-wire approximation** — conductors represented by axial line
  sources with field points on the surface; requires segment length large
  vs radius and small vs wavelength (theory.md §4.1).

## Symbols

Mathematical symbols as used in [theory.md](theory.md), with the Fortran
identifier(s) that hold them. Types live in `fortran/src/`: `tMesh`
(Mesh.f90), `tStudy` (Study.f90), `tMaterial` family (Material.f90),
`mGeometry` (Geometry.f90), `mImpedance` (Impedance.f90).

### Fields, currents, propagation (§2)

| Symbol | Meaning | theory.md | Code |
| --- | --- | --- | --- |
| $u$ | Node voltage (V), to remote earth | §1, §6 | `tMesh%voltage` |
| $i_1, i_2$ | Segment end currents (A), positive into the segment | §2 | `tMesh%current1`, `tMesh%current2` |
| $I_\ell$ | Mean longitudinal current, $(i_1-i_2)/2$ | §2 | derived from `current1`/`current2` at the call site |
| $I_t$ | Total transversal (leakage) current, $i_1+i_2$ | §2 | derived from `current1`/`current2` at the call site |
| $\omega$ | Angular frequency (rad/s) | §2 | `omega` (argument throughout) |
| $\gamma$ | Propagation constant, $\sqrt{j\omega\mu(\sigma+j\omega\varepsilon)}$ | §2 | `tMesh%propAir`/`propSoil`; `tLinear%propagationConstant` |
| $\sigma + j\omega\varepsilon$ | Medium immittance | §2 | inlined as `cmplx(sigma, omega*eps)` in `mMesh%calcParam`, `mMaterial%calcPropagationConstant` |

### Potentials, impedances, geometry factor (§3-4)

| Symbol | Meaning | theory.md | Code |
| --- | --- | --- | --- |
| $R_{ab}$, $\bar R_{ab}$ | Distance / mean distance between segments *a*, *b* | §4 | `mGeometry%meanDistance`; cached in `tStudy%geomRbar` |
| $g(a,b)$ | Geometry factor, $\iint dl_a\,dl_b/R_{ab}$ | §4.1-4.2 | `mGeometry%mutualGeometryFactor` (dispatches to `parallelGeometryFactor` or the quadrature oracle `mImpedance%geometryFactor2D`); cached in `tStudy%geomG` |
| $g_{self}$ | Coincident (self) geometry factor, closed form | §4.2 | `mGeometry%selfGeometryFactor` |
| $\theta_{ab}$, $\cos\theta_{ab}$ | Angle / direction cosine between segments | §4 | `mGeometry%directionCosine`; cached in `tStudy%geomCosTheta` |
| $Z_t$ | Transversal impedance matrix | §4, §6 | `tMesh%Ztrans` |
| $Z_\ell$ | Longitudinal impedance matrix | §4, §6 | `tMesh%Zlong` |
| $Z_{int}$ | Internal (skin-effect) impedance | §4.3 | `mImpedance%internalImpedance` |
| $\rho$ | Bessel-ratio argument in $Z_{int}$ | §4.3 | local `rho` in `mImpedance%internalImpedance` |
| $I_0, I_1$ | Modified Bessel functions (first kind) | §4.3 | SLATEC `zbesi`; local `ratio` in `mImpedance%internalImpedance` |

### Air-soil interface / image method (§5)

| Symbol | Meaning | theory.md | Code |
| --- | --- | --- | --- |
| $c_E$ | Electric constant, $1/(4\pi(\sigma+j\omega\varepsilon))$ | §5 | `tMesh%cEAir`, `tMesh%cESoil` |
| $c_M$ | Magnetic constant, $j\omega\mu/4\pi$ | §5 | `tMesh%cMAir`, `tMesh%cMSoil` |
| $\Gamma_t$, $\Gamma_\ell$ | Frequency-dependent reflection coefficients | §5 | not implemented; ideal $\pm1$ limits are hardcoded in `mMesh%calcZSelf`/`calcZMutual` (ROADMAP P2) |
| $g_i$, $\bar R_i$ | Image geometry factor / mean distance | §5 | `tStudy%geomGi`, `tStudy%geomRbari` |
| $\cos\theta_i$ | Direction cosine against the image | §5 | `tStudy%geomCosThetaI` |

### Nodal system (§6)

| Symbol | Meaning | theory.md | Code |
| --- | --- | --- | --- |
| $A, B, C, D$ | Incidence matrices | §6 | `tMesh%A`, `%B`, `%C`, `%D`; assembled by `mMesh%calcTopology` |
| $Z_{eq}$ | Augmented system matrix | §6 | `tMesh%Zeq`; assembled by `mMesh%calcFreq2` |
| $x = [u, i_1, i_2]$ | Unknown/solution vector | §6 | RHS/solution `y` inside `mMesh%injectSignal` (LAPACK `ZGESV`), copied out to `tMesh%voltage`/`current1`/`current2` (or via `mMesh%getOutputs`) |

### Soil dispersion (§7)

| Symbol | Meaning | theory.md | Code |
| --- | --- | --- | --- |
| $W(\omega)$ | Soil immittance, $\sigma(\omega) + j\omega\varepsilon(\omega)$ | §7 | `tMaterial%admittance` (deferred; `tPortelaSoil%admittance`/`tVisacroAlipioSoil%admittance`) |
| $\sigma_0$ | DC (low-frequency) conductivity | §7 | `tLinear%sigma`; base term of `tPortelaSoil`/`tVisacroAlipioSoil` |
| $\alpha$ | Dispersion exponent | §7 | `tPortelaSoil%alpha0` |
| $\Delta\sigma$ / $k_r$ | Dispersion magnitude at $\omega_0$ | §7 | `tPortelaSoil%kr` |
| $h(\sigma_0)$, $\xi$, $\varepsilon_{r\infty}$ | Alipio-Visacro mean-curve constants | §7 | fixed parameters inside `tVisacroAlipioSoil%admittance` |

### Validation (§9) and constants

| Symbol | Meaning | theory.md | Code |
| --- | --- | --- | --- |
| $R$ | Sunde/Dwight DC grounding resistance | §9.1 | local `rDc` in `fortran/test/test_solve.f90` |
| $\mu_0$ | Vacuum permeability | §2 | `mCtes%MU0` |
| $\varepsilon_0$ | Vacuum permittivity | §2 | `mCtes%EPSILON0` |
| $j$ | Imaginary unit | §2 | `mCtes%IMAG_I` |

## Software

- **Study / Structure / Element / Material / Mesh / Result** — the
  language-agnostic object model (ADR 0002); see
  [ARCHITECTURE.md](ARCHITECTURE.md) §2.
- **`t` / `m` prefixes** — Fortran naming: `tXxx` derived types, `mXxx`
  modules.
- **Assembly** — the discretisation step: elements turn themselves into
  nodes + electrodes registered with the structure
  (`tStructure%assembleStructure`).
- **FPM** — the Fortran Package Manager; build tool for the project.
- **FORD** — documentation generator consuming the `!!` comments
  (`fortran/Tupa.md` is its config).
- **feh** — fortran-error-handler library; all fatal errors route through
  `mError%raiseError`.
- **SLATEC** — legacy public-domain numerical library; used for the complex
  Bessel function `ZBESI` (built from the author's fork by `build.sh`).
- **ZGESV** — LAPACK dense complex linear solver (LU with partial
  pivoting); the only solver used (ADR 0003).
- **Gauss–Kronrod 7/15** — the adaptive quadrature rule pair in
  `mImpedance` evaluating the geometry factors.
- **`common/` cases** — JSON inputs (+ future expected outputs) shared by
  all language implementations; together with the JSON schema they form the
  project's public contract.
- **TAGS / PRTL / PRTL-mHEM** — companion open-source HEM implementations
  used as executable cross-checks ([BENCHMARKS.md](BENCHMARKS.md);
  references.md "Related open-source implementations").
- **TUPÃ** — "thunder(er)" in Tupi-Guarani; the name of the original 2003
  Matlab model and of this project.
