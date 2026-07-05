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
  potential (voltage to remote earth) $u$.
- **End currents $i_1, i_2$** — the currents entering a segment at its two
  end nodes (both positive *into* the segment, theory.md §2). Stored in
  `tMesh%corrente1/corrente2` as part of the solution $x = [u, i_1, i_2]$.
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
  31], `tLongmireSmithSoil` [15, 16], `tVisacroAlipioSoil` [13, 14] (ADR
  0007).
- **GPR (Ground Potential Rise)** — potential of the grounding structure vs
  remote earth under injected current; a primary engineering output.
- **NLT (Numerical Laplace Transform)** — time-domain route solving at
  damped complex frequencies $s = c + j\omega$ with data windows [17];
  planned replacement for the plain FFT drive (ROADMAP P4).
- **Thin-wire approximation** — conductors represented by axial line
  sources with field points on the surface; requires segment length large
  vs radius and small vs wavelength (theory.md §4.1).

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
