# References

Numbered as cited in [theory.md](theory.md).

## Core formulation

1. **Portela, C.** — "Frequency and Transient Behavior of Grounding Systems,
   Part I — Physical and Methodological Aspects", *Proceedings IEEE
   International Symposium on Electromagnetic Compatibility*, pp. 379–384,
   Austin, USA, August 1997. Foundation of the formulation: soil dispersion
   models, segment potentials, A/B/C/D nodal equations, FFT transient driver.
2. **Portela, C.** — "Frequency and Transient Behavior of Grounding Systems,
   Part II — Practical Application Examples", same symposium, pp. 385–390.
   Source of the project's primary validation curves.
3. **Arruda, C. K. C.** — *Modelagem de Linhas de Transmissão para Análise de
   Comportamento Quanto a Descargas Atmosféricas* (Modelling of Transmission
   Lines in Analysis of Lightning Performance), M.Sc. dissertation, COPPE/UFRJ,
   2003. DOI: 10.13140/RG.2.2.19894.56644. Geometry-factor separation,
   closed-form geometry factors, internal impedance expressions, linear-system
   derivation.
4. **Salari, J. C.; Azevedo, R. M.; Portela, C.** — "An efficient modeling of
   transmission lines towers and grounding systems for lightning propagation
   studies", *IX SIPDA*, Foz do Iguaçu, Brazil, Nov. 2007. Z_eq assembly as a
   single augmented system.
5. **Visacro, S.; Soares, A., Jr.** — "HEM: A model for simulation of lightning
   related engineering problems", *IEEE Trans. Power Delivery*, vol. 20, no. 2,
   pp. 1206–1208, Apr. 2005. Independent HEM formulation and validation cases.

## Method of Moments background

6. **Harrington, R. F.** — *Field Computation by Moment Methods*, Macmillan,
   1968 (reprint IEEE Press, 1993). The MoM framework: basis functions,
   testing, matrix formulation of integral equations.
7. **Gibson, W. C.** — *The Method of Moments in Electromagnetics*, 2nd ed.,
   CRC Press, 2014. Modern treatment; thin-wire kernels and numerical
   integration strategies.

## Supporting theory

8. **Stratton, J. A.** — *Electromagnetic Theory*, McGraw-Hill, 1941. Retarded
   potentials, spherical wave solutions in dissipative media.
9. **Bode, H.** — *Network Analysis and Feedback Amplifier Design*,
   D. van Nostrand, 1945. Minimum-phase-shift relations underpinning the soil
   dispersion model (real/imaginary part consistency).
10. **Sunde, E. D.** — *Earth Conduction Effects in Transmission Systems*,
    D. van Nostrand, 1949. Classical grounding resistance formulas used as DC
    validation anchors.

## Modified HEM (mHEM)

11. **Lima, A. C. S.; Moura, R. A. R.; Vieira, P. H. N.; Schroeder, M. A. O.;
    Correia de Barros, M. T.** — "A Computational Improvement in Grounding
    Systems Transient Analysis", *IEEE Trans. Electromagnetic Compatibility*,
    vol. 62, no. 3, pp. 765–773, Jun. 2020. DOI: 10.1109/TEMC.2019.2918621.
    Publishes the frequency-independent-integral separation (there named
    *modified HEM*, mHEM) that theory.md §4.1 derives from [3], plus the
    closed-form inner integral that reduces the geometry factor to a single
    1-D integral.
12. **Lima, A. C. S.; Dias, R. F. S.; Salim, K.; Correia de Barros, M. T.** —
    "An Open Framework for Lightning Performance of Overhead Transmission
    Lines", *XIV SIPDA*, Brazil, Oct. 2017. The PRTL framework: Laplace-domain
    nodal solution of line + towers + grounding, open (CDF/Wolfram) test cases.

## Frequency-dependent soil models

13. **Visacro, S.; Alipio, R.** — "Frequency Dependence of Soil Parameters:
    Experimental Results, Predicting Formula and Influence on the Lightning
    Response of Grounding Electrodes", *IEEE Trans. Power Delivery*, vol. 27,
    no. 2, pp. 927–935, Apr. 2012. Measurement-based σ(ω), εr(ω) formulas
    (used by PRTL-mHEM).
14. **Alipio, R.; Visacro, S.** — "Modeling the Frequency Dependence of
    Electrical Parameters of Soil", *IEEE Trans. Electromagnetic
    Compatibility*, vol. 56, no. 5, pp. 1163–1171, Oct. 2014.
    DOI: 10.1109/TEMC.2014.2313977. Causal (minimum-phase-consistent) model
    with recommended parameter sets (mean / relatively conservative /
    conservative); basis for `tVisacroAlipioSoil`.
15. **Longmire, C. L.; Smith, K. S.** — *A Universal Impedance for Soils*,
    Defense Nuclear Agency, Topical Report DNA 3788T, 1975. The 13-term Debye
    expansion behind `tLongmireSmithSoil`.
16. **Cavka, D.; Mora, N.; Rachidi, F.** — "A Comparison of
    Frequency-Dependent Soil Models: Application to the Analysis of Grounding
    Systems", *IEEE Trans. Electromagnetic Compatibility*, vol. 56, no. 1,
    pp. 177–187, Feb. 2014. Side-by-side comparison of the dispersive-soil
    models; the Smith–Longmire parametrisation as implemented in TAGS.

## Transient numerics and further validation cases

17. **Gómez, P.; Uribe, F. A.** — "The Numerical Laplace Transform: An
    Accurate Technique for Analyzing Electromagnetic Transients on Power
    System Devices", *Int. J. Electrical Power & Energy Systems*, vol. 31,
    2009. NLT with damping constant and window filters — the time-domain
    driver used by TAGS and PRTL.
18. **Grcev, L. D.; Heimbach, M.** — "Frequency Dependent and Transient
    Characteristics of Substation Grounding Systems", *IEEE Trans. Power
    Delivery*, vol. 12, no. 1, pp. 172–178, Jan. 1997. DOI: 10.1109/61.568238.
    Grounding-grid harmonic impedance curves; a standard validation case
    reproduced by TAGS.

## Related open-source implementations

Companion codes of the same model family, useful as executable cross-checks
(all inspected July 2026):

- **TAGS** (*Transient Analysis of Grounding Systems*, formerly HP-HEM) —
  <https://github.com/pedrohnv/transient-analysis-grounding-systems>, C99,
  GPLv3, DOI: 10.5281/zenodo.2644010. HEM per [5] with selectable integration
  (double integral, single, mHEM [11], midpoint); image reflection
  coefficients Γℓ, Γt as parameters; Alipio–Visacro [14] and Smith–Longmire
  [16] soils; NLT [17] time domain; potential/field/step-and-touch-voltage
  post-processing. Examples reproduce [18], Visacro & Soares, Alipio et al.,
  Sunjerga et al. There is a pure-Julia twin repository.
- **PRTL-mHEM** — <https://github.com/VitorLima1990/PRTL-mHEM>, Python.
  mHEM [11] grounding solver embedded in a full transmission-line lightning
  performance chain (multiconductor towers and spans, Kron reduction,
  disruptive-effect insulator flashover, outage rate); Visacro–Alipio [13]
  soil; solid and tubular Bessel internal impedances; NLT time domain.
- **PRTL** — <https://github.com/acslima/PRTL>, Wolfram/CDF notebooks of the
  original open framework [12] (four 138 kV test cases); grounding admittance
  imported from precomputed files.

## Related implementation notes

- The original TUPÃ implementation (C++/Fortran hybrid, private) and its user
  manual contain the working derivations that this repository's
  [theory.md](theory.md) consolidates; the model itself is fully specified by
  the public references above.
- TUPÃ's geometry-factor separation (theory.md §4.1, from [3], 2003) and the
  mHEM [11] are the same optimisation, arrived at independently; TAGS's
  closed-form self integral is identical to theory.md §4.2's `g_self` — a
  useful mutual confirmation of both codes.
