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

## HEM refinements and accuracy studies

19. **Schroeder, M. A. O.; Moura, R. A. R.; Machado, V. M.** — "A Discussion on
    Practical Limits for Segmentation Procedures of Tower-Footing Grounding
    Modeling for Lightning Responses", *IEEE Trans. Electromagnetic
    Compatibility*, vol. 62, no. 6, Dec. 2020. DOI: 10.1109/TEMC.2020.2982358.
    Parametric HEM study of segment-length limits: with acceptance criteria of
    +10 % on GPR peak and +5 % on insulator-overvoltage peaks (same waveshape),
    segments up to ~1000·r₀ remain acceptable — far coarser than the
    traditional 10·r₀ thin-wire prescription — with >30× speedups. Covers
    soils from 50 to 4000 Ω·m.
20. **Kuhar, A.; Arnautovski-Toševa, V.; Grčev, L.** — "High Frequency
    Enhancement of the Hybrid Electromagnetic Model by Implementing Complex
    Images", *Journal of Electrical Engineering and Information Technologies*
    (Skopje), vol. 2, no. 2, 2017. Replaces the quasi-static images of §5 with
    complex images (earth replaced by a perfect conductor at a complex depth),
    extending HEM agreement with full-wave NEC-4 results beyond the usual
    couple-of-MHz ceiling.
21. **Pereira, B.; Silveira, F. H.** — "Improvement of HEM-TD model: Advances
    on the representation of frequency-dependent soil parameters and on the
    calculation of time delays", *Electric Power Systems Research*, 2026.
    The time-domain HEM variant (HEM-TD): dispersive soil via a rational
    (pole–residue) model evaluated directly in time, plus refined time-delay
    computation; validated against the frequency-domain HEM and grounding-grid
    measurements. The time-domain route exists to handle nonlinear phenomena
    (soil ionisation, surge arresters, corona).
22. **Alípio, R. S.** — *Modelagem Eletromagnética de Aterramentos Elétricos
    nos Domínios do Tempo e da Freqüência*, M.Sc. dissertation, CEFET-MG,
    Belo Horizonte, 2008. Detailed derivation of the HEM in both domains (in
    Portuguese); a useful companion derivation to [3] and [5].

## Circuit, transmission-line and FDTD alternatives

23. **Grcev, L.; Kuhar, A.; Arnautovski-Toseva, V.; Markovski, B.** —
    "Evaluation of High-Frequency Circuit Models for Horizontal and Vertical
    Grounding Electrodes", *IEEE Trans. Power Delivery*, vol. 33, no. 6,
    pp. 3065–3074, Dec. 2018. Shows the popular HF circuit models are
    obtainable from the MoM integral equations by successive approximations,
    and maps their error against a rigorous full-wave model over electrode
    length, soil resistivity and frequency. Key finding: modelling the mutual
    coupling between electrode parts is what improves circuit models at HF.
24. **Duarte, N.; Alipio, R.; Vasconcellos, F.; Rachidi, F.** — "Efficient
    modeling of parallel counterpoise wires using an FDTD-based transmission
    line approach", *Electric Power Systems Research*, 2025. TL-theory model
    of parallel counterpoises solved by FDTD, frequency-dependent Z and Y
    included; deviations below 5 % against a rigorous electromagnetic model.
    Notable result: the effective length of counterpoise wires is independent
    of their separation.
25. **Cao, J.; Du, Y.; Ding, Y.; et al.** — "Lightning Surge Analysis of
    Transmission Line Towers with a Hybrid FDTD-PEEC Method", conference
    paper (venue/year not stated in the available copy; references through
    2019). 1-D FDTD for the line, PEEC for tower and lightning channel;
    quantifies the channel–tower coupling that circuit-based (and HEM-class)
    tools neglect.

## EMT-program integration (rational models / FDNE)

26. **Lima, A. C. S.; Parreiras, T. J. M. A.; Alípio, R.; Correia de Barros,
    M. T.** — "Realization of Rational Models for Tower-Footing Grounding
    Systems", *XVI IPST*, Guadalajara, Mexico, Jun. 2025. Rational
    approximation of the mHEM harmonic impedance vs a frequency-dependent
    network equivalent (FDNE) for EMT programs (ATP/EMTP/PSCAD): topology,
    passivity enforcement, minimum order, and the role of the effective
    length in the robustness of the realization.
27. **Salarieh, B.** — *Electromagnetic Transient Modelling of Power
    Transmission Line Tower and Tower-Footing Grounding System*, M.Sc.
    thesis, University of Manitoba, Winnipeg, 2019. Full-wave
    frequency-domain models of tower plus footing reduced to time-domain
    macro-models compatible with EMT simulators (PSCAD/EMTDC), aimed at
    backflashover prediction.

## Applications and diagnostics

28. **Alipio, R.; Duarte, N.; De Conti, A.** — "Revisiting the Influence of
    Dispersive Characteristics of Soil Electrical Parameters on Transient
    Behavior of Underground Cables", *XVI IPST*, Guadalajara, Mexico,
    Jun. 2025. Dispersive soil matters beyond grounding: for buried cables the
    effect is pronounced for ρ > 1000 Ω·m and short sections — further
    evidence for making dispersive soil the default in transient studies.
29. **Alam, A. K. M. M.; Kandic, M.; Bridges, G. E.** — "Single-Wire Time
    Domain Reflectometry Technique (SW-TDR): Detecting Faults in Power System
    Grounding Electrodes", *IEEE Access*, vol. 12, Oct. 2024.
    DOI: 10.1109/ACCESS.2024.3474478. Surface-wave TDR on vertical rods for
    corrosion/break detection; needs ~200 MHz bandwidth, i.e. full-wave
    territory well above HEM validity — cited to delimit the model's scope,
    not as a target application.

## Legacy implementation sources

Papers cited inside the legacy TUPÃ code (see "Related implementation notes"
below) as the sources of its dispersive-soil routines:

30. **Portela, C.** — "Statistical Aspects of Soil Electromagnetic Behavior in
    Frequency Domain", *Ground'2000 — International Conference on Grounding
    and Earthing*, Belo Horizonte, Brazil, June 2000. Cited by the legacy
    Matlab soil routine as the source of the power-law model and its
    parameter ranges (σ₀ = 50 µS/m–17 mS/m, α = 0.6–0.8,
    kr = 0.002–0.9 µS/m·s^α); the model behind `tPortelaSoil`.
31. **Lima, A. C. S.; Portela, C.** — "Inclusion of Frequency-Dependent Soil
    Parameters in Transmission-Line Modeling", *IEEE Trans. Power Delivery*,
    vol. 22, no. 1, pp. 492–499, Jan. 2007. The same power-law family
    referenced to ω₀ = 2π·1 MHz (with cot(πα/2) on the real parcel); the
    second dispersive-soil routine of the legacy Matlab code.

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

- Two private legacy TUPÃ implementations exist, re-inspected side by side in
  July 2026 (both outside this repository):
  - the **original Matlab code** (`mom_matlab`) — the implementation behind
    the dissertation [3] and, since July 2026, the **model reference of
    record**: where the two legacy codes disagree, the Matlab decides. It
    already contains the frequency-dependent image reflection coefficient
    Γ(ω) (ideal images are a runtime switch), all three solver layouts of
    theory.md §6 (reduced nodal, augmented, and a TAGS-style symmetric block
    system) as switchable methods, solid *and* tubular Bessel internal
    impedances, two dispersive-soil routines ([30] and [31]), and
    field/soil-potential/touch-voltage outputs. Its methodology notes
    (LaTeX + Mathematica impedance derivations) are the working derivations
    that [theory.md](theory.md) consolidates.
  - the **C++/Fortran hybrid port** (`mom_cpp`) — same physics kernel ported
    from the Matlab (plus XML input, HTML reports, bundle/L-profile internal
    impedances and a shielded-wire segment), but it dropped Γ(ω), keeping
    only the ideal-image limits. The user manual (WIP) lives with it.
  - known shared defect: the legacy self geometry factor (Matlab, ported
    verbatim to C++) is half the correct value and carries a `1`-for-`l`
    typo in its log argument — see theory.md §4.2 and implementation plan
    gap 8. The model itself is fully specified by the public references
    above.
- TUPÃ's geometry-factor separation (theory.md §4.1, from [3], 2003) and the
  mHEM [11] are the same optimisation, arrived at independently; TAGS's
  closed-form self integral is identical to theory.md §4.2's `g_self` — a
  useful mutual confirmation of both codes.
