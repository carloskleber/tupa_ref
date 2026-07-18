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
   Thesis-length origin and derivation: Soares [55].

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

## Foundations and neighbouring methods

32. **Portela, C.** — *Campos e Ondas — Problemas*, course problem collection
    (in Portuguese), ed. 02/1997. Works out, as solved problems, the building
    blocks later applied in [1] and [3]: mean-potential self and mutual
    resistances of parallel and of orthogonal cylindrical conductors in a
    homogeneous medium, the buried horizontal conductor with its interface
    image (including the equivalent-radius construction √(2h·r₀)), and the
    error analysis of the quasi-static approximation for the elementary
    dipole. The course-text ancestry of the closed-form geometry factors of
    theory.md §4.2.
33. **Li, Z.-X.; Chen, W.-J.; Wang, K.-C.** — "Frequency Domain and Time
    Domain Analysis of the Transient Behavior of Buried Grounding Grids in
    Horizontal Multilayered Earth Model", *Electrical Engineering*, vol. 104,
    pp. 2515–2529, 2022. DOI: 10.1007/s00202-022-01502-x. Hybrid (HEM-class)
    model of grounding grids in horizontally stratified soil: layered-earth
    Green's functions made affordable by the quasi-static complex image
    method (matrix-pencil fitted), frequency- and time-domain drivers, and
    soil ionisation represented by conductor-radius adjustment. The reference
    route should stratified soil ever be needed — TUPÃ assumes a uniform
    half-space (theory.md §5).
34. **Baba, Y.; Rakov, V. A.** — "Applications of Electromagnetic Models of
    the Lightning Return Stroke", *IEEE Trans. Power Delivery*, vol. 23,
    no. 2, pp. 800–811, Apr. 2008. DOI: 10.1109/TPWRD.2007.916169. Review of
    full-wave return-stroke models (channel representations, excitation
    methods, MoM/FDTD numerics). Places the HEM between electromagnetic and
    distributed-circuit models — non-TEM near fields, but electric and
    magnetic couplings handled as decoupled circuit quantities — and reports
    HEM channel currents consistent with full electromagnetic solutions.
35. **Poljak, D.; Doric, V.** — "Wire Antenna Model for Transient Analysis of
    Simple Grounding Systems, Part I: The Vertical Grounding Electrode",
    *Progress In Electromagnetics Research*, vol. 64, pp. 149–166, 2006
    (Part II, the horizontal electrode, pp. 167–189). Antenna-theory route:
    frequency-domain Pocklington integro-differential equation solved by a
    Galerkin–Bubnov boundary-element scheme, with the air–soil interface
    entering the kernel through a Fresnel reflection coefficient — a cheap
    middle ground between quasi-static images and rigorous Sommerfeld
    integrals.
36. **Silva, G. C. P.; Faria, F. A. C.; Moura, R. A. R.; Schroeder,
    M. A. O.** — "Comparação entre os Métodos PEEC e HEM na Modelagem
    Eletromagnética de Aterramentos Elétricos", *XVII Simpósio Brasileiro de
    Automação Inteligente (SBAI)*, 2025 (in Portuguese). Head-to-head PEEC vs
    HEM on a 60 m buried electrode with dispersive soil [14]: harmonic
    impedance agrees to MAPE below 0.01 %, GPR within a few percent. HEM's
    unified segment discretisation plus geometric-symmetry reuse cut matrix
    assembly by ~400× relative to both plain HEM and PEEC.

## Excitation waveforms

37. **Heidler, F.** — "Traveling Current Source Model for LEMP Calculation",
    *Proc. 6th Int. Symp. on Electromagnetic Compatibility*, Zurich, 1985,
    pp. 157–162. Origin of the Heidler function
    i(t) = (I₀/η)·(t/τ₁)ⁿ/(1 + (t/τ₁)ⁿ)·e^(−t/τ₂) with the analytic
    peak-correction factor η — the standard analytical lightning-current
    waveform (zero initial slope, independently adjustable front and tail).
    Implemented as `mSignal`'s parametrised `tHeidlerSignal` construction
    (ROADMAP Phase 7).
38. **De Conti, A.; Visacro, S.** — "Analytical Representation of Single- and
    Double-Peaked Lightning Current Waveforms", *IEEE Trans. Electromagnetic
    Compatibility*, vol. 49, no. 2, pp. 448–451, May 2007.
    DOI: 10.1109/TEMC.2007.897153. Sums of Heidler functions [37] (same η
    correction) fitted to median first- and subsequent-stroke parameters
    measured at Morro do Cachimbo and Mount San Salvatore, single- and
    double-peaked. Identifies the source of `newHeidlerSignal`'s legacy
    6-term set: its `i0`/`n`/`tau1`/`tau2` values are this paper's Table I,
    column MCS_FST#1 (the single-peaked Morro do Cachimbo first-stroke fit)
    exactly — previously ported without a recorded source. The paper's other tables (the
    double-peaked 7-term MCS/MSS first-stroke variants and the 2-term
    MCS/MSS subsequent-stroke fits) are further citable presets not yet
    exposed via `newHeidlerSignalTerms`.
39. **IEC 62305-1** — *Protection against lightning — Part 1: General
    principles*, IEC, ed. 2, 2010. Annex B expresses the standardised first
    (10/350 µs) and subsequent (0.25/100 µs) stroke currents as single-term
    Heidler functions [37] with tabulated I₀, k (= η), τ₁, τ₂ and n = 10 per
    lightning protection level — the citable parameter sets for
    `newHeidlerSignalTerms`.

## Conductor, cable and safety references (Phase 7 elements)

40. **Schelkunoff, S. A.** — "The Electromagnetic Theory of Coaxial
    Transmission Lines and Cylindrical Shields", *Bell System Technical
    Journal*, vol. 13, no. 4, pp. 532–579, 1934. Canonical surface-impedance
    formulas for solid and tubular cylindrical conductors (modified Bessel
    I/K forms) — the tubular internal impedance the legacy `zinterna.m`
    implements and theory.md §4.3 states (ROADMAP Phase 7 tubular
    conductor / metallic pipes).
41. **Sunde, E. D.** — *Earth Conduction Effects in Transmission Systems*,
    Van Nostrand, New York, 1949 (Dover reprint 1968). The classical
    treatise on buried-conductor impedance and admittance: grounding
    resistance formulas (theory.md §9.1), leakage admittance of **insulated
    (coated) buried wires** (coating admittance in series with soil
    leakage — the missing theory for the Phase 7 insulated conductor, left
    as a TODO placeholder in the legacy code), and layered-earth effects
    (background for the multi-layer soil item).
42. **IEEE Std 80-2013** — *IEEE Guide for Safety in AC Substation
    Grounding*. Normative definitions of touch, step and mesh voltages
    (1 m reach / 1 m step conventions, body-current limits, surface-layer
    derating). Reference for the Phase 7 GPR/touch/step outputs — the
    legacy computes a geometric variant (max surface-to-node potential
    difference on a 1 m circle) without the body-circuit factors.
43. **Ametani, A.** — "A General Formulation of Impedance and Admittance of
    Cables", *IEEE Trans. Power Apparatus and Systems*, vol. PAS-99, no. 3,
    pp. 902–910, 1980. Per-unit-length series-impedance/shunt-admittance
    matrices of multiconductor cables (the EMTP "Cable Constants" route) —
    the natural internal representation for the Phase 7 multipolar-cable
    element; complements Wedepohl & Wilcox and Schelkunoff [40] layer
    formulas.

## Return-stroke channel and induced-voltage modelling

44. **Baba, Y.; Rakov, V. A.** — "Electromagnetic Models of the Lightning
    Return Stroke", *Journal of Geophysical Research*, vol. 112, D04102,
    2007. DOI: 10.1029/2006JD007222. The classification review that [34]
    applies: four channel representations (perfectly conducting/resistive
    wire; wire embedded in a dielectric; wire loaded by distributed
    inductance or capacitance) crossed with three excitation methods, solved
    by MoM (time or frequency domain) or FDTD; its appendix checks HEM
    channel-current distributions against FDTD. The primary design reference
    for the planned lightning-channel element: distributed series loading is
    the catalogued technique for slowing channel propagation from c to a
    prescribed return-stroke speed (ROADMAP Phase 7).
45. **Silveira, F. H.** — *Modelagem para Cálculo de Tensões Induzidas por
    Descargas Atmosféricas*, D.Sc. thesis, PPGEE/UFMG, Belo Horizonte, 2006
    (in Portuguese). Book-length HEM application to lightning-*induced*
    voltages: return-stroke current distribution and channel–line
    electromagnetic coupling solved in one integrated model, channel
    corona/core-loss representation, and the lossy-ground effect on the
    coupling via Norton's approximation. Documents that the machinery TUPÃ
    implements extends from direct strikes to induced-voltage problems.
46. **Silveira, F. H.; Visacro, S.; Herrera, J.; Torres, H.** — "Evaluation
    of Lightning-Induced Voltages Over a Lossy Ground by the Hybrid
    Electromagnetic Model", *IEEE Trans. Electromagnetic Compatibility*,
    vol. 51, no. 1, Feb. 2009. DOI: 10.1109/TEMC.2008.2010403. Condensed
    journal version of [45] Ch. 6: Norton's closed-form field expressions
    replace the Sommerfeld integrals for the lossy-ground channel–line
    coupling inside the HEM; validated against Ishii's reduced-scale
    experiments and NEC-4.

## Time-domain HEM origin and direct time-domain alternatives

47. **Silva, B. P.** — *Novo Modelo Eletromagnético no Domínio do Tempo para
    Cálculo da Resposta de Sistemas Elétricos Frente a Descargas
    Atmosféricas*, D.Sc. thesis nº 359, PPGEE/UFMG, Belo Horizonte, 2021 (in
    Portuguese). Origin of the HEM-TD that [21] refines: full time-domain
    reformulation of the HEM, built to host the nonlinear phenomena the
    frequency domain excludes — soil ionisation, corona, surge arresters,
    impedance matching — validated against the frequency-domain HEM on rods,
    electrodes and grids and applied to line backflashover analysis.
    (Journal version: IEEE Trans. Power Delivery, 2022.)
48. **Boukhouna, M.; Nekhoul, B.; Khelifi, B.** — "Time Domain Modeling of
    Lightning Transients in Grounding Systems Considering Frequency
    Dependence and Soil Ionization", *Electric Power Systems Research*,
    2024. Transmission-line-theory route solved directly in time:
    frequency-dependent per-unit-length parameters and dispersive soil
    folded in by vector fitting, 1-D FDTD over the wire mesh, topological
    network assembly scaling from single electrodes to wind-farm grounding
    circuits, soil ionisation included; verified against Menter/EMTP,
    FEKO-MoM and Alipio results.

## Discretisation refinements

49. **Silva, G. C. P.** — *Modelagem Eletromagnética de Aterramentos
    Elétricos: Uso de Funções Base de Ordens Superiores para Representação
    das Distribuições de Correntes Elétricas em Eletrodos*, M.Sc.
    dissertation, PPGEL (UFSJ/CEFET-MG), São João del-Rei, c. 2024 (in
    Portuguese). The dissertation behind [36]: derives PEEC and HEM from the
    same MoM/Galerkin root (HEM as the thin-wire simplification of PEEC),
    then evaluates higher-order basis functions (piecewise linear,
    sinusoidal) against the pulse basis TUPÃ uses — more accurate per
    segment but costlier; geometric-symmetry assembly rules recover the
    efficiency. §5.3 also runs a segmentation-limits study for the pulse
    basis itself (same one TUPÃ uses): "10r" segment length as converged
    reference vs. coarser λ-fraction discretizations, on the same
    electrode/Alipio-Visacro-soil family as [36]'s Fig. 3/4 — coarser
    discretization systematically underestimates |Z(ω)| and GPR, used in
    silva2025-fig3.md's mid-band-knee discussion as indirect corroboration
    for the segment-count candidate cause. Otherwise relevant only if
    pulse-basis accuracy ever becomes the binding constraint (finer
    segmentation per [19] is the current answer).

## Line-level (EMT) context: tower models, coupling, LEMP

50. **Baba, Y.; Ishii, M.** — "Numerical Electromagnetic Field Analysis on
    Lightning Surge Response of Tower with Shield Wire", *IEEE Trans. Power
    Delivery*, vol. 15, no. 3, Jul. 2000. NEC-2 (frequency-domain MoM + FFT)
    study of a shield-wired tower under fast-front currents: the tower
    behaves as an antenna until the travelling wave completes several round
    trips — the apparent footing impedance initially exceeds the DC footing
    resistance, and shield-wire/phase-conductor coupling sits well below its
    TEM value — then proposes revised multistory-tower-model parameters for
    EMTP. Full-wave background for what EMT tower models compress.
51. **Stracqualursi, E.; Araneo, R.; Andreotti, A.; Brandão Faria, J.;
    Silveira, F. H.; Visacro, S.** — "Effects of Macromodeling on the
    Simulation of Transient Events Caused by Direct Lightning to Overhead
    Power Lines", *Electric Power Systems Research*, 2025. Electromagnetic
    model used to test what macromodeling discards: mutual coupling among
    tower, grounding electrodes and shield wires shifts injection-node
    overvoltages only marginally — less than typical soil-parameter
    uncertainty — so the component-wise (macromodel) decomposition of EMT
    chains is sound; what *is* essential is a frequency-dependent
    representation of the grounding system. Direct support for the FDNE
    consumption route [26,27].
52. **Yamanaka, A.; Ishimoto, K.; Tatematsu, A.** — "Incorporating the LEMP
    Impact on Lightning Surge Analysis of Transmission Lines in EMT
    Simulators", *IEEE Trans. Power Delivery*, vol. 39, 2024.
    DOI: 10.1109/TPWRD.2024.3382316. Adds return-stroke LEMP field-to-line
    coupling (Agrawal model, Cooray–Rubinstein field correction) to standard
    EMT tower/line models: the LEMP-induced component opposes the
    current-generated voltages in polarity and *raises* insulator voltages —
    conventional EMT underestimates them by up to ~58 %; the corrected
    method stays within ~10 % of 3-D FDTD across 77–500 kV lines and
    ρ = 0–5000 Ω·m. Quantifies the channel-coupling error HEM-class tools
    inherit when the channel is left unmodelled (cf. [25]); TUPÃ's planned
    channel element addresses exactly this term.
53. **Tatematsu, A.; Yamanaka, A.** — "Three-Dimensional FDTD-Based
    Simulation of Lightning-Induced Surges in Secondary Circuits With
    Shielded Control Cables Over Grounding Grids in Substations", *IEEE
    Trans. Electromagnetic Compatibility*, vol. 65, no. 2, Apr. 2023.
    DOI: 10.1109/TEMC.2023.3245206. Full-wave FDTD (CRIEPI's VSTL REV) with
    a vector-fitted surface transfer impedance for tape-shielded control
    cables over substation grounding grids, validated on an instrumented
    substation test platform. Illustrates the full-wave route for
    grid-plus-cable problems beyond thin-wire HEM scope.
54. **Silva, G. C. P.; Schroeder, M. A. O.; Moura, R. A. R.; Assis, F. A.;
    Lima, A. C. S.** — "Desempenho de Linhas de Transmissão frente às
    Descargas Atmosféricas Considerando como Fatores de Sensibilidade as
    Modelagens das Torres e Correlações Estatísticas entre Parâmetros das
    Ondas de Corrente", *XXV Congresso Brasileiro de Automática (CBA)*,
    2024 (in Portuguese). DOI: 10.20906/CBA2024/4852. ATP sensitivity study
    on a real 138 kV line: seven tower models × five soil resistivities ×
    with/without statistical correlation among current-waveform parameters
    (Morro do Cachimbo data) move insulator overvoltages and outage rates by
    up to ~70 % — the downstream modelling choices that consume TUPÃ-class
    grounding/tower results dominate the final numbers.

## HEM origin

55. **Soares, A., Jr.** — *Modelagem de Linhas de Transmissão para Avaliação
    de Desempenho Frente a Descargas Atmosféricas*, D.Sc. thesis nº 014,
    PPGEE/UFMG, Belo Horizonte, Mar. 2001 (advisor S. Visacro; C. Portela on
    the examining committee; in Portuguese). The thesis where the HEM was
    assembled — [5] is its published condensate. Integrated per-frequency
    electromagnetic coupling of tower, grounding and overhead cables as
    cylindrical segments (the §4.3 numbered solution procedure is
    essentially TUPÃ's per-frequency loop), frequency-dependent soil, FFT
    time domain, impedance matching at truncated conductors; validated
    against experimental results, then applied to a large parametric study
    of insulator-string overvoltages — current waveshape and front time,
    injection point, channel position, tower/grounding configuration, soil
    resistivity, from a single grounded tower up to three towers with spans.
    Also carries a substantial review of incidence models (electrogeometric
    model variants) and of the CIGRE/FLASH line-performance methods that
    consume this class of transient results.

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
    typo in its log argument — see theory.md §4.2 and ROADMAP
    gap 8. The model itself is fully specified by the public references
    above.
- TUPÃ's geometry-factor separation (theory.md §4.1, from [3], 2003) and the
  mHEM [11] are the same optimisation, arrived at independently; TAGS's
  closed-form self integral is identical to theory.md §4.2's `g_self` — a
  useful mutual confirmation of both codes.
