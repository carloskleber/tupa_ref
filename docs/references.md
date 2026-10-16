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
    validation anchors. The same book is listed again as [41], where its
    insulated-wire chapter is the point; a merge would renumber everything
    after [10], so both entries stay and theory.md §9.1 cites both.

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
    (ROADMAP Phase 7 item 1).
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

## Conductor, cable and safety references (ROADMAP Phases 11–13)

40. **Schelkunoff, S. A.** — "The Electromagnetic Theory of Coaxial
    Transmission Lines and Cylindrical Shields", *Bell System Technical
    Journal*, vol. 13, no. 4, pp. 532–579, 1934. Canonical surface-impedance
    formulas for solid and tubular cylindrical conductors (modified Bessel
    I/K forms) — the tubular internal impedance the legacy `zinterna.m`
    implements and theory.md §4.3 states (ROADMAP Phase 12 item 2 tubular
    conductor / metallic pipes).
41. **Sunde, E. D.** — *Earth Conduction Effects in Transmission Systems*,
    Van Nostrand, New York, 1949 (Dover reprint 1968). The classical
    treatise on buried-conductor impedance and admittance: grounding
    resistance formulas (theory.md §9.1), leakage admittance of **insulated
    (coated) buried wires** (coating admittance in series with soil
    leakage — the missing theory for the ROADMAP Phase 13 item 3 insulated conductor, left
    as a TODO placeholder in the legacy code), and layered-earth effects
    (background for the multi-layer soil item).
42. **IEEE Std 80-2013** — *IEEE Guide for Safety in AC Substation
    Grounding*. Normative definitions of touch, step and mesh voltages
    (1 m reach / 1 m step conventions, body-current limits, surface-layer
    derating). Reference for the ROADMAP Phase 11 GPR/touch/step outputs — the
    legacy computes a geometric variant (max surface-to-node potential
    difference on a 1 m circle) without the body-circuit factors.
43. **Ametani, A.** — "A General Formulation of Impedance and Admittance of
    Cables", *IEEE Trans. Power Apparatus and Systems*, vol. PAS-99, no. 3,
    pp. 902–910, 1980. Per-unit-length series-impedance/shunt-admittance
    matrices of multiconductor cables (the EMTP "Cable Constants" route) —
    the natural internal representation for the ROADMAP Phase 13 item 4 multipolar-cable
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
    for the lightning-channel element (implemented): distributed series loading is
    the catalogued technique for slowing channel propagation from c to a
    prescribed return-stroke speed (ROADMAP Phase 10b).
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
    inherit when the channel is left unmodelled (cf. [25]); TUPÃ's
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

## Model validity domains

56. **Grcev, L.; Arnautovski-Toseva, V.** — "Grounding Systems Modeling for
    High Frequencies and Transients: Some Fundamental Considerations", *IEEE
    Bologna Power Tech*, Bologna, Italy, Jun. 2003. The fundamental statement
    of what bounds every model in this class: the upper frequency of interest
    is set by the frequency content of the *response*, not only of the
    excitation (their 6 m rod example needs 8–16 MHz bandwidth depending on
    soil resistivity); the quasi-static approximation requires system
    dimensions below ~λ/10 measured in the soil at that frequency; and toward
    MHz frequencies the voltage to ground becomes path-dependent, so the
    "impedance to ground" itself loses unique definition. Catalogues the
    simplification ladder — electrostatic images, modified images
    ($\Gamma = (k_1^2 - k_2^2)/(k_1^2 + k_2^2)$, the coefficient the legacy
    Matlab TUPÃ implements, theory.md §5), quasi-static kernels, decoupled
    electric/magnetic couplings, static-analogy parameters — separating
    HEM-class models from the full-wave Sommerfeld reference, and shows
    circuit models overestimating grid voltages away from the injection
    point.

## Nonuniform-line modelling

57. **Liu, Y.; Theethayi, N.; Thottappillil, R.** — "An Engineering Model for
    Transient Analysis of Grounding System Under Lightning Strikes:
    Nonuniform Transmission-Line Approach", *IEEE Trans. Power Delivery*,
    vol. 20, no. 2, pp. 722–730, Apr. 2005. DOI: 10.1109/TPWRD.2004.843437.
    Origin of the nonuniform-TL route of theory.md §10.1: per-unit-length
    parameters made space- and time-dependent by summing the quasi-static
    couplings (image method included) over all segments, then Telegrapher's
    equations solved by 1-D FDTD. Predicts both the injection-point voltage
    and the effective length where uniform-TL parametrisations fail at one or
    the other; checked against circuit and electromagnetic references on
    horizontal wires and grids.
58. **Moura, R. A. R.** — *Representação de Linha de Transmissão com
    Geometria Não Uniforme para Estudos de Sobretensões Atmosféricas*, D.Sc.
    thesis, COPPE/UFRJ, Rio de Janeiro, Apr. 2018 (advisors A. C. S. Lima
    and M. A. O. Schroeder; in Portuguese). Nonuniform *overhead* spans —
    wide river crossings, tall towers: cascaded uniform-line quadripoles
    discard the electromagnetic coupling between segments and can go
    numerically unstable for tall towers, while the HEM applied directly to
    the catenary (same mean-distance separation as theory.md §4.1, reduced
    to closed-form single integrals) represents the nonuniform geometry
    natively; vector-fitting rational models with RLC synthesis then carry
    the result into EMT programs. Also finds grounding influences tall-tower
    overvoltages less than for conventional spans. Appendix B catalogues
    power-series and Padé closed forms for the grounding integrals — the
    precursors of sHEM [59].

## Closed-form HEM acceleration

59. **Moura, R. A. R.; Schroeder, M. A. O.; Lima, A. C. S.; Vieira,
    P. H. N.; Alipio, R.** — "Closed-Form Approximation for Grounding Grids
    Transient Analysis", *Journal of Control, Automation and Electrical
    Systems*, vol. 32, 2021. DOI: 10.1007/s40313-021-00708-x. sHEM
    (series-HEM): truncates $\exp(-\gamma r)/r$ to its two-term MacLaurin
    series $1/r - \gamma$, making both HEM double integrals fully
    closed-form for parallel and orthogonal segments — frequency-independent
    "geometrical" terms plus one $\gamma$-proportional term, no numerical
    integration at all. Up to ~2000× faster than the plain HEM on grounding
    grids; the error grows with soil resistivity and frequency (worst-case
    ~10–20 % on harmonic impedance above ~100 kHz for 1000–2000 Ω·m soils,
    largely washed out in time-domain responses); validated against grid
    measurements. The one-term variant (dropping $\gamma$) is markedly less
    accurate. Sibling of the mean-exponential separation [11]/theory.md
    §4.1, trading bandwidth for speed.

## Early full-wave and finite-element grounding models

60. **Papalexopoulos, A. D.; Meliopoulos, A. P.** — "Frequency Dependent
    Characteristics of Grounding Systems", *IEEE/PES 1986 Transmission and
    Distribution Conference*, Anaheim, USA, Sep. 1986, Paper 36 T&D 597-9.
    Finite-element/transmission-line grounding model predating HEM: segments
    treated as lossy TL elements (distributed R, L, C, mutual coupling to
    every other segment) from a quasi-static Maxwell solve, characterised by
    frequency-dependent Voltage Distribution Functions; validated against BPA
    field measurements on large tower footings. Early frequency-domain
    lineage of the circuit/TL family in theory.md §10.1.
61. **Grcev, L.; Dawalibi, F.** — "An Electromagnetic Model for Transients in
    Grounding Systems", *IEEE/PES 1990 Winter Meeting*, Atlanta, USA,
    Feb. 1990, Paper 90 WM 130-5 PWRD. The earliest of Grcev's papers in this
    library: thin-wire segment ensemble solved by electromagnetic field
    theory in the frequency domain, converted to the time domain by FFT —
    establishes the modified-image-theory route that [56] later classifies
    and that TUPÃ's $\Gamma(\omega)$ coefficient (theory.md §5) implements.
62. **Grcev, L.** — "Computer Analysis of Transient Voltages in Large
    Grounding Systems", *IEEE/PES 1995 Summer Meeting*, Portland, USA,
    Jul. 1995, Paper 95 SM 363-2 PWRD. Extends [61] to large grounding grids
    with the same modified-image-theory approach; validated against EDF field
    measurements.
63. **Grcev, L.** — "Impulse Efficiency of Ground Electrodes", *IEEE Trans.
    Power Delivery*, vol. 24, no. 1, pp. 441–451, Jan. 2009.
    DOI: 10.1109/TPWRD.2008.923396. Full-wave analysis of fast-fronted
    lightning-current pulses on ground electrodes; empirical
    impulse-efficiency (effective-length) formulas for slow and very-fast
    fronts, soil ionisation disregarded (conservative upper bound on impulse
    impedance).
64. **Heimbach, M.; Grcev, L.** — "Grounding System Analysis in Transients
    Programs Applying Electromagnetic Field Approach", *IEEE/PES 1996 Summer
    Meeting*, Denver, USA, Jul.–Aug. 1996, Paper 96 SM 392-1 PWRD. Rational
    function representation of frequency-dependent grounding impedance for
    direct incorporation into EMTP (arbitrary feeding points, mutual coupling
    retained) — precedes by ~25–30 years the FDNE/rational-model EMT-export
    route of [26,27]. A distinct paper from the same author pair's later
    journal article [18] despite the similar author order and topic.

## Excitation waveforms (addendum)

65. **Jones, R. D.** — "On the Use of Tailored Return-Stroke Current
    Representations to Simplify the Analysis of Lightning Effects on
    Systems", *IEEE Trans. Electromagnetic Compatibility*, vol. EMC-19,
    no. 2, pp. 95–96, May 1977. The source the legacy
    `sinais.DuplaExpJones` cites for its zero-initial-slope variant of the
    double-exponential surge (front exponential replaced by
    $e^{-(\alpha t)^2}$), ported as `mSignal`'s `tDoubleExpSignal` with
    `jones = .true.` (theory.md §8). Bibliographic details to be checked
    against the paper (added 2026-09-30 from the code comment's
    "R.D. Jones 1977").

## Portela-school lineage

66. **Salari Filho, J. C.** — *Efeito das Descargas Atmosféricas no
    Desempenho de Linhas de Transmissão — Modelagens nos Domínios do Tempo e
    da Freqüência*, D.Sc. thesis, COPPE/UFRJ, Rio de Janeiro, Dec. 2006
    (advisor C. Portela; in Portuguese). The thesis behind [4]: cylindrical
    electrodes in the Portela formulation (it names [3] among the works
    derived from it) combined with lumped linear and nonlinear circuit
    elements (RLC, transformers, switches, arresters) in one hybrid
    frequency–time program, plus corona on overhead cables, soil
    ionisation and a lightning-channel model, applied to grounding systems
    and 138–500 kV lines. Three parts matter for TUPÃ. §5.3 tabulates
    parameter sets for the Lima–Portela soil form at $\omega_0 = 2\pi\cdot$
    1 MHz, valid for 100 µS/m < σ₀ < 10 mS/m: median α ≈ 0.706,
    Δᵢ ≈ 11.71 mS/m, and two "reasonably safe" pairs (0.806, 9.23 mS/m;
    0.856, 7.91 mS/m). α and Δᵢ are treated as independent,
    Weibull-distributed variables (after Portela & Tavares, 2001).
    Appendix B derives the Fresnel reflection/transmission coefficients at
    the plane interface and their quasi-static limits, and uses them for
    cross-medium scalar potentials.
67. **Portela, C.** — *Sobretensões e Coordenação de Isolamento*, course
    text in three volumes, COPPE/UFRJ, 1982 (in Portuguese). Vol. II (ch. 8,
    lightning) introduces the front law
    $i = I\,(e^{\alpha t/T} - 1)/(e^{\alpha} - 1)$ that the legacy
    `impulso.m` implements. α is positive for first negative downward
    strokes and negative for subsequent strokes, so the legacy "concave"
    waveform also covers convex fronts. Vol. II also works through
    tower-top, mid-span and nearby-ground strokes with critical-current
    tables. Vol. III (ch. 9) sets out insulation coordination as a
    stress–strength risk integral.

## Frequency-dependent soil models (addendum)

68. **Schroeder, M. A. O.; Correia de Barros, M. T.; Lima, A. C. S.;
    Afonso, M. M.; Moura, R. A. R.** — "Evaluation of the Impact of
    Different Frequency Dependent Soil Models on Lightning Overvoltages",
    *Electric Power Systems Research*, 2017 (article in press).
    DOI: 10.1016/j.epsr.2017.09.020. Runs Portela (median set of [66]),
    Alipio–Visacro [13] and Longmire–Smith [15] soils through one chain:
    HEM-class tower-footing impedance, vector fitting [69], ATP. Portela's
    form disperses most. It cuts impulse impedance by up to ~80 %
    (4000 Ω·m, subsequent strokes), against 10–66 % for first strokes. GPR
    is far more sensitive to the soil model than insulator overvoltages
    are. All three models pass a Kramers–Kronig causality check. The paper
    prints Δᵢ as "11.71 S/m", but [66] gives mS/m.

## Rational macromodeling and EMT interchange

69. **Gustavsen, B.; Semlyen, A.** — "Rational Approximation of Frequency
    Domain Responses by Vector Fitting", *IEEE Trans. Power Delivery*,
    vol. 14, no. 3, pp. 1052–1061, Jul. 1999. DOI: 10.1109/61.772353.
    Vector fitting: iterative pole relocation from starting poles (complex
    ones for resonant responses) to a pole–residue model of sampled
    frequency data. This is the fitting step that FDNE export needs
    [26,64], and the one [48] and [68] rely on.
70. **Grivet-Talocia, S.; Gustavsen, B.** — *Passive Macromodeling: Theory
    and Applications*, Wiley, 2016. ISBN 978-1-118-09491-4. Textbook
    treatment of the rest of that route: causality and passivity
    definitions, passivity assessment and enforcement, model-order
    reduction, and circuit synthesis for time-domain solvers.
71. **IBIS Open Forum** — *Touchstone® File Format Specification*,
    version 2.1, ratified Jan. 2024. ASCII n-port network data (S, Y or Z
    parameters per frequency, any port count). The standard interchange
    file for sampled frequency responses, read by vector-fitting tools and
    circuit simulators.

## Numerical electromagnetic analysis guidelines

72. **CIGRE WG C4.501 (convenor A. Ametani)** — *Guideline for Numerical
    Electromagnetic Analysis Method and its Application to Surge
    Phenomena*, Technical Brochure 543, June 2013. Sets out where EMTP-type
    circuit/TL modelling fails (non-TEM propagation, tower and footing
    frequency dependence, grounding electrodes) and surveys the full-wave
    methods (FDTD, TLM, FEM-TD, MoM, PEEC), with benchmark cases for
    towers, grounding rods and grids, substations and induced surges. Its
    conclusion describes TUPÃ's role: use numerical EM analysis to compute
    the impedances that circuit-theory tools need but lack, and as
    reference cases to test the faster TL-based codes.

## Conductor internal impedance (addendum)

73. **de Arizon, P.; Dommel, H. W.** — "Computation of Cable Impedances
    Based on Subdivision of Conductors", *IEEE/PES 1986 Summer Meeting*,
    Mexico City, Jul. 1986, Paper 86 SM 399-0. Frequency-dependent R and L
    of arbitrary cross-sections, found by splitting conductors into
    circular, square or elemental subconductors. Checked against the exact
    coaxial solution, then applied to pipe-type cables with magnetic pipes
    and to stranded conductors in the power-line-carrier range. This is the
    numerical route where analytic formulas [40,43] need axial symmetry.

## Lightning return-stroke channel models

74. **Cooray, V. (ed.)** — *Lightning Electromagnetics*, IET Power and
    Energy Series 62, IET, London, 2012. ISBN 978-1-84919-215-6.
    DOI: 10.1049/PBPO062E (chapters 10.1049/pbpo062e_ch1–_ch25). The design
    reference for theory.md §4.5. Three chapters matter here.
    Ch. 8 (Baba & Rakov) is the book-length update of [44]. It classifies
    channels into six representations, gives the TEM speed and impedance
    estimates for loaded wires, and compares current- against
    voltage-source excitation. It also checks time-domain MoM, NEC-2 and
    FDTD against Chen's analytic solution for a vertical wire.
    Ch. 9 (Moini & Sadeghi) covers antenna models: closed-form
    height-dependent inductive loading, the antenna-mode and TL-mode split
    of the loaded-channel current, and modified-image reflection and
    transmission factors for a lossy ground.
    Ch. 10 (De Conti, Silveira & Visacro) covers transmission-line channel
    models: logarithmic $L(z)$ and $C(z)$, corona and nonlinear resistance.
    Read with care: the extraction turns some μs into ms and some Ω into W.

## Lightning-induced voltages: LEMP fields and field-to-line coupling

The source set of theory.md §4.6. Equation errata found in these papers are
listed at the end of theory.md §4.6.5; the corrected forms are the ones used.

75. **Nucci, C. A.; Rachidi, F.; Ianoz, M. V.; Mazzetti, C.** — "Lightning-
    Induced Voltages on Overhead Lines", *IEEE Trans. Electromagnetic
    Compatibility*, vol. 35, no. 1, pp. 75–86, Feb. 1993. DOI:
    10.1109/15.249398. The original MTL + Agrawal procedure: the MTL
    return-stroke model ($\lambda$ = 1.7 km) with a two-Heidler subsequent-stroke
    current gives the field, and Agrawal's equations (finite differences) give
    the voltage on a 1 km, 10 m high matched line with the stroke 50 m from its
    centre. The primary validation source of theory.md §4.6.5: Tables III–IV
    (peak, steepness and times against current peak, $di/dt$, speed and
    return-stroke model) and the 500 m formulation test of its Fig. 14. The
    paper does not state the wire radius, and §IV-A prints 50 kA/µs for a
    current whose own tables and waveform give 40 kA/µs.
76. **Nucci, C. A. (CIGRE Working Group 33.01)** — "Lightning-Induced
    Voltages on Overhead Power Lines. Part I: Return-Stroke Current Models
    with Specified Channel-Base Current for the Evaluation of the
    Return-Stroke Electromagnetic Fields", *Electra*, no. 161, Aug. 1995,
    pp. 75–102. No DOI.
    Review of the BG, TL, MTL, TCS, MULS and DU models (with the proof that
    MULS and MTL coincide apart from the uniform current), the dipole field
    equations over perfect ground, the wave-tilt, Rubinstein and Cooray options
    over a lossy ground, and the comparison of models at 50 m, 5 km and 100 km.
    Its eq. (13) prints a wrong sign on the $E_r$ radiation term.
77. **Nucci, C. A. (CIGRE Task Force 33.01.01)** — "Lightning-Induced
    Voltages on Overhead Power Lines. Part II: Coupling Models for the
    Evaluation of the Induced Voltages", *Electra*, no. 162, Oct. 1995,
    pp. 121–145. No DOI. The Rusck, Chowdhuri–Gross and Agrawal–Price–Gurbaxani
    formulations and the Taylor and Rachidi equivalents. Agrawal ≡ Taylor ≡
    Rachidi within the transmission-line approximation; Rusck equals Agrawal for
    a vertical channel over perfect ground; Chowdhuri–Gross omits a magnetic
    source term. Its in-text citation numbers are off by one from its
    ref. 46 on.
78. **Rachidi, F.; Nucci, C. A.; Ianoz, M.; Mazzetti, C.** — "Influence of a
    Lossy Ground on Lightning-Induced Voltages on Overhead Lines", *IEEE
    Trans. Electromagnetic Compatibility*, vol. 38, no. 3, pp. 250–264, Aug.
    1996. DOI: 10.1109/15.536054; erratum vol. 39, no. 2, p. 187, 1997, DOI:
    10.1109/TEMC.1997.584944 (the erratum text is not held). The
    Cooray–Rubinstein horizontal-field formula, the ground impedance of Sunde,
    Carson and the complex-plane method, Timotin's transient ground resistance,
    and the finding that the ground changes shape and polarity of the induced
    voltage while the line-propagation effect can be dropped below a critical
    line length (≈ 1.5 km at 0.001 S/m). Three equations are wrong in print
    (eqs. 4, 7, 11; eq. 40 doubtful).
79. **Rachidi, F.; Nucci, C. A.; Ianoz, M.** — "Transient Analysis of
    Multiconductor Lines Above a Lossy Ground", *IEEE Trans. Power Delivery*,
    vol. 14, no. 1, pp. 294–302, Jan. 1999. DOI: 10.1109/61.736741. Sunde's
    logarithmic ground impedance extended to the mutual terms of a
    multiconductor line, Timotin's ground transient resistance extended to the
    off-diagonal terms, and the numerical treatment of the singularity of the
    convolution at $t = 0$. Eq. (24) is misprinted (prefactor and exponent).
80. **Master, M. J.; Uman, M. A.; Lin, Y. T.; Standler, R. B.** —
    "Calculations of Lightning Return Stroke Electric and Magnetic Fields
    Above Ground", *Journal of Geophysical Research*, vol. 86, no. C12,
    pp. 12127–12132, 1981. DOI: 10.1029/JC086iC12p12127. The MULS model and
    the first return-stroke fields at altitude (0–10 km, 20 m–10 km range,
    perfect ground, image dipole); source of the $E_r$/$E_z$ dipole equations
    of theory.md §4.6.1, with the correct sign of the $E_r$ radiation term.
81. **Uman, M. A.; McLain, D. K.** — "Magnetic Field of Lightning Return
    Stroke", *Journal of Geophysical Research*, vol. 74, no. 28,
    pp. 6899–6910, 1969. DOI: 10.1029/JC074i028p06899. Origin of the
    transmission-line (TL) return-stroke model: ground-level magnetic flux
    density for four first-stroke and three subsequent-stroke current models.

## Lightning-induced voltages (addendum): coupling origins, measurements, ground and terrain

Library additions of 2026-10-12 to 2026-10-15 that extend the §4.6 source set:
the original coupling derivation and its first field test, the coupling review,
measurements, and the effects (lossy and dispersive ground, corona, channel
inclination, grounding, terrain, sag) that §4.6.3–§4.6.4 list as outside the
perfect-ground anchor.

82. **Agrawal, A. K.; Price, H. J.; Gurbaxani, S. H.** — "Transient Response
    of Multiconductor Transmission Lines Excited by a Nonuniform
    Electromagnetic Field", *IEEE Trans. Electromagnetic Compatibility*,
    vol. EMC-22, no. 2, pp. 119–129, May 1980. DOI: 10.1109/TEMC.1980.303824.
    The original derivation of the coupling equations named after the first
    author, for an $(n+1)$-conductor line in a conducting medium (nuclear EMP;
    no lightning): the scattered-voltage pair, with the horizontal incident
    field as the only distributed source, and the equivalent but costlier
    total-voltage pair; a finite-difference scheme checked against analytic
    solutions. Printed errors: the sign of $G$ in eq. (31) (it must be
    $+(\sigma/\varepsilon)C$) and the last term of the difference eq. (41).
83. **Rubinstein, M.; Tzeng, A. Y.; Uman, M. A.; Medelius, P. J.; Thomson,
    E. M.** — "An Experimental Test of a Theory of Lightning-Induced Voltages
    on an Overhead Wire", *IEEE Trans. Electromagnetic Compatibility*,
    vol. 31, no. 4, pp. 376–383, Nov. 1989. DOI: 10.1109/15.43631. The first
    field test of the Agrawal model on a power line: a 450 m, 9.5 m high
    three-phase line at Kennedy Space Center (1985), strokes beyond 5 km,
    measured vertical field and a wave-tilt horizontal field. Waveshapes agree
    and the horizontal field drives the 2ℓ/c oscillation; amplitudes are
    uneven (first peak −37 % and +19 % in the two cases shown). Settles the
    test-line length at 450 m (§4.6.5 errata).
84. **Barker, P. P.; Short, T. A.; Eybert-Bérard, A. R.; Berlandis, J. P.** —
    "Induced Voltage Measurements on an Experimental Distribution Line During
    Nearby Rocket Triggered Lightning Flashes", *IEEE Trans. Power Delivery*,
    vol. 11, no. 2, pp. 980–995, Apr. 1996. DOI: 10.1109/61.489360.
    Triggered strokes 145 m from a 682 m, two-conductor test line (Camp
    Blanding): 63 strokes, induced peaks 8–100 kV for currents up to 44 kA,
    linear in the stroke current, about 63 % above Rusck's simplified formula
    but consistent with telegrapher-equation coupling driven by the measured
    fields. Short, spike-shaped waveforms (rise ≈ 1.5 µs). A full-scale data
    set for the induced-voltage problem.
85. **Rachidi, F.** — "A Review of Field-to-Transmission Line Coupling
    Models With Special Emphasis to Lightning-Induced Voltages on Overhead
    Lines", *IEEE Trans. Electromagnetic Compatibility*, vol. 54, no. 4,
    pp. 898–911, Aug. 2012. DOI: 10.1109/TEMC.2011.2181519. The modern
    theory reference for §4.6.2: the thin-wire integral equations reduced to
    the Agrawal pair under the TL approximation, the Taylor and Rachidi forms
    and their equivalence, ground and wire losses (Sunde), multiconductor
    matrices, BLT and time-domain solution. Shows that ground losses and
    corona attenuate direct-strike surges but can *raise* induced voltages.
    Printed errors: eq. (46) gives the mutual ground inductance with
    $\mu_0/2\pi$ (twice the correct $\mu_0/4\pi$ of theory.md §4.6.4), eq. (64)
    a stray time derivative, eq. (67) the sign of $R_B$.
86. **Nucci, C. A.; Guerrieri, S.; Correia de Barros, M. T.; Rachidi, F.** —
    "Influence of Corona on the Voltages Induced by Nearby Lightning on
    Overhead Distribution Lines", *IEEE Trans. Power Delivery*, vol. 15,
    no. 4, pp. 1265–1273, Oct. 2000. DOI: 10.1109/61.891513. A dynamic
    (charge-dependent) capacitance in the lossy-ground Agrawal model: corona
    *raises* the induced peak for severe, close strokes (tens of kA at about
    50 m), by an amount comparable to the finite ground conductivity. Most of
    the inline numbers are lost in the library copy's text layer and were
    read from the page images.
87. **Yutthagowith, P.; Ametani, A.; Nagaoka, N.; Baba, Y.** —
    "Lightning-Induced Voltage Over Lossy Ground by a Hybrid Electromagnetic
    Circuit Model Method With Cooray–Rubinstein Formula", *IEEE Trans.
    Electromagnetic Compatibility*, vol. 51, no. 4, pp. 975–985, Nov. 2009.
    DOI: 10.1109/TEMC.2009.2029702. A partial-element circuit model of
    channel and line with the Cooray–Rubinstein correction, checked against
    FDTD, Camp Blanding, a reduced-scale model and a tower measurement. It
    carries the plus sign on the $E_r$ radiation term (agreeing with §4.6.1).
    Printed errors: eq. (1) lacks $\mu_0$ under the root, eq. (7) repeats
    $(h-z)$, eq. (C1) has a stray $j$.
88. **Akbari, M.; Sheshyekani, K.; Pirayesh, A.; Rachidi, F.; Paolone, M.;
    Borghetti, A.; Nucci, C. A.** — "Evaluation of Lightning Electromagnetic
    Fields and Their Induced Voltages on Overhead Lines Considering the
    Frequency Dependence of Soil Electrical Parameters", *IEEE Trans.
    Electromagnetic Compatibility*, vol. 55, no. 6, pp. 1210–1219, Dec. 2013.
    DOI: 10.1109/TEMC.2013.2258674. Full-wave FEM with the Longmire–Smith
    soil [15,16]: only the horizontal field changes appreciably; the induced
    voltage falls by roughly 15 % at 10⁻³ S/m and not at all at 10⁻² S/m.
    Its text and its Table V disagree on where the effect stops.
89. **Tossani, F.; Napolitano, F.; Borghetti, A.; Nucci, C. A.** —
    "Calculation of Lightning Electromagnetic Fields Above a Lossy Ground for
    Statistical Appraisal of Induced Overvoltages", conference paper,
    c. 2019 (venue not identified in the library copy; the same group's
    journal treatment is *IEEE Trans. EMC*, vol. 61, no. 3, pp. 637–643,
    2019, DOI: 10.1109/TEMC.2019.2899656). Sommerfeld integrals by a
    log-spaced trapezoidal rule inside LIOV-EMTP, against FEM and the
    Cooray–Rubinstein formula: Cooray–Rubinstein distorts and attenuates the
    waveform only for very poor ground, and the statistical (Monte Carlo)
    flashover performance barely changes, so it stays the practical choice.
90. **Faisal, T.; Kaddah, S. S.; Said, T.; Rizk, M. E. M.** — "The Impact
    of Ground Losses on Estimations of Lightning-Induced Voltage with
    Dispersive Soil Parameters", *Scientific Reports*, vol. 15, 2025. DOI:
    10.1038/s41598-025-08379-3. Agrawal coupling in the frequency domain
    with Cooray–Rubinstein fields, with constant and dispersive
    (Alipio–Visacro [14], Smith–Longmire [16]) soils: including the ground
    impedance in the line raises the induced peak by a few per cent (3–8 %
    for 6–10 m lines at 100 m), more at low return-stroke speed; the soil
    model choice matters little.
91. **Natsui, M.** — *Lightning Induced Overvoltages Caused by Non-Vertical
    Lightning and Earth Current Behavior*, PhD thesis, Polytechnique
    Montréal, July 2020
    (journal versions DOI: 10.1016/j.epsr.2018.03.022,
    10.1109/TEMC.2019.2893314, 10.1109/TEMC.2019.2954719). 3-D FDTD with a
    travelling-current-source channel: inclining the channel towards the line
    raises the peak to about 1.5–2 times the vertical-channel value; along the
    line it mainly skews the profile. A low-resistance pole ground absorbs
    current from a nearby strike (Camp Blanding data reproduced). The only
    source in this set that drops the vertical-channel premise of §4.6.
92. **Napolitano, F.; Paolone, M.; Borghetti, A.; Nucci, C. A.; Rachidi, F.;
    Rakov, V. A.; Schoene, J.; Uman, M. A.** — "Interaction Between Grounding
    Systems and Nearby Lightning for the Calculation of Overvoltages in
    Overhead Distribution Lines", *2011 IEEE Trondheim PowerTech*, pp. 1–7.
    DOI: 10.1109/PTC.2011.6019438. Adds to LIOV-EMTP the *conductive*
    coupling between the stroke current in the soil and the line's grounding
    rods, against the 2003 ICLRT triggered-lightning data: the coupled current
    lifts the neutral and so lowers phase-to-neutral voltages near a grounding
    point, which field-only models overestimate.
93. **Piantini, A.; Janiszewski, J. M.; Borghetti, A.; Nucci, C. A.;
    Paolone, M.** — "A Scale Model for the Study of the LEMP Response of
    Complex Power Distribution Networks", *IEEE Trans. Power Delivery*,
    vol. 22, no. 1, pp. 710–720, Jan. 2007. DOI: 10.1109/TPWRD.2006.881410.
    The São Paulo reduced-scale model (terminations, grounding, ground wires,
    transformers, arresters, nearby buildings) and its agreement with
    LIOV-EMTP. A source of measured multi-element network cases.
94. **Borghetti, A.; Gutierrez, J. A.; Nucci, C. A.; Paolone, M.; Petrache,
    E.; Rachidi, F.** — "Lightning-Induced Voltages on Complex Distribution
    Systems: Models, Advanced Software Tools and Experimental Validation",
    *Journal of Electrostatics*, vol. 60, no. 2–4, pp. 163–174, 2004. DOI:
    10.1016/j.elstat.2004.01.001. The LIOV code (multiconductor Agrawal line
    over lossy ground) and its interfaces to EMTP96 and Simulink, which take
    the method from one line to networks with arresters and transformers.
95. **Paolone, M.; Pérez, E.; Borghetti, A.; Nucci, C. A.; Rachidi, F.;
    Torres, H.** — "Comparison of Two Computational Programs for the
    Calculation of Lightning-Induced Voltages on Distribution Systems",
    *Proc. IPST 2005*, Montréal, paper IPST05-250. No DOI. LIV-ATP against
    LIOV-EMTP on single lines and distribution networks: a code-to-code
    benchmark of the two-step procedure.
96. **Stracqualursi, E.; Araneo, R.; Ravichandran, N.; Andreotti, A.;
    Celozzi, S.** — "Modeling of Conductors Catenary in Power Lines: Effects
    on the Surge Propagation Due to Direct and Indirect Lightning", *IEEE
    Trans. Electromagnetic Compatibility*, vol. 65, no. 5, pp. 1464–1475,
    Oct. 2023. DOI: 10.1109/TEMC.2023.3288029. Agrawal coupling on a
    nonuniform line with parabolic sag (implicit FDTD, piecewise-linear
    heights) against EMTP-RV/LIOV (piecewise-constant): sag changes
    direct-strike surges by under 1 % but induced voltages by over 20 % (27–35 %
    on the phases of a realistic line); a uniform average height
    underestimates the peak.
97. **Niu, J.; Zhang, J.; Tao, Y.; Zou, J.; Zhang, Q.; Xie, Z.; Wang, Y.;
    Li, X.** — "Lightning-Induced Voltages over Gaussian-Shaped Terrain
    Considering Different Lightning Strike Locations", *Applied Sciences*,
    vol. 15, no. 12, 6428, 2025. DOI: 10.3390/app15126428. 3-D FDTD: a hill
    beside the line raises the induced voltage (reflections), a hill between
    stroke and line shields it; height matters more than width.
98. **Soto Ríos, E. A.** — *Lightning Induced Voltages Study on Overhead
    Distribution Networks Placed over Non-Flat Terrains*, PhD thesis,
    Universidad Nacional de Colombia, Manizales (year not printed in the
    library copy; after 2013). No DOI. 2-D and 3-D FDTD (TL and full-wave)
    with a reduced-scale validation: induced voltages rise markedly on
    mountains, more with steeper slopes and poorer ground.
99. **Jiménez Mejía, R. E.** — *Lightning Induced Voltages on Overhead Lines
    above Non-Uniform and Non-Homogeneous Ground*, MSc dissertation,
    Universidad Nacional de Colombia, Medellín, Nov. 2014. No DOI.
    Non-uniform-mesh FDTD for mixed propagation paths and irregular ground:
    the horizontal field is the component most affected, and irregular
    ground under the line gave induced voltages up to about five times the
    flat-ground value.

## Neighbouring methods and channel physics (addendum)

100. **Baba, Y.; Rakov, V. A.** — "On the Mechanism of Attenuation of
     Current Waves Propagating Along a Vertical Perfectly Conducting Wire
     Above Ground: Application to Lightning", *IEEE Trans. Electromagnetic
     Compatibility*, vol. 47, no. 3, pp. 521–532, Aug. 2005. DOI:
     10.1109/TEMC.2005.850690. The current on a lossless vertical wire
     attenuates with height because the incident field is scattered by a
     conductor of nonzero thickness (non-TEM structure); a nonuniform line
     whose $Z_c$ rises with height reproduces it; the charge transfer does
     not change with height. The physics behind the unloaded-channel anchor
     of theory.md §4.5.
101. **Chen, X.; Hao, Y.; Li, L.; He, X.; Yeung, C.; Ding, Y.; Jia, L.;
     Zhao, X.; Du, Y.; Liao, M.** — "A Hybrid PEEC and Transmission
     Line Model for Stable and Accurate Transient Electromagnetic Coupling
     Simulations Based on Mixed-Potential Scattering", *Electric Power
     Systems Research*, vol. 257, 112926, 2026. DOI:
     10.1016/j.epsr.2026.112926. PEEC near the strike and towers, MTL in the
     weakly coupled middle of the line, joined by a mixed-potential
     (scalar + vector potential) scattering source; within 5 % of
     rocket-triggered peak overvoltages on a distribution line.
102. **Wang, S.; He, J.; Zhang, B.; Zeng, R.; Yu, Z.** — "A Time-Domain
     Multiport Model of Thin-Wire System for Lightning Transient
     Simulation", *IEEE Trans. Electromagnetic Compatibility*, vol. 52,
     no. 1, pp. 128–135, Feb. 2010. DOI: 10.1109/TEMC.2009.2035815. A
     quasi-static (no retardation) PEEC thin-wire system written in state
     space and reduced to a multiport companion network for EMT programs, so
     nonlinear elements can be attached; perfect ground only.
103. **Fan, J.-B.; Li, Z.-X.** — "Lightning Transient Impulse Problem of
     Substation Grounding Grid in Multilayer Horizontal Layered Soil
     Considering Soil Ionization Effect and Frequency Characteristics",
     *IEEE Trans. Electromagnetic Compatibility*, vol. 66, no. 6,
     pp. 1848–1857, Dec. 2024. DOI: 10.1109/TEMC.2024.3452419. Time-domain
     Galerkin BEM with closed-form quasi-static complex-image Green's
     functions for multilayer soil, with ionisation and dispersive soil; both
     effects lower the grid's response. The time-domain counterpart of the
     multilayer route of [33].
104. **Nazari, M.; Moini, R.; Fortin, S.; Dawalibi, F. P.; Rachidi, F.** —
     "Impact of Frequency-Dependent Soil Models on Grounding System
     Performance for Direct and Indirect Lightning Strikes", *IEEE Trans.
     Electromagnetic Compatibility*, vol. 63, no. 1, pp. 134–144, Feb. 2021.
     DOI: 10.1109/TEMC.2020.2986646. Full-wave MoM (layered media): soil
     dispersion lowers the GPR of a struck electrode, strongly for resistive
     soils, but changes the GPR induced by a *nearby* channel negligibly; the
     wind-turbine case shows the above-ground structure dominating the
     early response.
105. **Vasconcellos, F.; Alipio, R.; Moreira, F.** — "The Influence of the
     Frequency-Dependent Behavior of Ground Electrical Parameters on the
     Lightning Performance of Transmission Lines", *14th IEEE INDUSCON*,
     2021, pp. 1408–1414. DOI: 10.1109/INDUSCON51756.2021.9529544. A wideband
     tower-footing model in ATP with the disruptive-effect criterion:
     dispersive soil cuts the backflashover rate of a 138 kV line by 29–55 %
     for realistic resistivity distributions. The FDNE consumer chain of
     [26,27] applied to outage rates.
106. **Zanon, F. O. S.; Leal, O. E. S.; De Conti, A.** — "Implementation of
     the Universal Line Model in the Alternative Transients Program",
     *Electric Power Systems Research*, vol. 197, 107311, 2021. DOI:
     10.1016/j.epsr.2021.107311. The phase-domain ULM in ATP via foreign
     models, validated against EMTP-RV, then used with frequency-dependent
     ground impedance *and* admittance. The free-EMT line model on the
     consumer side of TUPÃ's output.

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
    typo in its log argument — see theory.md §4.2 and ADR 0017
    finding 2. The model itself is fully specified by the public references
    above.
- TUPÃ's geometry-factor separation (theory.md §4.1, from [3], 2003) and the
  mHEM [11] are the same optimisation, arrived at independently; TAGS's
  closed-form self integral is identical to theory.md §4.2's `g_self` — a
  useful mutual confirmation of both codes.
