# 10. Positioning: TUPÃ vs. the classic MoM and companion codes

[Theory index](../theory.md) · previous: [§9 Validation anchors](s09-validation.md)

> **Summary.** The HEM is one fixed set of choices within Harrington's Method
> of Moments, plus a circuit closure. This chapter compares those choices
> with three open-source codes of the same family, and places the family
> among the other methods used for lightning transients, from circuit models
> to full-wave solvers.

Methodology and premises of this model against Harrington's original Method
of Moments [6] and the three open-source companion codes described in
[references.md](../references.md). The TUPÃ column is the model specified by
these chapters. The proposals that came out of this comparison are tracked in
[ROADMAP.md §7](../ROADMAP.md).

| Aspect | Harrington MoM [6] | **TUPÃ (this doc)** | TAGS (C99) | PRTL-mHEM (Python) | PRTL (Wolfram/CDF) |
| --- | --- | --- | --- | --- | --- |
| Target problem | General field problems (antennas, scattering) as integral equations | Lightning/grounding transients, thin-wire structures | Grounding system transients | Line lightning performance incl. mHEM grounding | Line lightning performance; grounding imported from file |
| Unknowns | Expansion coefficients of the current distribution | $(\mathbf{u}, \mathbf{i}_1, \mathbf{i}_2)$: node voltages + segment end currents (§6) | $(\mathbf{u}, I_\ell, I_t)$ symmetric block system, or nodal $\mathbf{u}$ only | Nodal $\mathbf{u}$ (admittance reduction) | Nodal network quantities (Laplace domain) |
| Basis / testing | Arbitrary (Galerkin, point matching, …) — the general framework | Pulse basis per segment; matching on segment averages | idem | idem | n/a (circuit/TL level) |
| Coupling integrals | Full Green's-function integrals, re-evaluated per frequency | Frequency-independent geometry factor $g$, $\exp(-\gamma\bar R)$ at midpoint distance (§4.1); $g$ via 1-D mHEM integral (§4.2, default) or 2-D quadrature | Selectable: double, single, mHEM, midpoint-only | mHEM (1-D integral, precomputed $P$, $P_i$) | n/a — line by TL theory; grounding external |
| Half-space interface | Not treated (homogeneous medium assumed) | Images with $\Gamma_t(\omega)$ applied to both $Z_t$ and $Z_\ell$ (default); ideal $\pm 1$ signs selectable (§5) | Images with complex $\Gamma_\ell$, $\Gamma_t$ as free parameters | Images with $\Gamma_t(\omega)$ applied to both $Z_t$ and $Z_\ell$ | Earth return at TL level (line above lossy ground) |
| Cross-media coupling (air↔soil segments) | n/a | Neglected by default; quasi-static transmitted term opt-in (§5.1) | Neglected | Neglected | n/a |
| Soil dispersion | n/a (σ, ε constants) | Portela [1,31] and Alipio–Visacro [14] (mean set); Longmire–Smith [15,16] not available (§7) | Alipio–Visacro [14] and Smith–Longmire [16] built in | Visacro–Alipio [13] | Delegated to the imported grounding data |
| Conductor internal impedance | None (PEC wires) | Solid Bessel; any other section (tube, strip, stranded) as a cross-section entry with an equivalent radius; thin coaxial coating (§4.3) | None (neglected) | Solid + tubular Bessel | Tubular Bessel |
| Linear solve | Dense matrix inversion | Dense LU (`ZGESV`), full $Z_{\text{eq}}$; reduced $Z_g$ as consistency check (§6) | Dense LU; immittance or admittance path | Dense inversion of $Y_g$ | Dense (Mathematica `Inverse`) |
| Time domain | Out of scope (harmonic) | FFT↔IFFT (§8); NLT with damping + Hann window (§8, opt-in) | NLT with damping + window filters [17] | NLT (damped $s_k$ grid) + separate harmonic mode | NLT (`nILT`) |
| Frequency axis | Single frequency | Log-spaced sweep (harmonic); linear grid for transients (§8) | Linear (example-defined, incl. log for harmonic studies) | Log (harmonic) / linear (transient) | Linear (NLT grid) |
| Parallelism | n/a | OpenMP over the frequency loop, bit-identical for any thread count (the LU is ≈ 100 % of a frequency's cost) | OpenMP over the frequency loop, single-threaded BLAS | None (NumPy internal) | None |
| Validation anchors | Analytic canonical cases | §9: Sunde DC; published curves of Grcev [23], Lima [11], Poljak [35], Silva [36] (release oracle); Portela [2] and Grcev [18] pending data or cross-code check | Grcev [18], Visacro & Soares, Alipio, Sunjerga examples | Published line/grounding cases | Four 138 kV test cases [12] |

Premises shared by TUPÃ, TAGS and PRTL-mHEM (and inherited from [1,5] —
the HEM family reading of Harrington's framework, derived at thesis length
in [55]): thin-wire
approximation, uniform currents per segment (pulse basis), quasi-static image
treatment of the single air–soil interface, dense frequency-domain solve per
sample, linearity (no soil ionisation). Harrington's MoM is the general
umbrella: the HEM family fixes basis, testing and kernel choices and adds the
circuit-level closure (§6) that pure MoM does not have.

That closure is also what places the family among model classes at large:
Baba & Rakov's reviews of electromagnetic return-stroke models [34,44]
situate the HEM between full electromagnetic models and distributed-circuit
models —
it produces non-TEM near fields like the former, but couples electric and
magnetic effects through separate circuit quantities like the latter — and
reports HEM channel-current distributions consistent with full
electromagnetic solutions, an independent endorsement of the family's
physics from outside the grounding literature. TUPÃ's
lightning-channel element (§4.5) sits exactly in this class. The channel is ordinary HEM segments in air with
**added distributed series impedance, calibrated so the computed
propagation matches a prescribed return-stroke speed**. This is the
type-2 wire loading of [44; 74, ch. 8], and it makes the target speed a
user input rather than an emergent artefact of the unloaded wire. The same
channel-as-segments machinery is what the HEM family uses for
lightning-*induced* voltage studies — channel and line in one model, with the
lossy-ground coupling handled by Norton's approximation [45,46]. TUPÃ takes
that route for a perfect ground (§4.6, anchor 9 of §9);
the lossy-ground field correction stays outside it (§4.6.4).

## 10.1 Neighbouring model families

The same problem is attacked in the literature by methods that trade accuracy
for speed (circuit and TL models), extend the HEM's validity (complex images,
time domain), or sit above it as full-wave oracles. Where TUPÃ stands
relative to each:

| Model family | Domain | Approach | Relation to TUPÃ | Refs |
| --- | --- | --- | --- | --- |
| HEM-TD | Time | HEM physics solved directly in time; dispersive soil via rational (pole–residue) models; time delays computed in time domain | Same physics, other domain; needed only for nonlinear phenomena (soil ionisation, arresters, corona) that TUPÃ excludes by design; benchmarked against frequency-domain HEM | [47,21] |
| HEM + complex images | Frequency | Earth replaced by perfect conductor at complex depth instead of quasi-static images | Extends the §5 image treatment beyond the few-MHz ceiling; the refinement step after $\Gamma_t(\omega)$ | [20] |
| HF circuit models | Frequency / EMT | Lumped RLC (with or without mutual coupling) derived from the MoM equations by successive approximations | Degenerate limit of §4–§6; [23] maps their error vs. a full-wave reference over length, resistivity and frequency — mutual coupling is the decisive HF ingredient (which HEM keeps in full) | [23] |
| PEEC | Frequency / time | Partial-element equivalent circuits from the volume EFIE: separate current and potential cells (R, L, P matrices, MNA solve), no thin-wire restriction | Same MoM roots, more general discretisation; on grounding electrodes agrees with HEM to negligible differences (harmonic-impedance MAPE < 0.01 %), while HEM's unified segments + symmetry reuse are far cheaper for wire-like geometries; higher-order (piecewise-linear/sinusoidal) bases trade that efficiency for per-segment accuracy; a quasi-static (retardation-free) PEEC in state space reduces to an EMT multiport companion network to which nonlinear elements attach, at the cost of delays and of a perfect ground [102] | [36,49,102] |
| Antenna theory (Pocklington) | Frequency | Thin-wire Pocklington EFIE, sub-segment current expansion, boundary-element solve; interface via a Fresnel reflection coefficient in the kernel | The reflection-coefficient kernel is the antenna-theory analogue of §5's $\Gamma(\omega)$ images — accuracy between quasi-static images and full Sommerfeld treatment at a fraction of the Sommerfeld cost | [35] |
| Multilayer-soil hybrid | Frequency + time | Layered-earth Green's functions via quasi-static complex images (matrix pencil); soil ionisation via conductor-radius adjustment | Lifts §5's single-interface premise; TUPÃ assumes a uniform soil half-space by design — the reference route if stratified soil is ever required; a time-domain Galerkin BEM with closed-form complex-image Green's functions adds ionisation and dispersion to the same multilayer setting [103], and a full-wave layered-media MoM covers direct and nearby strikes [104] | [33,103,104] |
| TL-model + FDTD | Time | Per-unit-length parameters (frequency-dependent Z, Y via vector fitting), 1-D FDTD over the wire mesh; soil ionisation representable | Cheaper thin-wire route solved directly in time; route originated by the nonuniform-TL model [57] (space-dependent per-unit-length parameters from summed segment couplings — predicts effective length where uniform-TL fits fail), with an earlier finite-element/TL precedent [60] (frequency-dependent Voltage Distribution Functions per segment, pre-HEM); ≤5 % deviation from a full EM model on parallel counterpoises (effective length independent of wire separation); scales to grids and wind-farm grounding networks; Grcev's empirical impulse-efficiency formulas [63] quantify the same effective-length behaviour under fast-fronted pulses from the full-wave side | [24,48,57,60,63] |
| LEMP + transmission-line coupling (MTL/TCS fields, Agrawal/Taylor/Rusck equations) | Time (or frequency + FFT) | Return-stroke model gives the field from the channel-base current, and a telegrapher equation with a distributed source gives the line voltage | The reference procedure of the induced-voltage literature [75–79, 82, 85]; network codes (LIOV-EMTP, LIV-ATP) extend it to arresters, transformers and grounding [92, 94, 95]; a HEM line (§4.6.3) solves the same problem as a thin-wire structure, with the line–channel reaction, conductive coupling through the grounding and any 3-D wire layout (sag included [96]), at the price of segments instead of a 1-D grid; the perfect-ground base case is the validation anchor | [75–79, 82, 85, 92, 94–96] |
| FDTD–PEEC hybrid | Time | 1-D FDTD for the line + PEEC for tower and lightning channel | Models the lightning-channel↔tower coupling that HEM-class tools (TUPÃ included) neglect; relevant for tower-surge, not grounding, accuracy | [25] |
| PEEC–MTL hybrid | Time | PEEC near the strike and the towers, MTL along the weakly coupled middle of the line, joined by a mixed-potential (scalar + vector potential) scattering source | The induced-voltage counterpart of the row above; within 5 % of triggered-lightning peaks on a distribution line. TUPÃ treats the whole line with segments instead, which costs more and needs no interface between two models | [101] |
| Full-wave MoM (NEC-4 class) | Frequency | Sommerfeld-integral treatment of the interface, sub-segment current expansion | The accuracy oracle above HEM: [20] and [23] use it as reference; no geometry-factor shortcut, so far costlier; NEC-2 tower studies [50] show the non-TEM effects (transient footing impedance, sub-TEM shield-wire coupling) EMT models compress | [20,23,50] |
| EMT line-level analysis (multistory towers, LEMP-corrected) | Time (EMT programs) | Towers as TL/multistory circuit models calibrated from full-wave analyses [50]; grounding as macromodels; LEMP field-to-line coupling addable [52] | The consumer layer above TUPÃ for line lightning performance: component-coupling neglect is validated (< a few % on peaks, less than soil-parameter uncertainty [51]), but frequency-dependent grounding [51] and LEMP-induced voltages [52] must be represented — plain EMT underestimates insulator voltages by up to ~58 %; model choices swing outage rates by up to ~70 % [54]; nonuniform spans (wide river crossings, tall towers) break the cascaded-uniform-line recipe, which can go numerically unstable — HEM segments over the catenary handle them natively [58]; frequency-dependent ground in the line model itself is now available in free EMT through the ULM in ATP [106], and a wideband footing model with dispersive soil cuts computed outage rates by 29–55 % [105] | [50,51,52,54,58,105,106] |
| Rational models / FDNE for EMT | s-domain → time | Vector fitting / matrix-pencil approximation of $Z_g(\omega)$, passivity-enforced, plugged into ATP/EMTP/PSCAD | A *consumer* of TUPÃ's output, not a competitor; effective length drives realization order and robustness; the route dates back to Heimbach & Grcev's rational-function EMTP incorporation [64], ~25–30 years before [26,27]; the fitting and passivity machinery is vector fitting [69] and its textbook extension [70] | [26,27,64,69,70] |

Grcev & Arnautovski-Toseva [56] give the fundamental statement of the
validity bounds that organise this table: the upper frequency of interest is
set by the *response* spectrum, not the excitation's; the quasi-static
approximation requires system dimensions under ~λ/10 in the soil at that
frequency; and toward MHz frequencies the voltage to ground turns
path-dependent, so even the "impedance to ground" stops being uniquely
defined. Segmentation guidance ([19], §4.1) and the scope ceiling
illustrated by the
200-MHz SW-TDR diagnostics application [29] round out the picture: below the
thin-wire limits, circuit models suffice; above a few MHz, complex images or
full-wave methods take over. In the time domain the oracle role passes to
3-D FDTD, which handles inhomogeneous soil, nonlinearities and non-thin-wire
structures directly — it validates the LEMP-corrected EMT method [52] and
carries substation grid-plus-shielded-cable problems [53] that sit outside
HEM scope altogether. The same holds for induced voltages beyond the HEM
premises: the earth-current field of a nearby strike, which couples the
channel to buried conductors [91], and hills or irregular ground under the
line [97–99]. CIGRE TB 543 [72] gives the working rule for this
layering. EMTP-type tools fail on non-TEM propagation, and the full-wave
codes are too costly for whole networks. Numerical EM analysis is therefore
best used to compute the impedances that circuit-theory tools need, and to
supply reference cases for the faster codes. The first half of that rule is
TUPÃ's role.

The consumers at the end of the chain are the line lightning-performance
procedures (CIGRE [140, 141], IEEE [142], Hileman [143]), which need a
tower-footing impedance and, for distribution lines, induced voltages. The
circuit programs between them and a field model rest on Dommel's companion
models [138] and on frequency-dependent line models of the Martí type [139].
