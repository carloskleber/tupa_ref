# Marx generator in Tupa: requirements

Paths relative to `fortran/` in `carloskleber/tupa_ref` (HEAD `59c16c1`). Docs are in `docs/`.

## 1. Physics to implement

A charged capacitor C with initial voltage V0 is, for t ≥ 0, an **uncharged** C in series with a step source V0·u(t). In the Laplace domain: `I(s) = Σ V0_k/s / Z(s)`. So "free response of charged capacitors" is exactly a **linear network driven by step EMFs**. Nothing nonlinear is needed *if* every spark gap is modelled as already closed at t = 0 (small R_arc + L_gap) and fires simultaneously. The double exponential then emerges from R_f, R_t, C and the parasitic L/C.
Staged gap firing (real erection delay), gap breakdown, or corona is time-varying/nonlinear and is **out of reach of the frequency-domain/NLT architecture** (see R9).

## 2. What Tupa already has (evidence)

| Capability | Where |
|---|---|
| Lumped series R-L-C two-terminal `branch` between nodes (or node to remote earth); Y = sC/(1+sCR+s²LC), finite at s→0 | `src/Branch.f90:35-52`, `src/element/Branch.f90:42-66`, JSON parse `src/Tupa.f90:422-433` |
| Branch stamped as nodal admittance in the KCL rows after the EM block is assembled; no segment row touched, works for s = jω and s = c + jω | `src/Mesh.f90:190-214` (`stampBranch`), `src/Study.f90:539-549` |
| Time domain = transfer function H(s) × excitation spectrum X(s) → inverse FFT; NLT (s = c + jω, damping c≈ln(N²)/T) is explicitly good for "steady final value" cases | `src/Transient.f90:1-39`, `:174-182`, `docs/theory.md:563`, `docs/theory.md:1790-1800` |
| Two-node (source + return node) current and voltage sources; voltage sources by unit-injection superposition | `src/Signal.f90:118-128` (`tSignalSlot%returnNode/isVoltage`), `src/Study.f90:560-640`, ADR 0016, ADR 0025 |
| Segment-embedded series impedance, NEC "LD card" style: a "loaded" electrode swaps its internal impedance for (R'+sL')·ℓ, added on the Z_long diagonal | `src/Electrode.f90:31-38`, `src/Study.f90:220-262` (`segmentInternalImpedance*`), `src/Mesh.f90:351` (`Zlong(i,i) = ... + zint`); only the lightning `channel` sets it (`src/element/Channel.f90:300`) |
| Parasitic L and C from the EM model: L from Z_long mutual terms, shunt C/leakage from Z_trans, with air/soil images | `src/Mesh.f90:345-452` (`calcZSelf`, `calcZMutual`, `calcFreq2`) |
| Observation of node voltages and electrode currents i1, i2 as time series | `src/Transient.f90:560-715` |

## 3. Gaps and requirements

**R1. Step excitation (blocking, small).** `src/Signal.f90:18` says step/impulse waveforms are "not ported". Add `tStepSignal` (value `vmax` for t ≥ 0). In the current pipeline the signal is tapered at the tail (`tailTaper`) and zero-padded; a step with taper decays the late response, so either skip the taper or use the NLT exponential damping, which is the intended route.

**R2. Initial conditions (blocking, design choice).** Tupa has none: every solve starts from zero state. Two exact equivalents of a charged C:

- *Thevenin form*: step EMF V0 in series with uncharged C. Needs a series EMF (R4).
- *Norton form, available now*: branch (R, L, C) between a and b, **plus a two-node current source** at a with return node b, injecting `i_N(t) = L⁻¹{V0·Y(s)/s}`. For an R-C-L branch this is `V0·C/(1+sCR+s²LC)` inverse-transformed, a closed-form damped exponential or sinusoid. Zero solver changes, only a new waveform class (`tBranchNortonSignal(R,L,C,V0)`). Drawback: the waveform is tied to the branch values and cannot be used for an element that also has a segment-coupled part.

**R3. Lumped element embedded in a wire segment (the real requirement).** `branch` is a pure nodal admittance with **no electromagnetic coupling** (`docs/theory.md:1559-1568`); the wires around it supply parasitic L/C, but the component body itself (a 0.5-1 m capacitor column, a resistor stack) has no length or capacitance to ground. To get that, generalise the *loaded segment*:
- Replace the per-length `loadResistance/loadInductance` (`src/Electrode.f90:35-38`) by a **total** complex Z_lump(s) = R + sL + 1/(sC) per electrode, added (not substituted) to the conductor's internal impedance. Add it to `zint` in `segmentInternalImpedance` (`Study.f90:220-242`) and `...Laplace` (`:244-262`); the sC term must be evaluated at the Laplace s, so the Laplace routine needs the new term too (the 1/(sC) singularity at s→0 is why NLT's c>0 and the DC-bin handling matter; reuse the "finite at s→0" trick of `Branch.f90:35-52`, i.e. use Y form where possible).
- The segment keeps its length, radius, and Z_trans coupling, so parasitic L and C come out of the HEM automatically. Segment length must stay below ~λ_min/10 at the Nyquist frequency (`numerics.maxSegmentLength`, `src/Tupa.f90:351`).
- New JSON element or segment attribute (e.g. `"type":"line", ..., "lumped":{"resistance","inductance","capacitance"}` on one segment, or a `"lumpedLine"` element).
- `geomRadius`/`geomLength`-based internal impedance is skipped for `loaded` segments (`Study.f90:211`): decide whether the wire's skin-effect impedance stays in series (recommended, add it).

**R4. Series EMF in a segment (needed only if R3 + Thevenin).** The system matrix's first `ns` rows are segment equations with zero RHS; only the KCL rows get RHS (`Mesh.f90:517`, `:569`: `y(2*ns + pos) = sig`). A segment-embedded EMF means `y(j) = ±E_j(s)` in the segment row. This is the same type of term as an incident-field drive. Extend `injectSignal(s)` with an optional segment-row RHS, and extend `runSweepUnits` (`Study.f90:788+`) so unit "terminals" can be segment EMFs. Pin the sign with a DC test as the docs do for branches (`docs/theory.md:1553-1557`): the sign depends on the i2 convention (`theory.md` sign caveat after :1571).
If R4 is judged too invasive, the **Norton route (R2) on a `branch` plus short coupled wire stubs** is the cheap alternative.

**R5. Spark gaps (small).** Model as a closed short: branch with R_arc (0.1-1 Ω) and L_gap (tens of nH), a `branch` between stage nodes. A zero-impedance branch is refused at load (`element/Branch.f90:64`), hence the nonzero floor.

**R6. Marx circuit topology (small, input only).** n stages: C_k, charging resistors R_c (large, usually negligible after firing; keep as shunt branches if it matters for the tail), front/tail resistors R_f, R_t. Build as nodes + branches + (embedded) lumped elements + wires. Add a generator-builder script, or a `"marx"` macro element that expands to nodes/branches (similar to how `mesh` expands, ADR 0020).

**R7. Transient settings (config).** Use `signal.transform:"nlt"` with the default damping (`Transient.f90:174-182`); Nyquist well above 1/(π t_f) of the front and above the first wire resonance (tens of MHz for a metre-scale loop), record length T ≥ ~5× the tail time (50-100 µs for 1.2/50). Tail truncation/Gibbs: keep Hann window (`window:"hann"`) and check against the analytic RLC case.

**R8. Outputs (small).** Needs the current through the lumped element and voltage across it. Today: node voltages and electrode i1/i2 only; a **branch-current output is explicitly unimplemented** (`docs/ROADMAP.md:746`). Compute `I_branch = Y(u_a-u_b)` after the solve (the solved node voltages are in `mesh%voltage`). For the segment-embedded form, I = (i1-i2)/2 by the existing convention (`src/Incident.f90:21`). Add IEC 60060-1 front/tail extraction: peak, T1 = 1.67·(t90-t30), T2 = time to 50 % on the tail (k-factor optional).

**R9. Out of scope / known limits.** Staged firing, arc resistance varying in time, capacitor nonlinearities, sphere-gap breakdown, ionisation: all require a time-domain solver (the NLT/FFT path is LTI). A pragmatic workaround is a piecewise-LTI restart with the previous state as new initial conditions (Norton sources again), not available today.

**R10. Validation plan.**
1. Zero-coupling limit: one stage, wires tiny; compare against closed-form RLC (series C, R_f; shunt R_t; load C_b). Same style as `test/test_branch.f90`.
2. Two-stage Marx against Kuffel Table/chapter 1 formulas for the efficiency η = V_peak/(nV0) and front/tail times.
3. Parasitic study: vary loop height and compare to Ruehli PEEC or a SPICE network with partial inductances computed from the same geometry.
4. NLT convergence: halve Δt and double T and check the peak voltage and T1/T2 change < 0.5 %.

## 4. Recommended path

1. R1 (step) and R2-Norton waveform: gets a running Marx with branch elements and wire parasitics in a day or two. Only `Signal.f90`, `Tupa.f90` JSON, and a test.
2. R3 (segment-embedded Z_lump) plus R8 outputs: gives the body-level parasitics.
3. R4 (segment EMF) only if the Thevenin form matters (e.g. per-stage capacitor voltage output).

## 5. References

- Marx, E. (1924). Versuche über die Prüfung von Isolatoren mit Spannungsstößen. *Elektrotechnische Zeitschrift* 45, 652-654.
- Kuffel, E., Zaengl, W. S., Kuffel, J. (2000). *High Voltage Engineering: Fundamentals*, 2nd ed., Butterworth-Heinemann, ch. 1 (impulse generator circuits).
- IEC 60060-1:2010. *High-voltage test techniques, Part 1: General definitions and test requirements* (T1, T2, k-factor).
- Gómez, P., Uribe, F. A. (2009). The numerical Laplace transform: an accurate technique for analyzing electromagnetic transients on power system devices. *Int. J. Electr. Power Energy Syst.* 31(2-3), 116-123 (ref. [17] in `docs/references.md`).
- Grcev, L. D., Dawalibi, F. (1990). An electromagnetic model for transients in grounding systems. *IEEE Trans. Power Delivery* 5(4), 1773-1781.
- Portela, C. (1997/1999). Frequency and transient behavior of grounding systems I/II (refs [1], [2], `docs/references.md:7-12`).
- Ruehli, A. E. (1974). Equivalent circuit models for three-dimensional multiconductor systems. *IEEE Trans. Microwave Theory Tech.* 22(3), 216-221 (PEEC, for validation).
- Burke, G. J., Poggio, A. J. (1981). *Numerical Electromagnetics Code (NEC)-Method of Moments*, NOSC TD 116 (LD card: impedance loading of segments).
- Harrington, R. F. (1968). *Field Computation by Moment Methods*, Macmillan (delta-gap and loaded wire).

Bibliographic details of items not in the repo were written from memory; check volume/page numbers before citing.
