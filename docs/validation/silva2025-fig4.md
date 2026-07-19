# Silva et al. 2025 (SBAI), Fig. 4 — GPR time-domain comparison

**Reference**: Silva, G. C. P.; Faria, F. A. C.; Moura, R. A. R.; Schroeder,
M. A. O. — "Comparação entre os Métodos PEEC e HEM na Modelagem
Eletromagnética de Aterramentos Elétricos", *XVII Simpósio Brasileiro de
Automação Inteligente (SBAI)*, 2025 (references.md [36]). Same base case as
[silva2025-fig3.md](silva2025-fig3.md): buried horizontal electrode, 60 m
long, 7 mm radius, 0.5 m depth; frequency-dependent soil per Alipio &
Visacro [14], ρ0 ∈ {100, 300, 1000, 2400} Ω·m. TUPÃ case files:
[`common/silva2025_rho100_transient.json`](../../common/silva2025_rho100_transient.json),
`_rho300`, `_rho1000`, `_rho2400` (`_transient` suffix, ADR 0015's
`signal` block).

**What the paper plots**: GPR (ground potential rise) at the injection node
under a lightning first-stroke current, for PEEC and HEM — again
"praticamente sobrepostas" (MAPE < 5.5% at ρ0 = 100 Ω·m, decreasing to
< 0.75% at ρ0 = 2400 Ω·m, the paper's Table 1). The paper states the
excitation is "um somatório de funções de Heidler (De Conti and Visacro,
2007)" representing Morro do Cachimbo first-stroke currents, without
stating which of that paper's parameter sets or which peak current.

## Excitation current: MCS_FST#1 vs. MCS_FST#2

`mSignal::newHeidlerSignal`'s legacy 6-term set is De Conti & Visacro
[38]'s **MCS_FST#1** column — the *single*-peaked fit, normalised to the
median Ip2 = 45.3 kA (references.md [38], theory.md/Signal.f90 finding
2026-07-17). A first attempt at this comparison used exactly that
(`imax = 45300`, no `terms`): the DC-to-first-peak agreement was already
good (peaks within ~0-12% at every ρ0), but every ρ0's second hump came out
far too shallow — for ρ0 = 300/1000 Ω·m TUPÃ's curve
didn't even have a second local maximum in [0, 40 µs], just a monotonic
decline with a faint inflection, while the paper's Fig. 4 shows a clearly
resolved second peak at **every** ρ0, including the two where Fig. 3 shows
no impedance resonance at all (silva2025-fig3.md's finding: only
ρ0 = 1000/2400 Ω·m have a |Z(ω)| dip/peak).

That last point rules out a network-resonance origin for the second GPR
hump — it has to come from the *current* itself. De Conti & Visacro [38]'s
Table I also tabulates **MCS_FST#2**, a 7-term *double*-peaked fit
(Ip1 = 40.1 kA, Ip2 = 45.3 kA — the extra 7th term adds a second,
slightly-higher current peak after the first). Switching the case files'
`signal.terms` to MCS_FST#2's seven `{i0, n, tau1, tau2}` entries (physical
amplitudes, no `imax` rescale — `newHeidlerSignalTerms`, ADR 0015
amendment) reproduces the double hump at all four ρ0 (see Result below).
This is the more likely reading of "a somatório de funções de Heidler" for
a paper explicitly about *first stroke* currents (which are characterised
by multiple peaks — De Conti & Visacro §III-B), even though it isn't
stated outright; ROADMAP.md already flagged MCS_FST#2 as "not yet exposed
via `newHeidlerSignalTerms`" as a preset, ahead of this finding.

The physical picture: the first, sharper current sub-peak (Ip1, steeper
`di/dt`) drives a disproportionately large *inductive* GPR kick, while the
second hump tracks the current's true (higher) resistive-coupled peak
(Ip2 > Ip1) arriving over a slower front. Which hump ends up taller depends
on the balance between the grounding impedance's inductive and resistive
parts, which shifts with ρ0 — see Findings: the first hump is higher at
ρ0 = 100/300/1000 Ω·m, but at ρ0 = 2400 Ω·m the resistively-coupled second
hump overtakes it.

## Numerical setup

`nyquistHz = 4 MHz` (matches Fig. 3's frequency range), `fftPoints = 4096`
(512 µs record). The MCS_FST#2 tail term (τ2 = 200 µs) needs several time
constants to decay inside the FFT's periodic window before `tailTaper`
forcibly zeroes it — at the schema example's default 1024 points
(128 µs record) the truncation leaked into a spurious ~0.05-0.6 MV
(rho-dependent) DC-like offset visible at every sample including t = 0,
largest at ρ0 = 2400 Ω·m (~10% of peak, since the offset is a current
times the largest Z(0) among the four cases); 4096 points reduces it to
5% of peak at worst (ρ0 = 2400 Ω·m, t = 0 residual 0.05 MV against
a ≳ 2 MV signal) at ~3 s per case — negligible cost, see Findings.

## Digitization

Fig. 4's four subplots were re-digitized point-by-point against gridlines
(method: [README.md](README.md)), replacing an earlier pass that only read
a hand-picked 15-point common time grid. The new pass (38-50 raw points per
ρ0, [silva_fig4.xlsx](silva_fig4.xlsx)) traces each curve far more densely,
especially through the steep front (t < 7 µs) that a coarse grid under-
resolves — see Findings below for how much this changed the front-region
comparison. HEM and PEEC were read as coincident (per the paper's own MAPE
claim, confirmed here). The comparison plot
([plot_silva2025_fig4.py](plot_silva2025_fig4.py), mirroring
[plot_silva2025_fig3.py](plot_silva2025_fig3.py)) renders these raw points
directly as the digitized reference curve; the table below re-samples both
curves onto the old 15-point common time grid purely for a compact,
point-for-point readout across all four ρ0.

## Result

![TUPÃ vs. Silva et al. 2025 Fig. 4: GPR(t) for a 60 m buried electrode at four soil resistivities, simulated curve overlaid with digitized reference points](../figures/silva2025-fig4-comparison.svg)

| ρ0 (Ω·m) | t (µs) | digitized (MV) | TUPÃ (MV) | diff |
| --- | --- | --- | --- | --- |
| 100 | 2 | 0.059 | 0.055 | −6.6% |
| 100 | 4 | 0.121 | 0.114 | −6.2% |
| 100 | 6 | 0.190 | 0.177 | −6.9% |
| 100 | 7.3 | 0.394 | 0.376 | −4.4% |
| 100 | 8.5 | 0.294 | 0.281 | −4.2% |
| 100 | 10 | 0.233 | 0.222 | −5.0% |
| 100 | 12.5 | 0.247 | 0.238 | −3.7% |
| 100 | 15 | 0.190 | 0.187 | −1.5% |
| 100 | 18 | 0.133 | 0.133 | +0.4% |
| 100 | 22 | 0.092 | 0.098 | +6.5% |
| 100 | 26 | 0.073 | 0.081 | +10.2% |
| 100 | 30 | 0.068 | 0.073 | +6.5% |
| 100 | 34 | 0.065 | 0.069 | +6.7% |
| 100 | 36 | 0.063 | 0.068 | +7.3% |
| 300 | 2 | 0.099 | 0.099 | +0.3% |
| 300 | 4 | 0.225 | 0.210 | −6.4% |
| 300 | 6 | 0.352 | 0.328 | −6.7% |
| 300 | 7.3 | 0.725 | 0.692 | −4.5% |
| 300 | 8.5 | 0.552 | 0.539 | −2.2% |
| 300 | 10 | 0.437 | 0.432 | −1.1% |
| 300 | 12.5 | 0.480 | 0.480 | +0.0% |
| 300 | 15 | 0.401 | 0.416 | +3.8% |
| 300 | 18 | 0.329 | 0.345 | +4.7% |
| 300 | 22 | 0.297 | 0.306 | +2.8% |
| 300 | 26 | 0.281 | 0.283 | +1.0% |
| 300 | 30 | 0.263 | 0.266 | +1.1% |
| 300 | 34 | 0.253 | 0.253 | +0.0% |
| 300 | 36 | 0.248 | 0.247 | −0.5% |
| 1000 | 2 | 0.188 | 0.179 | −4.6% |
| 1000 | 4 | 0.397 | 0.393 | −1.0% |
| 1000 | 6 | 0.650 | 0.643 | −1.0% |
| 1000 | 7.3 | 1.347 | 1.311 | −2.7% |
| 1000 | 8.5 | 1.152 | 1.160 | +0.8% |
| 1000 | 10 | 1.013 | 1.042 | +2.9% |
| 1000 | 12.5 | 1.280 | 1.281 | +0.1% |
| 1000 | 15 | 1.237 | 1.252 | +1.2% |
| 1000 | 18 | 1.162 | 1.178 | +1.3% |
| 1000 | 22 | 1.088 | 1.096 | +0.7% |
| 1000 | 26 | 1.007 | 1.018 | +1.1% |
| 1000 | 30 | 0.945 | 0.950 | +0.6% |
| 1000 | 34 | 0.885 | 0.894 | +1.0% |
| 1000 | 36 | 0.858 | 0.870 | +1.4% |
| 2400 | 2 | 0.245 | 0.276 | +12.5% |
| 2400 | 4 | 0.561 | 0.614 | +9.4% |
| 2400 | 6 | 1.051 | 1.071 | +1.9% |
| 2400 | 7.3 | 2.088 | 2.064 | −1.2% |
| 2400 | 8.5 | 2.016 | 2.044 | +1.4% |
| 2400 | 10 | 2.001 | 2.097 | +4.8% |
| 2400 | 12.5 | 2.592 | 2.613 | +0.8% |
| 2400 | 15 | 2.660 | 2.710 | +1.9% |
| 2400 | 18 | 2.649 | 2.689 | +1.5% |
| 2400 | 22 | 2.514 | 2.556 | +1.7% |
| 2400 | 26 | 2.364 | 2.402 | +1.6% |
| 2400 | 30 | 2.219 | 2.258 | +1.8% |
| 2400 | 34 | 2.092 | 2.133 | +2.0% |
| 2400 | 36 | 2.043 | 2.078 | +1.7% |

(t = 0 excluded from the table — digitized and TUPÃ both read ≈ 0 there,
making a percent diff meaningless; TUPÃ's own t = 0 residual is
0.001-0.05 MV, see the truncation-offset caveat above.)

## Findings

- **Re-digitizing at 38-50 raw points per ρ0 (vs. the old hand-picked
  15-point grid) cuts the worst-case disagreement from −31% to +12.5%,
  and shows the old front-region "systematic underestimate" was mostly a
  coarse-grid reading artifact, not a real effect.** The old table read a
  single point at t = 2 µs — right on the curve's steepest early slope, so
  a small horizontal misreading there maps to a large vertical one (the
  same effect silva2025-fig3.md's mid-band knee discussion raises). With
  the front traced at native digitizer density the picture is both much
  smaller *and no longer single-signed*: ρ0 = 100/1000 Ω·m read slightly
  low there (−6.6/−4.6% at t = 2 µs, fading toward the first hump), ρ0 =
  300 Ω·m is flat (+0.3%), and ρ0 = 2400 Ω·m reads *high* (+12.5% at
  t = 2 µs, +9.4% at t = 4 µs) — the opposite sign from the old finding.
  A pattern that flips sign across otherwise-identical soils sharing the
  same injected current is digitization noise, not a current- or
  model-side effect; the candidate causes floated for the old finding
  (MCS_FST#2 front parameters, `nyquistHz` front-resolution limits) are
  no longer needed to explain it.
- **The double-hump structure is reproduced at every ρ0** once the
  excitation is switched to MCS_FST#2, confirming both the transient
  pipeline (`mTransient`, ADR 0014/0015) and that MCS_FST#2, not
  MCS_FST#1, is the right reading of the paper's excitation — this is a
  genuine two-peak *current*-driven feature, not a network resonance
  (ρ0 = 100/300 Ω·m have no impedance resonance at all per
  silva2025-fig3.md, yet clearly show the double hump here). Both humps
  agree closely at every ρ0: the first hump's peak (t ≈ 7.3 µs) is within
  −4.4% (ρ0 = 100) to −1.2% (ρ0 = 2400); the second (t ≈ 12.5 µs) is
  within −3.7% (ρ0 = 100) to +0.8% (ρ0 = 2400).
- **The relative hump heights are not uniform across ρ0, and the denser
  digitization corrects the earlier claim that the first hump is always
  higher.** The gap between the two humps shrinks steadily as ρ0
  increases — first-vs-second hump, digitized: 0.394 vs. 0.247 MV
  (ρ0 = 100), 0.725 vs. 0.480 MV (300), 1.347 vs. 1.280 MV (1000, nearly
  level) — and at ρ0 = 2400 it *inverts*: the first peak is 2.088 MV but
  the curve keeps rising to a second, higher plateau of 2.66-2.67 MV
  around t = 15-16 µs, and both TUPÃ (2.71 MV) and the digitized reference
  agree on this. The physical picture from silva2025-fig3.md's finding
  (only ρ0 = 1000/2400 Ω·m have any impedance resonance) still explains
  the first hump's *timing* (driven by MCS_FST#2's fast Ip1 sub-peak
  through Z's inductive/high-frequency response, which is fairly
  ρ0-independent in shape), but the grounding impedance's resistive part
  scales up roughly with ρ0 while the inductive part does not — so the
  second, resistively-coupled peak (driven by the larger Ip2) grows
  relative to the first as ρ0 increases, until it overtakes it at
  ρ0 = 2400.
- **The tail (t ≥ 15 µs) still drifts to a mild TUPÃ overestimate**, but
  far smaller than the old digitization suggested: a steady +0.6 to +2.0%
  for ρ0 = 300/1000/2400 Ω·m, and a larger +6.5 to +10.2% for ρ0 = 100 Ω·m
  (peaking at t = 26 µs). A slowly-decaying tail is controlled by the
  current's slow τ2 = 200 µs term and by Z(ω)'s low-frequency behaviour,
  both largely ρ0-independent in shape — consistent with the similar-sized
  effect recurring for three of the four cases; ρ0 = 100 Ω·m's larger tail
  gap has no obvious soil-side explanation and remains unresolved.
- **The finite FFT record length leaves a residual truncation offset**,
  worst at ρ0 = 2400 Ω·m (largest low-frequency impedance among the four
  cases) — see Numerical setup. `fftPoints = 4096` (512 µs, ~2.6 time
  constants of the current's slowest, τ2 = 200 µs term) was chosen to push
  this under ~5% of peak; a longer record would shrink it further at
  roughly linear extra cost (~3 s/case at 4096, all four cases run in
  well under a minute).

## Caveats

- Digitized points, not extracted data — see [README.md](README.md)'s
  reading-precision caveat. The old 15-point digitization's front-region
  "underestimate at every ρ0" turned out to be exactly this effect (see
  Findings), so treat any single-point disagreement remaining in the
  current table with the same suspicion, especially near the steep front
  and near t ≈ 0 where digitized values are close to zero and percent
  diffs blow up.
- The excitation's peak current and exact parameter set (MCS_FST#1 vs.
  MCS_FST#2, or a different peak current entirely) are inferred, not
  stated by the paper — De Conti & Visacro [38] is the only cited source
  for "a somatório de funções de Heidler" representing Morro do Cachimbo
  first strokes, and MCS_FST#2 is the one of that paper's four fits that
  reproduces the observed double hump, but this is inference from fit
  quality, not a confirmed citation match.
- Same anchor-table caveat as silva2025-fig3.md: a plausibility check
  (this repo's independent third HEM implementation lands on the same
  curve the paper's PEEC and HEM already agree on), not a
  [BENCHMARKS.md](../BENCHMARKS.md)-grade validation anchor.
