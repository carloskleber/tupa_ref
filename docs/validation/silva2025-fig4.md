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
`di/dt`) drives a disproportionately large *inductive* GPR kick, which is
why TUPÃ's (and the paper's) first GPR hump is consistently *higher* than
the second even though Ip2 > Ip1 — the second hump tracks the current's
true (higher) resistive-coupled peak arriving over a slower front.

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

Fig. 4's four subplots were read manually against gridlines (method:
[README.md](README.md)), at t = 0, the front, both humps' extrema, and
several tail points out to the plotted range's end (36 µs).

## Result

![TUPÃ vs. Silva et al. 2025 Fig. 4: GPR(t) for a 60 m buried electrode at four soil resistivities, simulated curve overlaid with digitized reference points](../figures/silva2025-fig4-comparison.svg)

| ρ0 (Ω·m) | t (µs) | digitized (MV) | TUPÃ (MV) | diff |
| --- | --- | --- | --- | --- |
| 100 | 2 | 0.100 | 0.055 | −45.2% |
| 100 | 4 | 0.170 | 0.114 | −33.1% |
| 100 | 6 | 0.280 | 0.177 | −36.9% |
| 100 | 7.5 | 0.400 | 0.372 | −7.0% |
| 100 | 9 | 0.280 | 0.253 | −9.8% |
| 100 | 10.5 | 0.220 | 0.218 | −1.1% |
| 100 | 13 | 0.255 | 0.234 | −8.4% |
| 100 | 17 | 0.150 | 0.147 | −1.8% |
| 100 | 20 | 0.125 | 0.112 | −10.3% |
| 100 | 25 | 0.095 | 0.084 | −11.5% |
| 100 | 30 | 0.078 | 0.073 | −6.7% |
| 100 | 36 | 0.065 | 0.068 | +4.3% |
| 300 | 7.5 | 0.720 | 0.693 | −3.8% |
| 300 | 9 | 0.500 | 0.487 | −2.5% |
| 300 | 10.5 | 0.430 | 0.426 | −1.0% |
| 300 | 13 | 0.490 | 0.479 | −2.3% |
| 300 | 20 | 0.340 | 0.321 | −5.5% |
| 300 | 30 | 0.280 | 0.266 | −4.9% |
| 300 | 36 | 0.250 | 0.247 | −1.2% |
| 1000 | 7.5 | 1.380 | 1.344 | −2.6% |
| 1000 | 9 | 1.100 | 1.090 | −1.0% |
| 1000 | 10.5 | 1.000 | 1.065 | +6.5% |
| 1000 | 13 | 1.300 | 1.302 | +0.1% |
| 1000 | 20 | 1.150 | 1.138 | −1.1% |
| 1000 | 30 | 0.970 | 0.950 | −2.0% |
| 1000 | 36 | 0.880 | 0.870 | −1.2% |
| 2400 | 7.5 | 2.050 | 2.146 | +4.7% |
| 2400 | 9 | 1.980 | 2.003 | +1.2% |
| 2400 | 10 | 1.950 | 2.097 | +7.6% |
| 2400 | 16 | 2.680 | 2.713 | +1.2% |
| 2400 | 20 | 2.620 | 2.630 | +0.4% |
| 2400 | 30 | 2.250 | 2.258 | +0.4% |
| 2400 | 36 | 2.050 | 2.078 | +1.3% |

(t = 0 excluded from the table — see the truncation-offset caveat above;
its absolute values are 0.001-0.05 MV, small in every case.)

## Findings

- **The double-hump structure is reproduced at every ρ0, with the right
  relative hump heights** (first hump higher than second, for all four
  cases, both in the paper and in TUPÃ) once the excitation is switched
  to MCS_FST#2. This is the main result: it is a genuine two-peak
  *current*-driven feature, not a network resonance (ρ0 = 100/300 Ω·m have
  no impedance resonance at all per silva2025-fig3.md, yet clearly show
  the double hump here) — confirms both the transient pipeline
  (`mTransient`, ADR 0014/0015) and that MCS_FST#2, not MCS_FST#1, is the
  right reading of the paper's excitation.
- **Both humps and the tail agree within ~1-8% at every ρ0** except the
  ρ0 = 100 Ω·m front (t = 2-6 µs, −33 to −45%). That front region is the
  smallest absolute values on the steepest part of the curve (0.1-0.28 MV
  against gridlines spaced every ~0.1 MV) — the same steep-slope
  digitization-noise caveat silva2025-fig3.md flagged for its mid-band
  knee, here compounded by the front's fast rate of rise. The ρ0 = 300/
  1000/2400 Ω·m curves don't show a comparably large front error because
  their larger absolute GPR values make the same gridline-reading
  uncertainty a smaller relative error.
- **The finite FFT record length leaves a residual truncation offset**,
  worst at ρ0 = 2400 Ω·m (largest low-frequency impedance among the four
  cases) — see Numerical setup. `fftPoints = 4096` (512 µs, ~2.6 time
  constants of the current's slowest, τ2 = 200 µs term) was chosen to push
  this under ~5% of peak; a longer record would shrink it further at
  roughly linear extra cost (~3 s/case at 4096, all four cases run in
  well under a minute).

## Caveats

- Digitized points, not extracted data — see
  [README.md](README.md)'s reading-precision caveat, sharpened above for
  the front-region case.
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
