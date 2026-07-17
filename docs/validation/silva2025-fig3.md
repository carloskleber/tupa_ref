# Silva et al. 2025 (SBAI), Fig. 3 — harmonic impedance comparison

**Reference**: Silva, G. C. P.; Faria, F. A. C.; Moura, R. A. R.; Schroeder,
M. A. O. — "Comparação entre os Métodos PEEC e HEM na Modelagem
Eletromagnética de Aterramentos Elétricos", *XVII Simpósio Brasileiro de
Automação Inteligente (SBAI)*, 2025 (references.md [36]).

**Case**: base case of the paper (§3.1) — buried horizontal electrode,
60 m long, 7 mm radius, 0.5 m depth; frequency-dependent soil per Alipio &
Visacro [14], ρ0 ∈ {100, 300, 1000, 2400} Ω·m; 128 log-spaced frequency
samples, 100 Hz–4 MHz. TUPÃ case files:
[`common/silva2025_rho100.json`](../../common/silva2025_rho100.json),
`_rho300`, `_rho1000`, `_rho2400` — see
[ROADMAP.md §P5](../ROADMAP.md) for the `tVisacroAlipioSoil` implementation
this exercises and [ADR 0007](../adr/0007-soil-dispersion-model.md) for the
model decision record.

**What the paper actually plots**: PEEC and HEM curves for \|Z(ω)\| that
"estão praticamente sobrepostas" (practically overlapping, MAPE < 0.01%) —
Fig. 3 is effectively a single curve per ρ0. TUPÃ is a third, independent
HEM implementation; this comparison checks whether it lands on that same
curve, not whether PEEC and HEM agree with each other (the paper already
established that).

## Digitization

Fig. 3's four subplots were read manually against their gridlines (method:
[../validation/README.md](README.md)) — no tabulated data exists for this
2025 paper. Frequencies were chosen at the gridlines and at each curve's
visually distinct local extrema (the flat DC plateau, the knee where the
curve starts rising, and — for ρ0 = 1000/2400 Ω·m — the dip and subsequent
peak(s)).

## Result

![TUPÃ vs. Silva et al. 2025 Fig. 3: |Z(ω)| for a 60 m buried electrode at four soil resistivities, simulated curve overlaid with digitized reference points](../figures/silva2025-fig3-comparison.svg)

| ρ0 (Ω·m) | f (Hz) | digitized (Ω) | TUPÃ (Ω) | diff |
| --- | --- | --- | --- | --- |
| 100 | 1e2 | 3.3 | 3.28 | −0.7% |
| 100 | 1e3 | 3.3 | 3.22 | −2.6% |
| 100 | 3e3 | 3.5 | 3.28 | −6.2% |
| 100 | 1e4 | 4.5 | 4.29 | −4.8% |
| 100 | 3e4 | 8.0 | 7.37 | −7.9% |
| 100 | 1e5 | 14.0 | 11.97 | −14.5% |
| 100 | 3e5 | 21.0 | 17.55 | −16.4% |
| 100 | 1e6 | 29.0 | 25.17 | −13.2% |
| 100 | 4e6 | 35.0 | 34.36 | −1.8% |
| 300 | 1e2 | 10.0 | 9.84 | −1.6% |
| 300 | 1e3 | 10.0 | 9.64 | −3.6% |
| 300 | 3e3 | 10.0 | 9.46 | −5.4% |
| 300 | 1e4 | 11.0 | 9.51 | −13.5% |
| 300 | 3e4 | 17.0 | 12.14 | −28.6% |
| 300 | 1e5 | 27.0 | 22.34 | −17.2% |
| 300 | 3e5 | 38.0 | 31.42 | −17.3% |
| 300 | 1e6 | 47.0 | 42.08 | −10.5% |
| 300 | 4e6 | 50.0 | 48.89 | −2.2% |
| 1000 | 1e2 | 33.0 | 32.72 | −0.8% |
| 1000 | 1e3 | 32.0 | 31.79 | −0.7% |
| 1000 | 1e4 | 29.0 | 28.82 | −0.6% |
| 1000 | 3e4 | 26.0 | 26.41 | +1.6% |
| 1000 | 5e4 | 26.0 | 26.50 | +1.9% |
| 1000 | 1e5 | 30.0 | 34.43 | +14.8% |
| 1000 | 2e5 | 45.0 | 49.52 | +10.0% |
| 1000 | 3e5 | 53.0 | 53.54 | +1.0% |
| 1000 | 1e6 | 64.0 | 62.52 | −2.3% |
| 1000 | 2e6 | 66.0 | 64.49 | −2.3% |
| 1000 | 4e6 | 64.0 | 62.32 | −2.6% |
| 2400 | 1e2 | 78.0 | 78.00 | +0.0% |
| 2400 | 1e3 | 77.0 | 74.45 | −3.3% |
| 2400 | 1e4 | 68.0 | 63.39 | −6.8% |
| 2400 | 3e4 | 55.0 | 52.15 | −5.2% |
| 2400 | 8e4 | 42.0 | 40.44 | −3.7% |
| 2400 | 1e5 | 41.0 | 40.56 | −1.1% |
| 2400 | 2e5 | 55.0 | 61.20 | +11.3% |
| 2400 | 3e5 | 75.0 | 75.98 | +1.3% |
| 2400 | 4e5 | 72.0 | 73.87 | +2.6% |
| 2400 | 6e5 | 78.0 | 72.12 | −7.5% |
| 2400 | 1e6 | 76.0 | 75.30 | −0.9% |
| 2400 | 2e6 | 76.0 | 74.93 | −1.4% |
| 2400 | 4e6 | 73.0 | 70.27 | −3.7% |

## Findings

- **DC plateau and 4 MHz endpoint agree closely** (within ~0.5-4%) at every
  ρ0 — this is the same territory the Sunde/Dwight DC formula already
  checks (see the main session's low-frequency cross-check), so it mostly
  confirms the soil model's DC limit rather than the dispersive behaviour
  itself.
- **The resonance structure is reproduced, not just the trend.** For
  ρ0 = 1000 Ω·m, TUPÃ's minimum lands at 37.4 kHz / 26.2 Ω against a
  digitized minimum around 30-50 kHz / 26 Ω, and its maximum at 2.05 MHz /
  64.5 Ω against a digitized maximum around 2 MHz / 66 Ω. For
  ρ0 = 2400 Ω·m, TUPÃ's minimum is 86.1 kHz / 40.2 Ω against a digitized
  minimum around 80-100 kHz / 41 Ω, with the subsequent double-peak
  structure (≈3×10⁵ Hz and ≈6×10⁵ Hz in the paper) showing up at the same
  approximate frequencies in TUPÃ's curve. This is the finding that
  matters most: a resonance's location and depth are far more sensitive to
  getting the electrode/soil physics right than a DC or high-frequency
  asymptote is, so this is a stronger check than the endpoint agreement.
- **The mid-band knee runs 10-29% off for ρ0 = 100/300 Ω·m** — the
  low-resistivity cases, which have no resonance and just rise smoothly.
  This is the least reliable part of the digitization (steepest slope, so
  a small horizontal misreading maps to a large vertical one — the
  −28.6% outlier at ρ0 = 300 Ω·m, f = 3×10⁴ Hz sits right at the steepest
  part of that curve) but a real, smaller gap likely remains underneath
  the reading noise. Candidate causes, none confirmed: the paper does not
  state its electrode segment count (TUPÃ uses 60 × 1 m segments here);
  Alipio & Visacro [14] publish three parameter sets (mean / relatively
  conservative / conservative) and the paper does not say which one it
  used (TUPÃ implements only the mean set — ADR 0007); or a genuine
  modelling difference between PEEC/HEM's transverse-field discretization
  and TUPÃ's mHEM geometry factors (theory.md §4).

## Caveats

- Digitized points, not extracted data — see
  [../validation/README.md](README.md) for the reading-precision caveat.
  Treat single-point outliers (e.g. the −28.6% entry above) as suspect
  before treating them as a model defect.
- No independent confirmation of the paper's segment count or Alipio-
  Visacro parameter set — both are plausible sources of the mid-band gap
  above and neither is settled by this comparison.
- This is a plausibility check, not a formal validation anchor (it isn't
  in [BENCHMARKS.md](../BENCHMARKS.md)'s anchor table criteria — no
  tabulated data, no stated tolerance in advance). It's evidence the model
  is behaving correctly, not proof to the tolerance BENCHMARKS.md's other
  anchors require.
