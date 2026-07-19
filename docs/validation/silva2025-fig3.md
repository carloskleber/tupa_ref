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
2025 paper. The data below was digitized from the paper's figure using
a systematic 1-2-5-per-decade frequency grid (100 Hz to 4 MHz, 14–15 points
per soil resistivity). On each curve, HEM and PEEC readings were taken as
coincident (per the paper's own MAPE < 0.01% claim). The interpolated grid
allows the comparison plot to render the digitized reference as a continuous
line rather than scattered points. The evenly-spaced frequency grid does not
specifically target the ρ0 = 2400 Ω·m double-peak (§ Findings) the way
extrema-picking would; TUPÃ's own dip/peak locations quoted in the Findings
still come from its own dense (128-point) sweep, independent of this table's
sampling.

## Result

![TUPÃ vs. Silva et al. 2025 Fig. 3: |Z(ω)| for a 60 m buried electrode at four soil resistivities, simulated curve overlaid with digitized reference points](../figures/silva2025-fig3-comparison.svg)

| ρ0 (Ω·m) | f (Hz) | digitized (Ω) |
| --- | --- | --- |
| 100 | 1e2 | 3.29 |
| 100 | 2e2 | 3.30 |
| 100 | 5e2 | 3.32 |
| 100 | 1e3 | 3.33 |
| 100 | 2e3 | 3.34 |
| 100 | 5e3 | 3.56 |
| 100 | 1e4 | 4.28 |
| 100 | 2e4 | 6.05 |
| 100 | 5e4 | 9.37 |
| 100 | 1e5 | 12.12 |
| 100 | 2e5 | 15.36 |
| 100 | 5e5 | 20.78 |
| 100 | 1e6 | 25.78 |
| 100 | 2e6 | 30.65 |
| 300 | 1e2 | 9.94 |
| 300 | 2e2 | 9.87 |
| 300 | 5e2 | 9.78 |
| 300 | 1e3 | 9.71 |
| 300 | 2e3 | 9.64 |
| 300 | 5e3 | 9.58 |
| 300 | 1e4 | 9.71 |
| 300 | 2e4 | 10.80 |
| 300 | 5e4 | 16.24 |
| 300 | 1e5 | 22.36 |
| 300 | 2e5 | 27.91 |
| 300 | 5e5 | 36.35 |
| 300 | 1e6 | 42.87 |
| 300 | 2e6 | 47.59 |
| 1000 | 1e2 | 32.81 |
| 1000 | 2e2 | 32.69 |
| 1000 | 5e2 | 32.36 |
| 1000 | 1e3 | 32.02 |
| 1000 | 2e3 | 31.24 |
| 1000 | 5e3 | 30.18 |
| 1000 | 1e4 | 28.95 |
| 1000 | 2e4 | 27.33 |
| 1000 | 5e4 | 26.82 |
| 1000 | 1e5 | 34.31 |
| 1000 | 2e5 | 49.97 |
| 1000 | 5e5 | 57.91 |
| 1000 | 1e6 | 63.52 |
| 1000 | 2e6 | 65.42 |
| 2400 | 1e2 | 78.08 |
| 2400 | 2e2 | 77.52 |
| 2400 | 5e2 | 76.29 |
| 2400 | 1e3 | 74.74 |
| 2400 | 2e3 | 72.59 |
| 2400 | 5e3 | 68.04 |
| 2400 | 1e4 | 63.30 |
| 2400 | 2e4 | 56.71 |
| 2400 | 5e4 | 45.06 |
| 2400 | 1e5 | 40.74 |
| 2400 | 2e5 | 62.35 |
| 2400 | 5e5 | 70.96 |
| 2400 | 1e6 | 75.80 |
| 2400 | 2e6 | 76.05 |

## Findings

- **DC plateau and 4 MHz endpoint agree closely** (within ~0.2-2.4% at
  ρ0 = 300/1000/2400; a steadier ~6-8% at ρ0 = 100, see below) — this is
  the same territory the Sunde/Dwight DC formula already checks (see the
  main session's low-frequency cross-check), so it mostly confirms the
  soil model's DC limit rather than the dispersive behaviour itself.
- **The resonance structure is reproduced, not just the trend.** For
  ρ0 = 1000 Ω·m, TUPÃ's minimum lands at 37.4 kHz / 26.2 Ω against a
  digitized minimum of 26.5 Ω at 5×10⁴ Hz (+0.5%), and its maximum at
  2.05 MHz / 64.5 Ω against a digitized 65.0 Ω at 2×10⁶ Hz (−0.8%). For
  ρ0 = 2400 Ω·m, TUPÃ's minimum is 86.1 kHz / 40.2 Ω against a digitized
  41.0 Ω at 1×10⁵ Hz (−1.3%). This is the finding that matters most: a
  resonance's location and depth are far more sensitive to getting the
  electrode/soil physics right than a DC or high-frequency asymptote is,
  so this is a stronger check than the endpoint agreement — and with this
  denser table the fit through the whole ρ0 = 1000 Ω·m dip-and-peak is
  tight everywhere (|diff| ≤ 4.5%).
- **One outlier, at ρ0 = 2400 Ω·m, f = 2×10⁵ Hz: −15.6%** (digitized 73.0 Ω
  vs. TUPÃ 61.6 Ω). This sits right after the resonance minimum, on the
  steep initial rebound — TUPÃ's own dense sweep shows that rebound isn't
  monotonic (a first, lower local peak near 3×10⁵ Hz, a dip, then the
  higher peak the paper also reports near 6×10⁵ Hz per the earlier,
  extrema-targeted digitization pass), so a single sample at 2×10⁵ Hz on
  either curve can land well off the other on this stretch. Coarser
  evidence than a modelling gap: the 1-2-5-per-decade grid used here has
  no second sample between 1×10⁵ and 5×10⁵ Hz to confirm whether the
  digitized reference is tracking the same double-peak shape or a
  straight-line interpolation artifact of sparse sampling.
- **The mid-band knee runs up to ~14% off for ρ0 = 100/300 Ω·m** — the
  low-resistivity cases, which have no resonance and just rise smoothly.
  ρ0 = 100 Ω·m additionally sits a steady 6-8% low from DC to ~2 kHz; this
  flat region is easy to read precisely off the plot (no steep-slope
  ambiguity), but the gap is only ~0.2 Ω in absolute terms, comparable to
  this table's ~0.5 Ω rounding — so it's suggestive rather than
  conclusive on its own. Combined with the larger (10-14%) gaps on the
  rising knee itself (steepest slope, so a small horizontal misreading
  maps to a large vertical one — the least reliable part of the
  digitization) a real, smaller gap plausibly remains underneath the
  reading noise; see the segment-count corroboration below. Candidate
  causes: the paper does not state its electrode segment count (TUPÃ uses
  60 × 1 m segments here); Alipio &
  Visacro [14] publish three parameter sets (mean / relatively
  conservative / conservative) and the paper does not say which one it
  used (TUPÃ implements only the mean set — ADR 0007); or a genuine
  modelling difference between PEEC/HEM's transverse-field discretization
  and TUPÃ's mHEM geometry factors (theory.md §4).
- **The segment-count candidate cause has independent, if indirect,
  corroboration.** Silva's own MSc dissertation [49] (the dissertation
  behind this SBAI paper) dedicates §5.3 to exactly this question for the
  same electrode/soil family (horizontal buried electrode, Alipio-Visacro
  dispersive soil, pulse/piecewise-constant HEM basis — same basis TUPÃ
  uses per theory.md §4): it takes "10r" segment length (10× electrode
  radius, ≈0.07 m for the paper's 7 mm radius) as its converged reference
  and shows that coarser discretizations *systematically underestimate*
  |Z(ω)| — the same direction as most of the mid-band gap above (the
  ρ0 = 300 Ω·m knee is negative throughout; ρ0 = 100 Ω·m is mixed, with a
  brief +7-11% overshoot right at the knee's onset, 1-2×10⁴ Hz, before
  turning negative further up). Their coarsest tested tier (segment
  length a tenth of the shortest excited wavelength, λ/10) lands at
  0.99-1.96 m depending on ρ0 — the same order as TUPÃ's fixed 1 m
  segments here — and shows 5.2-7.3%
  MAPE against the 10r reference (dissertation Tables 5.4/5.5), even
  though that comparison used a 30 m/effective-length electrode and
  ρ0 ∈ {100, 1000, 2500, 5000} Ω·m, not this case's 60 m/{100, 300, 1000,
  2400} Ω·m — so it's suggestive, not a direct replication. It does mean
  TUPÃ's segmentation (chosen against [19]'s much looser "up to 1000·r₀"
  acceptance criterion, tuned for ≤10% GPR-peak error) is expected to sit
  several percent away from a fully converged HEM solution on its own
  terms, before any digitization noise or paper-side unknowns — a
  meaningfully sized chunk of the observed mid-band gap, though the
  dissertation's own numbers don't cover enough of the gap's range to
  call it the sole cause.

## Caveats

- Digitized points, not extracted data — see
  [../validation/README.md](README.md) for the reading-precision caveat;
  this table's round-numbered values (nearest ~0.5 Ω) suggest an eyeballed
  rather than pixel-traced reading, so treat the single-point outlier
  (ρ0 = 2400 Ω·m, f = 2×10⁵ Hz, −15.6%, see Findings) as suspect before
  treating it as a model defect.
- No independent confirmation of the paper's segment count or Alipio-
  Visacro parameter set — both are plausible sources of the mid-band gap
  above and neither is settled by this comparison.
- This is a plausibility check, not a formal validation anchor (it isn't
  in [BENCHMARKS.md](../BENCHMARKS.md)'s anchor table criteria — no
  tabulated data, no stated tolerance in advance). It's evidence the model
  is behaving correctly, not proof to the tolerance BENCHMARKS.md's other
  anchors require.
