# Tupa versus prototype mHEM

## Scope

This comparison uses the 10 m horizontal-electrode cases from Grcev et al.
Fig. 12 because they provide a common external reference as well as exactly
matched inputs for both codes:

- electrode length: 10 m
- radius: 7 mm
- burial depth: 0.5 m
- segmentation: 40 segments (0.25 m each)
- soil relative permittivity: 10
- soil resistivity: 30, 300, and 3000 Ω·m
- source: 1 A injected at one endpoint
- frequency range: 100 Hz–10 MHz, 401 log-spaced solver points

The external benchmark is the digitized magnitude from the rigorous full-wave
model in Grcev et al. Fig. 12, stored in `grcev_fig12.xlsx`. Solver values are
interpolated linearly in log frequency onto the digitized frequencies. This
avoids the nearest-grid bias in the older validation table.

## Results

Mean absolute percentage error (MAPE) against the digitized full-wave curve:

| ρ (Ω·m) | Band | Tupa | mHEM | Tupa–mHEM difference |
| ---: | :--- | ---: | ---: | ---: |
| 30 | 100 Hz–1 MHz | 3.56% | 3.53% | 0.05% |
| 300 | 100 Hz–1 MHz | 2.76% | 2.81% | 0.09% |
| 3000 | 100 Hz–1 MHz | 2.30% | 3.29% | 1.07% |
| 30 | 100 Hz–10 MHz | 3.19% | 3.13% | 0.12% |
| 300 | 100 Hz–10 MHz | 3.63% | 3.78% | 0.44% |
| 3000 | 100 Hz–10 MHz | 42.06% | 49.99% | 6.74% |

Across all 29 digitized points at or below 1 MHz, Tupa has 3.00% MAPE and
mHEM has 3.31% MAPE. Their relative L2 errors are 2.49% and 3.17%,
respectively. Across the complete band, the corresponding aggregated MAPEs
are 25.35% and 29.87%. The full-band aggregate is dominated by the sharp
double-resonance curve for ρ = 3000 Ω·m.

On the common dense solver grid, the mean magnitude difference between Tupa
and mHEM is 0.11%, 0.18%, and 1.74% for ρ = 30, 300, and 3000 Ω·m. Their
complex relative L2 differences are 0.56%, 0.62%, and 1.89%. Mean absolute
phase differences are 0.07°, 0.06°, and 0.82°.

## Interpretation

Both implementations give essentially the same answer through 1 MHz. Tupa
is marginally closer to the external reference overall, but the difference
between the two codes is smaller than either code's difference from the
digitized reference for the 30 and 300 Ω·m cases.

At 3000 Ω·m above 1 MHz, both reproduce the reference's notch/peak/notch
structure, but their resonance locations differ from the digitized curve.
Pointwise percentage error becomes very large on those steep flanks: a small
horizontal frequency shift produces a large vertical error. Consequently,
the 42–50% full-band MAPE should not be read as a broadband amplitude bias.
Below the resonance region, both remain close to the reference.

The residual high-frequency difference is consistent with implementation
details. The archived mHEM example uses a frequency-dependent transverse
image reflection coefficient. The current Tupa buried-conductor path uses a
fixed positive image sign. Tupa also includes conductor internal impedance,
while the archived example does not. The geometry integrations differ too:
Tupa uses a 64×64 midpoint quadrature and the archive's modified HEM uses its
one-dimensional modified integral.

## Limitations

- The published reference is digitized from a plot, not tabulated source data.
- Only magnitude can be checked externally; phase is compared only between
  the two implementations.
- This comparison covers the matched 10 m electrode, not the archive's larger
  grounding-grid example. The 100 m Tupa cases use 400 segments and warrant a
  separate performance-oriented run.
- Runtime samples include compilation and were not collected with a benchmark
  harness, so they are not used to rank performance.

## Artifacts

- `docs/figures/tupa-mhem-grcev-l10-comparison.png`: magnitude and pointwise
  error plot.
- `docs/figures/tupa-mhem-aligned-cases-comparison.png`: direct Tupa–mHEM
  magnitude overlays and relative differences for the three horizontal cases
  and the matched vertical rod.
- `docs/validation/tupa-mhem-grcev-l10-results.csv`: 401-point complex results
  from both solvers for all three resistivities.
- `docs/validation/tupa-mhem-grcev-l10-metrics.csv`: per-case metrics.
- `julia/comparison/run_tupa_grcev_l10.jl`: Tupa runner.
- `julia/comparison/run_mhem_grcev_l10.jl`: runner for an extracted copy of the
  supplied ZIP.
- `docs/validation/compare_tupa_mhem.py` and
  `julia/comparison/plot_tupa_mhem_grcev_l10.jl`: analysis and plotting scripts.
