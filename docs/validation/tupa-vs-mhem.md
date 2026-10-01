# TUPÃ (Fortran and Julia) versus the mHEM prototype — Grcev et al. 2018 Fig. 12, ℓ = 10 m

> **Note (2026-10-01, ROADMAP Phase 10).** The numbers below predate Phase 10:
> both TUPÃ solvers used ideal images and the nested 2-D kernel. Fortran now
> defaults to `Γ(ω)` images and the single-integral kernel; the Julia port
> still computes the old numerics (and could not be re-run, so the
> Fortran-vs-Julia rows and `julia-grcev-l10-results.csv` are the 2026-09-30
> ones). Re-running only the Fortran-vs-mHEM comparison with the new default
> gives mean differences of 0.09 / 0.13 / 1.39 % (ρ = 30 / 300 / 3000 Ω·m,
> 100 Hz–10 MHz) against 0.07 / 0.17 / 1.74 % below — no change in
> conclusion ([phase10-image-model.md](phase10-image-model.md)).

Cross-code and external-reference comparison for the 10 m horizontal
electrode. Three independent solutions of the same problem are compared with
each other and with the digitized full-wave curve of Grcev et al. 2018
(references.md [23], Fig. 12): the reference Fortran solver, the Julia port
(`julia/`), and the pure-Julia mHEM prototype from the TAGS project
(<https://github.com/pedrohnv/transient-analysis-grounding-systems>). The
mHEM results were contributed by acslima, who ran the prototype with inputs
aligned to the `common/` cases; the prototype itself is not vendored here,
only its tabulated output.

> **Note (2026-09-30).** The Julia figures below were measured with the
> *contributed prototype* of the port (fixed 64×64 midpoint rule for the
> geometry factors). The realigned port (ROADMAP Phase 8J) uses the Fortran
> adaptive quadrature and reproduces the Fortran results to within 1e-6
> (identical to the printed digit on these three cases,
> [julia/README.md](../../julia/README.md#conformance-status)), so the
> Fortran rows now describe both codes. Regenerating this writeup with the
> realigned port is Phase 8J item 5.

## Scope

Matched inputs for all three codes (`common/grcev_fig12_l10_rho*.json`):

- electrode length 10 m, radius 7 mm, burial depth 0.5 m, 40 segments
  (0.25 m each);
- soil relative permittivity 10, resistivity 30, 300 and 3000 Ω·m;
- 1 A injected at one endpoint;
- 100 Hz–10 MHz (the Fortran runs use 20 points/decade, the Julia and mHEM
  runs 401 log-spaced points; curves are compared on the Fortran grid, which
  is a subset of the finer one, and on the digitized frequencies by
  interpolation linear in log frequency).

## Results

Mean absolute percentage error of |Z| against the digitized full-wave curve
(number of digitized points in parentheses):

| ρ (Ω·m) | Band | Fortran | Julia | mHEM |
| ---: | :--- | ---: | ---: | ---: |
| 30 | 100 Hz–1 MHz (14) | 3.48% | 3.56% | 3.53% |
| 300 | 100 Hz–1 MHz (6) | 2.70% | 2.76% | 2.81% |
| 3000 | 100 Hz–1 MHz (9) | 2.23% | 2.30% | 3.29% |
| 30 | 100 Hz–10 MHz (19) | 3.11% | 3.19% | 3.13% |
| 300 | 100 Hz–10 MHz (16) | 3.67% | 3.63% | 3.78% |
| 3000 | 100 Hz–10 MHz (46) | 42.24% | 42.01% | 49.93% |

Pooled over all 29 digitized points at or below 1 MHz: Fortran 2.93%, Julia
3.00%, mHEM 3.31%.

Code-to-code difference of |Z| on the Fortran frequency grid (mean, and
maximum in parentheses):

| ρ (Ω·m) | Band | Fortran vs Julia | Fortran vs mHEM |
| ---: | :--- | ---: | ---: |
| 30 | ≤ 1 MHz | 0.09% (0.11%) | 0.04% (0.12%) |
| 300 | ≤ 1 MHz | 0.08% (0.09%) | 0.04% (0.38%) |
| 3000 | ≤ 1 MHz | 0.08% (0.08%) | 0.40% (4.08%) |
| 30 | 100 Hz–10 MHz | 0.09% (0.11%) | 0.07% (0.37%) |
| 300 | 100 Hz–10 MHz | 0.09% (0.13%) | 0.17% (1.51%) |
| 3000 | 100 Hz–10 MHz | 0.09% (0.17%) | 1.74% (14.00%) |

The complete metrics (median, maximum, relative L2) are in
[tupa-vs-mhem-metrics.csv](tupa-vs-mhem-metrics.csv); the curves are in
[../figures/tupa-vs-mhem-grcev-l10.svg](../figures/tupa-vs-mhem-grcev-l10.svg).

### Julia port: transient path

The Julia port also reproduces the Fortran time-domain result. On
`common/portela1997_transient.json` the largest deviation over the whole
record, relative to each waveform's peak, is 7.5·10⁻⁴ for the node voltages
and 1.6·10⁻⁹ / 8.8·10⁻⁵ for the electrode end currents i1/i2, both with no
anti-aliasing filter and with `antialiasStart` = 0.85 (ADR 0021) — the
filter's effect is the same in both codes.

## Interpretation

- **The Julia port is a faithful second implementation of the frequency-domain
  model**: it tracks the Fortran solver to under 0.2% at every frequency and
  resistivity, including through the 3000 Ω·m double resonance. The small,
  uniform ~0.09% offset is consistent with the different geometry
  integration (the port uses a fixed 64×64 midpoint rule for the mutual
  terms; the Fortran solver uses adaptive quadrature).
- **All three codes agree with each other far more closely than with the
  digitized reference.** Through 1 MHz the Fortran solver is marginally the
  closest to the full-wave curve; the spread between the codes (≤ 0.4% mean)
  is much smaller than their common ~2–3.5% distance from the reference, a
  level set largely by digitization and by the physics HEM neglects relative
  to a full-wave model.
- **At 3000 Ω·m above 1 MHz** all three reproduce the reference's
  notch/peak/notch structure, but the resonances sit at slightly different
  frequencies. Pointwise percentage error on such steep flanks is dominated
  by that horizontal shift (a few percent in frequency gives a large vertical
  error), so the 42–50% full-band MAPE is not a broadband amplitude bias.
  The Fortran/Julia solvers and mHEM differ most there (mean 1.74%, maximum
  14% on the grid). The contributor attributes this to the prototype's
  frequency-dependent transverse image reflection coefficient (TUPÃ uses a
  fixed positive image sign), to TUPÃ including conductor internal
  impedance (the prototype's example does not), and to the different
  geometry integrals; these have not been isolated individually.

## Limitations

- The reference is digitized from a plot, not tabulated source data, and only
  the magnitude can be compared externally; phase is compared between codes
  only.
- Only the matched 10 m electrode is covered. The 100 m Grcev cases use 400
  segments and warrant a separate performance-oriented run; runtimes here
  are not benchmarks.
- The mHEM output is static data (`mhem-grcev-l10-results.csv`); regenerating
  it needs the upstream project.

## Reproducing

```sh
./docs/validation/run_all.sh                # runs everything, incl. Julia if installed
# or, after the Fortran results exist and with the committed Julia/mHEM data:
.venv/bin/python docs/validation/compare_fortran_mhem.py
```

The Julia sweep is `julia/comparison/run_tupa_grcev_l10.jl` (writes
`julia-grcev-l10-results.csv`); `julia/comparison/run_mhem_grcev_l10.jl`
regenerates the mHEM data from a checkout of the upstream project.
