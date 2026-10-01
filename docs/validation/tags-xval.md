# Cross-code validation against TAGS (ROADMAP Phase 10 item 5, §7 P3)

**Reference**: TAGS (pedrohnv, C99, `benchmarks/tags` submodule, commit
`224a6c9`) — the independent executable oracle for the mHEM formulation
(BENCHMARKS anchor 6). It is a different code base (C99, hcubature
quadrature, `zsysv` on the nodal admittance system), written from the same
HEM literature [11, 18], so agreement checks TUPÃ's kernels, image model and
nodal assembly rather than a shared source.

**Method.** [`benchmarks/tags-xval/xval.py`](../../benchmarks/tags-xval/xval.py)
exports each case's discretisation from the Rust implementation
(`--dump-structure`: nodes, electrodes, radii), so both codes see the same
segments, runs TAGS through a small driver
([`tags_hem.c`](../../benchmarks/tags-xval/tags_hem.c), adapted from the
TAGS example `harmonic_impedance.c`: mHEM integrals, homogeneous *linear*
soil, no conductor internal impedance) and TUPÃ's Fortran CLI, and compares
the driving-point impedance `Zin(f) = V(source)/1 A` over the sweep —
physical outputs only (BENCHMARKS.md comparison policy). Build and run:

```sh
git submodule update --init --recursive benchmarks/tags
make -C benchmarks/tags-xval                      # needs gcc, LAPACK, OpenBLAS, FFTW3
python3 benchmarks/tags-xval/xval.py --fortran-bin fortran/build/<profile>/app/Tupa
```

Raw sweeps: [`benchmarks/tags-xval/out/xval_results.csv`](../../benchmarks/tags-xval/out/xval_results.csv).

## Two convention differences, both found by this exercise

| Difference | TAGS | TUPÃ | Effect | Handling |
| --- | --- | --- | --- | --- |
| Longitudinal direction cosine | `|cos θ|` for the direct **and** the image parcel (`electrode.c`: `cost = fabs(cost)`) | signed `cos θ`, as the Matlab reference and theory.md §5 | Anti-parallel pairs (a closed loop is oriented around itself) and the image of a vertical electrode (cos θ_i = −1) differ | Electrodes are oriented towards +x/+y/+z for TAGS, which makes every direct cosine non-negative; a `signed` option of the driver restores the sign of the image parcel |
| Longitudinal image coefficient | `Γ_ℓ = 1` (an independent parameter) | `Γ(ω)` on both image parcels (Matlab reference, PRTL-mHEM) | 0.08–0.22 % up to 1 MHz on the cases below, 1.3 % at 10 MHz on the Grcev 10 m electrode | The driver takes `Γ_ℓ = Γ_t` for the main comparison and `Γ_ℓ = 1` as a variant |

Without the orientation step the loop case `grid` differs by 3.8–4.2 % at
100 kHz: a loop oriented around itself has
anti-parallel sides, where TAGS' `|cos θ|` flips the sign of their mutual
inductance. With every side oriented the same way (physically the same
structure) it agrees to 0.06 %. This is theory.md §9.6's warning made
quantitative.

## Result

Homogeneous linear soil, `Zin` modulus (%) and phase (degrees) of TUPÃ with
the default `Γ(ω)` images against TAGS with `Γ_ℓ = Γ_t`; maximum over the
frequencies of each band. The last column restores TAGS' image-parcel sign
(the `signed` option).

| Case | Electrodes · sweep | ≤ 100 kHz | ≤ 1 MHz | full band | full band, image cosine signed in TAGS |
| --- | --- | --- | --- | --- | --- |
| `portela1997` (10 m, 0.5 m deep) | 10 · 10 Hz–1 MHz | 0.031 % / 0.003° | 0.032 % / 0.085° | same | same |
| `rod` (3 m vertical) | 6 · 10 Hz–1 MHz | 0.086 % / 0.19° | 1.6 % / 0.99° | same | 0.10 % / 0.08° |
| `grid` (10 × 10 m loop) | 4 · 100 Hz–100 kHz | 0.060 % / 0.03° | same | same | same |
| `grcev_fig12_l10_rho300` | 40 · 100 Hz–10 MHz | 0.006 % / 0.002° | 0.026 % / 0.026° | 0.040 % / 0.19° | same |
| `grcev_fig12_l100_rho300` | 400 · 100 Hz–10 MHz | 0.30 % / 0.075° | 0.30 % / 0.075° | 0.30 % / 0.20° | same |
| `lima_fig6` (tower footing, 1000 Ω·m) | 78 · 100 Hz–10 MHz | 0.006 % / 0.018° | 0.040 % / 0.18° | 11 % / 69° | **1.6 % / 0.5°** |

Reading the table:

- **Below 1 MHz the codes agree to 0.3 % or better** wherever the image
  cosine is positive (`portela1997` 0.03 %, `grid` 0.06 %, the 10 m Grcev
  electrode 0.03 %, the 100 m one 0.3 %). The rod and the tower footing,
  whose vertical members have a negative image cosine, are off by 1.6 % and
  0.04 % (below 1 MHz); with TAGS' image sign restored the rod falls to
  0.1 % over its whole band and `lima_fig6` to 0.009 % below 1 MHz.
- **`lima_fig6`'s 11 % full-band figure is the image-cosine convention**
  acting near the 5–10 MHz resonances of a 78-segment footing with long
  vertical members; with the sign restored the worst point is 1.6 % (at
  8.6 MHz, next to a zero crossing of `Re Zin`), 146 of the 150 points are
  within 0.1 %, and the median deviation is 0.003 %.
- **`grcev_fig12_l100_rho300`: 0.3 % from DC upward.** TUPÃ's number is
  converged (6.40327502 Ω at 100 Hz for `--epsrel` 1e-6 and 1e-10, both
  kernels — the pairs are closed-form parallel); the 0.3 % is TAGS'
  hcubature tolerance (`abs 1e-6, rel 1e-4` per pair) on 400 segments. Both
  codes sit within 1.5 % of Sunde's R = ρ/(πL)(ln(2L/√(dh)) − 1) = 6.47 Ω.
- **Image model.** `Γ(ω)` vs ideal images, each against TAGS with
  `Γ_ℓ = Γ_t`: below 1 MHz the ideal model is 0.01–0.29 % off on the Grcev
  case and 1.6 % off on `lima_fig6`; the `Γ(ω)` default is 0.03 % and 0.04 %.
  Over the full band the ideal model is 2 % (Grcev 10 m) and 30 % (`lima_fig6`)
  off, the default 0.04 % and 11 % (1.6 % with the cosine sign). TAGS itself
  implements `Γ_t(ω)`, so this checks the implementation of the default, not
  its physics; the physics is checked against the published curves in
  [phase10-image-model.md](phase10-image-model.md).
- **Passivity near 7–9 MHz is shared.** `Re Zin` is negative at 7.3, 7.9 and
  8.6 MHz on `lima_fig6` (−0.70, −1.95, −1.10 Ω with `Γ(ω)`; −1.55, −2.93,
  −2.42 Ω with ideal images) — and TAGS (image cosine signed) gives the same (−0.69, −1.97,
  −1.15 Ω). It is a property of the mean-distance factorisation of the mHEM
  family above ~4 MHz (theory.md §4.1, Lima et al. [11]), not a TUPÃ defect;
  `Γ(ω)` roughly halves it.

## What this does and does not settle

- It **confirms** the frequency-domain kernels, the image model and the nodal
  assembly against an independent code, at the accuracy the factorisation
  allows, on a conductor, a vertical rod, a loop, two long electrodes and a
  78-segment footing.
- It **does not** cover dispersive soils (TAGS' own Alipio–Visacro fit
  differs from the one in `common/silva2025_*`), the time domain (TAGS'
  NLT is a separate implementation and no TAGS transient was run — see
  ADR 0024 §7 for why the NLT default is therefore left on the FFT),
  voltage sources or catenary elements.
- Open follow-up: a TAGS time-domain case (e.g. its `visacro57emc01`
  example) to compare against TUPÃ's FFT and NLT outputs; a signed-`cos θ`
  note belongs in any future TAGS-based benchmark.
