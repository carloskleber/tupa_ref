# Effect of the Phase 10 image model on the published-curve comparisons

ROADMAP Phase 10 item 2 ([ADR 0024](../adr/0024-phase10-numerics.md) §2) makes
the frequency-dependent image reflection coefficient `Γ(ω)` the default for
buried conductors. Every writeup in this folder written before 2026-10-01
([grcev-fig12.md](grcev-fig12.md), [lima-fig6.md](lima-fig6.md),
[lima-fig7.md](lima-fig7.md), [poljak-fig4.md](poljak-fig4.md),
[silva2025-fig3.md](silva2025-fig3.md), [silva2025-fig4.md](silva2025-fig4.md),
[tupa-vs-mhem.md](tupa-vs-mhem.md)) used ideal images (`Γ = ±1`); their
per-point tables and prose keep those numbers, and the figures under
`../figures/` were regenerated with the new default. This note measures what
changed, so a reader can tell which of the old statements still hold.

**Method.** [`phase10_image_model_check.py`](phase10_image_model_check.py)
runs each comparison case twice with the Fortran CLI
(`--image-model ideal`, `--image-model frequency-dependent`; 1-D kernel in
both), interpolates TUPÃ's |Z| log-log at the digitized frequencies of the
same xlsx files the plot scripts use, and reports the mean absolute
percentage error (MAPE) and the worst signed point, below 1 MHz and over the
whole digitized band. "n" counts the digitized points in each band.

| Comparison | n (≤ 1 MHz / all) | MAPE ≤ 1 MHz: ideal → Γ(ω) | worst ≤ 1 MHz | MAPE all | worst all |
| --- | --- | --- | --- | --- | --- |
| Grcev Fig. 12, ℓ = 10 m, ρ = 30 | 14 / 19 | 3.48 → 3.48 % | −7.4 → −7.4 % | 3.11 → 3.12 % | −7.4 → −7.4 % |
| Grcev Fig. 12, ℓ = 10 m, ρ = 300 | 6 / 16 | 2.70 → 2.75 % | −5.2 → −5.2 % | 3.66 → 3.77 % | −7.2 → −8.7 % |
| Grcev Fig. 12, ℓ = 10 m, ρ = 3000 | 9 / 46 | 2.24 → 3.23 % | −3.5 → −5.5 % | 40.65 → 39.10 % | +198.8 → +190.3 % |
| Grcev Fig. 12, ℓ = 100 m, ρ = 30 | 24 / 30 | 4.23 → 4.23 % | +14.3 → +14.3 % | 3.85 → 3.86 % | +14.3 → +14.3 % |
| Grcev Fig. 12, ℓ = 100 m, ρ = 300 | 23 / 30 | 5.60 → 5.57 % | +13.3 → +13.3 % | 4.53 → 4.52 % | +13.3 → +13.3 % |
| Grcev Fig. 12, ℓ = 100 m, ρ = 3000 | 17 / 37 | 10.34 → 8.72 % | +24.2 → +22.9 % | 7.34 → 5.44 % | +24.2 → +22.9 % |
| Lima Fig. 6 (case 9) | 4 / 20 | 14.68 → 15.22 % | −15.4 → −16.6 % | 38.53 → 38.90 % | +102.9 → +93.0 % |
| Lima Fig. 7 (case 10, 20 × 20 m) | 8 / 23 | 2.33 → 1.94 % | +6.6 → +5.1 % | 5.92 → 4.64 % | +15.4 → −15.3 % |
| Lima Fig. 7 (case 11, 40 × 40 m) | 17 / 34 | 3.56 → 3.25 % | +5.8 → +5.0 % | 5.63 → 4.13 % | +19.2 → +15.5 % |
| Poljak–Doric Fig. 4 | 6 / 108 | 4.25 → 4.01 % | +8.9 → +7.8 % | 8.03 → 7.73 % | −65.9 → −66.1 % |
| Silva Fig. 3, ρ0 = 100 | 10 / 14 | 1.18 → 1.19 % | −2.1 → −2.1 % | 1.28 → 1.33 % | −2.1 → −2.1 % |
| Silva Fig. 3, ρ0 = 300 | 15 / 21 | 1.17 → 1.21 % | −2.0 → −2.1 % | 1.31 → 1.47 % | −2.4 → −2.9 % |
| Silva Fig. 3, ρ0 = 1000 | 35 / 42 | 0.74 → 0.90 % | −1.7 → −2.0 % | 0.89 → 1.17 % | −2.5 → −3.4 % |
| Silva Fig. 3, ρ0 = 2400 | 53 / 65 | 0.80 → 1.11 % | −2.9 → −3.2 % | 0.91 → 1.36 % | −2.9 → −3.8 % |

## Findings

- **The change is small against the digitization and model error.** Every
  MAPE moves by less than 2 points (Grcev ℓ = 100 m, ρ = 3000 Ω·m:
  10.34 → 8.72 % below 1 MHz, 7.34 → 5.44 % over the band); the
  low-resistivity cases, where the image is nearly ideal anyway
  (`σ ≫ ωε0`), barely move (Grcev ρ = 30 and ρ = 300 at ℓ = 100 m, ρ = 30 at
  ℓ = 10 m). Every qualitative statement in the older writeups —
  asymptotes, resonance locations, the shape of the discrepancy — stands.
- **Where it helps:** the grids of Lima Fig. 7 (MAPE 5.9 → 4.6 % and
  5.6 → 4.1 % over the band; worst point of case 11 +19 → +15.5 %) and the long, resistive
  Grcev electrode (ℓ = 100 m, ρ = 3000 Ω·m). These are the cases the
  theory predicts: large structures in resistive soil at MHz, where
  `|Γ_t| < 1` with phase.
- **Where it does not:** the Silva `alipio-visacro` curves move by
  +0.01 to +0.45 points of MAPE (still ≤ 1.5 %, worst point within 4 %), the
  Lima Fig. 6 tower footing by +0.4–0.5 point (its 15 % gap is geometry the paper
  does not state — see [lima-fig6.md](lima-fig6.md)), and the 10 m,
  ρ = 3000 Ω·m electrode below 1 MHz by +1.0 point (2.2 → 3.2 %; both within
  the 3–6 % band of the digitization). The new default is the original
  Matlab's and agrees with TAGS (see [tags-xval.md](tags-xval.md)); the
  published curves do not prefer either model on these cases.
- **Poljak–Doric Fig. 4** (the case theory.md §5 named the natural
  regression test for `Γ(ω)`: 100 MHz in 5400 Ω·m soil, `Γ_t ≈ 0.82`):
  MAPE 8.03 → 7.73 %, and the worst-point statistics are unchanged because
  they sit at the steep null crossings (−66 % at a null). The 1 MHz-and-below
  MAPE improves 4.25 → 4.01 %.
- **Cross-code, [tupa-vs-mhem.md](tupa-vs-mhem.md).** Re-running its
  Fortran-vs-PRTL-mHEM comparison with the new default gives mean
  differences of 0.09 / 0.13 / 1.39 % (ρ = 30 / 300 / 3000 Ω·m, 100 Hz–10 MHz)
  against 0.07 / 0.17 / 1.74 % before — no change in conclusion; the
  Fortran-vs-Julia rows of that note compare against Julia results produced
  before Phase 10 (the Julia port still computes ideal images), so they are
  not regenerated here.
