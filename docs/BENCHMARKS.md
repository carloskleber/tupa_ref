# TUPÃ — Benchmarks and validation status

What "validated" means for this project, which cases count, and where they
stand. The physics definition of each anchor is in
[theory.md §9](theory.md); this file tracks execution status and policy.

**Current status (2026-07-20): the end-to-end frequency-sweep and
time-domain pipeline are wired and green (ROADMAP Phases 0–6).** Six
literature comparisons (anchors 7–11, Lima's two grids sharing anchor 10)
have run against published papers' own figures — see
[validation/](validation/). The formula/tabulated
anchors (1, 2, 5) and the cross-code anchor (6) are still pending: no
Sunde/Dwight DC pin or Portela 1997/Grcev & Heimbach 1997 tabulated data
exists yet, and the TAGS/PRTL-mHEM submodules (below) haven't been run
against any case. Unit-level pins (sign conventions, geometry factors vs
quadrature oracles, Bessel limits, hand-solved 4×4 system) are green — see
`fortran/test/`.

## Validation anchors

| # | Case | Source | Tolerance | Status |
| --- | --- | --- | --- | --- |
| 1 | DC grounding resistance, buried horizontal conductor (Sunde/Dwight formula) | [10] + theory.md §9.1 | solver precision at the DC limit | pending — no dedicated test yet (the `rod_air.json` case gets an informal ≈1% DC check against the rod formula, common/README.md, but that isn't wired as this anchor) |
| 2 | Harmonic input impedance, 10 m conductor, 0.5 m depth, σ = 0.01 S/m, εr ≈ 10, 100 Hz–1 MHz | Portela 1997 [2] | 5 % (project decision, driven by curve readability) | pending; **no tabulated data exists** — see "Data provenance" |
| 3 | Grounding-electrode harmonic/impulse responses | Visacro & Soares 2005 [5] | — | **dropped as a data anchor** — the paper has no usable comparison data (author, 2026-07-05); kept as formulation reference only |
| 4 | Internal consistency: full Zeq vs reduced Z_g; reciprocity; passivity; quadrature vs closed forms | theory.md §9.4 | solver/quadrature precision | partially green (reciprocity, closed forms — unit tests); rest pending |
| 5 | Square grounding grids, harmonic impedance | Grcev & Heimbach 1997 [18] | qualitative curve match; requires Γ(ω) (ROADMAP P2) for the MHz range | pending Phase 7 geometry (grids) |
| 6 | Cross-code: identical cases through TAGS (and PRTL-mHEM) | references.md, companion codes | input impedance over the sweep; compare **physical outputs only** | pending submodule setup (below) |
| 7 | `tVisacroAlipioSoil` harmonic impedance, 60 m buried electrode, ρ0 = 100/300/1000/2400 Ω·m | Silva et al. 2025 [36], Fig. 3 | qualitative (digitized plot, no tabulated data — see [validation/](validation/)) | **executed** — [validation/silva2025-fig3.md](validation/silva2025-fig3.md): endpoints within ~1-4%, resonance dips/peaks match in location and depth for ρ0 = 1000/2400; mid-band knee off by up to ~29% for ρ0 = 100/300 (unconfirmed cause — see writeup) |
| 8 | Time-domain GPR, same 60 m electrode, MCS_FST#2 double-peaked first-stroke current | Silva et al. 2025 [36], Fig. 4 | qualitative (digitized plot) | **executed** — [validation/silva2025-fig4.md](validation/silva2025-fig4.md): both GPR humps agree within ~5% at every ρ0 (worst point +12.5%); mild +0.6-2% tail overestimate |
| 9 | Harmonic input impedance, buried horizontal electrodes, ℓ = 10/100 m, ρ1 = 30/300/3000 Ω·m, vs. the paper's own rigorous full-wave MoM model | Grcev et al. 2018 [23], Fig. 12 | qualitative (digitized plot) | **executed** — [validation/grcev-fig12.md](validation/grcev-fig12.md): DC/HF asymptotes within 0-4%, resonance shape/depth/location reproduced; excluding digitization outliers, every other point within ±11.5% |
| 10 | Harmonic input impedance, distribution-tower counterpoise (Case #9) and two square grounding grids (Cases #10/#11), ρ = 1000 Ω·m | Lima et al. 2020 [11], Figs. 6–7 | qualitative (digitized plot) | **executed** — [validation/lima-fig6.md](validation/lima-fig6.md): resonance locations within ~10-15% but a systematic −13 to −17% DC-to-2MHz gap (case geometry only partially stated by the paper); [validation/lima-fig7.md](validation/lima-fig7.md): both grids within ±7% below ~4 MHz, 10-19% through the ~6.5-7 MHz peak |
| 11 | Harmonic input impedance, single vertical electrode, L = 2 m, ρ = 5400 Ω·m, DC-100 MHz | Poljak & Doric 2006 [35], Fig. 4 | qualitative (digitized plot) | **executed** — [validation/poljak-fig4.md](validation/poljak-fig4.md): closest agreement of any comparison in this folder — DC plateau, roll-off and all four resonance lobes within ±10% almost everywhere |

Anchors 7-11 are the primary executable oracle for the near term. Anchor
2's original data does not exist in tabulated form (only the published
equations and figures) and anchor 5's source paper (Grcev & Heimbach 1997,
square grids) is a different reference from anchor 9's (Grcev et al. 2018,
single electrodes) — it remains unexecuted. Additional validation
references will be supplied by the author as they are processed.

## Cross-code benchmark setup (submodules added, no case run yet)

Decision (author, 2026-07-05): add the companion codes as git submodules
under a top-level `benchmarks/` folder:

```
benchmarks/
  tags/        ← github.com/pedrohnv/transient-analysis-grounding-systems (C99, GPLv3)
  prtl-mhem/   ← github.com/VitorLima1990/PRTL-mHEM (Python)
  cases/       ← per-case drivers + comparison scripts (this repo's own code) — not created yet
```

`benchmarks/tags` and `benchmarks/prtl-mhem` were added on 2026-07-05 (see
`.gitmodules`); there is no `benchmarks/cases/` yet and neither code has
been run against a TUPÃ case. TAGS builds locally (C99 + Cubature + LAPACK)
and accepts arbitrary electrode lists — run the Phase 2 buried conductor
(and later the Grcev grid) through both codes and compare input impedance
over the sweep (ROADMAP P3).

## Comparison policy

- **Compare physical outputs only** (input impedance, node voltages,
  time-domain waveforms) — never raw matrices: TAGS assembles $Z_\ell$ with
  $|\cos\theta|$ and its own incidence/system conventions (theory.md §9.6).
- **Compare moduli and waveforms, never raw phases**, when the reference is
  a legacy TUPÃ implementation — the legacy codes mix time-factor
  conventions term by term (theory.md §2, legacy caveat; ADR 0008).
- Every benchmark case must eventually exist as a `common/` JSON input plus
  an expected-output file with a stated tolerance, so all language
  implementations run it identically (ADR 0002).

## Performance benchmarks

None defined yet — correctness first (CONVENTIONS.md). When the fill loop
and sweep driver exist, the candidates are: geometry-pass cost vs segment
count ($O(n_s^2)$ quadratures), per-frequency solve cost
($O((n_n+2n_s)^3)$), and the P1 (mHEM 1-D kernel) and P6 (parallelism axis)
before/after measurements — see ROADMAP §7. Known baseline datum: the
debug-profile 2-D quadrature fill of a 10-segment case exceeds 9 minutes,
release profile is the only practical mode for quadrature-heavy runs
(ROADMAP §5).
