# TUPÃ — Benchmarks and validation status

What "validated" means for this project, which cases count, and where they
stand. The physics definition of each anchor is in
[theory.md §9](theory.md); this file tracks execution status and policy.

**Current status (2026-07-05): no model output has been validated yet.**
The solver pipeline is not wired end-to-end (ROADMAP Phase 2), so every
anchor below is *defined* but *not executed*. Unit-level pins (sign
conventions, geometry factors vs quadrature oracles, Bessel limits,
hand-solved 4×4 system) are green — see `fortran/test/`.

## Validation anchors

| # | Case | Source | Tolerance | Status |
| --- | --- | --- | --- | --- |
| 1 | DC grounding resistance, buried horizontal conductor (Sunde/Dwight formula) | [10] + theory.md §9.1 | solver precision at the DC limit | pending Phase 2 |
| 2 | Harmonic input impedance, 10 m conductor, 0.5 m depth, σ = 0.01 S/m, εr ≈ 10, 100 Hz–1 MHz | Portela 1997 [2] | 5 % (project decision, driven by curve readability) | pending Phase 2; **no tabulated data exists** — see "Data provenance" |
| 3 | Grounding-electrode harmonic/impulse responses | Visacro & Soares 2005 [5] | — | **dropped as a data anchor** — the paper has no usable comparison data (author, 2026-07-05); kept as formulation reference only |
| 4 | Internal consistency: full Zeq vs reduced Z_g; reciprocity; passivity; quadrature vs closed forms | theory.md §9.4 | solver/quadrature precision | partially green (reciprocity, closed forms — unit tests); rest pending |
| 5 | Square grounding grids, harmonic impedance | Grcev & Heimbach 1997 [18] | qualitative curve match; requires Γ(ω) (ROADMAP P2) for the MHz range | pending Phase 7 geometry (grids) |
| 6 | Cross-code: identical cases through TAGS (and PRTL-mHEM) | references.md, companion codes | input impedance over the sweep; compare **physical outputs only** | pending submodule setup (below) |

Additional validation references will be supplied by the author as they are
processed; anchor 2's original data does not exist in tabulated form (only
the published equations and figures), so **anchor 6 is the primary
executable oracle** for the near term.

## Cross-code benchmark setup (planned)

Decision (author, 2026-07-05): add the companion codes as git submodules
under a top-level `benchmarks/` folder:

```
benchmarks/
  tags/        ← github.com/pedrohnv/transient-analysis-grounding-systems (C99, GPLv3)
  prtl-mhem/   ← github.com/VitorLima1990/PRTL-mHEM (Python)
  cases/       ← per-case drivers + comparison scripts (this repo's own code)
```

```bash
git submodule add https://github.com/pedrohnv/transient-analysis-grounding-systems benchmarks/tags
git submodule add https://github.com/VitorLima1990/PRTL-mHEM benchmarks/prtl-mhem
```

TAGS builds locally (C99 + Cubature + LAPACK) and accepts arbitrary
electrode lists — run the Phase 2 buried conductor (and later the Grcev
grid) through both codes and compare input impedance over the sweep
(ROADMAP P3).

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
