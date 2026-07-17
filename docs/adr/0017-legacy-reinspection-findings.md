# ADR 0017 — Findings from re-inspecting the legacy implementations (July 2026)

- **Status**: Recorded (informational — findings, not a single decision;
  moved here from ROADMAP.md §8 when the roadmap was tidied, 2026-07-17)
- **Date**: 2026-07-05 .. 2026-07-17

## Context

The original private repository was re-cloned in July 2026 with **both**
legacy MoM implementations side by side: the original Matlab code (the
dissertation implementation, 2003) and the C++/Fortran hybrid ported from
it. The Matlab version is the **model reference of record**: where
implementations disagree, the model is derived from it (see "Related
implementation notes" in [references.md](../references.md) for the two
codes' contents). This ADR records what the re-inspection established, so
the findings survive roadmap compaction. Finding numbers are stable —
other documents and code comments cite them as "ADR 0017 finding N"
(formerly "ROADMAP §8 finding N").

## Findings

1. **Γ(ω) images are original behaviour.** The Matlab's default mode
   computes the frequency-dependent image reflection coefficient
   (equal-permeability Fresnel form, applied to both `Z_t` and `Z_ℓ` image
   parcels) and keeps ideal images behind a `SOLO_IDEAL` switch; the C++
   port kept only the ideal limits. Feeds proposal P2 (ROADMAP §7;
   theory.md §5 updated accordingly).
2. **Self geometry factor bug lineage.** The legacy expression
   `r − h + l·log((1 + h)/r)`, `h = hypot(l, r)`, originates in the Matlab
   and was ported verbatim to the C++: it is half the correct `g_self`
   *and* has a literal `1` where `l` belongs (dimensionally inconsistent;
   the two coincide only for 1 m segments). The Phase 1 fix (theory.md
   §4.2, `mGeometry%selfGeometryFactor`, re-derived from the defining
   integral and verified against quadrature) stands.
3. **C++-only call-site bug.** The C++ self-term call passes the
   longitudinal image geometry factor in the transversal-image slot — do
   not use the C++ fill as an oracle for the diagonal terms. This bug
   class motivated ADR 0009 (theory factors inside `calcZSelf`/
   `calcZMutual`, no caller-side pre-scaling).
4. **All three solver layouts exist in the Matlab** as switchable methods:
   reduced nodal (two variants), augmented (LU / GMRES fallback), and a
   TAGS-style symmetric `(u, I_ℓ, I_t)` system ("método 5") — plus
   commented-out "Portela convention" sign variants. Ready-made
   consistency-test material for theory.md §6/§9.4.
5. **Convention mixing is real and must gate cross-validation.** The
   Matlab uses `σ + jωε` immittance and a decaying `e^{−γR}` (theory
   conventions), but a `−jωμ/4π` longitudinal constant and a `D` incidence
   stored as `−1` (compensated in solver assembly). Compare against it on
   moduli and time-domain waveforms only (theory.md §2 caveat; reinforces
   ADR 0008).
6. **Feature inventory is richer than previously documented**: tubular
   internal impedance (I/K Bessel) already implemented; two
   dispersive-soil routines (Portela power-law at ω₀ = 1 rad/s [30];
   Lima–Portela at 2π·1 MHz [31]); field/soil-potential/touch-voltage
   output classes (feeds P7); Heidler/double-exp/Jones/exponential/
   impulse/Portela-concave/sine signals (Phase 6 ported Heidler and the
   double-exponential family, including the Jones zero-initial-slope
   front); the element inventory listed under ROADMAP Phase 7; a direct
   inverse-Fourier quadrature over an interpolated spectrum besides the
   FFT driver (theory.md §8).
7. **Input formats**: the Matlab reads keyword-based text case files
   (`.caso`/`.est`); XML was a C++ addition. Neither constrains the JSON
   schema (ADR 0006), but the Matlab case files are the natural source
   when porting reference cases to `common/`.

## Consequences

- The Matlab implementation remains the reference of record; the C++ is
  consulted only for features the Matlab lacks (XML I/O, bundle/L-profile
  internal impedances, shielded wire).
- Findings 1 and 2 changed theory.md (§5, §4.2); finding 3 hardened the
  ADR 0009 interface; finding 5 gates every cross-code comparison
  (ADR 0008).
