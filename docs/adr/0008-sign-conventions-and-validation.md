# ADR 0008 — Single sign-convention set, enforced by validation tests

- **Status**: Accepted
- **Date**: 2026-07-03

## Context

The sources this project draws from use conflicting conventions:

- Portela [1] uses the physics time factor `e^{−iωt}` (immittance `σ − iωε`,
  factor `e^{+ikR}`); the dissertation follows it.
- Portela's A/B/C/D matrices assume `i₂` flowing *out* of the segment; the
  original code's comments assume both end currents flowing *into* it.
- The original C++ mesh code and its embedded Fortran module disagree with
  each other in the propagation-factor exponent (`exp(−d·k)` vs `exp(−i·d·k)`),
  and the current Fortran port introduced further sign changes (propagation
  constant imaginary part, air-image longitudinal sign, C/D matrix signs).

Sign errors here don't crash — they produce plausible-looking wrong answers.

> **Update (2026-07-05).** Re-inspection of the original Matlab (model
> reference of record) added more examples of exactly this hazard: it mixes
> conventions term by term (immittance `σ + jωε` and decaying propagation
> factor per `e^{+jωt}`, but longitudinal constant `−jωμ/4π`), stores the
> `D` incidence with entries `−1` and compensates in solver assembly, and
> keeps commented-out "Portela convention" sign variants beside the active
> code. Consequence for validation: compare against the legacy codes on
> impedance moduli and time-domain waveforms only, never raw phases
> (theory.md §2, legacy caveat).

## Decision

- Adopt **one** convention set, stated in [theory.md](../theory.md) §2:
  `e^{+jωt}`, immittance `σ + jωε`, `γ = √(jωμ(σ+jωε))` with `Re γ ≥ 0`,
  propagation factor `e^{−γR}`, both end currents into the segment, A/B/C/D
  as in theory.md §6.
- Theory.md is **normative**: papers are mapped to it (conjugation table in
  §2), never mixed into code directly.
- Every sign-sensitive rule is pinned by a test with an analytically known
  answer: propagation factor decay, DC grounding resistance of a buried rod
  (image sign), reciprocity/symmetry, full vs. reduced solve equivalence.

## Consequences

- The existing Fortran port must be **audited against theory.md** before any
  new solver work (implementation plan, Phase 0); discrepancies are resolved
  toward theory.md, not toward whichever source file was copied last.
- Cross-language implementations verify convention compliance for free by
  running the common reference cases.
