# ADR 0004 — Frequency-independent geometry factors with mean-distance propagation

- **Status**: Accepted
- **Date**: 2026-07-03

## Context

The exact mutual impedance integrals contain `e^{−γR}/R` and would have to be
re-evaluated numerically for every segment pair at every frequency (the plain
HEM approach). The dissertation [3] separates the integral into a real,
frequency-independent geometry factor `g(a,b) = ∬ dl dl' / R` and a propagation
factor `e^{−γR̄}` evaluated at the mid-point distance.

## Decision

Adopt the geometry-factor separation as the core numerical strategy:

- Precompute once per geometry: `G` (double integrals of `1/R`), image-geometry
  `Gᵢ`, mean distances `R̄`, `R̄ᵢ`, direction cosines, `1/(l_a l_b)`.
- Per frequency: only medium constants, propagation factors, and the block
  assembly.
- Evaluate `g` by adaptive Gauss–Kronrod 7/15 double quadrature in general
  position; use closed-form expressions for the self term (and later for
  parallel/orthogonal pairs) as fast paths and test oracles. (Precedent in
  the Matlab reference: closed form for parallel pairs — including collinear
  special cases — with numeric-quadrature fallback on failure; the
  orthogonal closed form is a stub there, and its self-term formula is the
  known-buggy one of theory.md §4.2.)

## Consequences

- Broadband sweeps cost one geometry pass plus cheap per-frequency assembly —
  the decisive speedup that made the original code practical.
- Validity requires segments short vs. the in-medium wavelength (≲ λ/10 at the
  highest frequency); the mesh generator must enforce or warn about this.
- The approximation is a documented, testable deviation from the exact
  integral: keep one slow "exact" integration path in the test suite to
  quantify the error on reference cases.
- OpenMP parallelism applies naturally to the geometry-factor fill loop
  (embarrassingly parallel over segment pairs).
