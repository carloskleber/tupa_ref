# ADR 0009 — Impedance-fill routines consume raw geometry factors

- **Status**: Accepted
- **Date**: 2026-07-05

## Context

theory.md §4.1/§4.3/§5 express each impedance entry as a product of medium
constants, propagation factors, direction cosines, length normalisation and
the real geometry factors:

$$Z_t(a,b) = c_E \left( e^{-\gamma\bar R} g \pm \Gamma_t e^{-\gamma\bar R_i} g_i \right) / (l_a l_b)$$
$$Z_\ell(a,b) = c_M \left( \cos\theta\, e^{-\gamma\bar R} g \pm \cos\theta_i\, \Gamma_\ell e^{-\gamma\bar R_i} g_i \right)$$

The ported `calcZSelf`/`calcZMutual` inherited the legacy calling
convention, in which the *caller* pre-multiplied $\cos\theta$ and
$1/(l_a l_b)$ into the "geometry factor" arguments, and the self term
carried no direct-path propagation factor at all. That implicit contract was
undocumented, and it is precisely where the C++ port had a known bug: its
self-term call site passed the longitudinal image factor in the
transversal-image argument slot. The documentation-reconstruction review
(2026-07-05) also found the missing direct-term $e^{-\gamma r_0}$ of
theory.md §4.3 — an undocumented deviation.

## Decision

`calcZSelf` and `calcZMutual` take the **raw outputs of
`mGeometry%buildGeometryMatrices`** — geometry factors `g`, `gi`, mean
distances `d`, `di`, direction cosines `cosTheta`, `cosThetaI`, and segment
lengths — and apply **every** theory factor internally, including the
direct-term propagation factor $e^{-\gamma \bar R}$ (with $\bar R = r_0$ for
the self term, per theory.md §4.3). Callers do no pre-scaling of any kind.

## Consequences

- The fill loop (ROADMAP Phase 2) becomes a mechanical iteration over the
  geometry matrices; there is no caller-side arithmetic left to get wrong,
  eliminating the argument-slot class of bug seen in the C++ port.
- Each routine is independently testable against hand-evaluated theory
  expressions; `test_mesh.f90` pins the air/soil image signs, the
  `cosThetaI` flip for vertical segments, the $1/(l_a l_b)$ normalisation
  and the mixed-media zero.
- The signatures are wider (13 arguments for `calcZMutual`). Accepted for the
  reference implementation; a future refactor may pass a per-pair geometry
  struct instead, without changing the contract.
- When the frequency-dependent reflection coefficient Γ(ω) lands (ROADMAP
  P2), it multiplies the image parcel inside these routines only — call
  sites are untouched.
