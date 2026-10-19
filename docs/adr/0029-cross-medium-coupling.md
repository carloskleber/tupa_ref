# ADR 0029 — Air–soil cross-medium coupling by a transmitted-field term (ROADMAP Phase 14)

- **Status**: Accepted
- **Date**: 2026-10-19
- **Amends**: [ADR 0005](0005-two-media-image-method.md) (mixed-media pairs are no longer
  always neglected), [ADR 0013](0013-input-schema-sources-frequencies-outputs.md)
  (one new `numerics` key)

## Context

ADR 0005 sets the coupling between a segment in air and one in soil to zero, as
both legacy codes do. The Matlab's cross-medium routine was left unfinished
(ADR 0017), so there was nothing to port. Studies that put a lightning channel
or a tower above buried electrodes need the coupling: the channel changes the
GPR and the touch and step voltages beyond the current it injects.

## Decision

Author Q&A of 2026-10-19:

1. **Kernel.** The exact static transmitted term, `cE× = 1/(2π(W_a+W_s))`, direct
   geometry factor only, no image parcel (theory.md §5.1).
2. **Retardation.** Piecewise straight ray, split where the midpoint ray crosses
   `z = 0`: `e^{−γ_a R_a − γ_s R_s}`. Reciprocal, and it reduces to `e^{−γR}` for
   equal media. Salari's `e^{−(γ₂−γ₁)η}` correction and an observation-medium-only
   `γ` were rejected.
3. **Longitudinal.** `Zℓ` gets the direct term only, `cM× cosθ e^{…} g`
   (`cM×` the mean of the two media's `cM`: `μ` is assumed equal).
4. **Switch.** `numerics.crossMedium: "none" | "transmission"`, default
   `"none"` so no existing fixture moves. Flipping the default is a later,
   reviewed step, as for `Γ(ω)` in ADR 0024.
5. **Scope.** Fortran only. The Rust and Julia loaders refuse `"transmission"`
   explicitly (as for Phase 10b/10d/11) rather than run without the coupling.

## Consequences

- **Where.** `buildGeometryMatrices(..., crossMedium)` computes `G`, `Rbar`,
  `cosTheta` for mixed pairs; `Rbari` then carries the *air part of the ray*
  (the image outputs stay 0). `tMesh%crossMedium`, `cEMixed`, `cMMixed`;
  `calcZMutual`'s mixed branch; `tStudy%crossMedium` from the `numerics` reader.
  `mPotentials` adds the same term to the observation potentials.
- **Self terms** are untouched: a segment's image is always in its own medium.
- **Verification.** `test_crossmedium.f90` (hand-evaluated kernel, reciprocity,
  equal-media limit, geometry bookkeeping, channel + buried rod trend);
  `test_potentials.f90` (continuity of ψ across `z = 0`, on versus off);
  fixtures `rod_air_cross` (golden) and `channel_tower_cross`.
- **Limits.** Not a Sommerfeld solution. The straight-ray retardation and the
  missing reflected vector potential are low-frequency approximations that
  degrade toward MHz, in poorly conducting soil and for long horizontal
  air segments close to the ground (lateral-wave regime). Stated in theory.md §5.1.
- **Not done.** The Rust and Julia ports; the GUI does not expose the key.
