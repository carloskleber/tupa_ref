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

## Amendment 2026-11-20 — passive mixed constants (decision 3 replaced)

**Context.** `common/aerial_buried_cross` puts a 1 km open-ended line 30 m
above ground parallel to a wire buried 1 m deep at 50 m. With the kernel above,
a pulse injected on the line grew on every round trip. The harmonic input
impedance had Re(Z_in) < 0 at 64 of 100 frequencies (10 kHz–3 MHz, down to
−9.6 kΩ), and the direct-only `Zℓ` of decision 3 alone was the cause (log
P14-2026-11-20). The earlier checks could not see it: the fixtures only couple
vertical conductors, and the limits tested are local.

**Why decision 3 was wrong.** The model applies the electric image
coefficient Γ to the `Zℓ` image parcels too (theory.md §5.0), so the air
self-term already treats the soil as screening the vector potential
(Γ_a ≈ −1, an earth return at the surface). At the interface the two
self-blocks become `c_M τ_a g` and `c_M τ_s g`, with τ = 1 + Γ (τ_a ≈ 0,
τ_s ≈ 2). A 2×2 kernel with that diagonal is positive semidefinite only if its
off-diagonal factor κ satisfies κ² ≤ τ_a τ_s. Decision 3's κ = 1 broke that
bound by orders of magnitude. The "μ continuous" argument holds for the
physical field but not for the screened field that this model's self-terms
represent.

**Decision.** Both mixed constants are the geometric mean of the two media's
transmitted constants:

    c_E× = √(c_E^a τ_a) · √(c_E^s τ_s),    c_M× = c̄_M · √τ_a · √τ_s,    τ = 1 + Γ

1. Under Γ(ω), `c_E×` is unchanged: `c_E τ = 1/(2π(W_a+W_s))` from either
   side, so decision 1 stands.
2. `c_M×` sits exactly at the semidefinite bound, so it is the largest
   coupling consistent with the self-terms. It is reciprocal, and it reduces
   to `c_M` for equal media.
3. Under `imageModel: "ideal"` τ_a = 0, so both constants vanish: a perfectly
   screening ground decouples the two media. Before this amendment `c_E×` was
   nonzero there.
4. Each root is taken separately. Every factor has a non-negative real part
   for s in the closed right half-plane, so the NLT path stays
   conjugate-symmetric.
5. Retardation (decision 2) is unchanged.

**Rejected.** (a) Dropping the retardation of the mixed terms: passive in the
probe, but the coupling between distant segments becomes instantaneous, and
the induced wave on the buried wire must travel with the line's wave.
(b) A transmitted vector potential `τ_a × direct` from the air side only:
not reciprocal, because `c_M` is the same in both media. (c) Removing the Γ
image from `Zℓ` (Γ_ℓ = 0, the low-frequency limit): it moves every fixture
with a conductor in air, and it belongs to an earth-return model, not to this
fix.

**Consequences.**
- `mMesh%calcMixedConstants`, called from `calcParamW` and
  `calcParamLaplace` after the image coefficients.
- `rod_air_cross_expected.csv` regenerated. The coupling effect on `rod_air`
  is now 7.9e-5 at 10 kHz and 9.5e-3 at 1 MHz (it was 5.7e-2 at 1 MHz). On
  `channel_tower_cross` it is 0.21 % at 10 kHz and 7.8 % at 1 MHz (it was
  9.5 % at 1 MHz).
- New tests in `test_crossmedium`: the `Zℓ` formula, the bound, the
  equal-media `Zℓ` limit, the ideal-image zero, Laplace conjugate symmetry,
  and a passivity sweep of `common/aerial_buried_cross_short`.
- **Residual.** The straight-ray retardation leaves isolated
  negative-resistance points near the MHz series resonances: at worst
  −0.23 % of |Z_in| on the short probe and −7 Ω against |Z_in| ≈ 76 Ω on the
  1 km case. The test bounds the first at 1 % of |Z_in|.
- **Limit.** Consistent with the screened self-terms, the inductive coupling
  vanishes as ω → 0. The deep earth-return coupling of power lines to
  pipelines at power frequency (Carson, Sunde) is outside the model (theory.md
  §5.1).
