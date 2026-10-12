# ADR 0028 — Lightning-induced voltages: the lumped branch element and the Nucci anchor (ROADMAP Phase 10d)

- **Status**: Accepted
- **Date**: 2026-10-11
- **Amends**: [ADR 0013](0013-input-schema-sources-frequencies-outputs.md)
  (one new element type), [ADR 0018](0018-author-interview-decisions-2026-07.md)
  (pulls ROADMAP Phase 12 item 3 forward)

## Context

Seven lightning papers entered the author's reference library on 2026-10-11
(Nucci et al. 1993; the two CIGRE reviews, Nucci 1995; Rachidi et al. 1996 and
1999; Master et al. 1981; Uman & McLain 1969 — references.md [75]–[81]). Together
they give a complete, tabulated validation case for lightning-*induced*
voltages: a 1 km, 10 m high line matched at both ends, a subsequent stroke 50 m
from its centre, perfect ground. Everything the case needs already exists in
TUPÃ (wires in air, ideal images, the loaded channel of ADR 0025) except one
thing: a matched termination, which is a resistor from the line end to ground.
That is the series-RLC element planned as ROADMAP Phase 12 item 3.

The author interview of 2026-10-11 settled the scope. The decisions are recorded
here.

## Decision

1. **Theory.** New theory.md §4.6 (sub-sections 4.6.1–4.6.5): the fields of the
   channel, the Faraday-law derivation of the Taylor, Agrawal, Rusck and
   Chowdhuri–Gross coupling forms, what the HEM line model contains (the
   Rusck structure, because a vertical channel and a horizontal line are
   orthogonal in `Z_ℓ`), the lossy-ground mechanisms and the validation anchor
   (§9 item 9). Errata and conflicts found in the sources are listed in §4.6.5
   and in references.md.
2. **The `branch` element** (`"type": "branch"`, Fortran only). A lumped series
   `R + sL + 1/(sC)` between node `from` and node `to`; an absent `to` means
   remote earth. It plants no node and no segment. The study layer stamps the
   branch admittance `Y(s)` into the Kirchhoff rows of the augmented system
   after `calcFreq2` (`mMesh::stampBranch`), on the harmonic and the Laplace
   path alike. This is the nodal-admittance equivalent of the legacy
   "Impedancia" mechanism (an extra unknown with a zeroed `Z_t` row,
   theory.md §6) and needs no extra unknown, so there is no singularity to
   guard. A branch of zero impedance is refused at load time; a short circuit
   is a small resistance. The DC pin demanded by theory.md §6 holds: a series
   resistor in front of a grounded rod, and a shunt resistor in parallel with
   it, reproduce the closed forms to 1e-9 (`test_branch.f90`).
3. **Matched ends are a resistor at the wire end (author decision), with the
   riser variant run alongside.** The interview chose the resistor at the wire
   end (variant A), not vertical risers. That choice is implemented and is the
   committed baseline `common/induced_nucci1993.json`. theory.md §4.6.3 shows
   it is the Rusck boundary condition without the riser term, so it differs
   from the reference's boundary condition (Agrawal's, with `h·E_z` at the
   riser) by an amount that depends on the geometry. Variant B (vertical wire
   from the line end down to the ground, resistor at its foot) costs two extra
   `line` elements and no code, and is the like-for-like check; it is committed
   as `common/induced_nucci1993_risers.json`. **This runs both, which goes one
   step beyond the interview**: the pass criteria gate variant B, and variant A
   is reported against the same numbers.
4. **The channel model gap is stated, not closed.** The loaded wire is a
   TL-type model with solved current, the reference is MTL with prescribed
   current. The channel is calibrated to the speed with the existing
   calibration, and `R' ≈ 2 Z_c/λ` is the starting value that matches the MTL
   decay to first order. The tolerance is the spread between return-stroke
   models in the reference paper itself (12 % in peak, 37 % in steepness).
5. **Anchors**: the base case; the trends of Table III (current peak, `di/dt`,
   speed); the formulation test of Fig. 14 (a 500 m line with the stroke on its
   prolongation); the lossy-ground variant of Rachidi et al. 1996, run without a
   tolerance as a documented limitation (the §5 images are quasi-static and
   lack the wave effect behind the Cooray–Rubinstein correction).
6. **Rust, Julia and the GUI lag.** The Rust and Julia loaders refuse a
   `branch` element explicitly (rather than skip it with a warning, which would
   run a case without its terminations); their tests list the two new cases as
   lagging. The GUI skips the element with a logged warning. The cases carry no
   golden `_expected.csv`: they are published-curve-style comparisons (the
   reference values come from the paper's tables), reproduced by
   `docs/validation/induced_checks.py` (validation/induced-validation.md).

## Consequences

- `tStructure` gains `branches`/`branchCount`; `fillAtFrequency` stamps them.
  No result file changes: the voltage across a branch to earth is the node
  voltage, and the branch current is `u/Z`. A `branches` results block and a
  branch current output are not added; a follow-up if a case needs them.
- A node with only a branch attached is legal (an isolated source node feeding
  a series branch). It is not a segment node, so geometry never sees it.
- Interior voltages of a line are reported as the scalar potential `φ`, not the
  Agrawal total voltage; converting needs `A_z^i`, which needs the field-vector
  post-processing still open from Phase 11. The anchor gates terminations only
  (theory.md §4.6.3).
- `fpm test` under the usual `-O2 -fopenmp` flags: the new tests pass and the
  one failure of the suite is the `channel_loaded` fixture that already failed
  at the previous commit.
