# ADR 0003 — Solve the full augmented Z_eq system with ZGESV

- **Status**: Accepted
- **Date**: 2026-07-03

## Context

The nodal HEM equations admit two solution strategies (theory.md §6):

1. **Full augmented system**: one `(nn + 2·ns)` dense complex LU solve of
   `Z_eq x = y`, yielding voltages and both end-current vectors directly.
2. **Reduced nodal form**: eliminate currents analytically, solving
   `ns × ns` subsystems (`Zₗ⁻¹A`, `Zₜ⁻¹B`) plus one `nn × nn` system for
   `Z_g`, then recover currents from `S₁`, `S₂`. This is what Portela's
   original TRANSMATER-era formulation and the legacy `solMalha` routine did.

The original C++/Fortran code contains both paths; only the augmented one
(`calcFreq2` + `injetaSinalF`) was completed and used.

## Decision

The reference implementation solves the **full augmented system** with LAPACK
`ZGESV`. The reduced form is kept in the theory doc as a consistency check and
as a future optimisation, not implemented initially.

## Consequences

- Simplest possible solver code: assemble blocks, one library call, unpack.
  Correctness is auditable against the block matrix in the theory doc.
- Memory and time scale as `(nn + 2ns)²` and `(nn + 2ns)³`; fine for reference
  cases (hundreds of segments), wasteful for large meshes. When performance
  matters, implement the reduced form and verify it against the augmented one
  (both must agree to solver precision).
- Multiple RHS (several injection patterns per frequency) are cheap with one
  factorisation — `ZGESV` accepts multiple columns; exploit this in the
  frequency sweep driver.
