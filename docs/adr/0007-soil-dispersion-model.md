# ADR 0007 — Soil dispersion models for tMaterial

- **Status**: Proposed
- **Date**: 2026-07-03

## Context

Frequency-dependent soil parameters are essential above ~10 kHz (theory.md §7).
Candidate models:

1. **Portela power-law** (minimum-phase): `W(ω) = σ₀ + Δσ·[1 + j·tan(πα/2)]·(ω/ω₀)^α`.
   This is the model behind the project's primary validation curves [1][2],
   and the `alpha0`/`kr` parameters already declared in the code's
   frequency-dependent soil type (and in the original code's dispersive
   medium class) are its parameters.
2. **Longmire–Smith**: a 13-term Debye expansion parametrised by the 100 Hz
   conductivity; widely used in EMP/lightning studies.
3. **Cole-Cole / Visacro–Alipio**: alternatives common in recent literature.

The project instructions list Longmire–Smith as preferred; the code and the
validation data point to Portela's model. Both can coexist behind the
`tMaterial` abstraction.

## Decision

Implement **Portela's power-law model first**, as `tPortelaSoil`, because:

- the reference validation curves (Portela 1997) were produced with it;
- its `alpha0`/`kr` fields were already declared on the type — no interface
  change;
- it is two lines of complex arithmetic (no root finding, no term tables).

Add **Longmire–Smith** as a second `tMaterial` subtype when lightning-study
interoperability requires it. Both must converge to the `tLinear` medium as
ω → 0 (regression test).

Other soil models can be implemented, **name each one with the original reference**:
tPortelaSoil, tLongmireSmithSoil, tVisacroAlipioSoil.

## Consequences

- Validation against Portela 1997 curves is apples-to-apples.
- Each dispersion model is a self-contained subtype; adding more (Cole-Cole,
  Alipio-Visacro) never touches the solver.
