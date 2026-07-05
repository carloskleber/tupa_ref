# ADR 0007 — Soil dispersion models for tMaterial

- **Status**: Accepted (2026-07-05; proposed 2026-07-03)
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

Earlier project instructions (the now-retired CLAUDE.md) listed
Longmire–Smith as preferred; the code and the validation data point to
Portela's model. Both can coexist behind the `tMaterial` abstraction; the
conflict was resolved in Portela's favour by the author (2026-07-05).

> **Update (2026-07-05).** The original Matlab (model reference of record)
> ships **two** dispersive-soil routines: the Portela power-law with
> `ω₀ = 1 rad/s` — so the legacy `kr` is the dispersive magnitude at
> 1 rad/s, *not* at 1 MHz (source: references.md [30]) — and the
> Lima–Portela variant referenced to `2π·1 MHz` with `cot(πα/2)` on the
> real parcel (references.md [31]), which is the same family under
> `Δσ = Δᵢ·cot(πα/2)`. `tPortelaSoil` must document which reference
> frequency its `kr` assumes, and a `tLimaPortelaSoil` parametrisation is a
> candidate alongside the subtypes below (theory.md §7).

## Decision

Implement **Portela's power-law model first**, as `tPortelaSoil`, because:

- the reference validation curves (Portela 1997) were produced with it;
- its `alpha0`/`kr` fields were already declared on the type;
- it is two lines of complex arithmetic (no root finding, no term tables).

**Accepted parametrisation (2026-07-05, author decision)**: the
**Lima–Portela form of references.md [31]**, with reference frequency
**ω₀ = 2π·1 MHz**:

$$W(\omega) = \sigma_0 + \Delta_i \left[ \cot\!\left(\tfrac{\pi\alpha}{2}\right) + j \right] \left(\tfrac{\omega}{2\pi \cdot 10^6}\right)^{\alpha}$$

This requires adding a `sigma0` field to the type (only `alpha0`/`kr` exist
today); `kr` takes the role of Δᵢ at ω₀ = 2π·1 MHz. Legacy Matlab `kr`
values (referenced to ω₀ = 1 rad/s, references.md [30]) must be converted
before reuse — never copied verbatim.

Add **Longmire–Smith** as a second `tMaterial` subtype when lightning-study
interoperability requires it. Both must converge to the `tLinear` medium as
ω → 0 (regression test).

Other soil models can be implemented, **name each one with the original reference**:
tPortelaSoil, tLongmireSmithSoil, tVisacroAlipioSoil.

## Consequences

- Validation against Portela 1997 curves is apples-to-apples.
- Each dispersion model is a self-contained subtype; adding more (Cole-Cole,
  Alipio-Visacro) never touches the solver.
