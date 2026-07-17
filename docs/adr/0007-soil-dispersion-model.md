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

**Exercised (ROADMAP Phase 4)**: `tPortelaSoil` is implemented in
`fortran/src/Material.f90` (`sigma0` field added as required above). The
propagation-constant formula `γ = √(jωμW(ω))` was factored onto the
abstract `tMaterial` base as a single non-deferred procedure that calls a
new deferred `admittance(ω)` function — the one quantity (`W(ω) = σ(ω) +
jωε(ω)`) that actually differs between `tLinear` and `tPortelaSoil` — so
`tStudy%run` (`fortran/src/Study.f90`) now calls `soil%admittance(omega)`
polymorphically instead of the `select type`/reject-non-linear-soil branch
this ADR's acceptance had left in place. `fortran/test/test_material.f90`
pins the Lima–Portela formula at ω₀ = 2π·1 MHz, the required DC-limit
convergence to a resistive `tLinear(epsilonr=0, sigma=σ₀)` medium as ω→0,
passivity across a decade sweep, and repeats the Phase 2 buried-conductor
passivity/DC-limit checks with `tPortelaSoil` in place of `tLinear` soil
(illustrative `alpha0`/`kr`, since no tabulated Lima–Portela parameter set
is available — ROADMAP §9 "Validation data").

**Exercised (ROADMAP §P5, 2026-07-16)**: `tVisacroAlipioSoil` is implemented
alongside it, same `tMaterial%admittance` interface, *mean* parameter set of
Alipio & Visacro [14] with `sigma0` (100 Hz conductivity) as the sole free
parameter (theory.md §7). Pinned by the same regression shape in
`test_material.f90` (formula value, DC-limit, passivity, plus the
higher-resistivity-disperses-more trend from [14]). The JSON case format
(`fortran/src/Tupa.f90`) gained a `soil.type` selector (`linear` default,
`portela`, `alipio-visacro`) so a case file can actually reach either
dispersive model — previously the loader always constructed `tLinear`
regardless of what `tMaterial` subtypes existed. The *relatively
conservative* / *conservative* parameter sets of [14] are not exposed.
