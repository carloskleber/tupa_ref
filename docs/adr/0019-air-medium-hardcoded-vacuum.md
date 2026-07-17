# ADR 0019 — Air medium hardcoded as vacuum (no JSON `air` block)

- **Status**: Accepted (decision made 2026-07-10, recorded as an ADR when
  the roadmap was tidied, 2026-07-17; formerly ROADMAP.md §1 item 9 /
  §3 finding 9)
- **Date**: 2026-07-10

## Context

While adding `common/rod_air.json` (the first committed case with an
element above the soil interface), the production load path turned out to
never populate `tStructure%air`: `loadStudy` (`Tupa.f90`) only reads the
JSON `"soil"` block, and `tStructure%air` was a bare `type(tLinear)` field
with no default component values. `tStudy%run` always passes
`structure%air%mur`/`structure%air%admittance(omega)` into `calcParamW`
regardless of geometry, so the air magnetic constant and admittance ended
up zero. That stayed harmless while every electrode sat in soil
(`geomPos = 2` — true of `portela1997.json`/`rod.json`/`grid.json`, which
is why it was never caught); with any electrode in air (`geomPos = 1`),
the γ-dependent self/mutual impedance formula divided by the zeroed air
admittance (`cEAir = 1/(4π·0)`) and the resulting NaN poisoned the whole
`Zeq` solve — every node, not just the air ones, once `ZGESV` mixed a NaN
row in.

Both legacies hardcode air: the Matlab reference (the model reference of
record) constructs a vacuum air medium unconditionally at startup; the C++
defaults its `Meio` to vacuum.

## Decision

`tStructure%air` default-initialises to vacuum (εr = 1, μr = 1, σ = 0) —
the same hardcoded air both legacies use. **Matlab-faithful hardcode, no
JSON `"air"` block**: air is deliberately not configurable from case
files.

## Consequences

- `common/rod_air.json` runs NaN-free with a physically sensible
  low-frequency Zin ≈ 20.9 Ω (analytical rod ground resistance ≈ 21.0 Ω).
- A zero-conductivity air admittance is still singular at exactly ω = 0;
  the transient driver substitutes a small nonzero `freqZeroHz` for the DC
  bin (legacy `FREQ_ZERO` convention — `mTransient`, ADR 0015).
- A legacy re-inspection during the fix also confirmed the surrounding
  model: mixed air↔soil coupling is zeroed in the Matlab too (its
  cross-media routine was left unfinished, with a syntactically incomplete
  body), matching theory.md §5 / ADR 0005.
- If a future case ever needs a non-vacuum upper half-space, that is a new
  ADR (schema addition), not a silent default change.
