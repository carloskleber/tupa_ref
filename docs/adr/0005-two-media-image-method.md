# ADR 0005 — Two half-space media via ideal images; no cross-media coupling

- **Status**: Accepted
- **Date**: 2026-07-03

## Context

TUPÃ models air (z > 0) and soil (z < 0) separated by a plane interface. The
rigorous treatment uses Sommerfeld integrals or frequency-dependent
reflection/transmission coefficients relating image sources (Portela [1]
§2.4). The original code implemented the simpler ideal-image limits.

## Decision

Represent the interface by **ideal images** (theory.md §5):

- both segments in soil → image contributions **add**;
- both segments in air → image contributions **subtract**;
- segments in different media → mutual coupling **neglected** (zero).

Multi-layer soil and frequency-dependent reflection coefficients are out of
scope for the reference core; the object model must not preclude them (the
`Material`/medium abstraction and the per-pair image terms are the extension
points).

## Consequences

- Matches the original implementation and the published validation cases, and
  keeps the per-pair cost at two geometry factors (direct + image).
- Accuracy degrades for conductors very close to the interface and for strong
  air↔soil coupling problems (e.g. tower + counterpoise interaction through
  the surface); this limitation must be stated in results and revisited when
  implementing multi-zone soil (planned dispersive-soil `tMaterial` subtypes,
  e.g. `tPortelaSoil`/`tLongmireSmithSoil` — ADR 0007).
- The sign rules are a frequent bug source; they are pinned by unit tests
  against the DC image formulas (theory.md §9.1).
