# ADR 0002 — One language-agnostic object model, shared test cases

- **Status**: Accepted
- **Date**: 2026-07-03

## Context

The project intends multiple implementations (Fortran now; Python, Rust later)
that must remain comparable and mutually validating. The original C++ code
already factored the domain into Study/Structure/Element/Node/Segment/Mesh/
Collector objects.

## Decision

Fix a single conceptual object model that every implementation maps onto:

- `Study` — top-level container; owns all data, handles I/O and orchestration.
- `Structure` — geometry + materials; owns `Element`s and, after assembly,
  flat arrays of `Node`s and `Electrode`s.
- `Element` (abstract) — geometric generators (`Line`, `Catenary`,
  `Circumference`, `Tower`, …) that discretise themselves into nodes and
  electrodes.
- `Material` (abstract) — `Linear` (constant σ, ε, μ) and `SoilFreq`
  (dispersive soil).
- `Mesh` — topology matrices, impedance matrices, frequency-domain solve.
- `Result`/`Collector` — post-processing outputs declared before the run.

Cross-implementation contracts:

- a common JSON input/output schema (ADR 0006);
- a `common/` folder of reference cases (input + expected results) that every
  implementation must reproduce within tolerance;
- [theory.md](../theory.md) as the normative physics specification.

## Consequences

- Language implementations may differ in internals (linked lists vs vectors)
  but not in observable behaviour on the common cases.
- New features must land as: theory doc update → schema/test case → per-language
  implementation, in that order.
- The PlantUML diagram in [object-model.plantuml](../object-model.plantuml)
  tracks the model.
