# ADR 0002 — One language-agnostic object model, shared test cases

- **Status**: Accepted
- **Date**: 2026-07-03

## Context

The project intends multiple implementations (Fortran now; Python, Rust later)
that must remain comparable and mutually validating. The original C++ code
already factored the domain into Study/Structure/Element/Node/Segment/Mesh/
Collector objects.

> **Update (2026-07-05).** The factoring predates the C++: the original
> Matlab code (now the model reference of record) already carries the same
> decomposition — case/structure/mesh/medium/segment classes plus element,
> signal and output packages that map one-to-one onto Study, Structure,
> Mesh, Material, Electrode, Element, source-signal and Result types. The
> object model is thus validated by three generations of the code, not one.

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
