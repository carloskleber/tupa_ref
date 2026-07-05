# TUPÃ documentation

| Document | Purpose |
| --- | --- |
| [theory.md](theory.md) | Normative physics reference: HEM formulation, conventions, impedance integrals, image method, nodal system, dispersive soil, validation anchors, positioning vs. companion codes and neighbouring model families |
| [references.md](references.md) | Bibliography, numbered as cited in the theory doc |
| [implementation-plan.md](implementation-plan.md) | Current-state gap analysis and phased plan for the Fortran implementation |
| [adr/](adr/) | Architecture Decision Records |
| [object-model.plantuml](object-model.plantuml) | Class diagram of the language-agnostic object model |

## ADR index

| # | Title | Status |
| --- | --- | --- |
| [0001](adr/0001-modern-fortran-reference-implementation.md) | Modern Fortran as the reference implementation | Accepted |
| [0002](adr/0002-language-agnostic-object-model.md) | One language-agnostic object model, shared test cases | Accepted |
| [0003](adr/0003-augmented-zeq-system.md) | Solve the full augmented Z_eq system with ZGESV | Accepted |
| [0004](adr/0004-geometry-factor-separation.md) | Frequency-independent geometry factors, mean-distance propagation | Accepted |
| [0005](adr/0005-two-media-image-method.md) | Two half-space media via ideal images; no cross-media coupling | Accepted |
| [0006](adr/0006-json-io.md) | JSON study format with a hand-rolled minimal parser | Accepted |
| [0007](adr/0007-soil-dispersion-model.md) | Soil dispersion models: tPortelaSoil first, more tMaterial subtypes to follow | **Proposed** |
| [0008](adr/0008-sign-conventions-and-validation.md) | Single sign-convention set, enforced by validation tests | Accepted |

Language-specific build documentation stays with each implementation
(e.g. [../fortran/README.md](../fortran/README.md)); FORD API docs are
generated from the Fortran sources (`fortran/Tupa.md`).
