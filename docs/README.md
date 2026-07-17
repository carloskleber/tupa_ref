# TUPÃ documentation

| Document | Purpose |
| --- | --- |
| [theory.md](theory.md) | **Normative physics reference**: HEM formulation, conventions, impedance integrals, image method, nodal system, dispersive soil, validation anchors, positioning vs. companion codes and neighbouring model families |
| [ARCHITECTURE.md](ARCHITECTURE.md) | Architectural vision: components, layers, flows, data management, state, extension mechanisms, known debts |
| [CONVENTIONS.md](CONVENTIONS.md) | Coding and project conventions (absorbs the retired CLAUDE.md) |
| [ROADMAP.md](ROADMAP.md) | Current-state gap analysis, phased plan, proposals from the open-source comparison, author-interview decisions (formerly `implementation-plan.md`) |
| [BENCHMARKS.md](BENCHMARKS.md) | Validation anchors, execution status, cross-code benchmark setup, comparison policy |
| [validation/](validation/) | Case-by-case comparisons against published papers' own figures (digitized plots, not tabulated data) |
| [DISTRIBUTION.md](DISTRIBUTION.md) | Build chain, dependency inventory, packaging/release, licensing, security surface |
| [GLOSSARY.md](GLOSSARY.md) | Main model and software terms |
| [references.md](references.md) | Bibliography, numbered as cited in the theory doc |
| [GUI_SDD.md](GUI_SDD.md) | **Proposal** (not accepted as an ADR): shared GUI frontend design — tree/3D input view, 1D/2D output plots. Phases G0/G1 implemented in [../gui/](../gui/README.md) |
| [adr/](adr/) | Architecture Decision Records |
| [object-model.plantuml](object-model.plantuml) | Class diagram of the language-agnostic object model |

Reading order for newcomers: root [README](../README.md) → GLOSSARY →
ARCHITECTURE → theory.md; contributors add CONVENTIONS and ROADMAP.

## ADR index

| # | Title | Status |
| --- | --- | --- |
| [0001](adr/0001-modern-fortran-reference-implementation.md) | Modern Fortran as the reference implementation | Accepted |
| [0002](adr/0002-language-agnostic-object-model.md) | One language-agnostic object model, shared test cases | Accepted |
| [0003](adr/0003-augmented-zeq-system.md) | Solve the full augmented Z_eq system with ZGESV | Accepted |
| [0004](adr/0004-geometry-factor-separation.md) | Frequency-independent geometry factors, mean-distance propagation | Accepted |
| [0005](adr/0005-two-media-image-method.md) | Two half-space media via ideal images; no cross-media coupling | Accepted |
| [0006](adr/0006-json-io.md) | JSON study format with a hand-rolled minimal parser | Accepted |
| [0007](adr/0007-soil-dispersion-model.md) | Soil dispersion models: tPortelaSoil first (Lima–Portela form, ω₀ = 2π·1 MHz) | Accepted |
| [0008](adr/0008-sign-conventions-and-validation.md) | Single sign-convention set, enforced by validation tests | Accepted |
| [0009](adr/0009-impedance-fill-interface.md) | Impedance-fill routines consume raw geometry factors | Accepted |
| [0010](adr/0010-sources-as-current-injections.md) | Excitation sources as nodal current injections | Accepted |
| [0011](adr/0011-gui-module-technology-and-scope.md) | GUI module: Python/PySide6/Qt3D, view-only v1 | Accepted |
| [0012](adr/0012-results-json-schema.md) | Results JSON schema v0 (harmonic sweep) | Accepted |
| [0013](adr/0013-input-schema-sources-frequencies-outputs.md) | Input schema: `sources`/`frequencies`/`outputs` blocks | Accepted |
| [0014](adr/0014-fft-implementation.md) | In-repo double-precision radix-2 FFT | Accepted |
| [0015](adr/0015-time-domain-signal-schema.md) | Input schema: `signal`; transient results schema v0 (amended: Heidler `terms`) | Accepted |
| [0016](adr/0016-voltage-sources-by-superposition.md) | Voltage sources by unit-injection superposition; `voltage` in sources | Accepted |
| [0017](adr/0017-legacy-reinspection-findings.md) | Legacy re-inspection findings (July 2026) | Recorded |
| [0018](adr/0018-author-interview-decisions-2026-07.md) | Author-interview decisions (2026-07-05) | Accepted |
| [0019](adr/0019-air-medium-hardcoded-vacuum.md) | Air medium hardcoded as vacuum (no JSON `air` block) | Accepted |

Language-specific build documentation stays with each implementation
(e.g. [../fortran/README.md](../fortran/README.md)); FORD API docs are
generated from the Fortran sources (`fortran/Tupa.md`). The shared JSON
cases and schema live in [../common/](../common/README.md). The solver-
agnostic GUI ([GUI_SDD.md](GUI_SDD.md)) lives in [../gui/](../gui/README.md).
