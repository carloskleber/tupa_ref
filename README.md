# TUPÃ

Reference model for electromagnetic field transient simulations — codename
TUPÃ.

TUPÃ computes the frequency-domain (and, via inverse transform, time-domain)
response of networks of thin cylindrical conductors in air and soil — tower
footings, counterpoises, grounding grids — under lightning-type excitation.
It is an application of the Method of Moments (MoM), known in the power
community as the **Hybrid Electromagnetic Model (HEM)**.

The model follows the author's M.Sc. dissertation — *Modelagem de Linhas de
Transmissão para Análise de Comportamento Quanto a Descargas Atmosféricas*
(UFRJ, 2003, DOI:10.13140/RG.2.2.19894.56644) — building on the advisor's
original work:

* Portela, C. "Frequency and Transient Behavior of Grounding Systems, Part I
  — Physical and Methodological Aspects; Part II — Practical Application
  Examples", *Proc. IEEE International Symposium on Electromagnetic
  Compatibility*, pp. 379–390, Austin, USA, August 1997.

and acknowledging colleagues' work in the field:

* Salari, J. C.; Azevedo, R. M.; Portela, C. "An efficient modeling of
  transmission lines towers and grounding systems for lightning propagation
  studies", *IX SIPDA*, Foz do Iguaçu, Brazil, Nov. 2007.
* Visacro, S.; Soares, A., Jr. "HEM: A model for simulation of lightning
  related engineering problems", *IEEE Trans. Power Del.*, vol. 20, no. 2,
  pp. 1206–1208, Apr. 2005.

## Problem, scope, metrics

**Problem.** Lightning performance of power systems hinges on how grounding
structures behave at frequencies from DC to a few MHz, where soil
dispersion, propagation delay and electromagnetic coupling between conductor
segments all matter. Circuit-level models lose accuracy exactly where it
counts; full-wave solvers are opaque and heavy. The HEM occupies the middle
ground, and TUPÃ is a **reference implementation** of it: readable,
verifiable against theory, and portable across languages.

**Scope.** MVP: tower-footing grounding electrodes under lightning. Full
application tier: complete transmission lines and substation grids. By
design the model is linear (no soil ionisation, arresters or corona),
frequency-domain, thin-wire, with a two-half-space air/soil medium — the
boundaries are stated precisely in [docs/theory.md](docs/theory.md) §10.
The project's primary role is a *scientifically citable* implementation;
engineering-tool convenience is secondary.

**Metrics.** An implementation is correct when it reproduces the validation
anchors of [docs/theory.md](docs/theory.md) §9 within stated tolerances —
DC grounding resistance (Sunde), the Portela 1997 harmonic-impedance case,
internal-consistency checks, and cross-code agreement with the open-source
TAGS/PRTL-mHEM solvers. Current status: **no end-to-end validation has run
yet** (the solver pipeline is being wired) — see
[docs/BENCHMARKS.md](docs/BENCHMARKS.md).

## Documentation

| | |
| --- | --- |
| [docs/README.md](docs/README.md) | Documentation index |
| [docs/theory.md](docs/theory.md) | Normative physics reference |
| [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) | Components, layers, flows, data management |
| [docs/ROADMAP.md](docs/ROADMAP.md) | Gap analysis, phased plan, decisions |
| [docs/CONVENTIONS.md](docs/CONVENTIONS.md) | Coding and project conventions |
| [docs/BENCHMARKS.md](docs/BENCHMARKS.md) | Validation status and benchmark policy |
| [docs/GLOSSARY.md](docs/GLOSSARY.md) | Terminology |
| [docs/adr/](docs/README.md#adr-index) | Architecture Decision Records |
| [common/README.md](common/README.md) | Shared JSON cases and schema (the public contract) |
| [fortran/README.md](fortran/README.md) | Building and testing the Fortran implementation |
| [gui/README.md](gui/README.md) | Solver-agnostic GUI (viewer); design in [docs/GUI_SDD.md](docs/GUI_SDD.md) |

## Implementations

The first implementation is **modern Fortran** (2008+, built with FPM) —
partly because the original numerical core was already Fortran, cleaned up
and modernised ([ADR 0001](docs/adr/0001-modern-fortran-reference-implementation.md)).
Python and Rust implementations are planned; all map the same object model
(Study → Structure → Element/Material → Node/Electrode → Mesh → Result,
[ADR 0002](docs/adr/0002-language-agnostic-object-model.md)) and must pass
the same [common/](common/README.md) cases.

```bash
cd fortran
bash build.sh                          # fetch+build SLATEC, optimised build
fpm run -- ../common/portela1997.json  # run the solver on a JSON case
fpm test                               # tests (see docs/ROADMAP.md §5 for the fast/slow split)
```

See [fortran/README.md](fortran/README.md) for the full setup (Windows/Linux)
and for the bundled Fortran demo programs (`fpm run --example example1`).
The JSON case files under [common/](common/README.md) are the shared,
language-neutral inputs every implementation must reproduce:

| Case | Description |
| --- | --- |
| `buried_conductor_short.json` | Smallest smoke case: 2 m buried conductor, 2 segments (structure-only) |
| `buried_conductor_long.json` | Two collinear buried conductors, 2 × 10 m (structure-only) |
| `portela1997.json` | Phase 2 validation conductor (10 m, εr = 10 soil), 10 Hz–1 MHz sweep |
| `rod.json` | Single vertical buried rod, same soil, 10 Hz–1 MHz sweep |
| `grid.json` | Small buried grounding grid (one square mesh), 100 Hz–100 kHz sweep |

## Regarding the name

According to Wikipedia, *Tupã* (or *Tupan*, *Tupave*, *Tenondete*) is the
word for God in the Tupi and Guarani languages, one of whose manifestations
is thunder — the name itself probably means "the sound of thunder". As the
model relates to lightning and was conceived in Brazil, the name TUPÃ was
used in the original Matlab routine; to avoid encoding problems it can be
written "TUPA".

![An idea of logo made by AI, something like an indian god with fire hair and a small lightning below](tupa.jpg "An idea of logo made by AI...")

## License

GPLv3 — see [LICENSE](LICENSE).
