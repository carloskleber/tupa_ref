# TUPÃ

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21454767.svg)](https://doi.org/10.5281/zenodo.21454767)

Reference model for electromagnetic field transient simulations — codename
TUPÃ.

TUPÃ (also spelled as Tupan) computes the frequency-domain (and, via inverse transform, time-domain)
response of networks of thin cylindrical conductors in air and soil — tower
footings, counterpoises, grounding grids — under lightning-type excitation.
It is an application of the Method of Moments (MoM), known in the power
community as the **Hybrid Electromagnetic Model (HEM)**.

The model follows and expands the author's dissertation — *Modelagem de Linhas de
Transmissão para Análise de Comportamento Quanto a Descargas Atmosféricas*
(UFRJ, 2003, DOI:10.13140/RG.2.2.19894.56644) — building on the advisor's
original work:

* Portela, C. "Frequency and Transient Behavior of Grounding Systems, Part I
  — Physical and Methodological Aspects; Part II — Practical Application
  Examples", *Proc. IEEE International Symposium on Electromagnetic
  Compatibility*, pp. 379–390, Austin, USA, August 1997.

and acknowledging colleagues' work in the field:

* Visacro, S.; Soares, A., Jr. "HEM: A model for simulation of lightning
  related engineering problems", *IEEE Trans. Power Del.*, vol. 20, no. 2,
  pp. 1206–1208, Apr. 2005.
* Salari, J. C.; Azevedo, R. M.; Portela, C. "An efficient modeling of
  transmission lines towers and grounding systems for lightning propagation
  studies", *IX SIPDA*, Foz do Iguaçu, Brazil, Nov. 2007.

Actually implemented in 3 languages - Fortran (at the moment, the reference), Rust and Julia, as an exercise for comparison. Feel free to port to others. The main requirement is read the same JSON input format.

## Problem

Lightning performance of power systems hinges on how grounding
structures behave at frequencies from DC to a few MHz, where soil
dispersion, propagation delay and electromagnetic coupling between conductor
segments all matter. Circuit-level models lose accuracy exactly where it
counts; full-wave solvers are opaque and heavy. The HEM occupies the middle
ground, and TUPÃ is a **reference implementation** of it: readable,
verifiable against theory, and portable across languages.

## Scope

MVP: tower-footing grounding electrodes under lightning. Full
application tier: complete transmission lines and substation grids. By
design the model is linear (no soil ionisation, arresters or corona),
frequency-domain, thin-wire, with a two-half-space air/soil medium — the
boundaries are stated precisely in [docs/theory.md](docs/theory.md) §10.
The project's primary role is a *scientifically citable* implementation;
engineering-tool convenience is secondary.

## Metrics

An implementation is correct when it reproduces the validation
anchors of [docs/theory.md](docs/theory.md) §9 within stated tolerances —
DC grounding resistance (Sunde), the Portela 1997 harmonic-impedance case,
internal-consistency checks, and cross-code agreement with the open-source
TAGS/PRTL-mHEM solvers.

Current status: the end-to-end frequency-sweep and
time-domain pipeline are wired and green (ROADMAP Phases 0–7, 9, 10, 10b and 11 —
Phase 9 adds scan-fed transients, windows, multiple injections and the
Numerical Laplace Transform; Phase 10 makes the single-integral mHEM kernel
and frequency-dependent image coefficients the defaults, threads the
frequency sweep and adds a segment-length target; Phase 10b adds the
lightning-channel element (speed-calibrated series loading) and two-node
current/voltage sources, validated against Chen's analytic current; Phase 11
adds surface potentials, GPR, touch and step voltage, checked against the
rod and point-source closed forms; Phase 8, the Rust port,
implemented — every golden fixture matched at 1e-6); six comparisons against
published papers' own figures — Silva et
al. 2025, Grcev et al. 2018, Lima et al. 2020 and Poljak & Doric 2006 —
mostly agree within ±10-20% (closer for some cases, see
[docs/validation/](docs/validation/README.md)), and the cross-code check
against TAGS agrees to 0.3 % or better below 1 MHz
([docs/validation/tags-xval.md](docs/validation/tags-xval.md)). Voltage and two-node sources, the transient windows and the Phase 11 outputs have internal-consistency checks that rerun on any solver executable with `docs/validation/run_checks.sh`. The formal
anchors needing tabulated data (Sunde, Portela 1997, Grcev & Heimbach 1997)
are still open — see [docs/BENCHMARKS.md](docs/BENCHMARKS.md).

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
| [docs/validation/](docs/validation/README.md) | External-reference comparisons |
| [common/README.md](common/README.md) | Shared JSON cases and schema (the public contract) |
| [fortran/README.md](fortran/README.md) | Building and testing the Fortran implementation |
| [rust/README.md](rust/README.md) | Building, testing and conformance status of the Rust implementation |
| [gui/README.md](gui/README.md) | Solver-agnostic GUI (viewer); design in [docs/GUI_SDD.md](docs/GUI_SDD.md) |

## Repository layout

```text
tupa_ref/
├── README.md, CHANGELOG.md, LICENSE, tupa.jpg
├── docs/                   documentation (index: docs/README.md)
│   ├── theory.md             normative physics reference
│   ├── ARCHITECTURE.md, ROADMAP.md, CONVENTIONS.md, GLOSSARY.md, DISTRIBUTION.md
│   ├── BENCHMARKS.md         validation anchors and their status
│   ├── references.md         bibliography, numbered as cited
│   ├── adr/                  Architecture Decision Records (0001 …)
│   ├── validation/           comparisons with published figures, internal-consistency
│   │                         checks, and the Python scripts that regenerate them
│   └── figures/              comparison plots (SVG) written by the validation scripts
├── common/                 shared JSON study cases + golden `*_expected.csv` files:
│                           the public contract every implementation must reproduce
├── fortran/                reference implementation (fpm; OpenMP, LAPACK, SLATEC)
│   ├── src/                  solver modules (Mesh, Impedance, Study, Transient, Potentials, …)
│   │   └── element/            Line, Mesh (grid), Catenary, Channel
│   ├── app/                  command-line driver (builds the `Tupa` executable)
│   ├── test/                 unit tests and the `common/` conformance test
│   ├── example/              small demo programs
│   └── slatec/               SLATEC special functions (submodule)
├── rust/                   conformance port (cargo; `src/`, `tests/`), reproduces every golden fixture
├── julia/                  lag-guarded Julia port (`src/`, `bin/tupa.jl`, `test/`, `comparison/`)
├── gui/                    solver-agnostic viewer (Python, Qt; `src/tupa_gui/`, `tests/`)
├── benchmarks/             cross-code material
│   ├── tags/, prtl-mhem/     companion codes (submodules)
│   ├── tags-xval/            driver and runner of the TAGS comparison
│   └── cross-impl/           Fortran / Rust / Julia timing benchmark
└── tools/                  `legacy_import.py`: converts legacy case files to the JSON schema
```

Every solver reads the same JSON case and writes `<case>_results.{csv,json}`
(harmonic sweep), `<case>_transient_results.{csv,json}` (time domain) and, for
studies with an `observation` block, `<case>_potentials.{csv,json}`.

## Implementations

### Fortran

Fortran 2008+, built with FPM — based in the original numerical core ([ADR 0001](docs/adr/0001-modern-fortran-reference-implementation.md)).

```bash
cd fortran
bash build.sh                          # fetch+build SLATEC, optimised build
./bin/Tupa ../common/portela1997.json   # run the solver on a JSON case
```

The solver takes one argument, the path to a study JSON, and writes
`<case>_results.csv` and `<case>_results.json` into `results/`
(git-ignored; `--output-dir DIR` overrides). The commands above assume you are in `fortran/`:
both the `./build/...` binary path and the `../common/` case path are relative to
it. From another directory, give the full path to both.

`build.sh` installs the optimised binary at the stable path `fortran/bin/Tupa`
(SLATEC is an fpm path dependency, compiled together with the project). A bare `fpm run` fails on the SLATEC sources; use `./fpmw`, which is `fpm` with
the needed flags preset (see [fortran/README.md](fortran/README.md)):

```bash
./fpmw run --profile release -- ../common/portela1997.json
./fpmw test --profile release
```

`build.sh` enables OpenMP: the frequency sweep runs on all cores (results are
bit-identical for any thread count; `OMP_NUM_THREADS` or `--threads <n>`
choose the count). Per-study numerics — geometry kernel, image model and a
segment-length target — are set by the optional `numerics` block of the case
file ([common/README.md](common/README.md), [ADR 0024](docs/adr/0024-phase10-numerics.md)).

See [fortran/README.md](fortran/README.md) for the full setup (Windows/Linux)
and for the bundled Fortran demo programs (`fpm run --example example1`).
The JSON case files under [common/](common/README.md) are the shared,
language-neutral inputs every implementation must reproduce.

### Rust

Only `cargo` is needed ([rust/README.md](rust/README.md)):

```bash
cd rust
cargo build --release
./target/release/tupa ../common/portela1997.json   # writes portela1997_results.{csv,json}
cargo test --release                               # tests
```

### Julia

(≥ 1.10) — no build step ([julia/README.md](julia/README.md)):

```bash
cd julia
julia --project=. -e 'using Pkg; Pkg.instantiate()'   # once
julia bin/tupa.jl ../common/portela1997.json          # writes portela1997_results.{csv,json}
julia --project=. -e 'using Pkg; Pkg.test()'          # tests
```

### GUI

Python ≥ 3.11 with [uv](https://docs.astral.sh/uv/)
([gui/README.md](gui/README.md)). It views a case and, optionally, the result
file written by any of the solvers above:

```bash
cd gui
uv sync
uv run tupa-gui ../common/buried_conductor_short.json
uv run tupa-gui ../common/portela1997.json --results path/to/portela1997_results.json
uv run pytest                                         # headless data-layer tests
```

All three solvers take the same JSON case and write result files with the same
names, so the GUI reads any of them unchanged.

## Cases

All files are in [common/](common/README.md), which documents each one in
full. A *golden* case also has an `_expected.csv` that every implementation must
reproduce at the 1e-6 rule.

### Basic and smoke cases

| Case | Description |
| --- | --- |
| `buried_conductor_short.json`, `buried_conductor_long.json` | Structure-only smoke cases: a 2 m buried conductor; two collinear 10 m conductors |
| `portela1997.json` (golden), `portela1997_ideal.json` (golden) | Phase 2 validation conductor (10 m, 0.5 m deep, σ = 0.01 S/m, εr = 10), 10 Hz–1 MHz; the `_ideal` variant uses ideal images and runs to 10 MHz |
| `rod.json` (golden), `rod_air.json` | Single buried rod; a 10 m air rod above a 5 m buried rod (air–soil interface) |
| `grid.json` (golden) | Small buried grounding grid, one square mesh |
| `line_rod.json`, `horizontal_vertical_mesh.json`, `counterpoise_3.json` | Horizontal line in air with a grounded rod; horizontal and vertical conductors with a grid; closed-square counterpoise with two central cables |
| `portelaMesh.json` (golden) | 32 × 32 m grid built with the `mesh` element, harmonic sweep and scan-fed transient |

### Transient cases

(`signal` block; golden unless noted)

| Case | Description |
| --- | --- |
| `portela1997_transient.json` | 1.2/50 µs, 30 kA double exponential on the Portela conductor (internal consistency only) |
| `portela1997_transient_{interpolated,hann,hann_time,multi,nlt}.json` | Phase 9 options on the same conductor: scan-fed transfer function, spectral and time Hann windows, three injections, Numerical Laplace Transform |
| `portela1997_transient_signals.json` | Three independent signals (ADR 0026) in one run |

### Literature comparisons

(compared against the papers' own figures in [docs/validation/](docs/validation/README.md); no golden file)

| Case | Reference |
| --- | --- |
| `silva2025_rho{100,300,1000,2400}.json`, `…_transient.json` | Silva et al. 2025, Figs. 3 and 4: 60 m electrode, dispersive soil, harmonic and MCS_FST#2 GPR |
| `grcev_fig12_l{10,100}_rho{30,300,3000}.json` | Grcev et al. 2018, Fig. 12: horizontal electrodes against a full-wave reference |
| `lima_fig6.json`, `lima_fig7_case{10,11}.json` | Lima et al. 2020, Figs. 6–7: tower counterpoise and two square grids |
| `poljak_fig4.json` | Poljak & Doric 2006, Fig. 4: 2 m vertical electrode, 10 Hz–100 MHz |

### Lightning channel and two-node sources

(Phase 10b, ADR 0025; golden)

| Case | Description |
| --- | --- |
| `channel_unloaded.json` | Unloaded 2 km channel above ideal ground driven by a ramp *voltage*; compared with Chen's analytic current |
| `channel_loaded.json` | 3 km channel loaded and speed-calibrated to c/2 |
| `channel_tower.json`, `channel_tower_gap.json` | Strike to a 30 m tower with a rod footing through a two-node source: 1 A current dipole; 1 kV delta-gap voltage source |

### Grounding-safety outputs

(Phase 11, ADR 0027; golden)

| Case | Description |
| --- | --- |
| `grid_safety.json` | 16 × 16 m grid with a tower riser: surface potential grid, step map, touch voltage, GPR and a step pair (`observation` block) |

### Legacy cases

Imported from the original Matlab implementation, ADR 0023; no golden file

| Case | Description |
| --- | --- |
| `linha{0,1,2,3,4,5,5a}.json` | Transmission-line cases: grounded lines, shield wire over towers (up to 6–10), a telephone wire, catenary spans, an indirect strike; three Portela fronts per run |
| `torre{0,1,2}.json` | Tower structures: a prism frame, a guyed lattice tower, a cross frame, each with a rod footing |

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

## Implementations compared

Fortran is the reference implementation; Rust follows it to the 1e-6
conformance rule on every golden fixture; Julia is a lag-guarded third port
(it refuses the schema features it does not implement rather than ignoring
them). State as of 2026-10-01 (Phase 11, Fortran package 0.7.0 plus unreleased work).

| Feature | Fortran (`fortran/`) | Rust (`rust/`) | Julia (`julia/`) |
| --- | --- | --- | --- |
| Harmonic sweep, current and voltage sources (ADR 0016) | ✔ | ✔ | ✔ |
| Elements: line, `mesh` grid (ADR 0020), `catenary` (ADR 0023) | ✔ | ✔ | ✔ |
| Transient by FFT, Heidler / double-exponential / Portela waveforms | ✔ | ✔ | ✔ |
| Phase 9 transients: scan-fed transfer function, windows, multiple injections, Numerical Laplace Transform | ✔ | ✔ | lag |
| Independent transient signals (`signal.signals`, ADR 0026): one transfer function, a response set per signal | ✔ | ✔ | ✔ (one sweep per distinct node; no `returnNode`/`quantity` on an entry) |
| Phase 10 numerics: single-integral kernel, Γ(ω) images, `numerics` block, segment-length target | ✔ | ✔ | ✔ |
| Threaded frequency sweep (OpenMP, Phase 10 item 4) | ✔ | serial | serial |
| Lightning `channel` element, speed calibration (Phase 10b, ADR 0025) | ✔ | ✔ | lag (refuses the element) |
| Two-node sources: `returnNode` current dipole, delta-gap voltage, `quantity` (ADR 0025) | ✔ | ✔ | lag (refuses the fields) |
| `channels` block in the results JSON | ✔ | ✔ | — |
| Grounding-safety outputs (Phase 11, ADR 0027): `observation` block — surface potentials, GPR, touch and step voltage, step map — and `<case>_potentials.*` | ✔ (harmonic) | ✔ (harmonic) | lag (refuses the block) |
| Golden fixtures in `common/` met at 1e-6 | all but `channel_loaded` (its fixture predates the case's later 2048-sample setting; fails in Fortran and Rust alike until regenerated) | all but `channel_loaded` (same) | harmonic ones (`grid`, `portela1997`, `portela1997_ideal`, `rod`, `portelaMesh`); the Phase 9 transient and Phase 10b channel ones lag (`julia/README.md`) |
| Test suite (2026-10-01) | 19 programs; 18 pass, `test_common_cases` fails on the stale `channel_loaded` fixture only | 112 tests; all pass but `channel_loaded` | 253 tests, all pass |
| Linear algebra / quadrature | LAPACK, SLATEC | in-repo LU, line-by-line GK 7/15 port, no external numerics | LAPACK LU via `LinearAlgebra`, line-by-line GK 7/15 port |
| Role | reference; also builds the CLI used by the GUI | conformance port (ADR 0022) | contributed prototype, follow-along port (Phase 8J) |
