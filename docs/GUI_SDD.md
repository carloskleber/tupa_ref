# TUPÃ — GUI Frontend Software Design Document (draft, for review)

**Status**: the decisions recorded in §8 are now binding —
[ADR 0011](adr/0011-gui-module-technology-and-scope.md) (technology, scope,
repo placement) and [ADR 0012](adr/0012-results-json-schema.md) (results
JSON schema) have been accepted. This document remains the detailed design
record and phased delivery plan behind those ADRs. Phases G0/G1 (§7) have
been implemented in [`gui/`](../gui/README.md) against the design below.

This is a design proposal for a **separate, solver-agnostic GUI module**:
one visual tool that loads TUPÃ study inputs and results (both JSON) and
displays them, shared by every current and future TUPÃ implementation
(Fortran now; Python/Rust planned, [ROADMAP Phase 8](ROADMAP.md)). It follows
the same contract discipline as the rest of the project
([ADR 0002](adr/0002-language-agnostic-object-model.md),
[ADR 0006](adr/0006-json-io.md)): the GUI talks to JSON files only, never to
a solver's internals.

## 1. Purpose and scope

**Problem.** Every implementation reads a JSON study and (once Phase 3
lands) will write JSON/CSV results. There is no way to *see* either today —
inspecting a study means reading JSON by eye, and results are text tables.
A visual tool would help both authoring studies (does this geometry look
right?) and interpreting results (impedance curves, waveforms).

**Scope (v1)**:

- Load an input study JSON ([common/README.md](../common/README.md) schema)
  and display it: a tree view of its elements/materials/nodes/soil, and a 3D
  render of the geometry.
- Load a results JSON (schema **does not exist yet** — see §5) and display
  it: 1D plots (impedance/voltage/current vs. frequency, time-domain
  waveforms) and 2D plots (spatial quantities — potential maps, touch/step
  voltage profiles — once ROADMAP P7 output classes exist).
- View-only in v1: no study authoring/editing. Invoking a solver (subprocess)
  is in scope for the project overall (decided, §8.4) but not required for
  v1 — see G5 in §7.

**Explicitly out of scope for v1** (candidates for later phases, §7):

- Editing/authoring studies in the GUI (creating or moving elements).
- Anything live/real-time, remote, or collaborative.

**Why a separate module.** The project's own rule
([CONVENTIONS.md](CONVENTIONS.md)): "future implementations get sibling
top-level folders sharing `common/`". A GUI is not a solver implementation,
but the same logic applies for the same reason — it must not create a
dependency from any solver language onto Python/Qt, and it must outlive any
one solver's language choice. It lives in its own top-level folder (§4),
depends only on the JSON contract, and is buildable/runnable independently
of `fortran/`.

## 2. Non-goals and guiding constraint

The root [README.md](../README.md) states the project's priorities
explicitly: *"scientifically citable implementation... engineering-tool
convenience is secondary."* This GUI is convenience tooling. It must never
gate solver work, never become a second place where physics logic lives (no
geometry discretisation, no unit conversion beyond display formatting — see
§5 for why this matters), and must be easy to drop or postpone without
affecting the reference implementations. This is a design constraint, not
just a scheduling note: any feature request that would require the GUI to
reimplement solver logic should be rejected in favour of a solver-side JSON
export instead.

## 3. Users and use cases

- **The author**, sanity-checking a hand-written study JSON before running
  it ("is this conductor really at 0.5 m depth, in soil not air?").
- **The author**, inspecting a completed run's results without leaving JSON/
  CSV in a plotting script each time.
- **A future contributor** porting a `common/` case, comparing what two
  implementations produced for the same input.

No multi-user, no persistence beyond the files the solver already produces.

## 4. Technology and repo placement

| Decision | Choice | Rationale |
| --- | --- | --- |
| Language | Python 3 | Matches the planned Python solver port (ROADMAP Phase 8) without depending on it; largest ecosystem for both Qt bindings and scientific 3D/plotting |
| GUI toolkit | **PySide6** (Qt for Python) — **decided** | User's suggestion; LGPL license fits a GPLv3 project better than PyQt6's GPL/commercial dual license |
| 3D viewer | **Qt3D** (`PySide6.Qt3DCore`/`Qt3DRender`/`Qt3DExtras`) — **decided** | Author's choice over PyVista/VTK: native PySide6 module, no extra VTK dependency, one less thing in the dependency inventory (DISTRIBUTION.md). Trade-off accepted: conductors/nodes are built from `Qt3DExtras` primitives (`QCylinderMesh`, `QSphereMesh`) by hand rather than PyVista's ready-made scientific-visualisation glyphs — more code in the view layer, but it stays inside the Qt ecosystem the rest of the GUI already depends on |
| 1D/2D plotting | **PyQtGraph** | Native Qt integration (no separate event loop to reconcile, unlike embedding Matplotlib's `FigureCanvas`), interactive pan/zoom/log-axes out of the box, handles large frequency sweeps without redraw lag; Matplotlib kept as an optional export path for publication-quality static figures |
| Repo location | New top-level `gui/` folder, sibling to `fortran/`, `common/` — **decided**, same repo | Same reasoning as future `python/`/`rust/` folders (CONVENTIONS.md); own `pyproject.toml`, own README, no code under `fortran/` |
| Packaging | **uv**-managed project (`pyproject.toml` + `uv.lock`), no bundled executable for now — **decided** | Matches DISTRIBUTION.md's "source only, no binary artifacts" default; revisit packaging as an executable only if the audience turns out to be non-developers |

## 5. Data contracts — the actual hard part

The GUI's entire design rests on JSON contracts, so its feasibility depends
on what those contracts contain. Two asymmetric situations:

### 5.1 Input — exists today (common/README.md schema v1, ADR 0013)

Usable as-is for the tree view, including the `sources`/`frequencies`/
`outputs` blocks ADR 0013 added to the schema (Phase 5) — the study loader
and tree view display them (empty/absent state shown explicitly, since
omitting them is meaningful: "everything" for `outputs`, "no sweep
configured" for `frequencies`). For the 3D view there is a real design
choice:

- **(a) Render authored (boundary) elements only** — i.e. the line
  endpoints as written in the JSON, not the solver's per-segment
  discretisation. Simple, zero solver dependency, available immediately.
  Downside: doesn't show what was actually meshed/solved (segment count,
  actual node positions after `assembleStructure`).
- **(b) Render the discretised structure** — the flat node/electrode arrays
  `tStructure` builds internally. Faithful, but requires a solver-side
  "dump the assembled structure" export, which does not exist in any
  implementation yet.

**Proposal**: ship (a) in v1 (Phase G1, §7); revisit (b) as a solver-side
output-schema addition once Phase 3/5 output writers exist (Phase G4). This
keeps the guiding constraint of §2: the GUI does not reimplement
`assembleStructure`'s discretisation logic itself, it only ever displays
what a solver already computed and exported.

### 5.2 Output — does not exist yet, sketch it now (decided, §8.5)

ROADMAP Phase 3 item 3 ("CSV writer (primary) and JSON results writer") and
Phase 5 ("freeze the input schema v1... documented next to the cases") cover
input; no output JSON schema had been drafted anywhere in the docs. Per §8.5,
the schema is drafted **now**, alongside this GUI design, rather than waiting
for the Fortran Phase 3 writer to exist and reverse-documenting it — all
three planned implementations (Fortran, Python, Rust) and this GUI need to
agree on one schema, so it should only be designed once. The sketch below has
been promoted to [ADR 0012](adr/0012-results-json-schema.md) and is
cross-referenced from ROADMAP Phase 3/5:

```json
{
  "title": "string",
  "frequencies": [1e2, 1e3, ...],
  "nodes": [ { "id": "Node_1", "voltage": [ {"re":.., "im":..}, ... ] } ],
  "electrodes": [ { "id": "...", "i1": [...], "i2": [...] } ],
  "derived": { "inputImpedance": [ {"re":.., "im":..}, ... ] }
}
```

indexed per frequency, keyed back to the input JSON's node/element `id`s so
a results file is only meaningful alongside the input file that produced it.
Spatial outputs (potential maps, touch/step voltage — ROADMAP P7) are a
later, separate extension once those `tResult` subtypes exist.

## 6. Architecture

Three layers, deliberately thin at the boundaries:

```
┌─────────────────────┐   ┌─────────────────────────────┐   ┌──────────────────────┐
│ Data layer          │   │ View layer                  │   │ Controller/state     │
│ - input JSON loader │──►│ - QTreeView (elements,      │◄──│ - selection sync:    │
│ - output JSON loader│   │   materials, nodes, soil)   │   │   tree ↔ 3D highlight│
│ - dataclasses mirror│   │ - Qt3D viewer               │   │ - file load/reload   │
│   the object model  │   │   (conductors, nodes,       │   │   state              │
│   (ADR 0002)        │   │   air/soil interface plane) │   │                      │
│                     │   │ - PyQtGraph plot panel      │   │                      │
│                     │   │   (1D: vs. frequency/time;  │   │                      │
│                     │   │    2D: spatial, later)      │   │                      │
└─────────────────────┘   └─────────────────────────────┘   └──────────────────────┘
```

- **Data layer** has no Qt dependency — plain dataclasses, unit-testable
  without a display, loadable in a headless CI-less test run
  (`fpm`-equivalent for this module would be `pytest`, run locally per the
  no-hosted-CI policy, ROADMAP §9).
- **View layer** is dumb: given a loaded study/result object, it renders.
  No JSON parsing inside view code.
- **Controller** is the only place that mutates view state — e.g. clicking
  a tree row highlights the corresponding conductor in the 3D view and, once
  output is loaded, focuses the plot panel on that node/electrode's curves.

This mirrors the main project's own two-layer split (object-model
orchestration over numerical kernels, [ARCHITECTURE.md §1](ARCHITECTURE.md))
for the same reason: keep the parts that must stay simple and auditable
(data contracts) separate from the parts that are allowed to be more
elaborate (Qt/3D/plotting code).

## 7. Phased delivery plan

| Phase | Deliverable | Depends on |
| --- | --- | --- |
| G0 | **Done** — Skeleton PySide6 app (uv project); input JSON loader (schema v1, incl. ADR 0013's `sources`/`frequencies`/`outputs`); tree view only | common/README.md schema (exists) |
| G1 | **Done** — Qt3D viewer for input geometry, authored elements only (§5.1a) | G0 |
| G2 | **Done** — Output JSON schema drafted (§5.2) and promoted to an ADR; results loader + 1D magnitude/phase plots (impedance, node voltages, electrode currents vs. frequency) | solver-side JSON results writer (any implementation) |
| G3 | 2D/spatial plots (touch voltage, GPR profiles) | ROADMAP P7 output classes |
| G4 | Discretised-geometry 3D view (§5.1b) | a solver-side "structure dump" export |
| G5 | Launch a solver as a subprocess from the GUI (decided in scope, §8.4); study authoring/editing remains out of scope | G0–G2 |

Nothing before G2 requires the output schema to *exist* in a solver's
output — only its design, which happens now (§5.2). G0/G1/G2 are implemented
in [`gui/`](../gui/README.md): `gui/src/tupa_gui/data/` (study + results
loaders and dataclasses, unit-tested headless in `gui/tests/`),
`gui/src/tupa_gui/view/` (`tree.py` for G0, `viewer3d.py` for G1,
`plot_panel.py` for G2, wired together as a three-pane splitter in
`main_window.py`). Run with `cd gui && uv sync && uv run tupa-gui
../common/example1.json [--results results.json]`; results can also be
opened from the File menu. G2 plots come from a hand-written ADR 0012
fixture (`gui/tests/fixtures/`) until a solver's results writer lands
(ROADMAP Phase 3). One implementation note that cost real debugging time is
recorded in [`gui/README.md`](../gui/README.md) Troubleshooting: PySide6
does not track Qt3D `QNode` parentage as ownership, so the view layer must
hold Python references to every Qt3D scene object or the scene is silently
emptied by the next Python GC cycle.

## 8. Decisions (author, 2026-07-05)

1. **PySide6 vs. PyQt6** — **PySide6**, for its LGPL license fit with GPLv3.
2. **3D library** — **Qt3D** (not PyVista/VTK): see §4 for the trade-off
   accepted (more hand-built view code, one fewer dependency).
2b. **Repo inclusion** — **same repo** as the rest of TUPÃ, own `gui/`
   folder, per CONVENTIONS.md's sibling-folder pattern.
3. **View-only v1** — **confirmed**: no study authoring/editing until at
   least G5.
4. **Solver invocation** — **the GUI can call the solver** (subprocess),
   confirmed in scope; scheduled as G5, after the core view/plot phases.
5. **Output JSON schema ownership** — **draft it now** (§5.2), not after a
   solver implements it. Frozen as [ADR 0012](adr/0012-results-json-schema.md)
   and cross-referenced from ROADMAP Phase 3/5, ahead of Fortran's JSON
   results writer, so it isn't designed twice.
6. **Packaging** — **uv**-managed (`pyproject.toml` + `uv.lock`); no bundled
   executable for now.

These decisions are formalised in
[ADR 0011](adr/0011-gui-module-technology-and-scope.md) (technology, scope,
repo placement, items 1–4, 6 above) and
[ADR 0012](adr/0012-results-json-schema.md) (item 5, the results schema).
