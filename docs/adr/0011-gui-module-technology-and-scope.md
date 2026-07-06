# ADR 0011 — GUI module: technology, scope, and repo placement

- **Status**: Accepted
- **Date**: 2026-07-05

## Context

Every implementation reads a JSON study, and Phase 3 will add a JSON/CSV
results writer, but there is no way to *see* either: inspecting a study means
reading JSON by eye, and results are text tables. [GUI_SDD.md](../GUI_SDD.md)
proposes a separate, solver-agnostic GUI module to load and display both,
shared by every current and future TUPÃ implementation. Building it required
picking a language and Qt binding, a 3D library, where the code lives in the
repo, how much of the tool ships in v1, whether it may ever invoke a solver,
and how it is packaged — recorded in GUI_SDD.md §4 and §8 and formalised here.

## Decision

1. **Language**: Python 3 — matches the planned Python solver port (ROADMAP
   Phase 8) without depending on it; largest ecosystem for Qt bindings and
   scientific 3D/plotting.
2. **GUI toolkit**: **PySide6** (Qt for Python), not PyQt6 — its LGPL license
   fits a GPLv3 project better than PyQt6's GPL/commercial dual license.
3. **3D viewer**: **Qt3D** (`PySide6.Qt3DCore`/`Qt3DRender`/`Qt3DExtras`), not
   PyVista/VTK — stays inside the Qt ecosystem the rest of the GUI already
   depends on, one fewer dependency in DISTRIBUTION.md's inventory. Trade-off
   accepted: conductors/nodes are built from `Qt3DExtras` primitives
   (`QCylinderMesh`, `QSphereMesh`) by hand rather than using PyVista's
   ready-made scientific-visualisation glyphs.
4. **1D/2D plotting**: **PyQtGraph** — native Qt integration (no separate
   event loop to reconcile), interactive pan/zoom/log-axes, handles large
   frequency sweeps without redraw lag. Matplotlib stays available as an
   optional export path for publication-quality static figures.
5. **Repo placement**: new top-level `gui/` folder in this repo, sibling to
   `fortran/` and `common/` — same sibling-folder rule CONVENTIONS.md applies
   to future `python/`/`rust/` implementations. Own `pyproject.toml`, own
   README, no code under `fortran/`.
6. **Scope (v1)**: view-only. Load and display an input study JSON (tree view
   + Qt3D render of authored elements) and, once it exists, a results JSON
   (1D plots). No study authoring/editing until at least phase G5
   (GUI_SDD.md §7).
7. **Solver invocation**: in scope for the project — the GUI may launch a
   solver as a subprocess — but deferred to phase G5, after the core
   view/plot phases land.
8. **Packaging**: **uv**-managed project (`pyproject.toml` + `uv.lock`), no
   bundled executable for now, matching DISTRIBUTION.md's "source only, no
   binary artifacts" default.

## Consequences

- The GUI depends only on the JSON contracts (ADR 0002, ADR 0006) and never
  on a solver's internals; no solver language gains a Python/Qt dependency,
  and the GUI module can be dropped or postponed without affecting `fortran/`
  or any future port.
- The Qt3D choice means the view layer carries more hand-built primitive
  code than a PyVista-based viewer would, in exchange for one fewer
  dependency; revisit only if that maintenance cost proves higher than
  expected.
- View-only v1 keeps the GUI from becoming a second place where physics or
  discretisation logic lives (GUI_SDD.md §2) — any feature request requiring
  the GUI to reimplement solver logic is rejected in favour of a solver-side
  JSON export instead.
- The full design, phased delivery plan (G0–G5), and rationale for each
  choice live in [GUI_SDD.md](../GUI_SDD.md); this ADR makes those decisions
  binding, superseding its "not binding until an ADR is written" caveat.
- The results JSON schema the GUI reads (GUI_SDD.md §5.2) is specified
  separately in [ADR 0012](0012-results-json-schema.md).
