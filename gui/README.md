# TUPÃ GUI

Solver-agnostic viewer for TUPÃ study inputs and results. Design doc:
[../docs/GUI_SDD.md](../docs/GUI_SDD.md) (proposal — implementation may run
ahead of formal ADR acceptance per that document's phase G0/G1 status).

This module talks to JSON files only ([common/README.md](../common/README.md)
schema), never to a solver's internals ([ADR 0002](../docs/adr/0002-language-agnostic-object-model.md),
[ADR 0006](../docs/adr/0006-json-io.md)). It has no dependency on `fortran/`
and is buildable/runnable on its own.

**Status**: G0 (tree view of an input study) and G1 (Qt3D geometry view,
authored elements only) are implemented. See GUI_SDD.md §7 for later phases.

## Setup

[uv](https://docs.astral.sh/uv/)-managed, Python 3.11+:

```bash
cd gui
uv sync
uv run tupa-gui ../common/example1.json
```

## Layout

- `src/tupa_gui/data/` — dataclasses mirroring the object model + the JSON
  loader. No Qt dependency; unit-testable headless.
- `src/tupa_gui/view/` — QTreeView, Qt3D viewer, main window. Dumb: renders
  what the data layer gives it, no JSON parsing.
- `src/tupa_gui/app.py` — entry point (`tupa-gui` script).

## Troubleshooting

**3D pane is blank, tree view works fine.** Two independent causes were
found and fixed while building G1 — worth knowing if the symptom ever comes
back after touching `view/viewer3d.py`:

- **Qt3D components need an explicit Python-visible parent.** Every mesh/
  material/transform in `viewer3d.py` is constructed as `Qt3DExtras.QSomething(entity)`
  (parent passed at construction), not `QSomething()` followed by
  `entity.addComponent(...)` alone. Without the explicit parent, nothing
  else holds a Python reference to the component once the `_add_*` method
  returns, Python's GC frees the underlying C++ object, and the entity is
  left with no mesh/material — silently, no exception, nothing rendered.
  Keep this pattern for any new geometry added here.
- **`QWidget.createWindowContainer` (used to embed the `Qt3DWindow`) is
  unreliable under Qt's native `wayland` QPA platform.** `app.py`
  auto-switches to `xcb` (XWayland) when it detects a Wayland session
  (`WAYLAND_DISPLAY` set) and no explicit `QT_QPA_PLATFORM`. If the pane is
  blank again, confirm the platform in use (`QT_DEBUG_PLUGINS=1`, or check
  `QApplication.instance().platformName()`) and force it:
  `QT_QPA_PLATFORM=xcb uv run tupa-gui ...`. Needs XWayland installed.

## Testing

```bash
uv run pytest
```

The data-layer tests run headless (no display needed). There is no hosted
CI for this project (see [../docs/ROADMAP.md](../docs/ROADMAP.md) §9); run
`uv run pytest` locally before merging GUI changes.
