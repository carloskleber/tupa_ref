# TUPÃ GUI

Solver-agnostic viewer for TUPÃ study inputs and results. Design doc:
[../docs/GUI_SDD.md](../docs/GUI_SDD.md); technology, scope, and repo
placement are fixed by [ADR 0011](../docs/adr/0011-gui-module-technology-and-scope.md),
and the results JSON schema it will read is fixed by
[ADR 0012](../docs/adr/0012-results-json-schema.md).

This module talks to JSON files only ([common/README.md](../common/README.md)
schema), never to a solver's internals ([ADR 0002](../docs/adr/0002-language-agnostic-object-model.md),
[ADR 0006](../docs/adr/0006-json-io.md)). It has no dependency on `fortran/`
and is buildable/runnable on its own.

**Status**: G0 (tree view of an input study), G1 (Qt3D geometry view,
authored elements only), and G2 (results loader + 1D magnitude/phase plots)
are implemented. See GUI_SDD.md §7 for later phases.

## Setup

[uv](https://docs.astral.sh/uv/)-managed, Python 3.11+:

```bash
cd gui
uv sync
uv run tupa-gui ../common/example1.json
# optionally open a results JSON (ADR 0012 schema) alongside it:
uv run tupa-gui ../common/example1.json --results path/to/results.json
```

Both files can also be opened from the File menu.

## Layout

- `src/tupa_gui/data/` — dataclasses mirroring the object model and the
  results schema (ADR 0012) + their JSON loaders. No Qt dependency;
  unit-testable headless.
- `src/tupa_gui/view/` — QTreeView, Qt3D viewer, PyQtGraph plot panel, main
  window. Dumb: renders what the data layer gives it, no JSON parsing.
- `src/tupa_gui/app.py` — entry point (`tupa-gui` script).

## Troubleshooting

**3D pane is blank (only the background clear color), everything else works.**
Root cause found while building G2, after several misdiagnoses (layouts,
QPA platform, surface formats — all red herrings): **PySide6 does not
register Qt3D's `QNode` parent-child links as ownership**, so any Qt3D scene
object (entity, mesh, material, transform, light) whose Python wrapper
becomes unreferenced is destroyed — C++ object included — at the next Python
GC cycle. Passing the parent at construction (`QSphereMesh(entity)`) is
*not* enough. The scene then empties silently: no exception, no log, the
viewport keeps clearing but draws nothing. The trigger is anything that
allocates enough to run the garbage collector (loading results into the
plot panel was the original trigger; `gc.collect()` reproduces it on
demand). The fix in `view/viewer3d.py` is to append every created Qt3D
object to a `self._scene` list kept alive for as long as the scene is
displayed — keep that pattern for any new geometry added there.

## Testing

```bash
uv run pytest
```

The data-layer tests run headless (no display needed). There is no hosted
CI for this project (see [../docs/ROADMAP.md](../docs/ROADMAP.md) §9); run
`uv run pytest` locally before merging GUI changes.
