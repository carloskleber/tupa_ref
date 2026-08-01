# ADR 0020 — Grid/mesh element (`"type": "mesh"`) and FIFO element order

- **Status**: Accepted
- **Date**: 2026-07-31

## Context

ROADMAP.md §7 lists "Grid/mesh generator element — S. Composite element
emitting `tLine`s on a rectangular pattern (legacy `Malha.m` is exactly
this, though unfinished)" as a remaining item. This ADR implements it: a
native JSON element type for a rectangular, axis-aligned grounding grid —
declared once as a compact object, expanded into main nodes and conductor
bars internally by the engine, mirroring the legacy Matlab `malha` command
and C++ `elementos::Malha`, rather than requiring every node/bar to be
enumerated by hand in the JSON `nodes`/`elements` arrays (as
`common/horizontal_vertical_mesh.json` had to do before this element
existed).

Both legacy implementations were compared (background only, outside this
repo per `CLAUDE.local.md` — neither copied verbatim):

- `mom_matlab/classes/+elementos/Malha.m` connects nodes via an index
  formula (`ninic + numcx*(i1-1) + i2-1`) that is only correct for a
  **square** grid (`numcx == numcy`). Every shipped `.est` case using it
  (`malha0.est` 5x5, `malha1.est` 9x9) happens to be square, which is
  presumably why the bug was never caught.
- `mom_cpp/src/elementos/Malha.{h,cpp}` is self-consistent (two explicit
  loops, nodes addressed by name rather than arithmetic) and documents a
  naming mnemonic, reused here unchanged:

  - main node: `"<mesh id>-<row:02d><col:02d>"`
  - bar id: `"<mesh id>-<row1:02d><col1:02d>-<row2:02d><col2:02d>"`

  (0-based `row`/`col`.) Main nodes must be externally referenceable — by a
  `sources[].node` current injection, or by another element's `from`/`to`
  (e.g. a down-conductor) — which is why this element creates them itself
  (like a `tLine`'s own internal nodes) rather than requiring the user to
  pre-declare them in the top-level `nodes` array.

A grid at `z = 0` (exactly the air-soil interface) is rejected: `Study.f90
::prepareStudy` classifies each segment as air/soil by the *sign* of its
midpoint `z`, so `z = 0` is a degenerate image distance (image coincides
with the source), not a supported third case.

**Ordering prerequisite.** `Structure.f90`'s element list was a linked list
built by *prepend* (`addElementToStructure`), so elements assembled in the
*reverse* of their JSON declaration order. This was invisible as long as
every element type only ever referenced pre-declared boundary nodes from
the top-level `nodes` array (already fully populated before any element
assembles) — true of `line`, the only element type that existed before this
ADR. A `mesh` element creates its *own* main nodes; if a later JSON element
(e.g. a `line` connecting to a mesh corner) is declared *after* the mesh,
correctness now depends on assembly happening in declaration order. Fixed
by appending at the tail instead (O(1), via a new `elementsTail` field) —
verified safe: the only other reader of element order,
`Study.f90`'s report-printing loop, is cosmetic.

**Performance is explicitly out of scope here.** ROADMAP.md §7 flags this
item "practical only after §7 P1 (quadrature cost)": any pair of
non-parallel, non-touching segments falls back to ~1-2s-per-pair 2-D
adaptive quadrature (`common/README.md`'s `grid.json` note) — a modest grid
already produces thousands of such pairs. `common/portelaMesh.json` (the
demo case for this element, named after Portela's *Frequency and Transient
Behavior of Grounding Systems* — the M2 point in that paper, x=30 y=-30,
sits inside this mesh's footprint) is therefore shipped **structure-only**
(no `sources`/`frequencies`, same as `buried_conductor_short/long.json`):
fast to parse/assemble/verify (185 nodes, 200 electrodes for its 5x5 rows,
8 m pitch), but not an invitation to an accidental hours-long sweep before
P1 lands.

## Decision

- New module `fortran/src/element/Mesh.f90` (`mElementMesh`), type
  `tMeshElement extends tElement` — named to avoid colliding with the
  unrelated `tMesh` linear-system type (`Mesh.f90`). `assembleMesh`
  validates parameters (`rowsX`/`rowsY >= 2` and `<= 100`, `segments >= 1`,
  positive lengths, `position(3) /= 0`), creates the `rowsX * rowsY` main
  nodes directly, then for every adjacent pair constructs a local `tLine`
  (`newElementLine`, `mElementLine`) and calls its `%assemble` directly —
  reusing `tLine`'s own internal-node/electrode chain logic instead of
  duplicating it, exactly the "composite element emitting `tLine`s"
  ROADMAP describes.
- JSON schema addition (`elements[].type == "mesh"`):
  `id`, `position` (grid corner, 3D — reuses the node `position` field
  name), `lengthX`, `lengthY`, `rowsX`, `rowsY`, `radius`, `segments`,
  `material` (the last four reuse `line`'s field names/semantics).
- `Structure.f90`: `addElementToStructure` appends (FIFO) instead of
  prepending (LIFO); `tStructure` gains an `elementsTail` pointer.
- `Tupa.f90::loadStudy` also gained `json_has` guards around the top-level
  `"nodes"`/`"elements"` blocks (previously unconditional, unlike
  `"materials"`) — found while adding `portelaMesh.json`, the first case
  with no pre-declared boundary nodes at all: an absent `"nodes"` key
  dereferenced a null `tJsonValue` pointer in `json_size`, segfaulting.

## Consequences

- A grounding grid is now a single compact JSON object instead of an
  enumerated `nodes`/`elements` list — and, being a single array item, it
  is not subject to the ADR 0006 minimal-parser's 64-items-per-array cap
  the way a manually flattened grid would be (`common/README.md`'s "Parser
  subset limits").
- `fortran/test/test_mesh_element.f90` covers: node/electrode counts and
  ID mnemonic for a square grid; full adjacency-degree topology
  verification for a **non-square** grid (the concrete regression test for
  the legacy Matlab stride bug); the FIFO ordering fix (a `line` declared
  after a `mesh`, referencing one of its main nodes).
- Running an actual frequency sweep over a real-sized grid (like
  `portelaMesh.json` at full scale) stays impractical until ROADMAP §7 P1
  (mHEM single-integral kernel) lands — unchanged by this ADR, just no
  longer blocked on the element type existing at all.
