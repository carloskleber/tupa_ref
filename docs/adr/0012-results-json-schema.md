# ADR 0012 — Results (output) JSON schema v0

- **Status**: Accepted (schema v0, open to revision before more than one
  writer/reader depends on it)
- **Date**: 2026-07-05

## Context

ROADMAP Phase 3 item 3 calls for a CSV writer (primary) and a JSON results
writer, but no output schema has been drafted anywhere in the docs — only the
input schema (`common/README.md`) is specified. The GUI module
([ADR 0011](0011-gui-module-technology-and-scope.md)) needs a results format
to load and plot, and all three planned implementations (Fortran, Python,
Rust — ADR 0002) will eventually write one. Designing the schema once, now,
avoids reverse-documenting it from whichever implementation happens to write
it first, and avoids the GUI and the Fortran writer independently inventing
incompatible shapes (GUI_SDD.md §5.2).

## Decision

Freeze the following as the v0 results JSON shape:

```json
{
  "title": "string",
  "frequencies": [1e2, 1e3, ...],
  "nodes": [ { "id": "Node_1", "voltage": [ {"re":.., "im":..}, ... ] } ],
  "electrodes": [ { "id": "...", "i1": [...], "i2": [...] } ],
  "derived": { "inputImpedance": [ {"re":.., "im":..}, ... ] }
}
```

- All per-frequency arrays are indexed positionally against `frequencies`;
  complex values use `{"re":.., "im":..}` pairs, matching the input schema's
  convention.
- `nodes`/`electrodes` are keyed back to the input JSON's element `id`s and
  carry no geometry of their own — a results file is only meaningful loaded
  alongside the input file that produced it.
- `derived` holds convenience quantities (starting with `inputImpedance`)
  that every consumer could recompute from `nodes`/`electrodes`, but are
  written once by the solver so the GUI and other readers don't each
  reimplement the same derivation.
- Spatial outputs (potential maps, touch/step voltage — ROADMAP P7) are out
  of v0; they need `tResult` subtypes that don't exist yet and will be a
  separate, later schema extension.

## Consequences

- Fortran's Phase 3 JSON results writer, the future Python/Rust ports, and
  the GUI's output loader (GUI_SDD.md §5.2, phase G2) all target this same
  shape from the start — no reverse-documentation from a first
  implementation, no schema designed twice.
- CSV stays the primary tabular output (ADR 0006); this schema is the
  structured counterpart, not a replacement.
- Adding a field (a new `derived` quantity, a new per-node series) is
  backward-compatible; renaming or removing a field, or adding spatial
  outputs, is a breaking change and needs a new ADR revising this one, per
  ADR 0002's cross-implementation contract discipline.
- **Exercised (2026-07-09)**: Fortran's `mResultsWriter%writeResultsJson`
  writes exactly this shape, tested in `fortran/test/test_sweep.f90`; no
  friction found. One implementation choice this ADR left open and the
  writer had to resolve: `derived.inputImpedance` assumes a single source
  node (the sweep's first), since the schema doesn't define a multi-port
  variant — worth a v1 note if a multi-source study ever needs it.
