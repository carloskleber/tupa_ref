# Common reference cases

Language-neutral study inputs (and, in the future, expected outputs) shared
by every TUPÃ implementation. Together with the JSON schema described here,
these files are the **public contract** of the project
([ADR 0002](../docs/adr/0002-language-agnostic-object-model.md),
[ADR 0006](../docs/adr/0006-json-io.md)): an implementation is conformant
when it reproduces every case within the stated tolerance.

## Cases

| File | Description | Expected output |
| --- | --- | --- |
| `example1.json` | Buried bare conductor, 2 m, 0.5 m depth, 2 segments — smallest smoke case | none yet (solver pipeline pending, ROADMAP Phase 2) |
| `example2.json` | Two collinear buried conductors, 2 × 10 m, 10 segments each | none yet |

Planned (ROADMAP Phase 5): the Portela-1997 validation conductor (10 m,
0.5 m depth, σ = 0.01 S/m, εr ≈ 10), a vertical rod, and a small grid, each
with an expected-results CSV and tolerance. Note the current examples use
εr = 1 soil — they are smoke tests, **not** the validation case.

## Schema (v1 — [ADR 0006](../docs/adr/0006-json-io.md) format, `sources`/`frequencies`/`outputs` frozen by [ADR 0013](../docs/adr/0013-input-schema-sources-frequencies-outputs.md))

```json
{
  "title": "string",
  "soil": { "conductivity": 0.01, "permittivity": 10.0, "permeability": 1.0 },
  "nodes": [ { "id": "Node_1", "position": [x, y, z] } ],
  "materials": [ { "id": "copper", "epsilonr": 1.0, "mur": 1.0, "sigma": 5.96e7 } ],
  "elements": [ { "type": "line", "id": "Line_1", "from": "Node_1", "to": "Node_2",
                  "radius": 0.01, "segments": 10, "material": "copper" } ],

  "sources": [ { "node": "Node_1", "current": { "re": 1.0, "im": 0.0 } } ],
  "frequencies": { "min": 100.0, "max": 1.0e6, "pointsPerDecade": 3 },
  "outputs": { "nodes": ["Node_1"], "electrodes": ["Line_1"],
               "quantities": ["voltage", "i1", "i2", "inputImpedance"] }
}
```

Semantics:

- Coordinates in metres, right-handed axes, `z` up; the air-soil interface
  is `z = 0` (soil below) — theory.md §2.
- `soil.permittivity`/`permeability` are **relative** (εr, μr);
  `conductivity` in S/m. Same for material `epsilonr`/`mur`/`sigma`.
- `elements[].type`: only `"line"` exists today; unknown types are skipped
  with a warning.
- `segments` is the discretisation count of the element; segment length
  must respect the λ/10 and thin-wire bounds (theory.md §4.1).
- `materials` is optional only if no element references one.
- `sources`, `frequencies`, `outputs` are **optional** (a structure-only
  case file, like `example1.json`/`example2.json`, stays valid) but
  required together to run a sweep. `sources[].current` and the output
  schema's per-frequency values share the same `{"re":..,"im":..}` pair.
  `frequencies` is log-spaced only (`min`/`max` in Hz, `pointsPerDecade`
  density); no explicit frequency list yet (ADR 0013 — waits on the
  json-fortran migration below). `outputs` is a selection *of* the ADR
  0012 result shape (`voltage` per node, `i1`/`i2` per electrode,
  `inputImpedance` derived); omitting it, or a sub-list within it, means
  "everything," matching pre-v1 behaviour. See ADR 0013 for the full
  rationale.

## Parser subset limits (ADR 0006)

Case files must stay inside the minimal-parser subset of the Fortran
implementation: objects/arrays/strings/numbers/booleans/null only, **no
string escape sequences**, at most **64 items per object or array**.
Exceeding a limit raises a feh error naming the ADR. The moment a real case
needs more (e.g. a long explicit frequency list), the plan is to switch to
json-fortran, not to grow the custom parser. These cases double as parser
conformance tests.
