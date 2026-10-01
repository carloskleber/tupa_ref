# ADR 0027 — Grounding-safety outputs: the `observation` block and potential post-processing

- **Status**: Accepted
- **Date**: 2026-10-01
- **Amends**: [ADR 0012](0012-results-json-schema.md) (spatial outputs go in a
  separate results file) and [ADR 0013](0013-input-schema-sources-frequencies-outputs.md)
  (one optional top-level input block)

## Context

ROADMAP Phase 11 (§7 P7) asks for the engineering outputs of a tower-footing
study: the potential at points off the electrodes, the ground potential rise,
touch and step voltage. Theory.md §3.1 already derives them as post-processing
of a solved sweep — the potential of the solved transversal currents at an
arbitrary point — and ADR 0012 deferred them as "a separate, later schema
extension" because they need `tResult` subtypes and a shape of their own.

The legacy Matlab has the pieces: `PotenciaisSolo2D` (potential along a line),
`PotenciaisSolo3D` (over a rectangle), `PotencialToque` (maximum
$|\psi - u_{\text{node}}|$ over a 36-point, 1 m circle at the surface around a
node) and the older `potsolonovo.m`. Two things in them were not carried over:
`potsolonovo.m` divides by the segment length twice (its `it` is already per
unit length), and `PotencialToque` is unfinished — its `coletaDadosFrequencia`
expects an argument list the solver no longer passes. The model of
`PotencialToque` (current per unit length, $1/(4\pi(\sigma + j\omega\varepsilon))$,
the geometry factor, ideal images) is the one theory.md §3.1 states and the one
implemented.

## Decision

### Input: an optional `observation` block

```json
"observation": {
  "points": [ { "id": "centre", "position": [8, 8, 0] } ],
  "grid":   { "id": "surface", "origin": [-4, -4], "z": 0,
              "lengthX": 24, "lengthY": 24, "nx": 13, "ny": 13,
              "step": { "length": 1.0, "directions": 8 } },
  "touch":  [ { "id": "tower", "node": "Tower_top", "radius": 1.0, "points": 36 } ],
  "steps":  [ { "id": "edge", "from": [16, 8, 0], "to": [17, 8, 0] } ]
}
```

All four members are optional; the block needs a harmonic sweep
(`sources` + `frequencies`) to be evaluated on, and is an error beside a
transient-only case (see Limits).

- **`points[]`**: explicit observation points. Both input forms the roadmap
  asked for exist from the start: this array, and the optional `grid` block.
- **`grid`**: rectangular grid of points in the horizontal plane `z`
  (default 0 — the soil surface), `nx × ny` points spanning `lengthX × lengthY`
  from `origin`. Sites are named `<id>_<ix>_<iy>` (1-based, x-major).
  With `step`, every grid point also gets a **step map** value.
- **`touch[]`**: touch-voltage sites by the legacy geometric definition
  (theory.md §3.1): `max |ψ − u_node|` over `points` (default 36) on a circle of
  `radius` (default 1 m) around the node's `(x, y)`, in the plane `z` (default
  0). `id` defaults to the node id. `node` is validated against the assembled
  structure like every other node reference.
- **`steps[]`**: a step pair, `Δψ = ψ(to) − ψ(from)` (complex).
- The **step map** at a grid point is the maximum over `directions` (default 8)
  equally spaced azimuths of `|ψ(P) − ψ(P + length·ê)|`, `length` default 1 m
  (the IEEE Std 80 stride).

Definitions follow the legacy; the body-circuit and surface-layer derating
factors of IEEE Std 80 [42] stay out of the solver, as decided at the
2026-07-17 Q&A (ADR 0018).

### Evaluation

`mPotentials::computeObservations` (Fortran), `potentials::compute_observations`
(Rust) evaluate theory.md §3.1 on the stored sweep after `runSweep`: no new
unknowns, no factorisation. The medium of a point is soil for `z ≤ 0` and air
above, only segments of that medium contribute, and the image coefficient is the
solver's Γ(ω) (so `numerics.imageModel` applies). A point closer to a segment
axis than the conductor radius is moved to the surface. The geometry factor is
the closed form `asinh(s₁/ρ) − asinh(s₂/ρ)` for every position. All evaluation
points (sites, touch circles, step pairs, step-map neighbours) go through one
pass; Fortran threads it over points with OpenMP, results independent of the
thread count.

### Results: new files, existing ones untouched

ADR 0012 calls spatial outputs a breaking change to its shape, so they are
written to **separate files** — `<base>_potentials.csv` and
`<base>_potentials.json` — next to the unchanged `<base>_results.*`. Existing
readers (the GUI, the fixtures) see no difference.

CSV, tidy form, columns `frequency_hz,quantity,id,x,y,z,re,im`:

| `quantity` | `id` | value |
| --- | --- | --- |
| `potential` | site (point or grid point), with its `x,y,z` | ψ, complex |
| `stepMap` | grid site, with its `x,y,z` | step voltage: `re` = magnitude, `im` = 0 |
| `gpr` | touch site (`x,y,z` empty) | node voltage `u`, complex |
| `touch` | touch site | touch voltage: `re` = magnitude, `im` = 0 |
| `step` | step pair | `Δψ`, complex |

JSON: `{title, frequencies, sites[{id, position, potential[, step]}],
grid{id, nx, ny}, touch[{id, node, gpr, touch}], steps[{id, difference,
magnitude}]}`; complex values are `{"re","im"}` pairs as everywhere, `touch`,
`step` and `magnitude` are real arrays, and `grid`/`touch`/`steps` are omitted
when empty. Everything is indexed positionally against `frequencies`.

The `tResult` family gains `tPotentials` (complex, per site and frequency) and
`tMagnitudes` (real); `tStudy` carries the parsed `observation` and the
`observationResults` of the last evaluation.

## Consequences

- A footing study gets its headline outputs from the run it already does: for
  `common/grid_safety.json` (16 × 16 m grid, tower riser, 13 × 13 surface grid
  with step map, 4 frequencies) the whole run, solve and outputs, takes about 25 ms.
- The surface potential beside a conductor is consistent with the solved
  voltage: at 10 Hz the potential on the surface of a rod segment is within
  0.3 % of its mean node voltage, and 400 m away the potential is the point-source
  field of the injected current, $\Sigma I_t = 1$ A (`test_potentials`).
- Only harmonic results exist. A time-domain map is the inverse transform of
  these phasors; the transient driver does not apply it yet (**Limits**).
- A touch circle in the plane of a conductor that reaches the surface (the
  riser of `grid_safety`) includes points within centimetres of the electrode,
  where ψ approaches the conductor voltage and the step map is steep — a real
  feature of the geometric definition, not a numerical artefact; the regularisation
  to the conductor radius keeps it finite.

## Limits and follow-ups

- **Transient maps** (the roadmap's "reuse the Phase 9 driver"): not done. The
  potential is linear in the solved currents, so it needs the transfer function
  of ψ at each site from the unit sweep and the same spectrum product as a node
  voltage — mechanical, but a change to the transient results shape that wants
  its own item. An `observation` block beside a transient-only case raises an
  error rather than being ignored.
- **Fields.** The electric field itself ($-\nabla\psi - j\omega\mathbf{A}$) and
  path voltages along an arbitrary route (the legacy `camposdif`/`calctensoes`
  line integral) are not provided; ψ at the route's points is.
- **Julia and the GUI** do not implement the block (Julia lags Phase 9 and 10b
  already; the GUI ignores unknown blocks).

## Verification

`fortran/test/test_potentials.f90`: the closed-form geometry factor against
Simpson quadrature (slanted segment, points beside, beyond and just off the end
of the axis, the radius regularisation); a solved rod against the far-field
point-source oracle and against its own node voltages; the reader; GPR equal to
the node voltage; touch voltage and step map recomputed independently from
`potentialsAt`; the $x \leftrightarrow y$ symmetry of the symmetric grid; the
writers. Golden fixtures `grid_safety_expected.csv` and
`grid_safety_potentials_expected.csv` (Fortran), matched by Rust at 2e-8.
