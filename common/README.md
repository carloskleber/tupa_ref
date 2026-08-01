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
| `buried_conductor_short.json` | Buried bare conductor, 2 m, 0.5 m depth, 2 segments — smallest smoke case | none (structure-only, no `sources`/`frequencies`) |
| `buried_conductor_long.json` | Two collinear buried conductors, 2 × 10 m, 10 segments each | none (structure-only) |
| `portela1997.json` | The Phase 2 validation conductor (10 m, 0.5 m depth, σ = 0.01 S/m, εr = 10), 1∠0° A at `Node_1`, 10 Hz-1 MHz | `portela1997_expected.csv` |
| `rod.json` | Single vertical buried rod (3 m, -0.5 to -3.5 m), same soil, 1∠0° A at `Node_1`, 10 Hz-1 MHz, 8 points/decade | `rod_expected.csv` |
| `rod_air.json` | Two collinear rods sharing `Node_2` at the air-soil interface (z=0): 10 m above ground down to a 5 m buried rod, same soil/material, 1∠0° A at `Node_1` (top), 10 Hz-1 MHz | none yet — runs NaN-free since the ADR 0019 fix; fixture still to be generated (see below) |
| `grid.json` | Small buried grounding grid, one square mesh (4 nodes/edges), 1∠0° A at `Node_A`, 100 Hz-100 kHz | `grid_expected.csv` |
| `portela1997_transient.json` | Same geometry/soil as `portela1997.json`, but a `signal` block (ADR 0015) instead of `sources`/`frequencies`: transient GPR under a 1.2/50 µs, 30 kA double-exponential surge | none (internal-consistency check only, theory.md §9.2 data gap — see `test_transient.f90`) |
| `silva2025_rho{100,300,1000,2400}.json` | Silva et al. 2025 (SBAI, references.md [36]) PEEC-vs-HEM base case: buried horizontal electrode, 60 m, 7 mm radius, 0.5 m depth, `alipio-visacro` dispersive soil (theory.md §7) at ρ0 = 100/300/1000/2400 Ω·m, 1∠0° A at `Node_1`, 128 log-spaced points 100 Hz–4 MHz (`pointsPerDecade: 27.6`, ADR 0013's `round(ppd·log10(fmax/fmin))+1` formula) — matches the paper's 2⁷ frequency samples. For comparison against the paper's Fig. 3 (\|Z(ω)\|); no tabulated digitised curve exists yet, so there is no `_expected.csv` (internal passivity/plausibility check only) | none yet |
| `silva2025_rho{100,300,1000,2400}_transient.json` | Same geometry/soil as the files above, but a `signal` block (ADR 0015): GPR at `Node_1` under De Conti & Visacro [38]'s **MCS_FST#2** double-peaked first-stroke current (7 `terms`, physical amplitudes, no `imax` rescale), `nyquistHz: 4e6`, `fftPoints: 4096`. For comparison against the paper's Fig. 4 (GPR(t)) — see [`docs/validation/silva2025-fig4.md`](../docs/validation/silva2025-fig4.md), including why MCS_FST#2 rather than the legacy 6-term MCS_FST#1 | none yet (plausibility check only, same caveat as the frequency-domain files above) |
| `grcev_fig12_l{10,100}_rho{30,300,3000}.json` | Grcev et al. 2018 (IEEE TPWRD, references.md [23]) §IX-B case: buried horizontal electrode, ℓ = 10 or 100 m, 7 mm radius, 0.5 m depth, homogeneous non-dispersive soil (ρ1 = 30/300/3000 Ω·m, εr = 10), 0.25 m segments (theory.md §4.1 λ/10 bound at 10 MHz), 1∠0° A at `Node_1`, 101 log-spaced points 100 Hz–10 MHz (`pointsPerDecade: 20`). For comparison against the paper's Fig. 12 (rigorous full-wave model's \|Z(ω)\|, not a circuit-model approximation) — see [`docs/validation/grcev-fig12.md`](../docs/validation/grcev-fig12.md) | none yet (plausibility check only, same caveat as the Silva files above) |
| `portelaMesh.json` | Native `"mesh"` element demo (ADR 0020): a single 32x32 m grounding grid, 5x5 main nodes (8 m pitch), 5 segments/bar, corner at `(0, -32, -1)` so the grid spans `x` in `[0, 32]`, `y` in `[-32, 0]`, from the classic layout in Portela's *Frequency and Transient Behavior of Grounding Systems* papers (references.md — the M2 point at x=30,y=-30 used there sits inside this mesh's footprint) | none — **structure-only** (no `sources`/`frequencies`): a full sweep over 200 electrodes' worth of mostly-non-parallel pairs is impractical before ROADMAP §7 P1 lands, so this case stays fast to parse/assemble (185 nodes, 200 electrodes — verified in `fortran/test/test_mesh_element.f90`'s topology tests, not by solving this file) rather than tempting an hours-long run |
| `lima_fig6.json` | Lima et al. 2020 (IEEE TEMC, references.md [11]) §III-B Case #9: distribution tower grounding — 4 horizontal electrodes (6 m) radiating 90° apart from a center node, each ending in a vertical rod (3 m), plus a 5th vertical rod at the center (injection point); homogeneous soil (σ1 = 1 mS/m, εr = 10); 12.5 mm radius, arms at -0.5 m with rods to -3.5 m (both inferred — see writeup), 0.5 m segments, 1∠0° A at `Node_C`, 150 log-spaced points 100 Hz–10 MHz (`pointsPerDecade: 29.8`). For comparison against the paper's Fig. 6 MHEM curve — see [`docs/validation/lima-fig6.md`](../docs/validation/lima-fig6.md) | none yet (plausibility check only; case geometry only partially specified by the paper) |

`buried_conductor_short.json`/`buried_conductor_long.json` stay εr = 1 soil smoke tests with no
`sources`/`frequencies` block. The other four carry `sources`/
`frequencies`/`outputs` (ADR 0013) and are runnable with `runStudyFromFile`
(`fortran/src/Tupa.f90`) or directly via the CLI (`fpm run -- ../common/rod.json`,
[fortran/README.md](../fortran/README.md#running-tupa)). `rod_air.json` is
the odd one out: it exercises a structure with elements in *both* media at
once (one rod entirely above ground, one entirely below, joined at the
z=0 interface node) — a case none of the others cover. Adding it exposed
a real bug, now fixed ([ADR 0019](../docs/adr/0019-air-medium-hardcoded-vacuum.md)): `tStructure%air`
(`fortran/src/Structure.f90`) was never populated, so every electrode
positioned in air computed against a zeroed-out air admittance and the
sweep returned `NaN` end to end. Air is now hardcoded to vacuum (εr=1,
μr=1, σ=0), exactly like the Matlab reference — there is deliberately no
JSON `"air"` block. The case runs NaN-free with a plausible
low-frequency Zin (≈ 20.9 Ω, vs ≈ 21.0 Ω from the analytical rod
ground-resistance formula); its `_expected.csv` fixture and
`fortran/test/test_common_cases.f90` wiring are still to be added once
the air-side physics is validated beyond that sanity check. The other
three's `*_expected.csv`
fixtures are **regression
(golden) files** generated by this implementation, not an independent
physics oracle — no tabulated Portela 1997 curve data exists yet
(theory.md §9.2) and no cross-code harness exists yet (ROADMAP §7 P3). They
pin today's numerics for this implementation and, per ADR 0002, are the
conformance target future Python/Rust ports must reproduce within
tolerance. `fortran/test/test_common_cases.f90` diffs a fresh run against
each fixture (relative tolerance 1e-6) and re-checks passivity
independently of the fixture. `grid.json` is deliberately kept to a single
4-electrode mesh (not a larger multi-cell grid): every non-parallel
segment pair falls back to 2-D adaptive quadrature (`geometryFactor2D`),
which costs roughly 1-2 s per pair regardless of touching/singularity at
today's tolerances (ROADMAP §6 "Quadrature tolerances", §7 P1) — a bigger
grid is worth adding once the P1 mHEM single-integral kernel lands.

## Schema (v1 — [ADR 0006](../docs/adr/0006-json-io.md) format, `sources`/`frequencies`/`outputs` frozen by [ADR 0013](../docs/adr/0013-input-schema-sources-frequencies-outputs.md), `signal` added by [ADR 0015](../docs/adr/0015-time-domain-signal-schema.md), voltage sources and Heidler `terms` by [ADR 0016](../docs/adr/0016-voltage-sources-by-superposition.md)/0015 amendment, `"mesh"` element by [ADR 0020](../docs/adr/0020-grid-mesh-element.md))

```json
{
  "title": "string",
  "soil": { "conductivity": 0.01, "permittivity": 10.0, "permeability": 1.0 },
  "nodes": [ { "id": "Node_1", "position": [x, y, z] } ],
  "materials": [ { "id": "copper", "epsilonr": 1.0, "mur": 1.0, "sigma": 5.96e7 } ],
  "elements": [ { "type": "line", "id": "Line_1", "from": "Node_1", "to": "Node_2",
                  "radius": 0.01, "segments": 10, "material": "copper" },
                { "type": "mesh", "id": "Grid_1", "position": [0.0, 0.0, -0.5],
                  "lengthX": 10.0, "lengthY": 10.0, "rowsX": 3, "rowsY": 3,
                  "radius": 0.01, "segments": 2, "material": "copper" } ],

  "sources": [ { "node": "Node_1", "current": { "re": 1.0, "im": 0.0 } } ],
  "frequencies": { "min": 100.0, "max": 1.0e6, "pointsPerDecade": 3 },
  "outputs": { "nodes": ["Node_1"], "electrodes": ["Line_1"],
               "quantities": ["voltage", "i1", "i2", "inputImpedance"] },

  "signal": {
    "waveform": "doubleExp", "imax": 30000.0, "front": "f1_2_50", "jones": false,
    "sourceNode": "Node_1", "observeNodes": ["Node_1"], "observeElectrodes": ["Line_1_e1"],
    "nyquistHz": 1.0e6, "fftPoints": 1024, "freqZeroHz": 1.0e-6
  }
}
```

Semantics:

- Coordinates in metres, right-handed axes, `z` up; the air-soil interface
  is `z = 0` (soil below) — theory.md §2.
- `soil.permittivity`/`permeability` are **relative** (εr, μr);
  `conductivity` in S/m. Same for material `epsilonr`/`mur`/`sigma`.
- `soil.type` (optional, default `"linear"`) selects the dispersion model
  (`fortran/src/Material.f90`, theory.md §7): `"linear"` (shown above) takes
  `permittivity`/`permeability`/`conductivity`; `"portela"` (Lima–Portela,
  ADR 0007) takes `permeability`/`sigma0`/`alpha0`/`kr`; `"alipio-visacro"`
  (Alipio & Visacro [14], mean parameter set) takes `permeability`/`sigma0`
  only — e.g. `{ "type": "alipio-visacro", "permeability": 1.0, "sigma0": 0.01 }`.
  See `silva2025_rho100.json` for a worked example.
- `elements[].type`: `"line"` or `"mesh"` (ADR 0020); unknown types are
  skipped with a warning.
- `segments` is the discretisation count of the element (per bar, for
  `"mesh"`); segment length must respect the λ/10 and thin-wire bounds
  (theory.md §4.1).
- `"mesh"` (ADR 0020) is a rectangular, axis-aligned grounding grid: a
  composite element that plants its own `rowsX * rowsY` main nodes on a
  regular grid — `rowsX` bars parallel to the X axis (each `lengthX` long,
  evenly spaced along Y), `rowsY` bars parallel to Y — from corner
  `position` (3D, same field as a node's `position`; `position[2] == 0`,
  exactly on the air-soil interface, is rejected — theory.md §2), and wires
  every adjacent pair with a `"line"`-equivalent bar (`radius`/`segments`/
  `material`, same meaning as `line`'s). Main nodes are named
  `"<mesh id>-<row:02d><col:02d>"` (0-based, so `rowsX`/`rowsY <= 100`) and
  are externally referenceable, e.g. by a `sources[].node` injection or
  another element's `from`/`to` (a down-conductor connecting to a grid
  corner) — same ID gotcha as `line` applies if `segments > 1` on a bar
  (the bar's own internal nodes/electrodes get `_nK`/`_eK` suffixes, not
  the main-node IDs). Because a `"mesh"` is one array item regardless of
  grid size, it is not subject to the 64-items-per-array parser cap below
  the way a manually flattened grid (enumerated `nodes`/`elements`, as
  `horizontal_vertical_mesh.json` predates this element and still does)
  would be — but running an actual frequency sweep over a real-sized grid
  is a different constraint: any non-parallel, non-touching segment pair
  falls back to ~1-2s-per-pair 2-D adaptive quadrature (see the `grid.json`
  note below), impractical until ROADMAP.md §7 P1 lands. See
  `common/portelaMesh.json` and ADR 0020.
- `materials` is optional only if no element references one.
- `nodes`/`elements` may each be omitted entirely (equivalent to an empty
  array) — e.g. a case built from a single `"mesh"` element needs no
  top-level `nodes` at all, since the element creates its own.
- `sources`, `frequencies`, `outputs` are **optional** (a structure-only
  case file, like `buried_conductor_short.json`/`buried_conductor_long.json`, stays valid) but
  required together to run a sweep. A `sources[]` entry carries **either**
  `"current"` (A) **or** `"voltage"` (V) ([ADR 0016](../docs/adr/0016-voltage-sources-by-superposition.md)
  — ideal voltage source, converted to an equivalent current injection by
  unit-injection superposition in the study layer, per ADR 0010); both use
  the same `{"re":..,"im":..}` complex pair as the output schema's
  per-frequency values.
  `frequencies` is log-spaced only (`min`/`max` in Hz, `pointsPerDecade`
  density); no explicit frequency list yet (ADR 0013 — waits on the
  json-fortran migration below). `outputs` is a selection *of* the ADR
  0012 result shape (`voltage` per node, `i1`/`i2` per electrode,
  `inputImpedance` derived); omitting it, or a sub-list within it, means
  "everything," matching pre-v1 behaviour. See ADR 0013 for the full
  rationale. **Fortran reader**: `fortran/src/Tupa.f90::loadStudy` (optional
  arguments) and the `runStudyFromFile` convenience wrapper; filtering
  itself happens at write time in `mResultsWriter` (`runSweep` always
  computes/stores every node and electrode).
- **`outputs.electrodes` ID gotcha**: electrode results are keyed by the
  *discretised segment* ID, not the input element ID — a `"line"` element
  `"id": "Line_1"` with `"segments": 10` produces electrodes `Line_1_e1`
  … `Line_1_e10` (internal nodes are `Line_1_n1` …), per
  `fortran/src/element/Line.f90::assembleLine`. `outputs.electrodes`/
  `outputs.nodes` must name these generated IDs, not the element/boundary-
  node ID, unless `segments: 1` and there are no internal nodes to worry
  about. None of the four sweep cases above use `outputs.nodes`/
  `outputs.electrodes` for this reason (only `outputs.quantities`, which
  has no such gotcha) — see `test_common_cases.f90` for a worked filtering
  example using the correct generated ID. Naming the wrong (undiscretised)
  ID here, or in `sources[].node`/`signal.sourceNode`/`observeNodes`/
  `observeElectrodes` below, is caught immediately by
  `fortran/src/Tupa.f90::validateStudyReferences` — right after structure
  assembly, before any geometry-factor or solve work runs — rather than
  deep inside a sweep, or (for `outputs.*`) not at all.
- **`signal`** ([ADR 0015](../docs/adr/0015-time-domain-signal-schema.md))
  is optional and independent of `sources`/`frequencies` — a case runs a
  transient (time-domain) solve instead of, or alongside, a harmonic sweep.
  `waveform` is `"doubleExp"` or `"heidler"` (`fortran/src/Signal.f90`);
  `front`/`jones` apply only to `"doubleExp"`. For `"heidler"`, an optional
  `terms` array (ADR 0015 amendment, 2026-07-17) gives the standard
  parametrised Heidler function (Heidler 1985 [37] / IEC 62305-1 [39]) —
  one `{"i0", "n", "tau1", "tau2"}` object per term; `imax` is then
  optional (absent = physical amplitudes, present = peak rescale). Without
  `terms`, `"heidler"` keeps the legacy fixed 6-term set (De Conti &
  Visacro [38], MCS_FST#1) and `imax` is required. `observeNodes` is an array
  (v(t) is computed for every entry at no extra solve cost — the transient
  pipeline's single unit-current sweep already covers every node);
  `observeElectrodes` is an optional array of *discretised* electrode IDs
  (same ID gotcha as `outputs.electrodes` above) for i1(t)/i2(t). `fftPoints`
  is the time/FFT sample count, stated explicitly (must be a power of two).
  See `portela1997_transient.json` for a worked example.

## Parser (ADR 0006)

The Fortran implementation reads case files with json-fortran (via a thin
wrapper, `fortran/src/JsonParser.f90`): the full JSON grammar is supported —
no item-count cap, string escape sequences work — and a malformed file
raises a feh error with json-fortran's own line/column-aware message. These
cases still double as parser conformance tests, and as conformance tests for
`validateStudyReferences`'s ID cross-checks (see the `signal`/`outputs`
notes above).
