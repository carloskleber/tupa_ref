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
| `portela1997_transient_interpolated.json` | ROADMAP Phase 9 item 1: as `portela1997_transient.json`, `transferFunction: "interpolated"` — H(f) solved on the case's `frequencies` axis (100 Hz–1 MHz, 20/decade, 81 points; `freqZeroHz: 100`) and pchip-interpolated onto the 513 FFT bins | `portela1997_transient_interpolated_expected.csv` (transient shape) |
| `portela1997_transient_hann.json`, `portela1997_transient_hann_time.json` | ROADMAP Phase 9 item 2: as `portela1997_transient.json`, with a half-Hann `window` in the `"spectral"` and the `"time"` placement | `portela1997_transient_hann_expected.csv`, `portela1997_transient_hann_time_expected.csv` |
| `portela1997_transient_multi.json` | ROADMAP Phase 9 item 4: three injections via `signal.sources` — the legacy differential pattern (+30 kA at `Node_1`, −30 kA at `Node_2`, 1.2/50 µs) plus a 1 kA, 5 kHz sine at `Node_1`; observes both ends of the conductor | `portela1997_transient_multi_expected.csv` |
| `portela1997_transient_nlt.json` | ROADMAP Phase 9 item 5: Numerical Laplace Transform (`transform: "nlt"`, default damping ln(N²)/T) on the same conductor under a slow 250/2500 µs, 1 kA surge, `nyquistHz: 1e5`, 512 samples — a well-resolved case; the fast `portela1997_transient` front is band-edge-limited and NLT output there is usable only over about half the record (docs/validation/phase9-transient-options.md) | `portela1997_transient_nlt_expected.csv` |
| `silva2025_rho{100,300,1000,2400}.json` | Silva et al. 2025 (SBAI, references.md [36]) PEEC-vs-HEM base case: buried horizontal electrode, 60 m, 7 mm radius, 0.5 m depth, `alipio-visacro` dispersive soil (theory.md §7) at ρ0 = 100/300/1000/2400 Ω·m, 1∠0° A at `Node_1`, 128 log-spaced points 100 Hz–4 MHz (`pointsPerDecade: 27.6`, ADR 0013's `round(ppd·log10(fmax/fmin))+1` formula) — matches the paper's 2⁷ frequency samples. For comparison against the paper's Fig. 3 (\|Z(ω)\|); no tabulated digitised curve exists yet, so there is no `_expected.csv` (internal passivity/plausibility check only) | none yet |
| `silva2025_rho{100,300,1000,2400}_transient.json` | Same geometry/soil as the files above, but a `signal` block (ADR 0015): GPR at `Node_1` under De Conti & Visacro [38]'s **MCS_FST#2** double-peaked first-stroke current (7 `terms`, physical amplitudes, no `imax` rescale), `nyquistHz: 4e6`, `fftPoints: 4096`. For comparison against the paper's Fig. 4 (GPR(t)) — see [`docs/validation/silva2025-fig4.md`](../docs/validation/silva2025-fig4.md), including why MCS_FST#2 rather than the legacy 6-term MCS_FST#1 | none yet (plausibility check only, same caveat as the frequency-domain files above) |
| `grcev_fig12_l{10,100}_rho{30,300,3000}.json` | Grcev et al. 2018 (IEEE TPWRD, references.md [23]) §IX-B case: buried horizontal electrode, ℓ = 10 or 100 m, 7 mm radius, 0.5 m depth, homogeneous non-dispersive soil (ρ1 = 30/300/3000 Ω·m, εr = 10), 0.25 m segments (theory.md §4.1 λ/10 bound at 10 MHz), 1∠0° A at `Node_1`, 101 log-spaced points 100 Hz–10 MHz (`pointsPerDecade: 20`). For comparison against the paper's Fig. 12 (rigorous full-wave model's \|Z(ω)\|, not a circuit-model approximation) — see [`docs/validation/grcev-fig12.md`](../docs/validation/grcev-fig12.md) | none yet (plausibility check only, same caveat as the Silva files above) |
| `portelaMesh.json` | Native `"mesh"` element demo (ADR 0020): a single 32x32 m grounding grid, 5x5 main nodes (8 m pitch), 5 segments/bar, corner at `(0, -32, -1)` so the grid spans `x` in `[0, 32]`, `y` in `[-32, 0]`, from the classic layout in Portela's *Frequency and Transient Behavior of Grounding Systems* papers (references.md — the M2 point at x=30,y=-30 used there sits inside this mesh's footprint) | none — **structure-only** (no `sources`/`frequencies`): a full sweep over 200 electrodes' worth of mostly-non-parallel pairs is impractical before ROADMAP §7 P1 lands, so this case stays fast to parse/assemble (185 nodes, 200 electrodes — verified in `fortran/test/test_mesh_element.f90`'s topology tests, not by solving this file) rather than tempting an hours-long run |
| `lima_fig6.json` | Lima et al. 2020 (IEEE TEMC, references.md [11]) §III-B Case #9: distribution tower grounding — 4 horizontal electrodes (6 m) radiating 90° apart from a center node, each ending in a vertical rod (3 m), plus a 5th vertical rod at the center (injection point); homogeneous soil (σ1 = 1 mS/m, εr = 10); 12.5 mm radius, arms at -0.5 m with rods to -3.5 m (both inferred — see writeup), 0.5 m segments, 1∠0° A at `Node_C`, 150 log-spaced points 100 Hz–10 MHz (`pointsPerDecade: 29.8`). For comparison against the paper's Fig. 6 MHEM curve — see [`docs/validation/lima-fig6.md`](../docs/validation/lima-fig6.md) | none yet (plausibility check only; case geometry only partially specified by the paper) |

### Legacy TUPÃ cases (`linha*.json`, `torre*.json`, ADR 0023)

Ten study cases from the original Matlab implementation's case library,
converted by [`tools/legacy_import.py`](../tools/legacy_import.py) (mapping
rules in [ADR 0023](../docs/adr/0023-legacy-case-import.md)). Node `N<k>`
and element `E<k>` keep the legacy numbering, so a legacy output on
element `k` is `E<k>_e1` here. Each file carries both a 1 A harmonic sweep
at the injection node and the legacy transient: the first `sinal` waveform
(`"portela"`) at the legacy Nyquist frequency and FFT size. The line cases
also list 2 µs and 10 µs fronts (same `imax`/`alpha`/`tTopEnd`/`tTailEnd`);
edit `tFront` to run those. `linha0`'s first signal is the legacy linear
`rampa` (`alpha: 0`), and its other two are Portela surges with 2 µs and
10 µs fronts.

| File | Structure | Soil | Injection | Segments | Fortran run (CPU) |
| --- | --- | --- | --- | --- | --- |
| `linha0.json` | 300 m copper line at 30 m, each end grounded by a thin lead and a 20 m rod | 1 mS/m, εr 1 | 1 kA linear ramp, 2 µs front, line start | 68 | 2 s |
| `linha1.json` | 200 m aluminium line at 30 m, steel down-lead and 20 m rod at the far end | 1 mS/m, εr 10 | 1 kA, 1 µs front, open end | 47 | 6 s |
| `linha2.json` | Shield wire over 10 iron towers with 3 m footings, 170 m channel to midspan | 1 mS/m, εr 10 | 1 kA, 1 µs, channel top | 160 | 3 min |
| `linha3.json` | Shield wire over 6 towers, channel to midspan, unconnected 100 m telephone wire at 5 m height, 100 m away | 1 mS/m, εr 10 | 1 kA, 1 µs, channel top | 310 | 26 min |
| `linha4.json` | Shield wire over 6 towers, outer spans as **catenaries** (5 m sag); grounded channel 100 m off the line (indirect strike) | 1 mS/m, εr 10 | 1 kA, 1 µs, channel top | 164 | 4 min |
| `linha5.json` | 6 steel towers with crossarms, shield wire, aluminium phase conductor 5 m below the crossarm tips (not connected), channel to midspan | 1 mS/m, εr 10 | 1 kA, 1 µs, channel top | 204 | 104 s |
| `linha5a.json` | Same as `linha5`, with the legacy `freq_log` scan: Nyquist 2 MHz instead of 5 MHz | 1 mS/m, εr 10 | same | 204 | 2 min |
| `torre0.json` | 2 × 2 × 100 m prism frame (`cubo`) of 0.2 mm wire, injection lead on top, 10 m lead down to a 10 m rod | 1 MS/m (near-ideal) | 1 kA, 10 µs, lead top | 87 | 31 s |
| `torre1.json` | Guyed lattice tower (`cubo`/`piramide`): top pyramid, crossarm pyramids, mast, 4 guy wires with anchors, 10 m rod | `portela` (σ0 50 µS/m, α 0.82) | 10 kA, 2 µs, tower top | 319 | 24 min |
| `torre2.json` | Cross frame: four 5 m arms at 40 m and at 20 m joined by 20 m verticals, 20 m mast, 10 m rod, injection lead on top | 10 kS/m (near-ideal) | 1 kA, 10 µs, lead top | 75 | 20 s |

Run times are the CPU time of the whole CLI run (sweep plus transient),
release build, single-threaded (AMD Ryzen 5 8500G). The transient solves every FFT bin
(`fftPoints/2 + 1` frequencies); since ROADMAP Phase 9 item 1 the
`frequencies` axis can serve as the scan grid instead
(`signal.transferFunction: "interpolated"`, see the schema notes), after
raising `frequencies.min` to or below `freqZeroHz` and `max` to or above
`nyquistHz`. There
is no `_expected.csv` for these cases: no legacy output files survive to
compare against. Legacy features with no counterpart yet (field points,
path voltages, impedance-matrix outputs, the `torre*` cases' Γ(ω) images)
are listed in ADR 0023. Cross-check of the new code paths: Fortran, Rust
and Julia agree to 5e-10 on `linha1` (`portela` waveform; harmonic
rows and transient series). On `linha4` (`catenary`) they agree
pairwise to about 1e-5 on harmonic rows and 3e-6 of peak on transient
series. The one exception is a segment current that is zero by symmetry
(~1e-6 A, pure round-off). The same 1e-5 spread appears with the sag set
to 0, so it is quadrature-tolerance noise on the case's non-parallel
segment pairs, not the catenary. The sag itself changes the line voltages
by up to a factor of 8. (The Julia `linha4` run predates the ADR 0017
finding 8 fix; Fortran and Rust were re-checked after it, with the same
result.)

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
conformance target future Python ports — and the Rust and Julia ports in
[`rust/`](../rust/README.md) and [`julia/`](../julia/README.md), which
match all three at 1e-6 — must reproduce within tolerance. `fortran/test/test_common_cases.f90` diffs a fresh run against
each fixture (relative tolerance 1e-6) and re-checks passivity
independently of the fixture. `grid.json` is deliberately kept to a single
4-electrode mesh (not a larger multi-cell grid): every non-parallel
segment pair falls back to 2-D adaptive quadrature (`geometryFactor2D`),
which costs roughly 1-2 s per pair regardless of touching/singularity at
today's tolerances (ROADMAP §6 "Quadrature tolerances", §7 P1) — a bigger
grid is worth adding once the P1 mHEM single-integral kernel lands.

## Schema (v1 — [ADR 0006](../docs/adr/0006-json-io.md) format, `sources`/`frequencies`/`outputs` frozen by [ADR 0013](../docs/adr/0013-input-schema-sources-frequencies-outputs.md), `signal` added by [ADR 0015](../docs/adr/0015-time-domain-signal-schema.md), voltage sources and Heidler `terms` by [ADR 0016](../docs/adr/0016-voltage-sources-by-superposition.md)/0015 amendment, `"mesh"` element by [ADR 0020](../docs/adr/0020-grid-mesh-element.md), `signal.antialiasStart` by [ADR 0021](../docs/adr/0021-transient-antialias-filter.md), `"catenary"` element and `"portela"` waveform by [ADR 0023](../docs/adr/0023-legacy-case-import.md), `signal.sources`/`window`/`transferFunction`/`transform`/`nltDamping` and the `"sine"` waveform by the ADR 0015 amendment of 2026-09-30)

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
                  "radius": 0.01, "segments": 2, "material": "copper" },
                { "type": "catenary", "id": "Span_1", "from": "Node_3", "to": "Node_4",
                  "sag": 5.0, "radius": 0.005, "segments": 20, "material": "steel" } ],

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
- `elements[].type`: `"line"`, `"mesh"` (ADR 0020) or `"catenary"` (ADR
  0023); unknown types are skipped with a warning.
- `"catenary"` (ADR 0023) is a `"line"` that sags: same fields plus `sag`,
  the drop at midspan below the chord (m, negative bows upward). The chain
  nodes lie on the parabola of theory.md §4.4, evenly spaced along the
  chord; generated IDs are the same as `line`'s. A profile that crosses
  `z = 0` is rejected. See `linha4.json`.
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
  `waveform` is `"doubleExp"`, `"heidler"` or `"portela"` (`fortran/src/Signal.f90`);
  `front`/`jones` apply only to `"doubleExp"`. `"portela"` (ADR 0023,
  theory.md §8) is Portela's piecewise surge: `imax` (A), `alpha` (front
  inclination; `0` is a linear ramp, `< 0` a convex front), and the end of
  the front, flat top and tail, `tFront` ≤ `tTopEnd` < `tTailEnd` (s). For `"heidler"`, an optional
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
  `antialiasStart` is optional (ADR 0021): the fraction of `nyquistHz` where
  a Tukey raised-cosine anti-aliasing roll-off of the synthesised spectrum
  begins (it reaches zero at Nyquist); a number in (0, 1], where absent or
  `1` means no filter. Enabling it changes transient results (on
  `portela1997_transient.json`, `0.85` lowers the `Node_1` GPR peak by
  about 3.4%), so it is never on by default.
  See `portela1997_transient.json` for a worked example.
- **`signal` — ROADMAP Phase 9 fields** (ADR 0015 amendment 2026-09-30),
  all optional; absent, a case runs exactly as before:
  - `sources`: array of `{ "node": ..., "waveform": ..., <waveform fields> }`,
    several simultaneous current injections, each with its own waveform;
    replaces `sourceNode` and the top-level waveform fields (both at once
    is an error). Responses superpose (one unit-current sweep per source).
    New waveform `"sine"`: `imax`, `frequencyHz`, optional `phaseDeg`
    (default 0), switched on at t = 0. See `portela1997_transient_multi.json`.
  - `window`: `{ "type": "none" | "hann", "placement": "spectral" | "time" }`
    (placement default `"spectral"`), the falling half of a Hann window over
    the one-sided spectrum or over the sampled excitation; multiplies with
    `antialiasStart`.
  - `transferFunction`: `"full"` (default) or `"interpolated"` — solve only
    the case's `frequencies` axis and pchip-interpolate H(f) onto the FFT
    bins. The axis must span [`freqZeroHz`, `nyquistHz`] (a log axis cannot
    start at the default `freqZeroHz` of 1e-6 Hz cheaply, so set
    `freqZeroHz` to the axis minimum, e.g. 100 Hz — below a few kHz the
    grounding response is quasi-static). If `sources` is also present the
    same axis still drives the harmonic sweep.
  - `transform`: `"fft"` (default) or `"nlt"` (Numerical Laplace
    Transform, s = c + jω); `nltDamping` sets c (1/s), default ln(N²)/T.
    `"nlt"` cannot be combined with `"interpolated"`. NLT output is reliable
    over the early part of the record only (theory.md §8).
- **Transient results with several sources**: the results JSON adds a
  `sources` array (`[{ "node", "current": [...] }]`, input order) after
  `injectedCurrent`, which, like `sourceNode`, keeps describing the first
  source; the CSV has one `injectedCurrent` row per distinct source node,
  holding the net current injected there.
- **Transient golden fixtures** (`*_expected.csv` in the
  `time_s,quantity,id,value` shape): rows are compared by position with
  identical text fields, and values pass when
  |fresh − expected| ≤ 1e-6 · max(|expected|, 1e-3 · peak), peak being the
  largest |expected| of that (quantity, id) series.

## Parser (ADR 0006)

The Fortran implementation reads case files with json-fortran (via a thin
wrapper, `fortran/src/JsonParser.f90`): the full JSON grammar is supported —
no item-count cap, string escape sequences work — and a malformed file
raises a feh error with json-fortran's own line/column-aware message. These
cases still double as parser conformance tests, and as conformance tests for
`validateStudyReferences`'s ID cross-checks (see the `signal`/`outputs`
notes above).
