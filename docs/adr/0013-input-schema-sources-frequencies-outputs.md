# ADR 0013 — Input schema v1: `sources`, `frequencies`, `outputs`

- **Status**: Accepted (schema v1 addition, open to revision before a JSON
  reader consumes it)
- **Date**: 2026-07-09

## Context

ROADMAP Phase 5 item 1 calls for freezing input schema v1 with `sources`,
`frequencies`, and `outputs` alongside the existing `title`/`soil`/`nodes`/
`materials`/`elements` (`common/README.md` schema v0). Today those three are
only Fortran call arguments — `tStudy%runSweep(freqHz, sourceNodeIds,
sourceCurrents)` and `logFrequencyAxis(fMinHz, fMaxHz, nPoints)`
(`fortran/src/Study.f90`) — wired by hand in examples such as
`fortran/example/example4.f90`, with every node/electrode/quantity always
stored and written. No JSON reader for any of the three exists yet in any
implementation.

## Decision

Add three optional top-level blocks to the input schema:

```json
{
  "sources": [
    { "node": "Node_1", "current": { "re": 1.0, "im": 0.0 } }
  ],

  "frequencies": { "min": 100.0, "max": 1.0e6, "pointsPerDecade": 3 },

  "outputs": {
    "nodes": ["Node_1", "Node_2"],
    "electrodes": ["Line_1"],
    "quantities": ["voltage", "i1", "i2", "inputImpedance"]
  }
}
```

- **`sources`**: one entry per current-injection node (ADR 0010's mechanism
  — voltage sources are converted to equivalents by the study layer, not
  represented here). `current` uses the same `{"re":..,"im":..}` pair as
  the output schema (ADR 0012) for consistency. Array, so multiple
  simultaneous sources are representable even though today's solver only
  exercises one (`inputImpedance`'s single-port assumption, ADR 0012).
- **`frequencies`**: `min`/`max` in Hz, log-spaced (the only mode
  `logFrequencyAxis` supports). `pointsPerDecade` is decade-invariant
  density — the schema's user-facing knob — rather than the total point
  count `logFrequencyAxis(fMinHz, fMaxHz, nPoints)` takes today; the JSON
  reader converts (`nPoints = round(pointsPerDecade * log10(max/min)) + 1`)
  before calling it, per ADR 0006's "study-building layer talks to a thin
  reader interface, not parser/solver internals." `logFrequencyAxis` itself
  is unchanged — it still takes a total count, since callers that aren't
  reading this schema (hand-written Fortran examples, tests) have no reason
  to think in decades. An explicit frequency list (bypassing log-spacing)
  is deliberately left out of v1: `runSweep` already accepts an arbitrary
  array from Fortran callers, but a JSON `values` array would sit against
  the 64-item parser cap (ADR 0006) for any real sweep, so it waits for the
  json-fortran migration that cap is already expected to force.
- **`outputs`**: opt-in projection over what the results writer stores/
  emits. `quantities` names match the ADR 0012 output shape directly —
  `voltage` (per node), `i1`/`i2` (per electrode, long./trans. current —
  `longCurrentResults`/`transCurrentResults` in `Study.f90`), and
  `inputImpedance` (derived). Omitting the block, or omitting `nodes`/
  `electrodes` within it, means "everything," matching today's behaviour —
  this keeps `example1.json`/`example2.json` (no `outputs` block) valid
  under v1 with no change in meaning.
- All three blocks are **optional**: a structure-only case file (today's
  smoke tests) stays valid without them; they're only required to actually
  run a sweep.

## Consequences

- No parser/solver code changes yet — this freezes the shape so the
  Fortran JSON reader (Phase 5 item 1's remaining work), the future
  Python/Rust ports, and the GUI's study loader (`gui/src/tupa_gui/data/
  loader.py`, currently `soil`/`nodes`/`materials`/`elements` only) target
  the same fields from the start.
- `frequencies.pointsPerDecade` is a schema-level convenience, not a solver
  concept — `Study.f90`'s public API keeps its existing total-count
  signature; only the JSON-reading layer does the conversion.
- Adding an explicit (`values`) frequency list, a `spacing: "linear"` mode,
  or a multi-port `inputImpedance` are all additive, backward-compatible
  extensions for a later v2; removing/renaming a field here is breaking and
  needs a new ADR, per ADR 0002's cross-implementation contract discipline.
- Binary results output (HDF5, per the ROADMAP §6 "Binary results format"
  entry) is explicitly out of scope here — `outputs` selects *what* gets
  written, not the container format, and stays JSON/CSV (ADR 0012) for now.

**Exercised (ROADMAP Phase 5)**: `fortran/src/Tupa.f90::loadStudy` gained
optional, allocatable `intent(out)` arguments for `sourceNodeIds`/
`sourceCurrents`/`freqHz`/`outputNodeIds`/`outputElectrodeIds`/
`outputQuantities` — all omittable, so every existing single-argument call
site (`runFromFile`) compiles unchanged. `pointsPerDecade` is converted to
`logFrequencyAxis`'s point count exactly as specified above. A new
`runStudyFromFile` convenience wrapper loads + `runSweep`s in one call.
**Scope decision**: `outputs` filtering happens only at write time, in
`mResultsWriter%writeResultsCsv`/`writeResultsJson` (new optional
`nodeIds`/`electrodeIds`/`quantities` arguments) — `runSweep` still
computes and stores every node/electrode regardless of `outputs`, since
that storage path is already well-tested and the write-time filter is
sufficient to satisfy "opt-in projection over what the results writer...
emits." **Gotcha found during implementation**: `outputs.electrodes`/
`outputs.nodes` must name the *discretised* electrode/internal-node IDs
generated by `tLine%assemble` (`Line_1_e1`, `Line_1_n1`, …), not the input
element/boundary-node ID — see `common/README.md` for the worked example.
`fortran/test/test_common_cases.f90` tests the reader (including the
structure-only-file case, where the optional arguments stay unallocated)
and the write-time filter.
