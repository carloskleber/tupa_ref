# ADR 0015 — Input schema: `signal`; transient results schema v0

- **Status**: Accepted (schema addition, open to revision before more than
  one writer/reader depends on it)
- **Date**: 2026-07-16

## Context

Phase 6 (`fortran/src/Signal.f90`/`mSignal`, `fortran/src/Transient.f90`/
`mTransient`) implements the excitation-waveform -> transfer-function ->
inverse-FFT transient pipeline (theory.md §8), but — unlike the
frequency-domain sweep, which ADR 0013 exposed via the `sources`/
`frequencies`/`outputs` JSON blocks — it is only reachable from hand-written
Fortran (`fortran/example/example5.f90`, `fortran/test/test_transient.f90`).
There is no JSON way to specify a transient run, and no output schema for
its result (a real-valued time series, not ADR 0012's complex
frequency-indexed shape). This closes both gaps, the same way ADR 0013/0012
did for the harmonic sweep.

## Decision

### Input: `signal` block

A new optional top-level block, independent of `sources`/`frequencies` (a
case file may carry either, both, or neither — a transient run and a
harmonic sweep are unrelated solves over the same structure):

```json
"signal": {
  "waveform": "doubleExp",
  "imax": 30000.0,
  "front": "f1_2_50",
  "jones": false,
  "sourceNode": "Node_1",
  "observeNodes": ["Node_1"],
  "observeElectrodes": ["Line_1_e1"],
  "nyquistHz": 1.0e6,
  "fftPoints": 1024,
  "freqZeroHz": 1.0e-6
}
```

- `waveform`: `"doubleExp"` or `"heidler"`, selecting `mSignal`'s
  `tDoubleExpSignal`/`tHeidlerSignal`.
- `imax`: peak current (A), both waveform families. (Optional for
  `"heidler"` with `terms` — see the amendment below.)
- `front`/`jones`: `tDoubleExpSignal` only — `front` is one of the four
  named forms `mSignal::newDoubleExpSignal` already accepts
  (`f1_2_5`/`f1_2_50`/`f1_2_200`/`f250_2500`); `jones` is optional, default
  `false`. Ignored for `"heidler"` (without `terms`, the legacy 6-term
  parameter set is fixed).
- `terms` (**amendment 2026-07-17**, ROADMAP Phase 7): optional array for
  `"heidler"` only — the standard parametrised Heidler function (Heidler
  1985 [37]; IEC 62305-1 [39] tabulates single-term parameter sets), one
  `{ "i0": <A>, "n": <->, "tau1": <s>, "tau2": <s> }` object per term,
  mapped to `mSignal::newHeidlerSignalTerms`. With `terms` present, `imax`
  becomes optional: absent, the terms are used at their physical
  amplitudes (each term's peak ≈ `i0` via the analytic η correction — the
  citable usage); present, the summed waveform is numerically peak-rescaled
  to `imax` (legacy convention). Without `terms`, behaviour is unchanged
  (legacy 6-term set, `imax` required). Additive field — existing case
  files are unaffected.
- `sourceNode`: the node receiving the unit-current sweep injection
  (`mTransient::transientResponse`'s `sourceNodeId`).
- `observeNodes`: **array**, at least one node ID whose v(t) is computed
  and returned — a list, not a single node, since a caller usually wants
  more than one observation point (e.g. GPR at the injection node plus a
  nearby node) and the underlying sweep already solves for every node
  regardless (see "Consequences").
- `observeElectrodes`: **optional** array of *discretised* electrode IDs
  (the `Line_1_e1`-style generated IDs, same gotcha as ADR 0013's
  `outputs.electrodes` — see `common/README.md`) whose i1(t)/i2(t) are
  additionally computed. Omitted: currents are not computed for this run.
- `nyquistHz`/`fftPoints`/`freqZeroHz`: map directly to
  `transientResponse`'s `nyquistHz`/`nSamples`/`freqZeroHz`. **`fftPoints`
  is explicit in the schema**, not derived from a duration/`dt` pair — the
  caller states the sample count directly (must be a power of two;
  `mFft::isPowerOfTwo` already validates this at solve time, so the reader
  does not duplicate that check). `freqZeroHz` is optional, default
  `1.0e-6` (matches `mTransient`'s own DC-bin-substitute convention).

### Output: transient results v0

A distinct file/shape from ADR 0012's frequency-indexed results (the axis
and quantities are unrelated — real time series, not complex spectra — but
deliberately parallel in structure so a reader that already understands
ADR 0012 recognises this one immediately):

```json
{
  "title": "string",
  "sourceNode": "Node_1",
  "time": [0.0, ...],
  "injectedCurrent": [...],
  "nodes": [ { "id": "Node_1", "voltage": [...] } ],
  "electrodes": [ { "id": "Line_1_e1", "i1": [...], "i2": [...] } ]
}
```

- Every array is indexed positionally against `time` (same convention as
  ADR 0012's `frequencies` indexing).
- `nodes`/`electrodes` mirror `observeNodes`/`observeElectrodes` from the
  input `signal` block; `electrodes` is present only when
  `observeElectrodes` was given (mirrors ADR 0012's already-optional
  `derived` treatment — an absent block means "not requested," not "empty
  result").
- Values are plain reals, not `{"re":..,"im":..}` pairs — the transient
  response is real by construction (real time-domain voltage/current), so
  the complex-pair convention (needed for phasors) does not apply here.

## Consequences

- `mTransient::transientResponse` changes signature: `observeNodeId`
  (scalar) becomes `observeNodeIds` (array), returning `nodeResponses(:,:)`
  shape `(nObserveNodes, nSamples)`; a new optional `observeElectrodeIds`
  argument returns `i1Responses(:,:)`/`i2Responses(:,:)`. This costs no
  extra `tStudy%run` calls: the single unit-current `runSweep` already
  solves and stores *every* node/electrode's spectrum
  (`voltageResults`/`longCurrentResults`/`transCurrentResults`), so
  observing more points is only more transfer-function lookups and IFFTs
  against data already computed. `fortran/example/example5.f90` and
  `fortran/test/test_transient.f90` are updated for the new signature — no
  behavioural change to the single-node case, just an added array
  dimension.
- `fortran/src/Tupa.f90::loadStudy` gains optional, allocatable
  `intent(out)` arguments (`signal`, `signalSourceNode`,
  `signalObserveNodeIds`, `signalObserveElectrodeIds`, `signalNyquistHz`,
  `signalFftPoints`, `signalFreqZeroHz`), same optional-argument pattern
  ADR 0013 used for `sourceNodeIds`/`freqHz` — every existing
  single-argument call site (`runFromFile`) keeps compiling unchanged.
- `runFromFile` runs the sweep and/or the transient pipeline independently
  (whichever of `sources`+`frequencies`/`signal` is present in the file),
  writing `<basename>_results.csv/.json` and/or
  `<basename>_transient_results.csv/.json` respectively; a file with
  neither stays a structure-only report, as before.
- `common/portela1997_transient.json` (Phase 2's validation geometry, same
  1.2/50 µs / 30 kA surge as `example5.f90`) is the first common case
  exercising this block, documented in `common/README.md` alongside the
  schema addition.
- The GUI's `signal` display is parameter-only (tree view, like
  `Frequencies`/`Outputs` today); actual waveform/response visualization
  loads a solver-written transient results file, per GUI_SDD.md §2's
  guiding constraint that the GUI never reimplements solver physics
  (recomputing the Heidler/double-exponential formula in Python to "plot
  the signal" would violate it) — it only ever displays what a solver
  computed and exported.
- Adding a field to either shape is backward-compatible; renaming/removing
  one, or changing the real-vs-complex value convention, is breaking and
  needs a new ADR, per ADR 0002's cross-implementation contract discipline.
