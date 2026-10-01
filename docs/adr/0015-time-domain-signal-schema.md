# ADR 0015 — Input schema: `signal`; transient results schema v0

- **Status**: Accepted (schema addition, open to revision before more than
  one writer/reader depends on it)
- **Date**: 2026-07-16
- **Amended by**: [ADR 0025](0025-lightning-channel-and-two-node-sources.md) (`returnNode` and `quantity` on `signal` sources, 2026-10-01),
  [ADR 0026](0026-independent-transient-signals.md) (`signal.signals`: independent signals sharing one transfer function, and their results variant, 2026-10-01)

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

## Amendment 2026-09-30 — transient pipeline completion (ROADMAP Phase 9)

Implements ROADMAP Phase 9 items 1, 2, 4 and 5 (item 3, the Portela
waveform, is [ADR 0023](0023-legacy-case-import.md)) in Fortran
(`mTransient`/`mSignal`/`mTupa`) and Rust (`rust/src/transient.rs`,
`json.rs`); the Julia port lags and rejects the new fields explicitly
(`julia/README.md`). Every field below is optional and additive; with none
of them present a case runs exactly as before (existing outputs are
unchanged to the last printed digit or better — see "Consequences"). It also
settles the three open questions of theory.md §8 (review 2026-09-30).

### Input: new `signal` fields

```json
"signal": {
  "sources": [
    { "node": "Node_1", "waveform": "doubleExp", "imax":  30000.0, "front": "f1_2_50" },
    { "node": "Node_2", "waveform": "doubleExp", "imax": -30000.0, "front": "f1_2_50" },
    { "node": "Node_1", "waveform": "sine", "imax": 1000.0, "frequencyHz": 5000.0, "phaseDeg": 90.0 }
  ],
  "observeNodes": ["Node_1", "Node_2"],
  "nyquistHz": 1.0e6,
  "fftPoints": 1024,
  "window": { "type": "hann", "placement": "spectral" },
  "transform": "fft",
  "transferFunction": "full"
}
```

- `sources` (item 4): array of current injections, each a node plus a
  complete waveform description (the same fields as the single-source
  form: `waveform`, `imax`, `front`/`jones`, `terms`, the Portela times, or
  the new `sine` fields). It replaces `sourceNode` and the top-level
  waveform fields; giving both is rejected. A node may appear more than
  once (the injections add). By linearity the response is
  Σ_k H_k(s)·X_k(s), H_k from a unit current at source k, summed per
  observe point before one inverse transform. Current injections only:
  voltage sources in the transient (ADR 0016's superposition per bin)
  are left for a later amendment.
- `waveform: "sine"` (item 4): i(t) = `imax`·sin(2π·`frequencyHz`·t +
  `phaseDeg`·π/180) for t ≥ 0, zero before; `frequencyHz` > 0,
  `phaseDeg` optional (default 0). Three sources 120° apart emulate a
  three-phase line voltage; the phase angle places the switching instant
  on the wave.
- `window` (item 2): `{ "type": "none" | "hann", "placement": "spectral" |
  "time" }`, `placement` default `"spectral"`. The window is the falling
  half of a Hann window, w(x) = ½[1 + cos(πx)], x ∈ [0, 1]: over the
  one-sided bins (x = f/`nyquistHz`) in the spectral placement, applied to
  every H·X product together with the ADR 0021 filter; over the record
  (x = t/t_last) on each sampled excitation in the time placement, after
  the erfc tail taper (the reported `injectedCurrent` is post-window).
- `transferFunction` (item 1): `"full"` (default, one solve per FFT bin)
  or `"interpolated"`: the case's own `frequencies` axis (ADR 0013) is
  solved and H(f) is interpolated onto the N/2+1 bins by pchip, applied
  separately to Re H and Im H — a port of Matlab `pchip` (Fritsch–Carlson
  monotone slopes, Fritsch–Butland weighted harmonic mean, one-sided
  shape-preserving end slopes; Moler, *Numerical Computing with MATLAB*,
  §3.4), as the legacy `imitancia.m`. No extrapolation: the loader rejects
  a `frequencies` axis that does not span [`freqZeroHz`, `nyquistHz`]
  (relative slack 1e-9 for log-axis round-off), and a missing
  `frequencies` block. The `frequencies` block still drives the harmonic
  sweep too if `sources` (ADR 0013) is also present.
- `transform` (item 5): `"fft"` (default) or `"nlt"`, the Numerical
  Laplace Transform (Gómez & Uribe [17]): each sampled excitation is
  multiplied by e^{−ct} before the forward FFT, the system is solved at
  s_k = c + jω_k (no `freqZeroHz` substitute: s_0 = c is regular), and the
  inverse FFT is multiplied by e^{ct}. `nltDamping` sets c (1/s, > 0,
  only with `"nlt"`); default c = ln(N²)/T, T = N/(2·`nyquistHz`) the
  record length (as TAGS `laplace_trans`). The physics kernels take the
  analytic continuation jω → s: W(s) of every soil model on the principal
  branch (linear σ + sε; Lima–Portela σ₀ + kr (s/ω₀)^α₀ / sin(πα₀/2);
  Alipio–Visacro σ₀ + σ₀h (s/ω₀)^ξ / cos(πξ/2) + sε₀ε∞), the internal
  impedance with ρ = r₀√(sμσ). These reduce to the harmonic formulas on
  s = jω (tested to 1e-12) and keep W(s̄) = conj W(s), so the
  conjugate-symmetric reconstruction stays valid. The real-ω code path is
  kept separate, so harmonic results stay bit-identical.
- `"nlt"` together with `"interpolated"` is rejected (open question 3): a
  pchip fit over a real-frequency scan is not the analytic continuation
  H(c + jω) the NLT needs.

### Decisions on theory.md §8 open questions

1. *Band-edge filter vs. `signal.window`.* Both kept, as separate fields
   that multiply. `antialiasStart` (ADR 0021) is already accepted and
   in use; the spectral Hann window is exactly its s → 0 limit (tested),
   but a named window generalises to other shapes (Blackman, Lanczos —
   Gómez & Uribe Table 1) and to the time placement, which the Tukey
   start fraction cannot express. The ADR 0021 field name is kept for
   compatibility; theory.md now calls it the band-edge filter.
2. *Interpolating delayed transfer functions.* Componentwise Re/Im pchip,
   as the legacy, with no loader-side sampling criterion. Measured on the
   60 m `silva2025_*_transient` electrode with the paper's 128-point scan
   (100 Hz–4 MHz): the remote-node voltage and a mid-conductor current
   stay within 2.7e-5 of peak of the per-bin solve (the driving point
   within 1.7e-5), so the phase wrap feared for remote observers does not
   show at that scale. Longer delays may need a denser scan; the `"full"`
   path remains the check.
3. *NLT with an interpolated transfer function.* Rejected at load time
   (above).

### Output: transient results

- JSON: when there is more than one source, a `sources` array
  `[{ "node": ..., "current": [...] }]` follows `injectedCurrent`, one
  entry per input source, in input order. `sourceNode`/`injectedCurrent`
  keep their meaning for the first source, so single-source files are
  byte-identical.
- CSV: one `injectedCurrent` row per *distinct* source node (first
  appearance order), holding the net current injected there, so
  (time, quantity, id) stays a unique key.

### Golden transient fixtures

`common/<case>_expected.csv` may now hold the transient tidy CSV
(`time_s,quantity,id,value`); the harnesses list the transient cases
explicitly. Rows match by position with identical text fields; a value passes
when |fresh − expected| ≤ 1e-6 · max(|expected|, 1e-3 · peak), peak being
the largest |expected| of that (quantity, id) series — a relative test
that does not amplify round-off at zero crossings. One fixture per new
option, all on the Portela 1997 conductor (10 segments, each runs in
well under a second): `portela1997_transient_interpolated`, `_hann`,
`_hann_time`, `_multi`, `_nlt`.

### Validation (ROADMAP Phase 9 exit criteria)

Details and tables: [validation/phase9-transient-options.md](../validation/phase9-transient-options.md).

- Scan-fed vs full on the four `silva2025_*_transient` cases
  (`freqZeroHz` 100 Hz, scan = the paper's 128 log points): max error
  ≤ 1.7e-5 of peak at the injection node, ≤ 2.7e-5 at the remote node and
  for the electrode currents, 12–15× less run time. **Stated tolerance:
  1e-4 of the series peak.**
- NLT vs a plain FFT on a 16× longer record (wrap-around-free reference),
  over the first half of a short record: 4.4e-5 of peak, against 2.6e-4
  for the plain FFT on the same short record.
- Rust reproduces the five new fixtures at the 1e-6 rule (worst 1.5e-8).

## Consequences of the 2026-09-30 amendment

- The driver is now `transientResponseSources` (Fortran) /
  `transient_response` over `TransientSpec.sources` (Rust);
  `transientResponse` stays as the single-source wrapper with an
  optional `tTransientOptions` argument. `writeTransientResults*` take
  source-ID and current arrays.
- Existing outputs: the harmonic sweeps (`portela1997`, `rod`) and the
  four `silva2025_*_transient` cases are byte-identical; `portela1997_transient` differs only
  in the last printed digit of a few near-zero late-record samples
  (≤ 1e-11 A), because the Fortran build's `-ffast-math` may now
  associate H·X·W differently once H·X is stored before filtering.
- NLT caveats (theory.md §8): the inverse damping e^{ct} reaches N² at the
  record end, so any residual truncation error grows along the record;
  plain NLT output degrades over roughly its last 20–40 % (case
  dependent), and the spectral Hann window confines the damage to the
  last few samples but, acting on the damped spectrum, smooths fronts
  that are only a few samples long differently than it does under the
  FFT. Read NLT results over the early part of the record, and size the
  record for it.
- Per-source sweeps cost one solve per bin (or scan point) per source; a
  multi-RHS solve per frequency (as ADR 0016 does for voltage sources)
  is a possible later optimisation.
