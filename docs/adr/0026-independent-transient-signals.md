# ADR 0026 — Independent transient signals: `signal.signals`, one transfer function, one response set each

- **Status**: Accepted
- **Date**: 2026-10-01
- **Amends**: [ADR 0015](0015-time-domain-signal-schema.md) (additive input
  field; a new results variant for it)

## Context

The legacy Matlab accepts a **list of signals** in one case (`sinal` followed
by a count, then one waveform per line, `lesinais.m`). The impedance model is
solved once for the injection node — `readCaso.m` states that the node is the
same for all signals "because the immittances are relative to it" — and
`grafsaida.m` then multiplies that one transfer function H(f) by each signal's
spectrum in turn, writing one response set per signal (`-s1`, `-s2`, …) over
the **same** observed nodes and electrodes. The three fronts (1, 2 and 10 µs)
of the legacy line cases `linha0`–`linha5a` are the standing example.

TUPÃ's `signal` block held one waveform. Its `sources` list (ADR 0015
amendment, ROADMAP Phase 9 item 4) is a different thing: simultaneous
injections that **superpose** into one response. A list of independent signals
could only be had by editing the case and re-running it, repeating the whole
solve each time — and the legacy importer (ADR 0023) kept only the first
`sinal`, dropping the other two fronts of every line case.

The driver also solved one full sweep **per source**, even when sources share
a node, so N signals on one node would have cost N times the solve for the
same transfer function.

## Decision

### Input

`signal.signals`: an ordered array of independent excitations.

```json
"signal": {
  "sourceNode": "N39",
  "signals": [
    { "name": "front_1us",  "waveform": "portela", "imax": 1000.0, "alpha": 2.0,
      "tFront": 1e-6,  "tTopEnd": 2e-5, "tTailEnd": 1e-4 },
    { "name": "front_10us", "waveform": "portela", "imax": 1000.0, "alpha": 2.0,
      "tFront": 10e-6, "tTopEnd": 2e-5, "tTailEnd": 1e-4 },
    { "name": "mid_span_hit", "node": "N20", "waveform": "doubleExp",
      "imax": 30000.0, "front": "f1_2_50" }
  ],
  "observeNodes": ["N38", "N1"], "observeElectrodes": ["E25_e1"],
  "nyquistHz": 2e6, "fftPoints": 4096
}
```

- Each entry takes the waveform fields of the single-signal form (`waveform`,
  `imax`, `front`/`jones`, `terms`, the Portela times, `frequencyHz`/
  `phaseDeg`), plus:
  - `name` — optional, default `signal<k>`; nonblank, at most 64 characters,
    unique, free of commas, quotes and backslashes (names are written
    unescaped into the results files);
  - `node` — optional; the entry's injection node, default the block's
    `sourceNode` (required if some entry has no `node`);
  - `returnNode`, `quantity` — optional two-node-source fields (ADR 0025),
    defaulting to the block-level values of the same names.
- `signals` cannot be combined with `sources` or with the top-level waveform
  fields (error, as `sources` already is).
- Everything else is **shared**: `observeNodes`, `observeElectrodes`, the FFT
  grid, `freqZeroHz`, `window`, `antialiasStart`, `transform`,
  `transferFunction`. All the Phase 9 options therefore apply to every signal.
- A list of one signal is accepted and behaves as the single-signal form,
  including its output shape (below).

### Solving: one factorisation per frequency for every distinct terminal

Signal k alone drives its terminal; H_k is the response to a unit current (or
voltage) there. The driver collects the **distinct terminals** (node, return
node, quantity) of the list and solves them together: per frequency (or scan
point) the system is filled and factorised once and every terminal is
back-substituted as one right-hand side of the same `ZGESV` call
(`tStudy%runSweepUnits`; `Study::run_sweep_units`). Signals sharing a terminal
share its transfer function outright, so the legacy case of N signals on one
node costs one solve, not N, and a further node costs one triangular solve
per frequency, not a sweep. A unit **voltage** terminal is the unit-current
solution scaled by 1/(voltage across its terminals), the one-source case of
ADR 0016.

The same routine now serves the existing superposition form (`sources`):
sources sharing a terminal no longer repeat the solve. It also **keeps only
the observed rows**, so a large structure no longer stores its whole sweep
per source. One side effect is deliberate: `transientResponse*` no longer
leaves a sweep in the study (`voltageResults`, `inputImpedance` …); the two
tests that read the input impedance afterwards now solve it explicitly.

### Output

Single-signal and `sources` runs are unchanged to the last digit. For two or
more independent signals the results are a new variant:

- **JSON** — `title`, the optional `channels` block (ADR 0025), `time`, and
  `signals`: per entry `name`, `sourceNode`, `injectedCurrent`, `nodes`
  (`id`, `voltage`) and `electrodes` (`id`, `i1`, `i2`). The top-level
  `sourceNode`, `injectedCurrent`, `nodes` and `electrodes` are **absent**:
  there is no "first signal" to stand for the file, and a reader written for
  ADR 0015's shape finds no `nodes` and fails instead of silently showing one
  signal of several. This is the breaking half of the amendment, confined to
  files that did not exist before.
- **CSV** — a `signal` column after `time_s`: `time_s,signal,quantity,id,value`.
  Per signal: one `injectedCurrent` row (id = that signal's source node), then
  the `voltage` and `i1`/`i2` rows; (time, signal, quantity, id) is the key.
  The existing fixture harnesses already key on everything after the time
  field, so they compare the new header unchanged.

A one-entry `signals` list keeps the ADR 0015 shape, so a script that varies
the list length sees the change at two signals, as `sources` already does.

### Implementations

- **Fortran**: `transientResponseSignals` (`independent = .true.` for a list;
  `transientResponseSources` is the superposition wrapper), `loadStudy`'s
  `signalNames`, `writeTransientSignalsCsv/Json`.
- **Rust**: `transient_signals` over `TransientSpec.signal_names`,
  `transient_signals_csv/json`.
- **Julia**: ported for shared and per-entry nodes (one sweep per distinct
  node: the port has no multi-right-hand-side path); `returnNode` and
  `quantity` on an entry are refused, as everywhere in the port.
- **Importer** (`tools/legacy_import.py`): every `sinal` entry becomes a
  `signals` entry named `front_<t>us` / `ramp_<t>us`, all at the first
  signal's node (the Matlab solver injects at that node for every signal,
  `solver.m`); the seven `linha*` cases are regenerated with their three
  fronts. The `torre*` cases have one signal and are unchanged.
- **GUI**: the `Signal` model is now a list of `Excitation`s with a `form`
  (`single`, `sources`, `signals`) and the Phase 9/10b options, so it loads
  every `common/` case (the former loader raised `KeyError` on the `sources`
  form, a Heidler `terms` waveform without `imax` and the sine); the results
  loader reads both shapes; the Transient tab gets one checkbox per signal and
  overlays the checked ones on the selected quantity.

## Consequences

- Each signal of a list is **bit-identical** to a single-signal run of the
  same waveform (checked for the new fixture, also for the entry on a second
  node), and independent responses add up to the superposed run to round-off
  (`test_transient`, `physics.rs`, `physics.jl`).
- Cost, measured on `linha5a` (204 segments, 4096 points, three
  fronts, transient only, Fortran): **24.4 s** against 25.1–25.3 s for each of
  the three single-signal runs, i.e. the extra signals are free and the list
  is about 3× cheaper than three runs.
- Cross-check on real cases (`linha0`, `linha1`, three fronts each, transient
  only): Fortran, Rust and Julia agree to 2.0e-9 and 1.1e-9 of the series
  peak.
- Fixture `portela1997_transient_signals` (two waveforms at `Node_1`, one at
  `Node_2`): Fortran golden; Rust at 2.4e-13 of series peak, Julia at the
  1e-6 rule. Its observed electrodes are mid-line on purpose: the
  open-end longitudinal current of a line is round-off noise (~1e-13 A against
  hundreds of A), which no implementation reproduces.
- Possible later extension: a `sources` list per entry (a signal that is
  itself a superposition). Nothing in the driver prevents it — the sources
  already map to terminals — but no case needs it.
