# ADR 0016 — Voltage sources by unit-injection superposition; `voltage` in the sources schema

- **Status**: Accepted
- **Date**: 2026-07-17

## Context

ADR 0010 decided that excitation sources reach the solver kernel only as
nodal current injections, and that voltage sources are converted "outside
the solver kernel by the study layer" — but left the conversion mechanism
open (Norton equivalent vs. iteration were both mentioned). ROADMAP Phase 7
implements the conversion. For a linear system no iteration is needed: an
exact one-step conversion exists.

## Decision

### Mechanism (study layer, `mStudy`)

For a solve with `nS` sources of which some are ideal voltage sources:

1. Solve the assembled system once with `nS` right-hand sides — a unit
   current at each source node in turn (`mMesh::injectSignals`, one ZGESV
   call with NRHS = nS; the LU factorisation is shared, ADR 0003). The
   source-node voltages of these unit solutions form the transfer-impedance
   matrix restricted to the source nodes.
2. Solve the small dense constraint system
   `Σ_k I_k · Vunit(node_j, k) = U_j` for the unknown injections `I_k` at
   the voltage-source nodes (current-source injections stay fixed at their
   given values and move to the right-hand side).
3. Superpose: the full field solution is the same linear combination of the
   unit solutions — no further full-size solve.

This is exact (linear system), pins the voltage-source node voltages to
`U_j` by construction, and costs one factorisation plus `nS` triangular
solves per frequency. Mixed current + voltage source sets are supported.
The effective injections are recorded (`tStudy%lastSourceCurrents`, and
per-frequency in `tStudy%sweepSourceCurrentsFreq` across `runSweep`), so
`inputImpedance` now divides by the *effective* current at each frequency —
frequency-dependent for voltage sources — instead of a constant source
value.

### API

`tStudy%run` / `tStudy%runSweep` gain an optional `sourceIsVoltage(:)`
logical array; entries flagged true reinterpret the corresponding
`sourceCurrents` value as a complex source voltage (V). Absent argument =
all current sources — every existing call site compiles and behaves
unchanged (ADR 0010's primary stroke-injection path pays nothing).

### Input schema (extends ADR 0013)

A `sources` array entry carries **either** `"current"` **or** `"voltage"`
(both `{re, im}` complex pairs):

```json
"sources": [
  { "node": "Node_1", "voltage": { "re": 10.0, "im": 0.0 } },
  { "node": "Node_2", "current": { "re": 1.0,  "im": 0.0 } }
]
```

If both appear (malformed), `voltage` wins; neither defaults to a zero
current injection (pre-existing ADR 0013 behaviour). `loadStudy` exposes
the flags through a new optional `sourceIsVoltage` argument, same
optional-argument pattern as ADR 0013.

## Consequences

- The solver kernel (`injectSignal`/`injectSignals`) still only ever sees
  current injections — ADR 0010's boundary holds; no constraint rows were
  added to `Zeq`.
- `mMesh` gained `injectSignals` (multi-RHS ZGESV wrapper returning
  per-pattern solutions without touching `mesh%voltage`/`current1`/
  `current2`); reusable for future multi-injection features (ROADMAP
  Phase 7 "multiple injections").
- An ideal voltage source between a node and remote earth is what is
  modelled; a source with internal impedance is representable today by the
  user adding a series element, or later by the Phase 7 RLC element.
- Exercised by `fortran/test/test_sweep.f90` (voltage sweep pins the node
  voltage exactly and reproduces the current-injection input impedance;
  mixed-source superposition consistency) and
  `fortran/test/test_signal_io.f90` (schema round-trip).
- Fixed along the way: `mResult`'s `alloc` routines were not idempotent
  (`allocate` on already-allocated components), so a second `runSweep` on
  the same study crashed — first hit by the voltage-source tests, latent
  for any repeated-sweep caller.
