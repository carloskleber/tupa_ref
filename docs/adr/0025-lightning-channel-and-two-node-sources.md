# ADR 0025 — Lightning channel element, two-node sources and speed calibration (ROADMAP Phase 10b)

- **Status**: Accepted
- **Date**: 2026-10-01
- **Amends**: [ADR 0013](0013-input-schema-sources-frequencies-outputs.md),
  [ADR 0015](0015-time-domain-signal-schema.md),
  [ADR 0016](0016-voltage-sources-by-superposition.md),
  [ADR 0012](0012-results-json-schema.md) (additive fields only)

## Context

ROADMAP Phase 10b adds the lightning channel in air. The physics is in
theory.md §4.5: the channel is a chain of ordinary air segments, and the only
new term is a per-unit-length series impedance `z_ch = R' + jωL'` on the `Z_ℓ`
diagonal that slows the current wave to a prescribed return-stroke speed
(the *type 2* representation of Baba & Rakov [44, 74]). Three things needed a
decision: the element's input schema and object-model footprint, how a source
connects the channel to the struck object, and how the loading is calibrated.

## Decision

### 1. The `channel` element

```json
{ "type": "channel", "id": "ch",
  "strike": "Ttop",                     // existing node, or "position": [x, y, z] (exactly one)
  "length": 3000.0, "radius": 0.03,
  "incidence": 0.0, "azimuth": 0.0,     // degrees; axis from the vertical / from +x (optional)
  "speed": 1.5e8,                       // target v: a number, or [{"upTo": 500, "value": 1.5e8}, ...]
  "inductance": 6.2e-6,                 // OR an explicit uniform L' (H/m); not both with speed
  "resistance": 0.5,                    // R': a number or an {upTo, value} profile (Ω/m)
  "calibrate": true,                    // calibrate L' against `speed` at load time
  "segments": 150,                      // uniform chain, OR
  "firstSegment": 5.0, "growth": 1.15, "maxSegment": 20.0 }   // graded from the foot
```

- **Nodes.** `<id>-base` (the channel foot), `<id>_n<k>`, `<id>-top`; segments
  `<id>_e<k>` numbered upward from the base. With `strike`, the base is a
  **separate node coincident with the strike node**, so a source can sit
  between them (§2). With `position` the channel is free-standing: only the
  base node exists, and a source acts against remote earth — the right model
  for the channel alone above ideal ground (Chen's case, the calibration).
  A strike node must be attached to something (the struck object): an
  isolated node leaves a zero column in the nodal system.
- **Object model.** `tElectrode` gains `loaded`, `loadResistance`,
  `loadInductance` (Rust: `Option<Loading>`). A loaded segment's internal
  impedance is `(R' + jωL')·l` (`R' + sL'` under the NLT) and **no skin-effect
  term**: the wire is a perfect conductor, `R'` is the channel's resistance.
  Loaded segments carry no conductor material.
- **Loading.** `speed` gives `L'(z) = κ·L0(z)(c²/v² − 1)`, `L0 = μ0/(2π)
  ln(2z/r0)`, `z` the height of the segment midpoint (strike height plus
  distance along the axis × cos incidence), `κ = 1` unless calibrated. A
  `speed` profile and an `R'` profile are piecewise constant in the distance
  `s` along the axis. `speed > c`, or `2z ≤ r0`, is an error.
- **Segmentation.** `segments` gives a uniform chain (raised to
  `⌈length/target⌉` by `numerics.maxSegmentLength`, as for lines). Otherwise
  segment `k` is `min(firstSegment·growth^(k−1), maxSegment)` (the study
  target caps `maxSegment`) until the length is covered, then **all segments
  are scaled by the same factor** so the chain ends exactly at `length`.
  Scaling preserves every adjacent ratio, so the ratio never exceeds
  `growth`; `canal.m`'s spacing (1 % top segment beside a 21 % one) is not
  ported.

### 2. Two-node sources (`returnNode`, `quantity`)

A source (`sources[]`, `signal.sources[]`, or the single-source `signal`)
may carry `"returnNode"`. Its unit pattern is the **±1 dipole** — `+1` at
`node`, `−1` at `returnNode` — and:

- a **current** source pushes `+I` into `node` and `−I` into `returnNode`
  (the right-hand side sums to zero; ADR 0010 is unchanged — the kernel still
  sees only current injections);
- a **voltage** source fixes `u(node) − u(returnNode) = U` (a delta gap).
  ADR 0016's constraint system holds with `Vunit(pos_j, k)` read as that
  difference.

Sign convention: the stroke current is `+I` into the struck object (`node`
= object, `returnNode` = `<id>-base`); the channel current is then `−I`
in segment `i1` (flowing down the channel, toward the base). `inputImpedance`
of a two-node source is `(u(node) − u(returnNode))/I`.

Transient sources (`signal.sources[]` and the single-source form) also take
`"quantity": "current" | "voltage"` (default `current`): with `voltage` the
waveform is a source voltage in V across the node pair, and the row labelled
`injectedCurrent` in the results holds that voltage. The solver builds the
transfer function of a unit current or unit voltage accordingly. A voltage
source is what a strike to a tall grounded object needs (an ideal current
source isolates the channel from waves reflected up the object); the
unloaded channel over ideal ground is driven this way (Chen's case).

Implementation detail: sources sharing a node (two dipoles with the same
return node) are merged into one right-hand-side vector over the distinct
nodes. The previous vector-subscript assignment of `injectSignal(s)` was
undefined for repeated nodes; the study layer now never passes any.

### 3. Speed calibration

The closed form is a starting value, not the final loading (theory.md §4.5).
`"calibrate": true` solves for the scale `κ` at load time:

1. **Setup.** A free-standing copy of the first `min(length, 2.5 km)` of the
   channel (same breaks, radius and speed profile) above ideal ground
   (`imageModel: "ideal"`), **lossless** (`R' = 0`, see the metric note
   below), excited at its base by a unit current ramp (1 µs rise, then
   constant), solved by the NLT with 512 samples over `1.5·L/v_min`.
2. **Metric** (Baba & Rakov [44]). At two segments whose midpoints are
   nearest to `0.2·L` and `0.8·L`, take the segment current; the front's
   onset time is where the tangent through its 10 % and 90 % levels crosses
   the time axis (`t10 − (t90 − t10)/8`). The peak is searched in a window
   ending at `t_direct + 2·t_rise + 0.2·t_direct` (and before the top
   reflection); the 90 % point is the *first* rise through its level, the
   10 % point the *last* rise through its level before that, so the ripple
   of the NLT synthesis before the front cannot fake an early onset. The speed is the
   distance over the difference of the onset times. A speed *profile* is
   compared with its harmonic mean between the two heights.
3. **Solve.** Secant iteration on `(c/v(κ))² − (c/v_t)²` (nearly linear in
   `κ`), tolerance 2e-3 on `v`, at most 20 evaluations; geometry factors are
   built once. Typically 3–5 evaluations, 1–3 s. The metric jitters by about
   0.5 % (one time sample, NLT ripple), so when 2e-3 is not reached the best
   evaluation is accepted if its error is within 6e-3; otherwise it is an
   error. A final `κ` outside 0.25–4 is also an error: the metric has then
   locked onto something other than the wave (seen for a 0.23 m radius with
   20 m segments, which gave `κ ≈ 29` at the target speed), and the message
   asks for shorter segments.
4. **Record.** The result files gain an additive `"channels"` array (below).

*Why lossless.* Baba & Rakov's own note: with `R'` the current front becomes
convex and slowly rising, and the 10–90 % tangent metric stops being a
property of the wire (their speeds exceed `c` for `R' > 2 Ω/m`). Calibrating
the 3 cm / `c/3` / 1 Ω/m case with `R'` on, the measured speed was not even a
monotonic function of `κ` (jumps of 10 %), so the iteration failed. The
lossless wire gives a well-posed metric; `R'` afterwards only damps the wave.
The calibrated `κ` is therefore a property of (radius, speed, segmentation),
not of `R'`.

*Determinism.* The Fortran geometry-factor cache is process-global and quantised, and entries left by an earlier study differ from fresh values at the quadrature tolerance — enough to move the secant path (`test_common_cases` failed on the calibrated fixture when run after other cases). The cache is therefore cleared before and after each calibration, so the calibrated scale does not depend on what ran before in the process.

*Cost and limits.* Calibration happens in `loadStudy`/`load_study`, once per
channel with `calibrate`; it uses the process-default geometry kernel and
tolerance, not the study's `numerics.kernel` (a study's kernel applies to its
own geometry build). A channel shorter than five segments per observation
height cannot be calibrated (error).

### 4. Results schema (additive)

Both results JSON files (ADR 0012 sweep, ADR 0015 transient) gain, **only
when the study has a channel**, a top-level array after `title`:

```json
"channels": [ { "id": "ch", "calibrated": true, "scale": 1.0309, "measuredSpeed": 1.4998e8,
                "inductance": [ ... per segment, H/m ], "resistance": [ ... per segment, Ω/m ] } ]
```

`scale`/`measuredSpeed` appear only when calibrated. Files of studies without
a channel are unchanged. The CSV files are unchanged.

### 5. Implementations

Fortran (`mElementChannel`, `mChannelCalibration`) and Rust
(`element/channel.rs`, `channel_calibration.rs`) implement everything above and
match the four new fixtures (`common/channel_*`) to the 1e-6 rule, the
calibrated case included. **Julia lags**: its loader refuses the `channel`
element, `returnNode` and `quantity` (the port has since followed Phase 10 but
not Phase 9: no NLT, so none of the channel fixtures could run there) —
`julia/README.md`.

## Consequences

- No kernel change. The solver still sees nodal current injections and, for
  voltage sources, the ADR 0016 superposition.
- A tower strike is two files' worth of structure plus a `channel` element and
  one two-node source: `common/channel_tower.json` (current) and
  `channel_tower_gap.json` (delta gap).
- The channel couples to other **air** segments only; buried-electrode
  outputs see it through the source current alone until ROADMAP Phase 14
  item 1 exists (theory.md §5).
- NLT noise: the channel's isolated-capacitor low-frequency behaviour makes
  the NLT the right default for channel transients, and its late-record noise
  amplification (`e^{ct}`) applies as for any NLT run; the fixtures'
  tolerance is relative to each series' peak, which that noise dominates
  late in the record. Validation numbers are quoted over the first half of
  the record, as for the other NLT cases (docs/validation/phase9-transient-options.md).
- The Ishii reduced-scale induced-voltage case (ROADMAP Phase 10b item 3, last
  anchor) is **not done**: it needs the measured waveform of the original
  experiment, which exists here only as a description. Everything else of item
  3 is in [validation/channel-validation.md](../validation/channel-validation.md).
