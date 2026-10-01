# ADR 0021 — Optional anti-aliasing filter for transient synthesis (`signal.antialiasStart`)

- **Status**: Accepted
- **Date**: 2026-09-29

## Context

`mTransient::transientResponse` (theory.md §8) builds v(t)/i(t) by
multiplying the excitation spectrum by the frequency-domain transfer
function up to `nyquistHz` and inverse-FFT-ing. The spectrum is cut off
abruptly at Nyquist, so any content the transfer function or the excitation
still carries near the band edge shows up as Gibbs-like ringing and
folded-back energy in the time series. The existing `tailTaper` acts in the
*time* domain and only controls record-truncation leakage; nothing
suppresses the band edge itself.

A contributor's fork (acslima/tupa_ref, September 2026) proposed a
Tukey raised-cosine roll-off applied to the one-sided spectrum before the
inverse FFT — unity up to a fraction of Nyquist, cosine down to zero at
Nyquist — and unconditionally enabled it in the Fortran code, together with
a Julia port of the solver using the same filter.

Applied unconditionally, it **changes the results of every transient study**
and departs from the legacy Matlab pipeline (`ifourier.m`), which is the
model reference of record (theory.md §8 documents the legacy quirks this code
reproduces on purpose). Measured on `common/portela1997_transient.json`
(1 MHz Nyquist, 1.2/50 µs double exponential) with the taper starting at
0.85 of Nyquist:

| Output | Peak change vs. no filter |
| --- | --- |
| GPR at `Node_1` | −3.43 % |
| GPR at `Node_2` | −0.73 % |
| i1 in `Line_1_e1` | +1.74 % |
| i2 in `Line_1_e1` | +1.31 % |

The effect is not negligible because that case's front is fast relative to
its Nyquist, so it is a deliberate modelling choice, not a cosmetic fix.

## Decision

Add the filter as an **opt-in** feature:

- New optional field `signal.antialiasStart` (number in (0, 1]): the
  fraction of `nyquistHz` at which the raised-cosine roll-off begins. The
  response is

  H(x) = 1 for x ≤ s, and 0.5·[1 + cos(π (x − s) / (1 − s))] for s < x ≤ 1,

  with x = f / `nyquistHz` and s = `antialiasStart`. Absent, or exactly
  `1`, means no filter — the transient results are unchanged from
  before this ADR (checked to 1e-12 relative in the tests). Values outside (0, 1] are rejected at load time.
- `mTransient::tukeyAntialiasFilter(nBins, taperStart)` builds the one-sided
  response; `transientResponse` gains an optional trailing argument
  `antialiasStart` and applies the filter (real-valued, so conjugate
  symmetry of the rebuilt full spectrum is preserved) to every observe
  point's transfer-function × excitation product.
- The Julia implementation (`julia/`) reads the same JSON field
  (`TransientSpec.antialias_start`, filter `tukey_antialias_filter`; named
  `transient_response(...; antialias_start=)` before the 2026-09-30
  realignment) with the same default.
  A single shared parameter and default keeps the two implementations
  interchangeable, as ADR 0002 requires.
- The filter stays separate from `tailTaper`: they address different
  problems (band-edge content vs. record truncation) and are used together.
- *Update 2026-09-30:* the ADR 0015 amendment of that date (ROADMAP
  Phase 9) adds `signal.window`; its spectral Hann window is this filter's
  s → 0 limit, and the two multiply when both are given. The field keeps
  its name; theory.md §8 calls the mechanism the band-edge filter, since it
  acts on truncation (Gibbs) ringing rather than on aliasing.
  The Rust implementation reads the field too (`TransientOptions.antialias_start`).

## Consequences

- Existing case files and their expected results are unaffected.
- Choosing s is the user's responsibility. The fork's value, s = 0.85, is a
  reasonable starting point; smaller s trades more high-frequency detail
  (fast fronts, peak amplitudes) for less ringing. The Portela numbers
  above show the size of that trade at s = 0.85.
- The transient validations against the literature (Silva et al. 2025
  Fig. 4 and the like) were made without the filter and have **not** been
  re-run with it.
- Tests: `fortran/test/test_transient.f90` covers the filter shape (unity
  pass band, monotone taper, zero at Nyquist, `s = 1` identity), that
  `antialiasStart = 1` reproduces the unfiltered response, and that a real
  taper changes it while keeping it finite; `julia/test/unit.jl` and
  `julia/test/physics.jl` mirror these.
