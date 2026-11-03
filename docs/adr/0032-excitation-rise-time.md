# ADR 0032 — Optional excitation rise time (`signal.riseTime`) against the NLT record-end artefact

- **Status**: Accepted
- **Date**: 2026-11-03
- **Related**: [ADR 0015](0015-time-domain-signal-schema.md) (amendment: NLT),
  [ADR 0021](0021-transient-antialias-filter.md), theory.md §8,
  engineering log INV-2026-11-02, ROADMAP "Near-term work (2026-11)" item 1

## Context

With `transform: "nlt"` the response is undamped by e^{ct}, which reaches
e^{cT} = N² at the last sample (default c = ln(N²)/T). Anything that leaks into
the damped record is multiplied by that factor at the record end. LabUAT
(`labuat_impulse*`) and `marx_lab_2MV` drive the structure with
`branchNorton` sources: a hard current step of 187 kA at t = 0 into a response
that decays slowly. The band-limited reconstruction of that front has
pre-ringing, which in the periodic FFT record lands on the last samples, and
the last 10–25 % of every output is garbage (1e10–1e11 V; INV-2026-11-02).
The spectral Hann window and the band-edge filter act on the *damped* spectrum
and only confine the growth to the last few samples.

Measured on `marx_lab_2MV` (2048 samples, 4 MHz, T = 256 µs, node N10, peak
1.49 MV), error against a reference with a 4× longer record (8192-point FFT,
same smoothing, wrap-around negligible), as % of peak, worst value per section
of the record:

| Run | 0–25 % | 25–50 % | 50–75 % | 75–90 % | 90–100 % |
| --- | --- | --- | --- | --- | --- |
| NLT, default c | (front differs by design) | 0.16 | 0.08 | 0.38 | 3e7 |
| NLT, c·T = 3 | (idem) | 0.008 | 0.004 | 0.005 | 148 |
| **NLT, c·T = 6, riseTime 0.5 µs** | **0.014** | **0.007** | **0.004** | **0.002** | **1.0** |
| FFT, riseTime 0.5 µs | 6.8 | 3.4 | 1.7 | 0.8 | 0.5 |
| FFT, plain | (idem) | 3.2 | 1.6 | 0.8 | 7.8 |

(The unsmoothed runs differ from the smoothed reference at the front by the
smoothing itself, hence "by design".) Two separate effects are visible: the
default c is too large for a case whose leakage is far above machine
precision, and the sharp front is the leakage source. Smoothing alone cut the
end-of-record peak by about 3000×; a symmetric (non-causal) kernel only halved
it, because it leaves a jump at t = 0.

The plain FFT has no artefact at the end but carries a wrap-around bias of
several percent here, because the response has not decayed within the record.

## Decision

New optional field `signal.riseTime` (seconds, ≥ 0; absent or 0 = off, results
unchanged):

- Each sampled excitation is convolved, before the tail taper, the window and
  the NLT damping, with a causal triangular kernel of unit area and base
  2·`riseTime` (`mTransient::smoothedWaveform`; the kernel is evaluated on 16
  sub-samples per interval of the *analytic* waveform, not of its samples). The
  smoothed excitation is therefore delayed by `riseTime` and starts at zero.
  A step becomes a raised ramp reaching its value at 2·`riseTime`.
- `riseTime` must be 0 or at least one sample interval 1/(2·`nyquistHz`).
- The reported `injectedCurrent` is the smoothed one.
- Applies to every transform (`fft` too); it is the physical rise of the real
  source (a spark gap does not close in zero time) as much as a numerical tool.
- `nltDamping` keeps its default. The recommended value for step-like sources
  is c·T ≈ 6 (T = N/(2·`nyquistHz`)), i.e. `nltDamping` = 12·`nyquistHz`/N,
  instead of the default c·T = ln N² (16.6 at N = 2048). It is not made the
  default because it moves every existing NLT result.

## Consequences

- Fortran only for now. The Rust and Julia loaders ignore the field and would
  silently produce the unsmoothed result; conformance fixtures using
  `riseTime` have to wait for the port (open item).
- Results move by the delay `riseTime` and by the rounded front; choose
  `riseTime` of a few samples and well below the physical front time.
- Lower c trades the end-of-record artefact for a wrap-around offset that
  scales as e^{−cT} times the late response (0.4 % at c·T = 3 on the case
  above); lengthen the record if the response is slow.
