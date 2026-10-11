# ROADMAP Phase 9 — transient pipeline options: internal-consistency checks

Checks behind the [ADR 0015 amendment of 2026-09-30](../adr/0015-time-domain-signal-schema.md#amendment-2026-09-30--transient-pipeline-completion-roadmap-phase-9)
(scan-fed transfer function, windows, multiple injections, Numerical Laplace
Transform). Unlike the other pages in this folder there is no published
curve: each new option is compared against a slower or longer run of the
existing pipeline that it must reproduce (the windows of §3 have no such
reference and are a sensitivity table plus two identities). Regenerate every table with

```sh
python3 docs/validation/phase9_transient_checks.py <path to the tupa executable>
```

(Fortran `fpm run` binary or `rust/target/release/tupa`; both give the same
tables to the digits shown, windows included. Run times below are the Rust
binary on the session machine; the Fortran binary took 7.0–7.2 s / 0.49–0.59 s.)

## 1. Scan-fed vs per-bin transfer function (item 1)

Cases: the four `common/silva2025_rho*_transient.json` (60 m buried
electrode, 60 segments, `alipio-visacro` soil, MCS_FST#2 current,
`nyquistHz` 4 MHz, 4096 samples → 2049 bins), with `freqZeroHz` = 100 Hz
in both runs. Scan grid: the paper's own 128 log-spaced points,
100 Hz–4 MHz (`pointsPerDecade` 27.6). Quantity: max |interpolated − full|
over the whole record, divided by the peak of the full-solve series.

| ρ0 (Ω·m) | GPR `Node_1` | V `Node_2` (60 m away) | i1 `Line_1_e30` | i2 `Line_1_e30` | run time full / interpolated (s) |
| --- | --- | --- | --- | --- | --- |
| 100 | 1.8e-6 | 2.7e-5 | 9.1e-6 | 1.3e-5 | 2.73 / 0.22 |
| 300 | 1.7e-6 | 1.8e-5 | 5.6e-6 | 4.6e-6 | 2.82 / 0.20 |
| 1000 | 8.4e-6 | 2.1e-5 | 9.1e-6 | 1.1e-5 | 2.71 / 0.21 |
| 2400 | 1.7e-5 | 2.1e-5 | 1.4e-5 | 1.5e-5 | 3.04 / 0.21 |

Stated tolerance (ADR amendment): **1e-4 of the series peak**. The
remote-node and mid-conductor quantities — the delayed transfer functions
of theory.md §8 open question 2 — stay within the same order as the
driving point on this 60 m geometry, so no loader-side sampling criterion
was added. The solve count drops from 2049 to 128 (12–15× in wall time;
the remaining cost is the geometry-factor preparation and the FFTs).

## 2. Numerical Laplace Transform vs FFT (item 5)

Case: the Portela 1997 conductor (10 m, 10 segments, linear soil
σ = 0.01 S/m, εr = 10) under the slow 250/2500 µs double exponential,
`nyquistHz` 10 kHz. The short record (256 samples, 12.8 ms ≈ 5 tail time
constants) is deliberately wrap-around-limited; the reference is a plain
FFT on a 16× longer record (4096 samples, same Δt), where wrap-around is
negligible over the first part. No window (see caveat 2 below). Quantity:
max |error| over the first *n* samples, divided by the reference peak
over the same span.

| Samples compared (of 256) | FFT, 256 samples | NLT, 256 samples (c = ln(N²)/T) |
| --- | --- | --- |
| first 64 | 2.6e-4 | 2.5e-5 |
| first 128 | 2.6e-4 | 4.4e-5 |
| first 192 | 3.2e-3 | 9.6e-3 |
| all 256 | 2.5e-2 | 9.2 |

Over the first half NLT is 6–10× closer to the wrap-around-free reference.
Beyond about 75 % of the record both short runs differ from the reference
anyway (the erfc tail taper of the last 20 % acts on a different part of
the waveform), and the NLT error grows as e^{ct}, reaching N² = 65 536 at
the last sample. The same pattern shows on `silva2025_rho100_transient`
(4096 samples): NLT removes the plain FFT's ≈ 1150 V (0.3 % of peak)
wrap-around offset — the GPR at t = 0 and after the current has died out
goes from ≈ 1150 V to ≈ 0 — and is clean over the first ~80 % of the
record without a window, or up to the last ~16 samples with the spectral
Hann window.

Caveats, stated in theory.md §8:

1. *Record end.* Read NLT output over the early part of the record and size
   the record for it (or lower `nltDamping`, trading aliasing suppression
   for less amplification).
2. *Windows under NLT.* The spectral window multiplies the *damped*
   spectrum, so the smoothing it applies differs from the same window under
   the FFT; on a front only a few samples long (here 5) that difference is
   larger than the wrap-around error removed. Gómez & Uribe [17] recommend
   the window for Gibbs suppression; it pays off when the front is well
   resolved.
3. *Band-limited cases.* On `portela1997_transient` (1.2 µs front at
   1 MHz Nyquist, ADR 0021) the response still has content at Nyquist and
   NLT output is usable only over roughly the first half of the record.
   That case is therefore not the NLT fixture; `portela1997_transient_nlt`
   uses the slow surge.

## 3. Windows and the band-edge filter (item 2)

Two cases, each run with no window, the ADR 0021 band-edge filter
(`antialiasStart` 0.85), the spectral half-Hann window and the time-placed
half-Hann window: `common/portela1997_transient.json` (1.2/50 µs surge, 30 kA,
1 MHz Nyquist, 1024 samples — a front only a few samples long) and
`common/silva2025_rho100_transient.json` (MCS_FST#2, 4 MHz Nyquist, 4096
samples — the Fig. 4 case of [silva2025-fig4.md](silva2025-fig4.md)). The
columns are the peak GPR change against the unwindowed run, the largest
|v| in the last 10 % of the record and in the first sample, both as a
fraction of the peak (the record end and start are where leakage and
wrap-around show).

| Case | Variant | peak GPR vs none (%) | last 10 % / peak | first sample / peak |
| --- | --- | --- | --- | --- |
| Portela 1.2/50 µs | none | 0 | 5.3e-2 | 1.1e-1 |
| | `antialiasStart` 0.85 | −3.43 | 8.4e-2 | 1.5e-1 |
| | hann, spectral | −10.52 | 1.2e-2 | 3.3e-1 |
| | hann, time | −0.01 | 5.3e-2 | 1.1e-1 |
| Silva ρ0 = 100 Ω·m | none | 0 | 3.5e-3 | 3.1e-3 |
| | `antialiasStart` 0.85 | 0.00 | 3.5e-3 | 3.1e-3 |
| | hann, spectral | −0.56 | 3.5e-3 | 3.4e-3 |
| | hann, time | −0.16 | 2.3e-3 | 2.0e-3 |

Reading the table:

- The −3.43 % of the band-edge filter on the Portela case is the number ADR
  0021 recorded when it was introduced, reproduced here to the digit.
- The spectral Hann window is the strongest smoother — 10.5 % off the peak of
  the Portela surge, whose 1.2 µs front is only a few samples at 1 MHz, and it
  cuts the record-end ripple 4×, but it also raises the first sample from 11 % to a third
  of the peak — on this unresolved front the smoothing moves content to the
  start of the record rather than removing it (cause not investigated further).
  On the well-resolved Silva case (4 MHz, MCS_FST#2) it costs 0.56 % of the
  peak: **the Fig. 4 comparison of silva2025-fig4.md, made without a window,
  would not change by more than that** with `hann/spectral`.
- The time-placed window multiplies the excitation, not the transfer
  function: the GPR peak moves by 0.01 % (Portela) and 0.16 % (Silva), the
  window acting where the surge has died away; it suppresses the leakage at
  the record end (Silva: 3.5e-3 → 2.3e-3), not the band-edge content.
- The band-edge filter at 0.85 is a no-op on the Silva case, whose content
  at 4 MHz is already negligible.

Two identities, checked on the Portela case: the spectral Hann window equals
`antialiasStart` → 0 to **3.8e-7** of the peak (ADR 0021 update); and the time
window multiplies the sampled excitation by ½[1 + cos(π k/N)] to **3.0e-9**.
Both are exercised at the unit level too (below). Choosing the window
remains the user's call: this is the size of the trade, not a recommendation
(the Gómez & Uribe [17] guidance is that the window pays off when the front
is resolved, which is where the Silva number is small).

## 4. Unit-level checks

In `fortran/test/test_transient.f90` (ported to `rust/tests/physics.rs`):
pchip against SciPy's `PchipInterpolator` reference values (same algorithm
as Matlab `pchip`) and exactness on linear data; spectral Hann = the
ADR 0021 filter in the s → 0 limit; two half-amplitude sources at one node
= one full source (to 1e-12), and a doubleExp + sine pair at two nodes =
the sum of the single-source runs (to 1e-10); the time window multiplies
the sampled excitation exactly. In `test_material.f90`/`test_impedance.f90`
(and the Rust unit tests): W(jω) of all three soil models and Z_int(jω)
from the Laplace-domain formulas match the harmonic ones to 1e-12 from
1 Hz to 100 MHz, and both are conjugate-symmetric.

## 5. Golden fixtures and cross-implementation

`common/portela1997_transient_{interpolated,hann,hann_time,multi,nlt}_expected.csv`
(Fortran output). The Rust implementation reproduces all five under the
fixture rule (1e-6 · max(|expected|, 1e-3 · series peak)); worst case
1.5e-8, on the NLT fixture's amplified record end. The Julia port does not
implement the Phase 9 fields yet and rejects them (julia/README.md).
