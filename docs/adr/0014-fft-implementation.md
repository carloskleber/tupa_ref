# ADR 0014 — FFT implementation for the transient driver

- **Status**: Accepted
- **Date**: 2026-07-15

## Context

ROADMAP.md Phase 6 needs a forward/inverse FFT to drive the transient
(time-domain) response (theory.md §8): sample the excitation waveform,
transform to a one-sided spectrum, multiply by the frequency-domain
transfer function, and inverse-transform back to the time domain. §6 of
the roadmap left this as an open decision ("stdlib vs FFTW — decide in
Phase 6"). Three options were available when the phase actually landed:

1. **stdlib**: the pinned dependency (`fpm.toml`) is stdlib 0.8.1, which has
   no `stdlib_fft` module (added upstream after this pin) — not usable
   without bumping the dependency.
2. **SLATEC `CFFTI`/`CFFTF`/`CFFTB`**: already a build dependency
   (`fortran/slatec`, linked for `mImpedance`'s `ZBESI`), so reusing it adds
   no new external dependency. But its Fortran 77 source declares `COMPLEX
   C(*)`/`REAL WSAVE(*)` with no explicit kind — i.e. **single precision**,
   not `complex(dp)`. Every other physics routine in this project computes
   in double precision (`mCtes`'s `dp`); routing the transient path through
   a single-precision transform would downcast the already-computed
   double-precision transfer function and excitation spectrum for no
   reason connected to the physics.
3. **FFTW**: double precision, but a new external C dependency (its own
   build/link story on every OS), for one feature used by one driver.

## Decision

Write a small self-contained double-precision **radix-2 Cooley-Tukey FFT**
in `fortran/src/Fft.f90` (`mFft`), forward (unnormalized analysis,
$X(k)=\sum_n x(n)e^{-2\pi i kn/N}$) and inverse (1/N-normalized synthesis,
matching MATLAB's `fft`/`ifft` and the legacy `ifourier.m` convention the
transient driver's conjugate-symmetric reconstruction depends on). Requires
transform length to be a power of two — `mTransient` always samples a
power-of-two record, so this is not a practical restriction.

Correctness is pinned in `fortran/test/test_fft.f90` against an independent
$O(N^2)$ brute-force DFT (not just internal round-trip/Parseval checks,
which cannot catch a wrong transform-sign convention), plus known
transform pairs (constant signal, unit impulse).

## Consequences

- No new external dependency, and no precision loss relative to the rest
  of the double-precision physics stack.
- The transform is $O(N\log N)$ but restricted to power-of-two lengths;
  `mTransient` always requests one, so this costs nothing in practice. A
  general mixed-radix transform (what `stdlib_fft`, once available, or
  SLATEC's `CFFTF1` provide) would lift the restriction but is unneeded
  scope for the current use case.
- If a future feature needs an FFT at a non-power-of-two length, or the
  pinned stdlib version is bumped past the point `stdlib_fft` lands, that
  is grounds to revisit this ADR — not to quietly grow `mFft` into a
  general-purpose transform library.
