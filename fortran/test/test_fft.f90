program test_fft
  !! Tests for mFft (ROADMAP.md Phase 6 item 2): the in-repo double-precision
  !! radix-2 FFT that drives the transient response, since SLATEC's
  !! CFFTF/CFFTB are single precision only and stdlib has no FFT module
  !! (ADR 0014).
  use mCtes, only: dp, PI
  use mFft
  use check
  implicit none

  integer(4), parameter :: n = 8
  complex(dp) :: x(n), xOrig(n), xRef(n)
  complex(dp) :: randVals(n), roundTrip(n)
  real(dp) :: maxErr, energyTime, energyFreq
  integer(4) :: k

  call test_init("isPowerOfTwo / nextPowerOfTwo")
  call test_ok("1 is a power of two", isPowerOfTwo(1), "")
  call test_ok("8 is a power of two", isPowerOfTwo(8), "")
  call test_ok("0 is not a power of two", .not. isPowerOfTwo(0), "")
  call test_ok("6 is not a power of two", .not. isPowerOfTwo(6), "")
  call test_ok("nextPowerOfTwo(5) == 8", nextPowerOfTwo(5) == 8, "")
  call test_ok("nextPowerOfTwo(8) == 8", nextPowerOfTwo(8) == 8, "")

  ! ----------------------------------------------------------------
  ! Forward transform matches the brute-force DFT definition exactly,
  ! for an input with no special symmetry (delta at n=1, not n=0, so a
  ! sign error in the transform convention shows up as a wrong phase).
  ! ----------------------------------------------------------------
  call test_init("fftForward matches the brute-force DFT definition (delta at n=1)")

  x = (0.0_dp, 0.0_dp)
  x(2) = (1.0_dp, 0.0_dp)
  call bruteForceDft(x, xRef, -1.0_dp)
  call fftForward(x)

  maxErr = maxval(abs(x - xRef))
  call test_ok("max error vs brute-force DFT < 1e-12", maxErr < 1.0d-12, "maxErr too large")

  ! ----------------------------------------------------------------
  ! Same check for a generic (non-symmetric) random-looking input
  ! ----------------------------------------------------------------
  call test_init("fftForward matches the brute-force DFT definition (generic input)")

  randVals = [(1.0_dp, 0.0_dp), (0.5_dp, -0.3_dp), (-0.2_dp, 0.7_dp), (0.9_dp, 0.1_dp), &
              (-1.3_dp, 0.4_dp), (0.2_dp, 0.2_dp), (0.0_dp, -0.6_dp), (1.1_dp, -1.1_dp)]
  x = randVals
  call bruteForceDft(x, xRef, -1.0_dp)
  call fftForward(x)

  maxErr = maxval(abs(x - xRef))
  call test_ok("max error vs brute-force DFT < 1e-12", maxErr < 1.0d-12, "maxErr too large")

  ! ----------------------------------------------------------------
  ! Round trip: forward then inverse recovers the original signal
  ! ----------------------------------------------------------------
  call test_init("fftForward + fftInverse round-trips to the original signal")

  xOrig = randVals
  roundTrip = xOrig
  call fftForward(roundTrip)
  call fftInverse(roundTrip)

  maxErr = maxval(abs(roundTrip - xOrig))
  call test_ok("round-trip error < 1e-12", maxErr < 1.0d-12, "round trip did not recover the input")

  ! ----------------------------------------------------------------
  ! Parseval's theorem: sum|x(n)|^2 == (1/N) sum|X(k)|^2
  ! ----------------------------------------------------------------
  call test_init("Parseval energy conservation")

  x = randVals
  call fftForward(x)
  energyTime = sum(abs(randVals) ** 2)
  energyFreq = sum(abs(x) ** 2) / real(n, dp)
  call test_ok("time-domain energy matches frequency-domain energy", &
               abs(energyTime - energyFreq) < 1.0d-12 * max(1.0_dp, energyTime), &
               "Parseval's theorem violated")

  ! ----------------------------------------------------------------
  ! Known transform pairs
  ! ----------------------------------------------------------------
  call test_init("known transform pairs")

  ! Constant signal -> all energy at DC
  x = (1.0_dp, 0.0_dp)
  call fftForward(x)
  call test_ok("constant signal: X(0) == N", abs(x(1) - cmplx(real(n, dp), 0.0_dp, dp)) < 1.0d-12, "")
  call test_ok("constant signal: X(k>0) == 0", all(abs(x(2:n)) < 1.0d-12), "")

  ! Delta at n=0 -> flat spectrum
  x = (0.0_dp, 0.0_dp)
  x(1) = (1.0_dp, 0.0_dp)
  call fftForward(x)
  call test_ok("delta at n=0: flat unit-magnitude spectrum", &
               all(abs(abs(x) - 1.0_dp) < 1.0d-12), "")

  call test_summary()

contains

  subroutine bruteForceDft(xin, xout, sgn)
    !! O(N^2) reference DFT: X(k) = sum_n x(n) exp(sgn*2*pi*i*k*n/N),
    !! 0-indexed k,n. Used only to cross-check mFft's fast transform.
    complex(dp), intent(in) :: xin(:)
    complex(dp), intent(out) :: xout(:)
    real(dp), intent(in) :: sgn
    integer(4) :: nn, kk, jj
    complex(dp) :: acc, w

    nn = size(xin)
    do kk = 0, nn - 1
      acc = (0.0_dp, 0.0_dp)
      do jj = 0, nn - 1
        w = cmplx(cos(sgn * 2.0_dp * PI * kk * jj / nn), sin(sgn * 2.0_dp * PI * kk * jj / nn), kind=dp)
        acc = acc + xin(jj + 1) * w
      end do
      xout(kk + 1) = acc
    end do
  end subroutine bruteForceDft

end program test_fft
