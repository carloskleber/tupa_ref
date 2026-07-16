module mFft
  !! Minimal double-precision radix-2 Cooley-Tukey FFT (ROADMAP.md Phase 6).
  !!
  !! SLATEC's `CFFTF`/`CFFTB` (`fortran/slatec`) declare `COMPLEX`/`REAL`
  !! (single precision) work arrays, and the pinned stdlib 0.8.1 dependency
  !! has no `stdlib_fft` module — so neither existing dependency can drive
  !! the transient response without downcasting the double-precision
  !! physics results. `mTransient` uses this module instead (ADR 0014).
  !!
  !! Convention: `fftForward` is the DFT analysis transform
  !! X(k) = Σₙ x(n)·exp(-2πi·kn/N) (unnormalized, matching the classic
  !! Cooley-Tukey/FFTPACK/MATLAB `fft` convention); `fftInverse` is the
  !! 1/N-normalized synthesis transform x(n) = (1/N)·Σₖ X(k)·exp(+2πi·kn/N).
  !! This pairing matches theory.md §2's engineering convention e^{+jωt}
  !! (synthesis carries the +j exponent) and MATLAB's `fft`/`ifft`, which
  !! the legacy `sinal/Sinal.fourier` and `ifourier.m` build on directly.
  use mCtes, only: dp, PI
  use mError, only: raiseError
  implicit none
  private
  public :: fftForward, fftInverse, isPowerOfTwo, nextPowerOfTwo

contains

  pure logical function isPowerOfTwo(n)
    !! True iff `n` is a positive power of two (1, 2, 4, 8, ...).
    integer(4), intent(in) :: n
    isPowerOfTwo = (n > 0) .and. (iand(n, n - 1) == 0)
  end function isPowerOfTwo

  pure function nextPowerOfTwo(n) result(p)
    !! Smallest power of two >= `n` (>= 1).
    integer(4), intent(in) :: n
    integer(4) :: p
    p = 1
    do while (p < n)
      p = p * 2
    end do
  end function nextPowerOfTwo

  subroutine fftForward(x)
    !! Unnormalized forward (analysis) DFT, in place. `size(x)` must be a
    !! power of two.
    complex(dp), intent(inout) :: x(:)
    call fftCore(x, -1.0_dp)
  end subroutine fftForward

  subroutine fftInverse(x)
    !! Normalized inverse (synthesis) DFT, in place. `size(x)` must be a
    !! power of two.
    complex(dp), intent(inout) :: x(:)
    call fftCore(x, 1.0_dp)
    x = x / real(size(x), kind=dp)
  end subroutine fftInverse

  subroutine fftCore(x, sgn)
    !! In-place iterative radix-2 decimation-in-time Cooley-Tukey transform
    !! (the classic Numerical-Recipes `four1` bit-reversal + butterfly
    !! structure, adapted to complex(dp) arithmetic directly). `sgn = -1`
    !! gives the forward (analysis) transform, `sgn = +1` the unnormalized
    !! inverse (synthesis) transform.
    complex(dp), intent(inout) :: x(:)
    real(dp), intent(in) :: sgn
    integer(4) :: n, i, j, m, mmax, istep
    real(dp) :: theta
    complex(dp) :: w, t

    n = size(x)
    if (n <= 1) return
    if (.not. isPowerOfTwo(n)) then
      call raiseError("mFft: array length must be a power of two")
      return
    end if

    ! Bit-reversal permutation (1-indexed)
    j = 1
    do i = 1, n
      if (j > i) then
        t = x(j); x(j) = x(i); x(i) = t
      end if
      m = n / 2
      do while (m >= 2 .and. j > m)
        j = j - m
        m = m / 2
      end do
      j = j + m
    end do

    ! Iterative butterflies over successively doubling block sizes
    mmax = 1
    do while (mmax < n)
      istep = 2 * mmax
      do m = 1, mmax
        theta = sgn * PI * real(m - 1, dp) / real(mmax, dp)
        w = cmplx(cos(theta), sin(theta), kind=dp)
        do i = m, n, istep
          j = i + mmax
          t = w * x(j)
          x(j) = x(i) - t
          x(i) = x(i) + t
        end do
      end do
      mmax = istep
    end do
  end subroutine fftCore

end module mFft
