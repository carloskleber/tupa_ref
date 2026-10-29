module mBessel
  !! Modified Bessel functions of complex argument, the subset the solver needs
  !! (ADR 0031; replaces SLATEC `ZBESI`).
  !!
  !! `besselI0OverI1(z)` returns I0(z)/I1(z) for `Re z >= 0`, the ratio in the
  !! solid-cylinder internal impedance (theory.md §4.3). The solver only
  !! reaches `arg z` in [0, π/4] (`z = r0·sqrt(s·μ·σ)`, `Re s >= 0`).
  !! Two regimes, the same as `rust/src/bessel.rs`:
  !!
  !! - `|z| < SERIES_LIMIT`: ascending power series (term growth bounded by
  !!   e^|z|; cancellation loses at most ≈ e^(|z| − Re z) ≈ 400 at 45°);
  !! - otherwise the Hankel asymptotic series truncated at its smallest term.
  !!   The `e^z/sqrt(2πz)` factor common to I0 and I1 cancels in the ratio, so
  !!   nothing overflows at any |z|; the dropped `e^(-2z)` companion term is
  !!   below ≈ 1e-13 relative for `Re z >= 14`.
  !!
  !! Pure and stateless: safe in OpenMP regions, no machine-constant tables.
  !! Measured against AMOS (`scipy.special.ive`): ≤ 3e-15 for arg ≤ 30°,
  !! ≤ 3e-14 at 45° (worst at |z| ≈ 24), 1e-6 ≤ |z| ≤ 2000. (A switch at 20
  !! gives 6.6e-13 just above it; 25 was chosen for that reason.)
  use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
  use mCtes, only: dp
  implicit none
  private

  public :: besselI0OverI1

  real(dp), parameter :: SERIES_LIMIT = 25.0_dp
  !! Switch between the series and the asymptotic expansion (|z|)
  integer, parameter :: MAX_TERMS = 500
  !! Series terms cap (never reached for |z| < SERIES_LIMIT)
  integer, parameter :: MAX_ASYMPTOTIC_TERMS = 200

contains

  pure function besselI0OverI1(z) result(r)
    !! I0(z)/I1(z) for `Re z >= 0`. NaN at z = 0 (I1 vanishes there; the
    !! drivers never pass ω = 0, they use `freqZeroHz`).
    complex(dp), intent(in) :: z
    complex(dp) :: r

    if (abs(z) == 0.0_dp) then
      r = cmplx(ieee_value(1.0_dp, ieee_quiet_nan), ieee_value(1.0_dp, ieee_quiet_nan), kind=dp)
    else if (abs(z) < SERIES_LIMIT) then
      r = seriesI(z, 0) / seriesI(z, 1)
    else
      r = asymptoticSum(z, 0) / asymptoticSum(z, 1)
    end if
  end function besselI0OverI1

  pure function seriesI(z, nu) result(s)
    !! I_nu(z) = (z/2)^nu · sum_k (z²/4)^k / (k! (k+nu)!), nu = 0 or 1.
    complex(dp), intent(in) :: z
    integer, intent(in) :: nu
    complex(dp) :: s, term, q
    integer :: k

    q = z * z * 0.25_dp
    if (nu == 1) then
      term = z * 0.5_dp
    else
      term = cmplx(1.0_dp, 0.0_dp, kind=dp)
    end if
    s = term
    do k = 1, MAX_TERMS
      term = term * q / (real(k, dp) * real(k + nu, dp))
      s = s + term
      if (abs(term) < 1.0e-18_dp * abs(s)) exit
    end do
  end function seriesI

  pure function asymptoticSum(z, nu) result(s)
    !! sum_k (-1)^k a_k(nu)/z^k with the factor e^z/sqrt(2πz) omitted
    !! (it cancels in the I0/I1 ratio); stops at the smallest term.
    complex(dp), intent(in) :: z
    integer, intent(in) :: nu
    complex(dp) :: s, term
    real(dp) :: mu, odd, n, prev
    integer :: k

    mu = 4.0_dp * real(nu * nu, dp)
    term = cmplx(1.0_dp, 0.0_dp, kind=dp)
    s = term
    prev = huge(1.0_dp)
    do k = 1, MAX_ASYMPTOTIC_TERMS
      odd = 2.0_dp * real(k, dp) - 1.0_dp
      term = -term * (mu - odd * odd) / (real(k, dp) * 8.0_dp) / z
      n = abs(term)
      if (n > prev) exit        ! the asymptotic series started to diverge
      s = s + term
      if (n < 1.0e-17_dp) exit
      prev = n
    end do
  end function asymptoticSum

end module mBessel
