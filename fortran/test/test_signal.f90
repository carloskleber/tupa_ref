program test_signal
  !! Tests for mSignal (ROADMAP.md Phase 6 item 1): Heidler and
  !! double-exponential (+ Jones) excitation waveforms ported from the
  !! legacy Matlab `+sinais` package.
  use mCtes, only: dp
  use mSignal
  use check
  implicit none

  type(tHeidlerSignal) :: heidler
  type(tDoubleExpSignal) :: dexp, dexpJones
  real(dp), allocatable :: t(:), i(:), iJones(:)
  real(dp) :: dt, peak, imax
  integer(4) :: n, k, iPeak

  ! ----------------------------------------------------------------
  ! Heidler: causal, peak amplitude matches imax
  ! ----------------------------------------------------------------
  call test_init("tHeidlerSignal: causality and peak normalisation")

  imax = 30.0d3
  heidler = newHeidlerSignal(imax)

  n = 2000
  dt = 500.0d-6 / real(n - 1, dp)
  allocate(t(n))
  t = [(-100.0d-6 + real(k - 1, dp) * dt, k = 1, n)]

  i = heidler%waveform(t)

  call test_ok("zero for t <= 0", all(abs(i) < 1.0d-9 .or. t > 0.0d0), "nonzero current before t=0")
  block
    logical :: allZeroBeforeOrigin
    allZeroBeforeOrigin = .true.
    do k = 1, n
      if (t(k) <= 0.0d0 .and. abs(i(k)) > 1.0d-9) allZeroBeforeOrigin = .false.
    end do
    call test_ok("current is exactly zero for t <= 0", allZeroBeforeOrigin, "")
  end block

  peak = maxval(i)
  call test_ok("peak amplitude matches imax to 0.1%", &
               abs(peak - imax) < 1.0d-3 * imax, "peak does not match the requested imax")

  iPeak = maxloc(i, dim=1)
  call test_ok("peak occurs strictly inside the sampled record", &
               iPeak > 1 .and. iPeak < n, "peak at a record boundary — sampling window too narrow")

  ! ----------------------------------------------------------------
  ! Double exponential: causal, decays to ~0 well after the tail time,
  ! peak located near the nominal front time
  ! ----------------------------------------------------------------
  call test_init("tDoubleExpSignal: causality, decay, and front timing")

  imax = 10.0d3
  dexp = newDoubleExpSignal(imax, "f1_2_50")

  deallocate(t, i)
  n = 4000
  dt = 500.0d-6 / real(n - 1, dp)
  allocate(t(n))
  t = [(-10.0d-6 + real(k - 1, dp) * dt, k = 1, n)]
  i = dexp%waveform(t)

  block
    logical :: allZeroBeforeOrigin
    allZeroBeforeOrigin = .true.
    do k = 1, n
      if (t(k) <= 0.0d0 .and. abs(i(k)) > 1.0d-6 * imax) allZeroBeforeOrigin = .false.
    end do
    call test_ok("current is ~zero for t <= 0", allZeroBeforeOrigin, "")
  end block

  call test_ok("decays to < 5% of imax by 10x the tail time", &
               abs(i(n)) < 0.05d0 * imax, "waveform has not decayed by 10x tau_tail")

  iPeak = maxloc(i, dim=1)
  call test_ok("peak within an order of magnitude of the nominal front time (1.2 us)", &
               t(iPeak) > 0.1d-6 .and. t(iPeak) < 12.0d-6, "peak timing implausible for a 1.2/50 us waveform")

  ! ----------------------------------------------------------------
  ! Jones correction: zero initial slope vs the plain double
  ! exponential's nonzero initial slope (theory.md §8 finding 6)
  ! ----------------------------------------------------------------
  call test_init("Jones correction removes the nonzero initial slope")

  dexpJones = newDoubleExpSignal(imax, "f1_2_50", jones=.true.)
  iJones = dexpJones%waveform(t)

  ! Centred finite difference exactly around t=0, with a step small enough
  ! not to be dominated by the Jones front's own curvature (~1/alpha): the
  ! coarse plotting grid `t` above lands near, but not exactly at, t=0, and
  ! exp(-(alpha*t)^2) curves away fast enough (scale 1/alpha ~ 0.4 us for
  ! this waveform) that a grid-relative slope estimate is unreliable here.
  block
    real(dp) :: slopePlain, slopeJones, h, tSmall(3), iSmall(3), iJonesSmall(3)
    h = 1.0d-9
    tSmall = [-h, 0.0d0, h]
    iSmall = dexp%waveform(tSmall)
    iJonesSmall = dexpJones%waveform(tSmall)
    slopePlain = (iSmall(3) - iSmall(1)) / (2.0d0 * h)
    slopeJones = (iJonesSmall(3) - iJonesSmall(1)) / (2.0d0 * h)
    call test_ok("plain double-exp has a substantial initial slope", &
                 abs(slopePlain) > 0.1d0 * imax / dexp%tFront, "expected a nonzero front slope")
    call test_ok("Jones-corrected front has a much smaller initial slope", &
                 abs(slopeJones) < 0.1d0 * abs(slopePlain), &
                 "Jones correction should flatten the initial slope")
  end block

  ! ----------------------------------------------------------------
  ! tailTaper: ~1 over the first 80%, decays toward the last sample
  ! ----------------------------------------------------------------
  call test_init("tailTaper: unity front, tapered tail")

  block
    real(dp), allocatable :: w(:)
    integer(4) :: nTap
    nTap = 200
    w = tailTaper(nTap)
    call test_ok("taper ~1 at record start", abs(w(1) - 1.0d0) < 1.0d-6, "")
    call test_ok("taper ~1 at 50% (well before the 80% roll-off)", &
                 abs(w(nTap / 2) - 1.0d0) < 1.0d-3, "")
    call test_ok("taper ~0.5 at the 80% roll-off point", &
                 abs(w(nint(0.8d0 * nTap)) - 0.5d0) < 0.05d0, "")
    call test_ok("taper decays toward the last sample", &
                 w(nTap) < w(nint(0.8d0 * nTap)), "taper should keep decreasing into the tail")
  end block

  call test_summary()

end program test_signal
