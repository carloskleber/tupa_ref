module mChannelCalibration
  !! Calibration of a lightning channel's series loading against a
  !! prescribed return-stroke speed (ROADMAP.md Phase 10b item 1,
  !! theory.md §4.5, ADR 0025).
  !!
  !! The closed-form L'(z) is only a starting value: against full-wave
  !! results it is off by up to 0.12c in speed. The calibration runs the
  !! channel alone above ideal ground, measures its speed with Baba & Rakov's
  !! metric, and solves for the scale factor κ on the closed form that makes
  !! the measured speed equal the target. Segmentation, radius and R' are the
  !! channel's own, so the calibrated κ also absorbs the discretisation's
  !! numerical dispersion; a calibration is valid for that set only. The
  !! calibration channel is lossless (R' = 0): the front-tracking metric is
  !! defined for the front of a lossless loaded wire, and a resistive wire's
  !! convex, slowly rising front makes it ill-conditioned (Baba & Rakov note
  !! the same for their speeds); R' then only damps the calibrated wave.
  !!
  !! Measurement: the channel is excited at its base by a current ramp
  !! (1 µs rise, then constant) and solved by the Numerical Laplace Transform
  !! (a steady final value is its native case). At two heights, 0.2 L and
  !! 0.8 L of the calibration length L = min(channel length, 2.5 km), the
  !! front of the segment current is characterised by the point where the
  !! tangent through its 10 % and 90 % levels crosses the time axis; the
  !! speed is the height difference over the difference of those times. The
  !! target of a channel with a speed profile is the harmonic-mean speed
  !! between the two heights.
  use mStudy
  use mStructure
  use mNode
  use mMaterial
  use mElement
  use mElementChannel
  use mSignal, only: tSignalSlot, newPortelaSignal
  use mTransient, only: transientResponseSources, tTransientOptions
  use mMesh, only: IMAGE_IDEAL
  use mError, only: raiseError
  use mGeometryCache, only: geomCacheClear
  use mVerbosity
  use mCtes, only: MU0, EPSILON0
  implicit none
  private

  public :: calibrateChannels, calibrateChannel, frontOnsetTime

  real(8), parameter :: LIGHT_SPEED = 1.0d0 / sqrt(MU0 * EPSILON0)
  real(8), parameter :: RISE_TIME = 1.0d-6
  !! Rise time of the calibration current ramp (s)
  real(8), parameter :: MAX_CAL_LENGTH = 2500.0d0
  !! Longest channel part simulated for the calibration (m)
  integer, parameter :: N_SAMPLES = 512
  integer, parameter :: MAX_ITER = 20
  real(8), parameter :: TOLERANCE = 2.0d-3
  !! Relative speed tolerance at which the iteration stops
  real(8), parameter :: ACCEPT = 6.0d-3
  !! Relative speed error up to which the best evaluation is accepted when
  !! the iteration does not reach `TOLERANCE`: the metric has a jitter of
  !! about 0.5 % (one time sample, and the ripple of the NLT synthesis)
  real(8), parameter :: KAPPA_MIN = 0.25d0
  real(8), parameter :: KAPPA_MAX = 4.0d0
  !! Credible range of the calibrated scale: outside it the speed metric has
  !! locked onto something other than the wave (segments too long for the
  !! radius and rise time), and the loading would be meaningless

contains

  subroutine calibrateChannels(study)
    !! Calibrate every channel of the study that asked for it
    !! (`"calibrate": true`).
    class(tStudy), intent(inout) :: study
    class(tElement), pointer :: el
    integer :: i

    do i = 1, study%structure%getElementCount()
      el => study%structure%getElement(i)
      select type (el)
      type is (tChannel)
        if (el%wantCalibration) then
          ! The geometry-factor cache is process-global and quantised: entries left by
          ! an earlier study differ from fresh values at the quadrature tolerance, which
          ! is enough to move the iteration's path. Start (and leave) it empty, so the
          ! calibrated scale does not depend on what ran before in the process.
          call geomCacheClear()
          call calibrateChannel(el)
          call geomCacheClear()
        end if
      end select
    end do
  end subroutine calibrateChannels

  subroutine calibrateChannel(ch)
    !! Find the loading scale `ch%loadScale` that gives the channel its
    !! target speed; sets `calibrated` and `calibratedSpeed`.
    class(tChannel), intent(inout) :: ch
    type(tStudy) :: mini
    class(tElement), allocatable :: el
    type(tChannel) :: cal
    real(8), allocatable :: brk(:)
    real(8) :: lCal, s1, s2, vTarget, kappa, kPrev, vMeas, hCur, hPrev, hTgt, slope
    real(8) :: bestErr, bestK, bestV, err
    integer :: iter, n, j1, j2
    integer :: nSeg

    ! Calibration channel: the first part of the channel, vertical, on its own foot
    n = size(ch%breaks) - 1
    nSeg = n
    do j1 = 1, n
      if (ch%breaks(j1 + 1) >= MAX_CAL_LENGTH * (1.0d0 - 1.0d-9)) then
        nSeg = j1
        exit
      end if
    end do
    brk = ch%breaks(1:nSeg + 1)
    lCal = brk(nSeg + 1)
    s1 = 0.2d0 * lCal
    s2 = 0.8d0 * lCal
    j1 = segmentAt(brk, s1)
    j2 = segmentAt(brk, s2)
    if (j1 >= j2) then
      call raiseError("tChannel '" // trim(ch%id) // "': too few segments to calibrate the speed")
      return
    end if
    ! Heights actually observed: the segment midpoints
    s1 = 0.5d0 * (brk(j1) + brk(j1 + 1))
    s2 = 0.5d0 * (brk(j2) + brk(j2 + 1))
    vTarget = targetSpeed(ch, s1, s2)

    mini%imageModel = IMAGE_IDEAL
    mini%structure%soil = newMaterialLinear("soil", 10.0d0, 1.0d0, 0.01d0)
    el = newElementChannel("cal", "", lCal, 0.0d0, 0.0d0, ch%radius, brk)
    select type (el)
    type is (tChannel)
      el%speed      = ch%speed
    end select
    call mini%structure%addElement(el)
    call mini%structure%assembleStructure()
    cal%breaks = brk
    cal%radius = ch%radius
    cal%speed = ch%speed
    cal%incidenceDeg = 0.0d0

    ! Secant iteration on h(κ) = (c/v(κ))² - (c/v_t)², nearly linear in κ
    hTgt = (LIGHT_SPEED / vTarget)**2
    kappa = 1.0d0
    kPrev = 0.0d0
    hPrev = 0.0d0
    bestErr = huge(1.0d0)
    bestK = kappa
    bestV = 0.0d0
    do iter = 1, MAX_ITER
      vMeas = measureSpeed(kappa)
      if (vMeas <= 0.0d0) return
      if (verbosityLevel() .eq. VERB_VERBOSE) print '(A,F10.5,A,ES12.5,A,ES12.5)', &
        "   kappa = ", kappa, "  v = ", vMeas, "  target = ", vTarget
      err = abs(vMeas - vTarget) / vTarget
      if (err < bestErr) then
        bestErr = err
        bestK = kappa
        bestV = vMeas
      end if
      if (err <= TOLERANCE) exit
      hCur = (LIGHT_SPEED / vMeas)**2 - hTgt
      if (iter == 1) then
        ! First step: assume (c/v)² - 1 proportional to κ
        kPrev = kappa
        hPrev = hCur
        kappa = kappa * (hTgt - 1.0d0) / max((LIGHT_SPEED / vMeas)**2 - 1.0d0, 1.0d-6)
      else
        slope = (hCur - hPrev) / (kappa - kPrev)
        kPrev = kappa
        hPrev = hCur
        if (slope == 0.0d0) exit
        kappa = kappa - hCur / slope
      end if
      if (kappa <= 0.0d0) kappa = 0.5d0 * kPrev
    end do

    if (bestErr > ACCEPT) then
      call raiseError("tChannel '" // trim(ch%id) // "': the loading calibration did not reach the target speed " // &
                      "(measured speed does not respond to the loading — target above the unloaded channel's speed?)")
      return
    end if
    if (bestK < KAPPA_MIN .or. bestK > KAPPA_MAX) then
      call raiseError("tChannel '" // trim(ch%id) // "': the calibrated loading scale is implausible (outside 0.25-4): " // &
                      "the speed metric is not reliable for this radius and segmentation — use shorter segments")
      return
    end if
    ch%loadScale = bestK
    ch%calibrated = .true.
    ch%wantCalibration = .false.
    ch%calibratedSpeed = bestV
    call verbose(VERB_NORMAL, " Channel '" // trim(ch%id) // "' calibrated")

  contains

    real(8) function measureSpeed(scale) result(v)
      !! Speed of the calibration channel at loading scale `scale`.
      real(8), intent(in) :: scale
      type(tSignalSlot) :: slots(1)
      type(tTransientOptions) :: opts
      real(8), allocatable :: t(:), inj(:,:), nodeResp(:,:), i1(:,:), i2(:,:)
      real(8) :: tRec, vMin, nyq, t1, t2
      integer :: k, nn
      real(8) :: rP, lP
      integer :: err

      ! Loading of every segment at this scale (geometry factors are reused)
      cal%loadScale = scale
      do k = 1, nSeg
        call cal%segmentLoading(k, 0.0d0, rP, lP, err)
        mini%structure%electrodes(k)%loadResistance = rP
        mini%structure%electrodes(k)%loadInductance = lP
      end do

      vMin = minval(ch%speed%value)
      tRec = 1.5d0 * lCal / vMin
      nyq = real(N_SAMPLES, kind=8) / (2.0d0 * tRec)
      allocate(slots(1)%sig, source=newPortelaSignal(1.0d0, 0.0d0, RISE_TIME, 1.0d3, 2.0d3))
      opts%transform = "nlt"
      nn = 0
      call transientResponseSources(mini, slots, ["cal-base"], ["cal-base"], nyq, N_SAMPLES, 1.0d-6, &
                                     t, inj, nodeResp, observeElectrodeIds=[electrodeId(j1), electrodeId(j2)], &
                                     i1Responses=i1, i2Responses=i2, options=opts)
      if (.not. allocated(t)) then
        v = 0.0d0
        return
      end if
      t1 = frontOnsetTime(t, i1(1, :), s1 / vTarget, (2.0d0 * lCal - s1) / vTarget, RISE_TIME)
      t2 = frontOnsetTime(t, i1(2, :), s2 / vTarget, (2.0d0 * lCal - s2) / vTarget, RISE_TIME)
      if (t2 <= t1) then
        v = 0.0d0
        return
      end if
      v = (s2 - s1) / (t2 - t1)
    end function measureSpeed

    function electrodeId(j) result(id)
      integer, intent(in) :: j
      character(len=32) :: id
      write(id, '("cal_e",I0)') j
    end function electrodeId

  end subroutine calibrateChannel

  integer function segmentAt(brk, s) result(j)
    !! 1-based index of the segment whose midpoint is nearest to `s`.
    real(8), intent(in) :: brk(:), s
    real(8) :: best, d
    integer :: k

    j = 1
    best = huge(1.0d0)
    do k = 1, size(brk) - 1
      d = abs(0.5d0 * (brk(k) + brk(k + 1)) - s)
      if (d < best) then
        best = d
        j = k
      end if
    end do
  end function segmentAt

  real(8) function targetSpeed(ch, s1, s2) result(v)
    !! Speed the calibration aims at between heights `s1` and `s2`: the
    !! uniform target, or for a profile the harmonic mean
    !! (s2 - s1) / ∫ ds / v(s), integrated over the profile pieces.
    class(tChannel), intent(in) :: ch
    real(8), intent(in) :: s1, s2
    integer, parameter :: NSUB = 2000
    real(8) :: tTravel, sm, ds
    integer :: k

    ds = (s2 - s1) / real(NSUB, kind=8)
    tTravel = 0.0d0
    do k = 1, NSUB
      sm = s1 + (real(k, kind=8) - 0.5d0) * ds
      tTravel = tTravel + ds / evalPiecewise(ch%speed, sm, LIGHT_SPEED)
    end do
    v = (s2 - s1) / tTravel
  end function targetSpeed

  real(8) function frontOnsetTime(t, i, tDirect, tReflected, tRise) result(t0)
    !! Time where the 10-90 % tangent of the first front of the series `i(t)`
    !! crosses the time axis (Baba & Rakov's wave-front tracking, theory.md
    !! §4.5). The front's peak is searched for in a window that ends
    !! `2·tRise + 0.2·tDirect` after the start, but never later than midway
    !! between the expected direct arrival `tDirect` and the expected arrival
    !! of the wave reflected from the top `tReflected`: a longer window would
    !! let the slow creep of the plateau and the late-record noise of the NLT
    !! (amplified as e^{ct}) move the peak, and with it the 90 % level. The
    !! 90 % point is the first rise through its level, the 10 % point the last
    !! rise through its level before that (robust to a band-limit precursor
    !! ripple). Returns 0 if the front is not found.
    real(8), intent(in) :: t(:), i(:)
    real(8), intent(in) :: tDirect, tReflected
    real(8), intent(in) :: tRise
    !! Rise time of the excitation ramp (s)
    real(8) :: tCut, peak, t10, t90
    integer :: k, kPeak, m90

    t0 = 0.0d0
    tCut = min(0.5d0 * (tDirect + max(tReflected, tDirect)), tDirect + 2.0d0 * tRise + 0.2d0 * tDirect)
    peak = -huge(1.0d0)
    kPeak = 0
    do k = 1, size(t)
      if (t(k) > tCut) exit
      if (i(k) > peak) then
        peak = i(k)
        kPeak = k
      end if
    end do
    if (kPeak < 2 .or. peak <= 0.0d0) return

    ! 90 %: the first rise through the level; 10 %: the last rise through its
    ! level *before* that, so a precursor ripple ahead of the front (band
    ! limit) cannot be taken for the start of the front
    t90 = riseTime(0.9d0 * peak, kPeak, m90)
    if (t90 < 0.0d0) return
    t10 = riseTimeBackward(0.1d0 * peak, m90)
    if (t10 < 0.0d0 .or. t90 <= t10) return
    t0 = t10 - (t90 - t10) / 8.0d0

  contains

    real(8) function riseTime(level, kLast, mFound) result(tc)
      !! First time the series rises through `level` (linear interpolation),
      !! searched up to sample `kLast`; `mFound` is the sample ending that step.
      real(8), intent(in) :: level
      integer, intent(in) :: kLast
      integer, intent(out) :: mFound
      integer :: m

      tc = -1.0d0
      mFound = 0
      do m = 2, kLast
        if (i(m - 1) < level .and. i(m) >= level) then
          tc = t(m - 1) + (level - i(m - 1)) / (i(m) - i(m - 1)) * (t(m) - t(m - 1))
          mFound = m
          return
        end if
      end do
    end function riseTime

    real(8) function riseTimeBackward(level, mStart) result(tc)
      !! Last rise through `level` at or before sample `mStart`.
      real(8), intent(in) :: level
      integer, intent(in) :: mStart
      integer :: m

      tc = -1.0d0
      do m = mStart, 2, -1
        if (i(m - 1) < level .and. i(m) >= level) then
          tc = t(m - 1) + (level - i(m - 1)) / (i(m) - i(m - 1)) * (t(m) - t(m - 1))
          return
        end if
      end do
    end function riseTimeBackward

  end function frontOnsetTime

end module mChannelCalibration
