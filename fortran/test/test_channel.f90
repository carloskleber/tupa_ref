program test_channel
  !! Lightning channel in air (ROADMAP Phase 10b, theory.md §4.5, ADR 0025):
  !! segment grading, loading formula, assembly, two-node sources (current
  !! dipole and delta-gap voltage), the Chen cross-check of the unloaded
  !! channel, and the speed calibration.
  use mCtes, only: PI
  use mStudy
  use mStructure
  use mNode
  use mMaterial
  use mElementLine
  use mElementChannel
  use mChannelCalibration
  use mSignal, only: tSignalSlot, newPortelaSignal
  use mTransient, only: transientResponseSources, tTransientOptions
  use mMesh, only: IMAGE_IDEAL
  use mVerbosity, only: setVerbosity, VERB_QUIET, VERB_NORMAL
  use tupa, only: loadStudy
  use check
  implicit none

  real(8), parameter :: c0 = 299792458.0d0

  call testGrading()
  call testLoadingFormula()
  call testFrontOnset()
  call testAssembly()
  call testTwoNodeSources()
  call testChenUnloaded()
  call testCalibration()

  call test_summary()

contains

  subroutine testGrading()
    real(8), allocatable :: b(:)
    real(8) :: maxRatio, maxSeg
    integer :: n, k

    call test_init("Segment grading")
    b = gradedBreaks(3000.0d0, 5.0d0, 1.15d0, 20.0d0)
    n = size(b) - 1
    call test_ok("starts at 0 and ends at the channel length", &
                 b(1) == 0.0d0 .and. abs(b(n + 1) - 3000.0d0) < 1.0d-9, "break endpoints")
    maxRatio = 0.0d0
    maxSeg = 0.0d0
    do k = 2, n
      maxRatio = max(maxRatio, (b(k + 1) - b(k)) / (b(k) - b(k - 1)))
    end do
    do k = 1, n
      maxSeg = max(maxSeg, b(k + 1) - b(k))
    end do
    call test_ok("ratio of adjacent segments never exceeds growth", maxRatio <= 1.15d0 + 1.0d-12, "ratio bound")
    call test_ok("no segment longer than maxSegment", maxSeg <= 20.0d0 + 1.0d-12, "maxSegment")
    call test_ok("first segment no longer than requested", b(2) <= 5.0d0 + 1.0d-12, "firstSegment")
    call test_ok("segments only coarsen away from the foot", b(3) - b(2) >= b(2) - b(1) - 1.0d-12, "grading direction")

    b = gradedBreaks(95.0d0, 10.0d0, 1.0d0, 10.0d0)
    call test_ok("growth 1: uniform ceil(L / maxSegment) segments", size(b) - 1 == 10 .and. &
                 abs(b(2) - 9.5d0) < 1.0d-12, "uniform chain")
  end subroutine testGrading

  subroutine testLoadingFormula()
    type(tPiecewise) :: p

    call test_init("Loading estimate and profiles")
    call test_ok("L' at 500 m, r0 = 3 cm, v = c/2: 6.2 uH/m (theory.md 4.5)", &
                 abs(channelLoadingEstimate(500.0d0, 0.03d0, c0 / 2.0d0) - 6.2d-6) < 0.1d-6, "c/2")
    call test_ok("L' at 500 m, r0 = 3 cm, v = c/3: 17 uH/m", &
                 abs(channelLoadingEstimate(500.0d0, 0.03d0, c0 / 3.0d0) - 16.7d-6) < 0.3d-6, "c/3")
    call test_ok("v = c needs no loading", abs(channelLoadingEstimate(500.0d0, 0.03d0, c0)) < 1.0d-12, "v = c")

    allocate(p%upTo(2), p%value(2))
    p%upTo = [500.0d0, 1500.0d0]
    p%value = [3.0d0, 1.0d0]
    call test_ok("piecewise: first piece up to its break (inclusive)", evalPiecewise(p, 500.0d0, 0.0d0) == 3.0d0, "break 1")
    call test_ok("piecewise: second piece", evalPiecewise(p, 501.0d0, 0.0d0) == 1.0d0, "piece 2")
    call test_ok("piecewise: last value continues beyond the last break", evalPiecewise(p, 9.0d3, 0.0d0) == 1.0d0, "tail")
  end subroutine testLoadingFormula

  subroutine testFrontOnset()
    integer, parameter :: n = 400
    real(8) :: t(n), i(n)
    integer :: k

    call test_init("10-90 % tangent front tracking")
    do k = 1, n
      t(k) = real(k - 1, kind=8) * 5.0d-8
      i(k) = min(1.0d0, max(0.0d0, (t(k) - 2.0d-6) / 1.0d-6))
    end do
    call test_ok("ramp starting at 2 us: onset 2 us", &
                 abs(frontOnsetTime(t, i, 2.0d-6, 9.0d-6, 1.0d-6) - 2.0d-6) < 1.0d-12, "exact on linear data")
    ! A later, larger reflection outside the window must not move the front
    do k = 1, n
      if (t(k) > 10.0d-6) i(k) = 5.0d0
    end do
    call test_ok("a reflection after the window is ignored", &
                 abs(frontOnsetTime(t, i, 2.0d-6, 9.0d-6, 1.0d-6) - 2.0d-6) < 1.0d-12, "window")
  end subroutine testFrontOnset

  subroutine writeTiltCase(fileName)
    character(len=*), intent(in) :: fileName
    integer :: u

    open(newunit=u, file=fileName, status="replace", action="write")
    write(u, '(A)') '{ "title": "tilted channel", "soil": {"conductivity": 0.01, "permittivity": 10, "permeability": 1},'
    write(u, '(A)') '  "nodes": [ {"id": "S", "position": [1.0, 2.0, 10.0]}, {"id": "T", "position": [1.0, 2.0, 0.0]} ],'
    write(u, '(A)') '  "materials": [ {"id": "m", "epsilonr": 1, "mur": 1, "sigma": 5e6} ],'
    write(u, '(A)') '  "elements": [ {"type": "line", "id": "L", "from": "S", "to": "T", "radius": 0.1, "segments": 2, "material": "m"},'
    write(u, '(A)') '    {"type": "channel", "id": "ch", "strike": "S", "length": 100, "radius": 0.03, "incidence": 30, "azimuth": 90,'
    write(u, '(A)') '     "speed": 1.5e8, "resistance": [{"upTo": 40, "value": 2.0}, {"upTo": 1000, "value": 0.5}], "segments": 4} ] }'
    close(u)
  end subroutine writeTiltCase

  subroutine removeFile(fileName)
    character(len=*), intent(in) :: fileName
    integer :: u

    open(newunit=u, file=fileName, status="old")
    close(u, status="delete")
  end subroutine removeFile

  subroutine testAssembly()
    type(tStudy) :: s
    integer :: iBase, iTop, iS, k
    real(8) :: p(3)

    call test_init("Channel assembly from JSON")
    call writeTiltCase("test_channel_tilt.json")
    call loadStudy("test_channel_tilt.json", s)
    call removeFile("test_channel_tilt.json")
    call s%structure%assembleStructure()
    iBase = s%structure%findNodeIndex("ch-base")
    iTop  = s%structure%findNodeIndex("ch-top")
    iS    = s%structure%findNodeIndex("S")
    call test_ok("base node is separate from the strike node", iBase /= 0 .and. iBase /= iS, "ch-base")
    call test_ok("base coincides with the strike node", &
                 norm2(s%structure%nodes(iBase)%p - s%structure%nodes(iS)%p) < 1.0d-12, "coincident")
    p = s%structure%nodes(iTop)%p - s%structure%nodes(iS)%p
    call test_ok("top: 100 m along 30 deg from vertical, azimuth 90 deg", &
                 abs(p(1)) < 1.0d-9 .and. abs(p(2) - 50.0d0) < 1.0d-9 .and. abs(p(3) - 50.0d0 * sqrt(3.0d0)) < 1.0d-9, &
                 "top position")
    call test_ok("4 channel segments plus the 2 tower segments", s%structure%getElectrodeCount() == 6, "electrode count")
    k = s%structure%findElectrodeIndex("ch_e1")
    call test_ok("segments are loaded, perfect-conductor", s%structure%electrodes(k)%loaded .and. &
                 .not. associated(s%structure%electrodes(k)%material), "loaded flag")
    call test_ok("R' follows the profile along the axis (25 m: 2 Ohm/m)", &
                 abs(s%structure%electrodes(k)%loadResistance - 2.0d0) < 1.0d-12, "R' piece 1")
    k = s%structure%findElectrodeIndex("ch_e4")
    call test_ok("R' of the last segment (87.5 m: 0.5 Ohm/m)", &
                 abs(s%structure%electrodes(k)%loadResistance - 0.5d0) < 1.0d-12, "R' piece 2")
    call test_ok("the tower segments carry no loading", &
                 .not. s%structure%electrodes(s%structure%findElectrodeIndex("L_e1"))%loaded, "tower")
  end subroutine testAssembly

  subroutine buildTower(s)
    type(tStudy), intent(out) :: s
    class(tMaterial), allocatable :: mat
    class(tElement), allocatable :: elem

    s%title = "tower with channel"
    s%structure%soil = newMaterialLinear("soil", 10.0d0, 1.0d0, 0.001d0)
    call s%structure%addNode(newNode("Tfoot", [0.0d0, 0.0d0, 0.0d0]))
    call s%structure%addNode(newNode("Ttop", [0.0d0, 0.0d0, 30.0d0]))
    call s%structure%addNode(newNode("Rend", [0.0d0, 0.0d0, -3.0d0]))
    mat = newMaterialLinear("steel", 1.0d0, 1.0d0, 5.0d6)
    call s%structure%addMaterial(mat)
    elem = newElementLine("Tower", "Tfoot", "Ttop", 0.3d0, 6, "steel")
    call s%structure%addElement(elem)
    elem = newElementLine("Rod", "Tfoot", "Rend", 0.0125d0, 3, "steel")
    call s%structure%addElement(elem)
    elem = newChannelUniform("ch", "Ttop", 300.0d0, 0.03d0, 15, 1.5d8, 0.5d0)
    call s%structure%addElement(elem)
  end subroutine buildTower

  function newChannelUniform(id, strike, length, radius, n, speed, rPrime) result(el)
    character(len=*), intent(in) :: id, strike
    real(8), intent(in) :: length, radius, speed, rPrime
    integer, intent(in) :: n
    class(tElement), allocatable :: el
    real(8) :: b(n + 1)
    integer :: k

    do k = 0, n
      b(k + 1) = length * real(k, kind=8) / real(n, kind=8)
    end do
    el = newElementChannel(id, strike, length, 0.0d0, 0.0d0, radius, b)
    select type (el)
    type is (tChannel)
      allocate(el%speed%upTo(1), el%speed%value(1), el%resistance%upTo(1), el%resistance%value(1))
      el%speed%upTo = huge(1.0d0)
      el%speed%value = speed
      el%resistance%upTo = huge(1.0d0)
      el%resistance%value = rPrime
    end select
  end function newChannelUniform

  subroutine testTwoNodeSources()
    type(tStudy) :: s1, s2, s3
    real(8) :: freq(2)
    complex(8) :: one, a, b, zin, i2
    integer :: iTop, iBase, k, iC
    real(8) :: dmax
    complex(8), allocatable :: zs(:)

    call test_init("Two-node sources (ADR 0025)")
    freq = [1.0d5, 1.0d6]
    one = cmplx(1.0d0, 0.0d0, kind=8)

    ! Current dipole = the same two single-node injections
    call buildTower(s1)
    call buildTower(s2)
    call s1%runSweep(freq, ["Ttop"], [one], returnNodeIds=["ch-base"])
    call s2%runSweep(freq, ["Ttop   ", "ch-base"], [one, -one])
    iTop  = s1%structure%findNodeIndex("Ttop")
    iBase = s1%structure%findNodeIndex("ch-base")
    dmax = 0.0d0
    do k = 1, 2
      dmax = max(dmax, abs(s1%voltageResults%get(iTop, k) - s2%voltageResults%get(iTop, k)), &
                       abs(s1%voltageResults%get(iBase, k) - s2%voltageResults%get(iBase, k)))
    end do
    call test_ok("current dipole equals +I and -I injections", dmax < 1.0d-9, "dipole vs two sources")

    ! Terminal impedance, and a voltage gap of that value draws 1 A
    zs = s1%inputImpedance("Ttop")
    call test_ok("input impedance is the terminal voltage over the current", &
                 abs(zs(1) - (s1%voltageResults%get(iTop, 1) - s1%voltageResults%get(iBase, 1))) < 1.0d-9, "Zin")
    call buildTower(s3)
    call s3%runSweep(freq, ["Ttop"], [zs(1)], sourceIsVoltage=[.true.], returnNodeIds=["ch-base"])
    a = s3%voltageResults%get(iTop, 1) - s3%voltageResults%get(iBase, 1)
    call test_ok("voltage gap pins u(Ttop) - u(ch-base)", abs(a - zs(1)) < 1.0d-8 * abs(zs(1)), "gap voltage")
    call test_ok("the gap source draws the dipole current (1 A at the first frequency)", &
                 abs(s3%sweepSourceCurrentsFreq(1, 1) - one) < 1.0d-8, "effective current")
    call test_ok("fields of the gap solution equal those of the current dipole", &
                 abs(s3%voltageResults%get(iTop, 1) - s1%voltageResults%get(iTop, 1)) < 1.0d-8 * abs(zs(1)), "same solution")
    iC = s3%structure%findElectrodeIndex("ch_e1")
    call test_ok("the channel base segment carries the source current (downward: -1 A)", &
                 abs(s3%longCurrentResults%get(iC, 1) + one) < 5.0d-2, "channel base current")

    ! Dipoles sharing a node: sums, no repeated-index trouble
    b = (0.0d0, 0.0d0)
    call s1%run(freq(1) * 2.0d0 * PI, ["Ttop   ", "Tfoot  "], [one, one], returnNodeIds=["ch-base", "ch-base"])
    a = s1%mesh%voltage(iBase)
    call s2%run(freq(1) * 2.0d0 * PI, ["Ttop   ", "Tfoot  ", "ch-base"], [one, one, -2.0d0 * one])
    b = s2%mesh%voltage(iBase)
    call test_ok("two dipoles sharing the return node add up there", abs(a - b) < 1.0d-9 * max(1.0d0, abs(b)), "shared node")
    i2 = a
  end subroutine testTwoNodeSources

  subroutine testChenUnloaded()
    !! Unloaded 1 km channel over ideal ground, voltage ramp (5 MV, 1 us):
    !! after the front the segment current follows Chen's analytic step
    !! response (Baba & Rakov, references.md [44]), convolved with the ramp.
    type(tStudy) :: s
    class(tElement), allocatable :: elem
    type(tSignalSlot) :: slots(1)
    type(tTransientOptions) :: opts
    real(8), allocatable :: t(:), inj(:,:), v(:,:), i1(:,:), i2(:,:)
    real(8) :: z, ref, err, peak
    integer :: k, n
    real(8), parameter :: eta = 376.730313668d0, a0 = 0.23d0

    call test_init("Unloaded channel vs Chen's analytic current")
    call setVerbosity(VERB_QUIET)
    s%imageModel = IMAGE_IDEAL
    s%structure%soil = newMaterialLinear("soil", 10.0d0, 1.0d0, 0.01d0)
    elem = newFreeChannel("ch", 1000.0d0, a0, 100)
    call s%structure%addElement(elem)
    allocate(slots(1)%sig, source=newPortelaSignal(5.0d6, 0.0d0, 1.0d-6, 1.0d3, 2.0d3))
    slots(1)%isVoltage = .true.
    opts%transform = "nlt"
    call transientResponseSources(s, slots, ["ch-base"], ["ch-base"], 5.0d6, 512, 1.0d-6, t, inj, v, &
                                  observeElectrodeIds=["ch_e31"], i1Responses=i1, i2Responses=i2, options=opts)
    call setVerbosity(VERB_NORMAL)

    z = 305.0d0                      ! midpoint of segment 31 (300-310 m)
    err = 0.0d0
    peak = 0.0d0
    n = 0
    do k = 1, size(t)
      if (t(k) < 3.0d-6 .or. t(k) > 4.5d-6) cycle   ! the top reflection reaches z at 5.65 us
      ref = chenRamp(z, t(k), 5.0d6, 1.0d-6, a0, eta)
      err = max(err, abs(i1(1, k) - ref))
      peak = max(peak, ref)
      n = n + 1
    end do
    call test_ok("window of samples checked", n >= 12, "no samples in the window")
    call test_ok("segment current within 1.5 % of Chen's after the front", err < 0.015d0 * peak, &
                 "worst deviation too large")
  end subroutine testChenUnloaded

  function newFreeChannel(id, length, radius, n) result(el)
    character(len=*), intent(in) :: id
    real(8), intent(in) :: length, radius
    integer, intent(in) :: n
    class(tElement), allocatable :: el
    real(8) :: b(n + 1)
    integer :: k

    do k = 0, n
      b(k + 1) = length * real(k, kind=8) / real(n, kind=8)
    end do
    el = newElementChannel(id, "", length, 0.0d0, 0.0d0, radius, b)
  end function newFreeChannel

  real(8) function chenStepIntegral(z, t, eta, a0) result(s)
    !! ∫0^t of Chen's monopole step response to 1 V (A·s/V), by midpoint rule.
    real(8), intent(in) :: z, t, eta, a0
    integer, parameter :: m = 4000
    real(8) :: tau, dt, arg, tStart
    integer :: k

    s = 0.0d0
    tStart = z / c0
    if (t <= tStart) return
    dt = (t - tStart) / real(m, kind=8)
    do k = 1, m
      tau = tStart + (real(k, kind=8) - 0.5d0) * dt
      arg = sqrt((c0 * tau)**2 - z**2) / a0
      if (arg <= 1.0d0) cycle
      s = s + 2.0d0 * (2.0d0 / eta) * atan(PI / (2.0d0 * log(arg))) * dt
    end do
  end function chenStepIntegral

  real(8) function chenRamp(z, t, v0, tr, a0, eta) result(i)
    !! Chen's current for a ramp of height `v0` and rise `tr`: the step
    !! response convolved with the ramp, (v0/tr)·(S(t) - S(t - tr)).
    real(8), intent(in) :: z, t, v0, tr, a0, eta

    i = v0 / tr * (chenStepIntegral(z, t, eta, a0) - chenStepIntegral(z, t - tr, eta, a0))
  end function chenRamp

  subroutine testCalibration()
    type(tStudy) :: s
    class(tElement), allocatable :: elem
    class(tElement), pointer :: el
    call test_init("Speed calibration")
    call setVerbosity(VERB_QUIET)
    elem = newChannelUniform("ch", "", 1500.0d0, 0.03d0, 75, 1.5d8, 0.5d0)
    select type (elem)
    type is (tChannel)
      elem%wantCalibration = .true.
    end select
    s%structure%soil = newMaterialLinear("soil", 10.0d0, 1.0d0, 0.01d0)
    call s%structure%addElement(elem)
    call calibrateChannels(s)
    call setVerbosity(VERB_NORMAL)
    el => s%structure%getElement(1)
    select type (el)
    type is (tChannel)
      call test_ok("calibrated flag set", el%calibrated, "calibrated")
      call test_ok("measured speed within 0.6 % of the target", abs(el%calibratedSpeed - 1.5d8) < 9.0d5, "speed")
      call test_ok("scale near the closed form (0.8 .. 1.3)", el%loadScale > 0.8d0 .and. el%loadScale < 1.3d0, "kappa")
    end select
  end subroutine testCalibration

end program test_channel
