program test_cross_section
  !! Pins the cross-section internal-impedance entries (ADR 0030): evaluation
  !! against the Bessel cylinder for the round-wire entry, conductivity
  !! scaling, the DC pin, the table kind, loader errors, and an end-to-end
  !! sweep with a `crossSection` element against the same case with
  !! `radius` + `material`.
  use mCtes, only: PI
  use mCrossSection
  use mImpedance, only: internalImpedance
  use mStudy
  use mError
  use tupa
  use check
  use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
  implicit none

  character(len=*), parameter :: WIRE = "../common/cross_sections/wire_Cu_r10mm.json"
  character(len=*), parameter :: CASE_FILE = "test_cross_section_case.json"
  character(len=*), parameter :: HEAD = &
    '{"title":"cs","soil":{"permittivity":10,"permeability":1,"conductivity":0.01},' // &
    '"nodes":[{"id":"A","position":[0,0,-1]},{"id":"B","position":[10,0,-1]}],'
  character(len=*), parameter :: MATS = '"materials":[{"id":"cu","epsilonr":1,"mur":1,"sigma":5.9979877e7}],'
  character(len=*), parameter :: SRC = '"sources":[{"node":"A","current":{"re":1,"im":0}}],' // &
    '"frequencies":{"min":100,"max":1000000,"pointsPerDecade":1}}'
  character(len=*), parameter :: TABLE_ENTRY = &
    '{"schema":"tupa.crossSection/0-draft","id":"tab","kind":"table","equivalentRadius":0.01,' // &
    '"reference":{"sigma":5.8e7,"mur":1},"band":{"fMin":10,"fMax":1e6},' // &
    '"model":{"f":[10,100,1000,1e4,1e5,1e6],"R":[1e-4,1e-4,1.2e-4,2e-4,6e-4,2e-3],' // &
    '"X":[1e-7,1e-6,1e-5,1e-4,6e-4,2e-3]}}'

  type(tCrossSection), allocatable :: entries(:)
  type(tCrossSection) :: wireCs, scaled, tab
  real(8), parameter :: r0 = 0.01d0
  real(8) :: f, omega, worst, worstL, sigma0, worstS
  complex(8) :: zc, zi, zl
  integer :: i

  ! ------------------------------------------------------------------
  call test_init("round-wire entry vs the Bessel cylinder (ADR 0030 validation 1)")
  call loadCrossSectionFile(WIRE, entries)
  call test_ok("entry file loads", .not. errorRaised() .and. allocated(entries), "cannot load " // WIRE)
  if (.not. allocated(entries)) stop 1
  wireCs = entries(1)
  sigma0 = wireCs%sigmaRef
  call test_ok("equivalent radius is the wire radius", abs(wireCs%equivalentRadius - r0) < 1.0d-12)

  worst = 0.0d0
  worstL = 0.0d0
  do i = 0, 120
    f = 1.0d0 * (2.5d7) ** (real(i, kind=8) / 120.0d0)
    omega = 2.0d0 * PI * f
    zc = internalImpedance(r0, 1.0d0, omega, sigma0, 1.0d0)
    zi = wireCs%perMetre(omega)
    zl = wireCs%perMetreLaplace(cmplx(0.0d0, omega, kind=8))
    worst = max(worst, abs(zi - zc) / abs(zc))
    worstL = max(worstL, abs(zl - zi) / abs(zi))
  end do
  call test_ok("harmonic Z_int within 1.5e-3 of ZBESI-grade cylinder over 1 Hz - 25 MHz", worst < 1.5d-3)
  print '(A,ES9.2)', "   worst relative error: ", worst
  call test_ok("Laplace evaluation on the jw axis equals the harmonic one", worstL < 1.0d-12)

  ! ------------------------------------------------------------------
  call test_init("conductivity scaling and DC pin")
  scaled = withConductivity(wireCs, 2.0d0 * sigma0, "scaled")
  worstS = 0.0d0
  do i = 0, 100
    f = 10.0d0 * (1.0d6) ** (real(i, kind=8) / 100.0d0)
    omega = 2.0d0 * PI * f
    zc = internalImpedance(r0, 1.0d0, omega, 2.0d0 * sigma0, 1.0d0)
    worstS = max(worstS, abs(scaled%perMetre(omega) - zc) / abs(zc))
  end do
  call test_ok("entry used at 2 sigma matches the cylinder at 2 sigma (1.5e-3)", worstS < 1.5d-3)
  call test_ok("band follows the conductivity (f_max / 2)", effectiveBandHigh(scaled) < 0.51d0 * wireCs%fMax)
  zc = wireCs%perMetre(1.0d-6)
  call test_ok("DC resistance is 1/(sigma pi r^2) (1e-6)", &
               abs(real(zc) * sigma0 * PI * r0**2 - 1.0d0) < 1.0d-6)

  ! ------------------------------------------------------------------
  call test_init("table entry")
  call writeText(TABLE_ENTRY)
  call loadCrossSectionFile(CASE_FILE, entries)
  call test_ok("table entry loads", .not. errorRaised() .and. allocated(entries))
  tab = entries(1)
  zi = tab%perMetre(2.0d0 * PI * 1.0d4)
  call test_ok("value at a grid point", abs(zi - cmplx(2.0d-4, 1.0d-4, kind=8)) < 1.0d-12)
  zi = tab%perMetre(2.0d0 * PI * sqrt(1.0d3 * 1.0d4))
  call test_ok("log-linear midpoint", abs(real(zi) - 1.6d-4) < 1.0d-12)
  zi = tab%perMetre(2.0d0 * PI * 1.0d-3)
  call test_ok("below the band: resistance held", abs(real(zi) - 1.0d-4) < 1.0d-12)
  zi = tab%perMetre(2.0d0 * PI * 4.0d6)
  call test_ok("above the band: sqrt(f) continuation", abs(zi / cmplx(2.0d-3, 2.0d-3, kind=8) - 2.0d0) < 1.0d-12)
  zi = tab%perMetreLaplace(cmplx(1.0d3, 1.0d4, kind=8))
  call test_ok("table has no Laplace continuation (NaN)", ieee_is_nan(real(zi)))

  ! ------------------------------------------------------------------
  call test_init("loader: crossSection elements, end to end")
  block
    type(tStudy) :: sCs, sRef
    complex(8) :: zCs(3), zRef(3)
    real(8) :: fr(3)
    integer :: k

    fr = [1.0d3, 1.0d5, 1.0d6]
    call loadAndRun(HEAD // '"crossSections":[{"file":"' // WIRE // '"}],' // &
      '"elements":[{"type":"line","id":"L","from":"A","to":"B","crossSection":"wire_Cu_r10mm","segments":4}],' // SRC, &
      sCs, zCs, fr)
    call loadAndRun(HEAD // MATS // &
      '"elements":[{"type":"line","id":"L","from":"A","to":"B","radius":0.01,"segments":4,"material":"cu"}],' // SRC, &
      sRef, zRef, fr)
    call test_ok("both cases solved", .not. errorRaised())
    do k = 1, 3
      call test_ok("Zin matches the radius+material case within 1e-4 at f=" // numStr(fr(k)), &
                   abs(zCs(k) - zRef(k)) / abs(zRef(k)) < 1.0d-4)
    end do

    call loadAndRun(HEAD // MATS // '"crossSections":[{"file":"' // WIRE // '"}],' // &
      '"elements":[{"type":"line","id":"L","from":"A","to":"B","crossSection":"wire_Cu_r10mm","material":"cu","segments":4}],' // SRC, &
      sCs, zCs, fr)
    call test_ok("crossSection + material (sigma override) solves", .not. errorRaised())
    do k = 1, 3
      call test_ok("sigma-override Zin matches within 1e-4 at f=" // numStr(fr(k)), &
                   abs(zCs(k) - zRef(k)) / abs(zRef(k)) < 1.0d-4)
    end do
  end block

  ! ------------------------------------------------------------------
  call test_init("loader and solver errors")
  call expectError("radius together with crossSection", &
    HEAD // '"crossSections":[{"file":"' // WIRE // '"}],' // &
    '"elements":[{"type":"line","id":"L","from":"A","to":"B","crossSection":"wire_Cu_r10mm","radius":0.01,"segments":2}],' // SRC, &
    "both radius and crossSection")
  call expectError("unknown cross-section id", &
    HEAD // '"elements":[{"type":"line","id":"L","from":"A","to":"B","crossSection":"nope","segments":2}],' // SRC, &
    "not found")
  call expectError("duplicate id", &
    HEAD // '"crossSections":[{"file":"' // WIRE // '"},{"file":"' // WIRE // '"}],"elements":[]}', "already used")
  call expectError("mur mismatch with the conductor material", &
    HEAD // '"materials":[{"id":"fe","epsilonr":1,"mur":200,"sigma":5e6}],"crossSections":[{"file":"' // WIRE // '"}],' // &
    '"elements":[{"type":"line","id":"L","from":"A","to":"B","crossSection":"wire_Cu_r10mm","material":"fe","segments":2}],' // SRC, &
    "different mur")
  call expectError("unstable (positive) pole", &
    HEAD // '"crossSections":[' // positivePole() // '],"elements":[]}', "not strictly negative")
  call expectError("unknown kind", &
    HEAD // '"crossSections":[' // replaceKind() // '],"elements":[]}', "unknown kind")
  call expectError("table entry rejected under the numerical Laplace transform", &
    HEAD // '"crossSections":[' // TABLE_ENTRY // '],' // &
    '"elements":[{"type":"line","id":"L","from":"A","to":"B","crossSection":"tab","segments":2}],' // SRC, &
    "cannot be used with the numerical Laplace", runDamping=1.0d5)

  call removeCase()
  call test_close()

contains

  real(8) function effectiveBandHigh(cs)
    type(tCrossSection), intent(in) :: cs
    real(8) :: lo, hi
    call cs%effectiveBand(lo, hi)
    effectiveBandHigh = hi
  end function effectiveBandHigh

  function numStr(x) result(s)
    real(8), intent(in) :: x
    character(len=16) :: s
    write(s, '(ES10.2)') x
  end function numStr

  function positivePole() result(s)
    character(len=:), allocatable :: s
    s = '{"id":"bad","kind":"poleResidue","equivalentRadius":0.01,"reference":{"sigma":5.8e7,"mur":1},' // &
        '"band":{"fMin":1,"fMax":1e6},"model":{"d":1e-4,"e":0,"poles":[100.0],"residues":[-1.0]}}'
  end function positivePole

  function replaceKind() result(s)
    character(len=:), allocatable :: s
    s = '{"id":"odd","kind":"spline","equivalentRadius":0.01,"reference":{"sigma":5.8e7,"mur":1},' // &
        '"band":{"fMin":1,"fMax":1e6},"model":{}}'
  end function replaceKind

  subroutine writeText(text)
    character(len=*), intent(in) :: text
    integer :: u
    open(newunit=u, file=CASE_FILE, status="replace", action="write")
    write(u, '(A)') text
    close(u)
  end subroutine writeText

  subroutine removeCase()
    integer :: u, ios
    open(newunit=u, file=CASE_FILE, status="old", iostat=ios)
    if (ios == 0) close(u, status="delete")
  end subroutine removeCase

  subroutine loadAndRun(text, study, zin, freqHz, damping)
    !! Load a case from text, discretise it and solve at `freqHz`; `zin(k)` is
    !! the voltage at node A for 1 A injected there.
    character(len=*), intent(in) :: text
    type(tStudy), intent(out) :: study
    complex(8), intent(out) :: zin(:)
    real(8), intent(in) :: freqHz(:)
    real(8), intent(in), optional :: damping
    character(len=256), allocatable :: srcNodes(:), outNodes(:), outElec(:)
    complex(8), allocatable :: srcCur(:)
    real(8), allocatable :: fileFreq(:)
    integer :: k, idx

    zin = (0.0d0, 0.0d0)
    call writeText(text)
    call clearError()
    call setLibraryMode(.true.)
    call loadStudy(CASE_FILE, study, sourceNodeIds=srcNodes, sourceCurrents=srcCur, freqHz=fileFreq, &
                   outputNodeIds=outNodes, outputElectrodeIds=outElec)
    if (.not. errorRaised()) call study%structure%assembleStructure()
    if (.not. errorRaised()) then
      if (present(damping)) then
        call study%runSweep(freqHz, srcNodes, srcCur, damping=damping)
      else
        call study%runSweep(freqHz, srcNodes, srcCur)
      end if
    end if
    call setLibraryMode(.false.)
    if (errorRaised()) return
    idx = study%structure%findNodeIndex("A")
    do k = 1, size(freqHz)
      zin(k) = study%voltageResults%get(idx, k)
    end do
  end subroutine loadAndRun

  subroutine expectError(name, text, needle, runDamping)
    character(len=*), intent(in) :: name, text, needle
    real(8), intent(in), optional :: runDamping
    type(tStudy) :: study
    complex(8) :: z(2)
    logical :: found
    integer :: i

    if (present(runDamping)) then
      call loadAndRun(text, study, z, [1.0d3, 1.0d4], runDamping)
    else
      call loadAndRun(text, study, z, [1.0d3, 1.0d4])
    end if
    found = .false.
    do i = 1, errorCount()
      if (index(errorMessage(i), needle) > 0) found = .true.
    end do
    call test_ok(name // " is rejected", errorRaised() .and. found, &
                 "expected an error containing '" // needle // "'")
    call clearError()
  end subroutine expectError

end program test_cross_section
