program test_potentials
  !! Grounding-safety outputs (ROADMAP Phase 11, theory.md §3.1, ADR 0027):
  !! the segment-to-point geometry factor against quadrature, the potential of
  !! a solved rod against the far-field point-source oracle and its own node
  !! voltage, GPR/touch/step bookkeeping against independent re-evaluations,
  !! the symmetry of a symmetric grid, the `observation` reader and the
  !! writers.
  use mCtes, only: PI, MU0
  use mStudy
  use mObservation
  use mPotentials
  use mResultsWriter, only: writeObservationCsv, writeObservationJson
  use mMesh, only: tMesh, calcParamW
  use mVerbosity, only: setVerbosity, VERB_QUIET
  use tupa, only: loadStudy, validateStudyReferences
  use check
  implicit none

  call setVerbosity(VERB_QUIET)

  call testGeometryFactor()
  call testRodFarField()
  call testRodNearSurface()
  call testSafetyCase()

  call test_summary()

contains

  real(8) function simpson(a, b, p, n) result(g)
    !! Composite Simpson quadrature of 1/R along a→b, the independent oracle
    real(8), intent(in) :: a(3), b(3), p(3)
    integer, intent(in) :: n
    real(8) :: h, x(3)
    integer :: i

    h = 1.0d0 / n
    g = 0.0d0
    do i = 0, n
      x = a + (b - a) * (i * h)
      g = g + merge(1.0d0, merge(4.0d0, 2.0d0, mod(i, 2) == 1), i == 0 .or. i == n) / norm2(p - x)
    end do
    g = g * h / 3.0d0 * norm2(b - a)
  end function simpson

  subroutine testGeometryFactor()
    real(8) :: a(3), b(3)
    real(8) :: g

    call test_init("segmentPotentialFactor: closed form vs Simpson quadrature")
    a = [0.0d0, 0.0d0, 0.0d0]
    b = [3.0d0, 0.0d0, -4.0d0]

    g = segmentPotentialFactor(a, b, [1.0d0, 2.0d0, 0.5d0], 0.01d0)
    call test_ok("point off a slanted segment", abs(g - simpson(a, b, [1.0d0, 2.0d0, 0.5d0], 20000)) < 1.0d-9 * g, "g")
    g = segmentPotentialFactor(a, b, [-2.0d0, 0.5d0, 1.0d0], 0.01d0)
    call test_ok("point beyond one end", abs(g - simpson(a, b, [-2.0d0, 0.5d0, 1.0d0], 20000)) < 1.0d-9 * g, "g")
    ! 2 m beyond the far end, 1 cm off the axis line
    g = segmentPotentialFactor(a, b, [3.0d0 + 1.2d0, 0.01d0, -4.0d0 - 1.6d0], 0.01d0)
    call test_ok("point just off the axis line beyond the far end", &
                 abs(g - simpson(a, b, [3.0d0 + 1.2d0, 0.01d0, -4.0d0 - 1.6d0], 40000)) < 1.0d-8 * g, "g")
    g = segmentPotentialFactor(a, b, [1.5d0, 0.01d0, -2.0d0], 0.01d0)
    call test_ok("point at the radius from the midpoint: 2 asinh(l/2ρ)", &
                 abs(g - 2.0d0 * asinh(2.5d0 / 0.01d0)) < 1.0d-9, "g")
    g = segmentPotentialFactor(a, b, [1.5d0, 0.0d0, -2.0d0], 0.01d0)
    call test_ok("point on the axis is regularised to the conductor surface", &
                 abs(g - 2.0d0 * asinh(2.5d0 / 0.01d0)) < 1.0d-9, "g")
  end subroutine testGeometryFactor

  subroutine solveCase(file, study)
    character(len=*), intent(in) :: file
    type(tStudy), intent(out) :: study
    character(len=256), allocatable :: nodeIds(:), retIds(:)
    complex(8), allocatable :: currents(:)
    logical, allocatable :: isVoltage(:)
    real(8), allocatable :: freqHz(:)

    call loadStudy(file, study, sourceNodeIds=nodeIds, sourceCurrents=currents, sourceIsVoltage=isVoltage, &
                   freqHz=freqHz, sourceReturnNodeIds=retIds)
    call study%runSweep(freqHz, nodeIds, currents, sourceIsVoltage=isVoltage, returnNodeIds=retIds)
  end subroutine solveCase

  subroutine testRodFarField()
    type(tStudy) :: study
    complex(8), allocatable :: psi(:,:)
    real(8) :: pts(3, 2), omega, mu
    complex(8) :: w, expected, itSum
    type(tMesh) :: mesh
    integer :: k, b, nseg

    call test_init("Rod: potential far away is the point-source field of the injected current")
    call solveCase("../common/rod.json", study)
    nseg = study%structure%getElectrodeCount()
    pts(:, 1) = [400.0d0, 0.0d0, 0.0d0]
    pts(:, 2) = [0.0d0, -300.0d0, 0.0d0]
    call potentialsAt(study, pts, psi)

    call test_ok("shape: two points, every frequency", size(psi, 1) == 2 .and. &
                 size(psi, 2) == study%voltageResults%frequencyCount(), "shape")

    mu = study%structure%soil%mur * MU0
    do k = 1, study%voltageResults%frequencyCount(), 5
      omega = study%voltageResults%frequency(k)
      itSum = (0.0d0, 0.0d0)
      do b = 1, nseg
        itSum = itSum + study%longCurrentResults%get(b, k) + study%transCurrentResults%get(b, k)
      end do
      call test_ok("Σ I_t equals the injected 1 A", abs(itSum - 1.0d0) < 1.0d-6, "current conservation")
      call calcParamW(mesh, omega, MU0, study%structure%air%admittance(omega), mu, study%structure%soil%admittance(omega))
      ! Rod centre at (0, 0, -2): ideal-image point source, Γ(ω) = +1 in soil up to the Phase 10 correction
      w = mesh%cESoil * (1.0d0 + mesh%gammaSoil) * exp(-mesh%propSoil * 400.0d0) / 400.0d0
      expected = w
      call test_ok("surface point at 400 m", abs(psi(1, k) - expected) < 2.0d-3 * abs(expected), "far field")
    end do
  end subroutine testRodFarField

  subroutine testRodNearSurface()
    type(tStudy) :: study
    complex(8), allocatable :: psi(:,:)
    real(8) :: pts(3, 1), z1, z2
    integer :: iSeg, n1, n2
    complex(8) :: uMean

    call test_init("Rod: potential on the conductor surface matches the solved node voltages")
    call solveCase("../common/rod.json", study)
    iSeg = 3
    n1 = study%structure%electrodes(iSeg)%nodeIndices(1)
    n2 = study%structure%electrodes(iSeg)%nodeIndices(2)
    z1 = study%structure%nodes(n1)%p(3)
    z2 = study%structure%nodes(n2)%p(3)
    pts(:, 1) = [study%structure%electrodes(iSeg)%radius, 0.0d0, 0.5d0 * (z1 + z2)]
    call potentialsAt(study, pts, psi)
    uMean = 0.5d0 * (study%voltageResults%get(n1, 1) + study%voltageResults%get(n2, 1))
    call test_ok("10 Hz: ψ at the middle of a segment within 10 % of its mean node voltage", &
                 abs(psi(1, 1) - uMean) < 0.10d0 * abs(uMean), "surface potential")
  end subroutine testRodNearSurface

  subroutine testSafetyCase()
    type(tStudy) :: study
    complex(8), allocatable :: psi(:,:)
    real(8), allocatable :: pts(:,:)
    real(8) :: ang, worst, c(3), expectedMap, diff
    integer :: k, nf, i, j, iNode, ix, iy, nPts, nGrid
    complex(8) :: u
    integer :: unit, nLines, ios
    character(len=512) :: line

    call test_init("Safety case: reader, GPR/touch/step bookkeeping, symmetry, writers")
    call solveCase("../common/grid_safety.json", study)

    call test_ok("reader: 2 points", size(study%observation%points) == 2, "points")
    call test_ok("reader: 13 x 13 grid with a step map", study%observation%hasGrid .and. &
                 study%observation%grid%nx == 13 .and. study%observation%grid%step .and. &
                 study%observation%grid%stepDirections == 8, "grid")
    call test_ok("reader: one touch site with defaults", size(study%observation%touch) == 1 .and. &
                 study%observation%touch(1)%nPoints == 36 .and. study%observation%touch(1)%radius == 1.0d0, "touch")
    call test_ok("reader: one step pair", size(study%observation%steps) == 1, "steps")
    call validateStudyReferences(study)

    call computeObservations(study)
    nf = study%observationResults%potentials%frequencyCount()
    call test_ok("4 frequencies", nf == 4, "nf")
    call test_ok("sites: 2 points + 169 grid points", study%observationResults%potentials%entityCount() == 171, "sites")
    call test_ok("grid sites named <id>_<ix>_<iy>", &
                 trim(study%observationResults%potentials%entityId(3)) == "surface_1_1" .and. &
                 trim(study%observationResults%potentials%entityId(171)) == "surface_13_13", "ids")

    ! GPR is the node's solved voltage
    iNode = study%structure%findNodeIndex("Tower_top")
    do k = 1, nf
      u = study%voltageResults%get(iNode, k)
      call test_ok("GPR equals the node voltage", abs(study%observationResults%gpr%get(1, k) - u) < 1.0d-12 * abs(u), "gpr")
    end do

    ! Touch voltage: independent re-evaluation of the 36-point circle
    allocate(pts(3, 36))
    c = study%structure%nodes(iNode)%p
    do j = 1, 36
      ang = 2.0d0 * PI * real(j - 1, 8) / 36.0d0
      pts(:, j) = [c(1) + sin(ang), c(2) + cos(ang), 0.0d0]
    end do
    call potentialsAt(study, pts, psi)
    do k = 1, nf
      u = study%voltageResults%get(iNode, k)
      worst = 0.0d0
      do j = 1, 36
        worst = max(worst, abs(psi(j, k) - u))
      end do
      call test_ok("touch voltage = max |ψ − u| over the circle", &
                   abs(study%observationResults%touch%get(1, k) - worst) < 1.0d-9 * worst, "touch")
    end do
    call test_ok("touch voltage below the GPR at 50 Hz (the surface is below the structure potential)", &
                 study%observationResults%touch%get(1, 1) < abs(study%observationResults%gpr%get(1, 1)), "touch < gpr")

    ! Step pair and step map
    deallocate(pts)
    allocate(pts(3, 2))
    pts(:, 1) = [16.0d0, 8.0d0, 0.0d0]
    pts(:, 2) = [17.0d0, 8.0d0, 0.0d0]
    call potentialsAt(study, pts, psi)
    call test_ok("step pair Δψ = ψ(to) − ψ(from)", &
                 abs(study%observationResults%steps%get(1, 2) - (psi(2, 2) - psi(1, 2))) < 1.0d-9 * abs(psi(1, 2)), "step")

    nPts = size(study%observation%points)
    nGrid = study%observation%grid%nx * study%observation%grid%ny
    call test_ok("step map covers the grid", study%observationResults%stepMap%entityCount() == nGrid, "stepMap size")
    i = nPts + (7 - 1) * 13 + 7                 ! the grid point (x, y) = (8, 8)
    deallocate(pts)
    allocate(pts(3, 9))
    pts(:, 1) = study%observationResults%sitePos(:, i)
    do j = 1, 8
      ang = 2.0d0 * PI * real(j - 1, 8) / 8.0d0
      pts(:, 1 + j) = pts(:, 1) + [cos(ang), sin(ang), 0.0d0]
    end do
    call potentialsAt(study, pts, psi)
    expectedMap = 0.0d0
    do j = 1, 8
      expectedMap = max(expectedMap, abs(psi(1, 3) - psi(1 + j, 3)))
    end do
    call test_ok("step map = max over 8 azimuths of |ψ(P) − ψ(P + 1 m)|", &
                 abs(study%observationResults%stepMap%get(i - nPts, 3) - expectedMap) < 1.0d-9 * expectedMap, "stepMap")

    ! The grid and its source are symmetric about the diagonal x = y
    diff = 0.0d0
    do ix = 1, 13
      do iy = 1, 13
        i = nPts + (ix - 1) * 13 + iy
        j = nPts + (iy - 1) * 13 + ix
        diff = max(diff, abs(study%observationResults%potentials%get(i, 2) - study%observationResults%potentials%get(j, 2)) &
                         / abs(study%observationResults%potentials%get(i, 2)))
      end do
    end do
    call test_ok("ψ(x, y) = ψ(y, x) on the symmetric grid", diff < 1.0d-5, "symmetry")

    ! The potential decays away from the footing
    call test_ok("remote point is far below the GPR", &
                 abs(study%observationResults%potentials%get(2, 1)) < 0.25d0 * abs(study%observationResults%gpr%get(1, 1)), &
                 "remote")

    ! Writers
    call writeObservationCsv(study, "test_potentials.csv")
    open(newunit=unit, file="test_potentials.csv", status="old", action="read")
    nLines = 0
    do
      read(unit, '(A)', iostat=ios) line
      if (ios /= 0) exit
      nLines = nLines + 1
    end do
    close(unit)
    call test_ok("CSV: header + 4 x (171 potentials + 169 step-map + 2 touch/gpr + 1 step) rows", &
                 nLines == 1 + 4 * (171 + 169 + 2 + 1), "csv rows")
    call writeObservationJson(study, "test_potentials.json")
    open(newunit=unit, file="test_potentials.json", status="old", action="read")
    read(unit, '(A)') line
    close(unit)
    call test_ok("JSON written", trim(line) == "{", "json")
    open(newunit=unit, file="test_potentials.json", status="old")
    close(unit, status="delete")
    open(newunit=unit, file="test_potentials.csv", status="old")
    close(unit, status="delete")
  end subroutine testSafetyCase

end program test_potentials
