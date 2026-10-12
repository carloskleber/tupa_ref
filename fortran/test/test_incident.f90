program test_incident
  !! Total voltage of the lightning channel's incident field (theory.md §4.6.3,
  !! ADR 0028): u = φ + s ∫₀^h A_z^i dz at a node above the ground.
  !!
  !! The drop `incidentDrop` evaluates is compared with a brute-force double
  !! quadrature of the retarded potential (Simpson over the channel segment
  !! and over the vertical path, no closed form, midpoint distances nowhere),
  !! built from the currents of the same solve. Further checks: the scalar
  !! default is untouched, a node at the ground is unchanged, the reader sets
  !! the option.
  use mCtes, only: PI, MU0
  use mStudy
  use mStructure
  use mTransient, only: tTransientOptions
  use mVerbosity, only: setVerbosity, VERB_QUIET
  use mMesh, only: tMesh, calcParamW
  use mSignal, only: tSignalSlot
  use tupa, only: loadStudy
  use check
  implicit none

  call setVerbosity(VERB_QUIET)
  call testTotalVoltage()
  call testReader()

  call test_summary()

contains

  complex(8) function brute(study, b, node, omega, gam, img) result(s)
    !! s ∫ A_z dz at `node` from electrode `b` carrying unit current: Simpson
    !! in the channel coordinate and along the vertical path
    type(tStudy), intent(in) :: study
    integer, intent(in) :: b, node
    real(8), intent(in) :: omega
    complex(8), intent(in) :: gam, img
    integer, parameter :: nl = 400, nz = 100
    real(8) :: p1(3), p2(3), pn(3), x(3), xi(3), pt(3), zz, wl, wz, uz, len, r, ri
    integer :: il, iz

    p1 = study%structure%nodes(study%structure%electrodes(b)%nodeIndices(1))%p
    p2 = study%structure%nodes(study%structure%electrodes(b)%nodeIndices(2))%p
    pn = study%structure%nodes(node)%p
    len = norm2(p2 - p1)
    uz = (p2(3) - p1(3)) / len
    s = (0.0d0, 0.0d0)
    do iz = 0, nz
      zz = pn(3) * real(iz, 8) / real(nz, 8)
      wz = merge(1.0d0, merge(4.0d0, 2.0d0, mod(iz, 2) == 1), iz == 0 .or. iz == nz)
      pt = [pn(1), pn(2), zz]
      do il = 0, nl
        wl = merge(1.0d0, merge(4.0d0, 2.0d0, mod(il, 2) == 1), il == 0 .or. il == nl)
        x = p1 + (p2 - p1) * (real(il, 8) / real(nl, 8))
        xi = [x(1), x(2), -x(3)]
        r = norm2(pt - x)
        ri = norm2(pt - xi)
        s = s + wz * wl * uz * (exp(-gam * r) / r - img * exp(-gam * ri) / ri)
      end do
    end do
    s = s * (pn(3) / real(nz, 8) / 3.0d0) * (len / real(nl, 8) / 3.0d0)
    s = s * cmplx(0.0d0, omega, kind=8) * MU0 / (4.0d0 * PI)
  end function brute

  subroutine testTotalVoltage()
    type(tStudy) :: study
    complex(8), allocatable :: hS(:,:,:), hT(:,:,:), hI1(:,:,:), hI2(:,:,:), dummy1(:,:,:), d1(:,:,:)
    integer, allocatable :: nodeRows(:), elecRows(:)
    real(8), allocatable :: freqHz(:)
    real(8) :: omega, errMax, scaleMax
    complex(8) :: expected, cur, gam, img
    type(tMesh) :: mesh
    integer :: k, i, b, nseg, iNode, nObs
    character(len=8), parameter :: names(4) = ["Lw      ", "Le      ", "Fw      ", "Line_n2 "]
    real(8) :: muAir

    call test_init("Total voltage: drop against a brute-force retarded-potential quadrature")
    call loadStudy("test/fixture_incident.json", study)
    call study%structure%assembleStructure()
    nseg = study%structure%getElectrodeCount()
    nObs = 4
    allocate(nodeRows(nObs), elecRows(nseg))
    do i = 1, nObs
      nodeRows(i) = study%structure%findNodeIndex(trim(names(i)))
      call test_ok("node found", nodeRows(i) > 0, trim(names(i)))
    end do
    do b = 1, nseg
      elecRows(b) = b
    end do
    freqHz = [1.0d4, 3.0d5, 1.0d6]

    call study%runSweepUnits(freqHz, ["ch-base"], [""], [.false.], nodeRows, elecRows, hS, hI1, hI2)
    call study%runSweepUnits(freqHz, ["ch-base"], [""], [.false.], nodeRows, elecRows, hT, d1, dummy1, &
                             totalVoltage=.true.)
    call test_ok("the currents do not depend on the voltage kind", maxval(abs(hI1 - d1)) <= 1.0d-12 * maxval(abs(hI1)), "i1")

    muAir = study%structure%air%mur * MU0
    mesh%imageModel = study%imageModel
    errMax = 0.0d0
    scaleMax = 0.0d0
    do k = 1, size(freqHz)
      omega = 2.0d0 * PI * freqHz(k)
      call calcParamW(mesh, omega, muAir, study%structure%air%admittance(omega), &
                      study%structure%soil%mur * MU0, study%structure%soil%admittance(omega))
      gam = mesh%propAir
      img = mesh%gammaAir
      do i = 1, nObs
        expected = (0.0d0, 0.0d0)
        iNode = nodeRows(i)
        if (study%structure%nodes(iNode)%p(3) > 0.0d0) then
          do b = 1, nseg
            if (.not. study%structure%electrodes(b)%loaded) cycle
            cur = 0.5d0 * (hI1(b, k, 1) - hI2(b, k, 1))
            expected = expected + cur * brute(study, b, iNode, omega, gam, img)
          end do
        end if
        errMax = max(errMax, abs((hT(i, k, 1) - hS(i, k, 1)) - expected))
        scaleMax = max(scaleMax, abs(expected))
        if (study%structure%nodes(iNode)%p(3) <= 0.0d0) &
          call test_ok("a node at the ground keeps its scalar potential", hT(i, k, 1) == hS(i, k, 1), "ground")
      end do
    end do
    call test_ok("drop is not trivially zero", scaleMax > 1.0d-3, "scale")
    call test_ok("drop within 0.3 % of the brute-force value", errMax < 3.0d-3 * scaleMax, "drop")
    write(*, '(A,ES10.3,A,ES10.3)') "    max |error| = ", errMax, " of ", scaleMax
  end subroutine testTotalVoltage

  subroutine testReader()
    type(tStudy) :: study
    type(tTransientOptions) :: opts
    type(tSignalSlot), allocatable :: slots(:)

    call test_init("Reader: signal.totalVoltage")
    call loadStudy("test/fixture_incident.json", study, signalSources=slots, signalOptions=opts)
    call test_ok("totalVoltage is read", opts%totalVoltage, "true")
    deallocate(slots)
    call loadStudy("../common/induced_nucci1993.json", study, signalSources=slots, signalOptions=opts)
    call test_ok("default is the scalar potential", .not. opts%totalVoltage, "false")
  end subroutine testReader

end program test_incident
