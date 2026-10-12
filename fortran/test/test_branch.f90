program test_branch
  !! Lumped series R-L-C branch (ROADMAP Phase 12 item 3 as pulled forward,
  !! ADR 0028, theory.md §6): the DC pin of §6 (a lumped R in series with a
  !! grounded electrode reproduces the series resistance), a shunt to remote
  !! earth against the closed-form parallel combination, the full R + jwL +
  !! 1/(jwC) impedance, the same at a complex frequency, and a pure
  !! capacitor with no 1/0 at low frequency.
  use mCtes, only: PI
  use mStudy
  use mStructure
  use mBranch
  use mVerbosity, only: setVerbosity, VERB_QUIET
  use tupa, only: loadStudy
  use check
  implicit none

  call setVerbosity(VERB_QUIET)
  call testAdmittance()
  call testSeries()
  call testShunt()
  call testComplexFrequency()

  call test_summary()

contains

  subroutine rodImpedance(freqHz, zrod)
    !! Input impedance of the bare rod (no branch) at one frequency.
    real(8), intent(in) :: freqHz
    complex(8), intent(out) :: zrod
    type(tStudy) :: bare
    complex(8), allocatable :: z(:)
    character(len=256), allocatable :: srcIds(:)
    complex(8), allocatable :: srcI(:)
    logical, allocatable :: srcV(:)

    call loadStudy("../common/rod.json", bare, sourceNodeIds=srcIds, sourceCurrents=srcI, sourceIsVoltage=srcV)
    call bare%runSweep([freqHz], srcIds, srcI)
    z = bare%inputImpedance("Node_1")
    zrod = z(1)
  end subroutine rodImpedance

  subroutine testAdmittance()
    type(tBranch) :: br
    complex(8) :: s, z

    call test_init("Branch admittance Y(s) = 1/(R + sL + 1/(sC))")
    s = cmplx(0.0d0, 2.0d0 * PI * 1.0d6, kind=8)
    br%resistance = 5.0d0; br%inductance = 1.0d-6; br%capacitance = 1.0d-9
    z = br%resistance + s * br%inductance + 1.0d0 / (s * br%capacitance)
    call test_ok("series RLC at 1 MHz", abs(br%admittance(s) - 1.0d0 / z) < 1.0d-12 * abs(1.0d0 / z), "RLC")
    br%capacitance = 0.0d0
    z = br%resistance + s * br%inductance
    call test_ok("without a capacitor: RL", abs(br%admittance(s) - 1.0d0 / z) < 1.0d-12 * abs(1.0d0 / z), "RL")
    br%resistance = 0.0d0; br%inductance = 0.0d0; br%capacitance = 1.0d-9
    call test_ok("pure capacitor: Y = sC, finite and zero at s -> 0", &
                 abs(br%admittance(s) - s * br%capacitance) < 1.0d-12 * abs(s * br%capacitance) .and. &
                 abs(br%admittance(cmplx(0.0d0, 1.0d-9, kind=8))) < 1.0d-17, "C")
  end subroutine testAdmittance

  subroutine testSeries()
    type(tStudy) :: study
    character(len=256), allocatable :: srcIds(:)
    complex(8), allocatable :: srcI(:)
    logical, allocatable :: srcV(:)
    real(8), allocatable :: freq(:)
    complex(8) :: zrod, zbr, zsrc, jw
    integer :: iSrc, iHead, k
    real(8) :: devHead, devBranch

    call test_init("Series branch between an isolated source node and a grounded rod")
    call loadStudy("test/fixture_branch_series.json", study, sourceNodeIds=srcIds, sourceCurrents=srcI, &
                   sourceIsVoltage=srcV, freqHz=freq)
    call study%runSweep(freq, srcIds, srcI)
    iSrc  = study%structure%findNodeIndex("Src")
    iHead = study%structure%findNodeIndex("Node_1")
    call test_ok("one branch registered", study%structure%branchCount == 1, "branchCount")
    call test_ok("a branch adds no node and no segment (3 case nodes + 5 rod nodes, 6 segments)", &
                 study%structure%getNodeCount() == 8 .and. study%structure%getElectrodeCount() == 6, "topology")

    devHead = 0.0d0
    devBranch = 0.0d0
    do k = 1, size(freq)
      call rodImpedance(freq(k), zrod)
      jw = cmplx(0.0d0, 2.0d0 * PI * freq(k), kind=8)
      zbr = 5.0d0 + jw * 1.0d-6 + 1.0d0 / (jw * 1.0d-9)
      zsrc = study%voltageResults%get(iSrc, k)
      devHead   = max(devHead, abs(study%voltageResults%get(iHead, k) - zrod) / abs(zrod))
      devBranch = max(devBranch, abs((zsrc - study%voltageResults%get(iHead, k)) - zbr) / abs(zbr))
    end do
    call test_ok("the rod head sees the source current unchanged (1 A through the branch)", devHead < 1.0d-9, "head voltage")
    call test_ok("u(src) - u(head) = Z_branch * 1 A over 10 Hz - 1 MHz", devBranch < 1.0d-9, "branch drop")
  end subroutine testSeries

  subroutine testShunt()
    type(tStudy) :: study
    character(len=256), allocatable :: srcIds(:)
    complex(8), allocatable :: srcI(:), zin(:)
    logical, allocatable :: srcV(:)
    real(8), allocatable :: freq(:)
    complex(8) :: zrod, zpar
    integer :: k
    real(8) :: dev

    call test_init("Shunt resistor to remote earth in parallel with a grounded rod")
    call loadStudy("test/fixture_branch_shunt.json", study, sourceNodeIds=srcIds, sourceCurrents=srcI, &
                   sourceIsVoltage=srcV, freqHz=freq)
    call study%runSweep(freq, srcIds, srcI)
    zin = study%inputImpedance("Node_1")
    dev = 0.0d0
    do k = 1, size(freq)
      call rodImpedance(freq(k), zrod)
      zpar = 50.0d0 * zrod / (50.0d0 + zrod)
      dev = max(dev, abs(zin(k) - zpar) / abs(zpar))
    end do
    call test_ok("Zin = 50 ohm || Zrod to 1e-9 (the DC pin of theory.md 6)", dev < 1.0d-9, "parallel combination")
    call rodImpedance(freq(1), zrod)
    call test_ok("at 10 Hz the rod is resistive and the parallel value sits below both", &
                 real(zin(1)) < 50.0d0 .and. real(zin(1)) < real(zrod), "ordering")
  end subroutine testShunt

  subroutine testComplexFrequency()
    !! The Laplace path (damping c > 0) stamps Y(c + jw): the branch drop is Z(s) * I.
    type(tStudy) :: study
    character(len=256), allocatable :: srcIds(:)
    complex(8), allocatable :: srcI(:)
    logical, allocatable :: srcV(:)
    real(8), allocatable :: freq(:)
    complex(8) :: s, zbr, drop
    integer :: iSrc, iHead
    real(8) :: omega, damping

    call test_init("Branch at a complex frequency s = c + jw (Numerical Laplace Transform path)")
    call loadStudy("test/fixture_branch_series.json", study, sourceNodeIds=srcIds, sourceCurrents=srcI, &
                   sourceIsVoltage=srcV, freqHz=freq)
    omega = 2.0d0 * PI * 1.0d5
    damping = 2.0d5
    call study%run(omega, srcIds, srcI, damping=damping)
    iSrc  = study%structure%findNodeIndex("Src")
    iHead = study%structure%findNodeIndex("Node_1")
    s = cmplx(damping, omega, kind=8)
    zbr = 5.0d0 + s * 1.0d-6 + 1.0d0 / (s * 1.0d-9)
    drop = study%mesh%voltage(iSrc) - study%mesh%voltage(iHead)
    call test_ok("u(src) - u(head) = Z(s) * 1 A", abs(drop - zbr) < 1.0d-9 * abs(zbr), "Laplace branch drop")
  end subroutine testComplexFrequency

end program test_branch
