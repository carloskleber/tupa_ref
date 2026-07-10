program test_material
  !! Pins `tPortelaSoil` (ROADMAP.md Phase 4, ADR 0007): the Lima-Portela
  !! dispersive soil admittance formula, its DC-limit convergence to a
  !! purely resistive `tLinear` medium, and passivity across a sweep.
  !! Also repeats the Phase 2 buried-conductor validation
  !! (test_solve.f90) with a dispersive soil in place of the linear one.
  use mCtes
  use mMaterial
  use mStudy
  use mNode
  use mElementLine
  use check
  implicit none

  type(tPortelaSoil) :: soil
  type(tLinear) :: dcEquivalent
  real(8), parameter :: sigma0 = 0.01d0, alpha0 = 0.7d0, kr = 0.02d0
  real(8) :: omega0, omegaLow, cotTerm
  complex(8) :: wAtOmega0, expectedAtOmega0

  soil = newMaterialPortela("soil", 1.0d0, sigma0, alpha0, kr)

  ! ----------------------------------------------------------------
  ! Admittance pinned at omega = omega0 = 2*pi*1e6, where (omega/omega0)^alpha0 = 1
  ! (theory.md §7, ADR 0007): W = sigma0 + kr*(cot(pi*alpha0/2) + j)
  ! ----------------------------------------------------------------
  call test_init("tPortelaSoil%admittance at omega0 (theory.md §7)")

  omega0 = 2.0d0 * PI * 1.0d6
  cotTerm = 1.0d0 / tan(0.5d0 * PI * alpha0)
  expectedAtOmega0 = cmplx(sigma0, 0.0d0, kind=8) + kr * cmplx(cotTerm, 1.0d0, kind=8)
  wAtOmega0 = soil%admittance(omega0)

  call test_ok("W(omega0) == sigma0 + kr*(cot(pi*alpha0/2) + j)", &
               abs(wAtOmega0 - expectedAtOmega0) < 1.0d-12 * abs(expectedAtOmega0), &
               "Lima-Portela admittance formula does not match at the reference frequency")

  call soil%calcPropagationConstant(omega0)
  call test_ok("gamma(omega0)**2 == j*omega0*mu*W(omega0)", &
               abs(soil%propagationConstant**2 - cmplx(0.0d0, omega0, kind=8) * MU0 * wAtOmega0) &
                 < 1.0d-12 * abs(cmplx(0.0d0, omega0, kind=8) * MU0 * wAtOmega0), &
               "propagation constant does not follow gamma = sqrt(j*omega*mu*W)")

  ! ----------------------------------------------------------------
  ! DC limit: as omega -> 0, tPortelaSoil must converge to a purely
  ! resistive tLinear(epsilonr=0, sigma=sigma0) medium (ADR 0007's
  ! required regression: "must reduce to tLinear as omega -> 0").
  ! ----------------------------------------------------------------
  call test_init("tPortelaSoil DC-limit convergence to tLinear (ADR 0007)")

  dcEquivalent = newMaterialLinear("soil-dc", 0.0d0, 1.0d0, sigma0)
  omegaLow = 2.0d0 * PI * 1.0d-3 ! far below ~1 Hz: dispersive term is negligible

  call soil%calcPropagationConstant(omegaLow)
  call dcEquivalent%calcPropagationConstant(omegaLow)

  call test_ok("gamma(omega->0) matches the sigma0-only resistive medium", &
               abs(soil%propagationConstant - dcEquivalent%propagationConstant) &
                 < 1.0d-6 * max(1.0d-30, abs(dcEquivalent%propagationConstant)), &
               "tPortelaSoil does not converge to tLinear(epsilonr=0, sigma=sigma0) as omega -> 0")

  ! ----------------------------------------------------------------
  ! Passivity: Re(gamma) >= 0 (decaying propagation factor) across a
  ! decade sweep, same convention already pinned for tLinear (test_mesh.f90).
  ! ----------------------------------------------------------------
  call test_init("tPortelaSoil passivity across a frequency sweep")

  block
    real(8) :: freqHz(5)
    integer :: k
    freqHz = [10.0d0, 1.0d2, 1.0d4, 1.0d6, 1.0d8]
    do k = 1, size(freqHz)
      call soil%calcPropagationConstant(2.0d0 * PI * freqHz(k))
      call test_ok("Re(gamma) >= 0 at f=" // freqStr(freqHz(k)), &
                   real(soil%propagationConstant) >= 0.0d0, &
                   "dispersive soil propagation constant must not amplify")
    end do
  end block

  ! ----------------------------------------------------------------
  ! Repeat the Phase 2 buried-conductor validation (test_solve.f90) with
  ! dispersive soil (ROADMAP Phase 4 item 2). alpha0/kr above are
  ! illustrative (no tabulated Lima-Portela parameter set is available yet,
  ! ROADMAP §9 "Validation data") — chosen small enough that the DC
  ! plateau and Sunde/Dwight comparison below still hold, since no
  ! tabulated Portela dispersive curve exists to match against
  ! (theory.md §9.2).
  ! ----------------------------------------------------------------
  call test_init("tStudy%run with dispersive soil: passivity + DC-limit (ROADMAP Phase 4)")

  block
    type(tStudy) :: study
    class(tMaterial), allocatable :: mat
    class(tElement), allocatable :: elem
    real(8), parameter :: length = 10.0d0, r0 = 0.007d0, depth = 0.5d0
    real(8) :: freqHz(4), omega, zinMag, rDc
    complex(8) :: zin
    integer :: k, idx

    freqHz = [10.0d0, 1.0d2, 1.0d4, 1.0d6]

    call study%structure%addNode(newNode("Node_1", [0.0d0, 0.0d0, -depth]))
    call study%structure%addNode(newNode("Node_2", [length, 0.0d0, -depth]))

    mat = newMaterialLinear("copper", 1.0d0, 1.0d0, 5.96d7)
    call study%structure%addMaterial(mat)
    study%structure%soil = newMaterialPortela("soil", 1.0d0, sigma0, alpha0, kr)

    elem = newElementLine("Line_1", "Node_1", "Node_2", r0, 10, "copper")
    call study%structure%addElement(elem)

    idx = study%structure%findNodeIndex("Node_1")
    rDc = 1.0d0 / (2.0d0 * PI * sigma0 * length) * &
          (log(2.0d0 * length / r0) + log(2.0d0 * length / (2.0d0 * depth)) - 2.0d0)

    do k = 1, size(freqHz)
      omega = 2.0d0 * PI * freqHz(k)
      call study%run(omega, ["Node_1"], [cmplx(1.0d0, 0.0d0, kind=8)])

      zin = study%mesh%voltage(idx) / cmplx(1.0d0, 0.0d0, kind=8)
      zinMag = abs(zin)

      call test_ok("Re(Zin) >= 0 (passivity) at f=" // freqStr(freqHz(k)), &
                   real(zin) >= -1.0d-9 * max(1.0d0, zinMag), &
                   "input impedance must not have negative real part")

      if (k == 1) then
        call test_ok("|Zin(10 Hz)| within 20% of Sunde/Dwight R (sigma0-limited)", &
                     abs(zinMag - rDc) < 0.2d0 * rDc, &
                     "low-frequency input impedance should approximate the DC grounding resistance")
      end if
    end do
  end block

  call test_summary()

contains

  function freqStr(f) result(s)
    real(8), intent(in) :: f
    character(len=32) :: s
    write(s, '(F0.1," Hz")') f
  end function freqStr

end program test_material
