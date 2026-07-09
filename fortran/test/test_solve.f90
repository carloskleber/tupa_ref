program test_solve
  !! End-to-end integration test for the wired `tStudy%run` (ROADMAP.md Phase 2).
  !!
  !! Builds the Portela-1997-parameter buried conductor (10 m, r0 = 7 mm,
  !! 0.5 m depth, soil sigma = 0.01 S/m, epsilon_r = 10) directly (sources and
  !! frequency are not yet in the JSON schema, ROADMAP Phase 5), injects 1 A
  !! at one end, and sweeps frequency from 10 Hz to 1 MHz.
  !!
  !! Checks the closed-form DC limit only (Sunde/Dwight, theory.md §9.1): no
  !! tabulated Portela 1997 curve data exists (theory.md §9.2), so matching
  !! the published harmonic curve is deferred to ROADMAP §7 P3 (TAGS
  !! cross-validation), a separate, larger task.
  use mCtes
  use mStudy
  use mNode
  use mMaterial
  use mElementLine
  use check
  implicit none

  type(tStudy) :: study
  class(tMaterial), allocatable :: mat
  class(tElement), allocatable :: elem
  real(8), parameter :: length = 10.0d0, r0 = 0.007d0, depth = 0.5d0
  real(8), parameter :: sigmaSoil = 0.01d0, epsrSoil = 10.0d0
  real(8) :: freqHz(6), omega, zinMag, zinAt10Hz, zinAt100Hz
  complex(8) :: zin
  integer :: k, idx

  freqHz = [10.0d0, 1.0d2, 1.0d3, 1.0d4, 1.0d5, 1.0d6]

  study%title = "Phase 2 validation - buried conductor (Portela 1997 parameters)"
  call study%structure%addNode(newNode("Node_1", [0.0d0, 0.0d0, -depth]))
  call study%structure%addNode(newNode("Node_2", [length, 0.0d0, -depth]))

  mat = newMaterialLinear("copper", 1.0d0, 1.0d0, 5.96d7)
  call study%structure%addMaterial(mat)
  study%structure%soil = newMaterialLinear("soil", epsrSoil, 1.0d0, sigmaSoil)

  elem = newElementLine("Line_1", "Node_1", "Node_2", r0, 10, "copper")
  call study%structure%addElement(elem)

  ! ----------------------------------------------------------------
  ! Frequency sweep: geometry factors computed once (first run() call),
  ! only the per-frequency fill+solve repeats (theory.md §4.1).
  ! ----------------------------------------------------------------
  call test_init("tStudy%run: frequency sweep solves successfully")

  do k = 1, size(freqHz)
    omega = 2.0d0 * PI * freqHz(k)
    call study%run(omega, ["Node_1"], [cmplx(1.0d0, 0.0d0, kind=8)])

    idx = study%structure%findNodeIndex("Node_1")
    zin = study%mesh%voltage(idx) / cmplx(1.0d0, 0.0d0, kind=8)
    zinMag = abs(zin)

    call test_ok("Re(Zin) >= 0 (passivity, theory.md §9 item 4) at f=" // freqStr(freqHz(k)), &
                 real(zin) >= -1.0d-9 * max(1.0d0, zinMag), &
                 "input impedance must not have negative real part")

    if (k == 1) zinAt10Hz  = zinMag
    if (k == 2) zinAt100Hz = zinMag
  end do

  call test_ok("|Zin(10 Hz)| ~= |Zin(100 Hz)| (DC plateau)", &
               abs(zinAt10Hz - zinAt100Hz) < 0.05d0 * zinAt100Hz, &
               "input impedance should be flat (resistive) at these low frequencies, not still varying")

  ! ----------------------------------------------------------------
  ! DC limit vs Sunde/Dwight resistance formula (theory.md §9.1):
  ! R = 1/(2*pi*sigma*l) * [ln(2l/r0) + ln(2l/(2h)) - 2]
  ! Compared at 10 Hz (not literally DC) with a generous tolerance, since
  ! the formula itself drops higher-order terms ("+ ...", theory.md §9.1).
  ! ----------------------------------------------------------------
  call test_init("Low-frequency Zin vs Sunde/Dwight DC resistance (theory.md §9.1)")

  block
    real(8) :: rDc
    rDc = 1.0d0 / (2.0d0 * PI * sigmaSoil * length) * &
          (log(2.0d0 * length / r0) + log(2.0d0 * length / (2.0d0 * depth)) - 2.0d0)

    call test_ok("|Zin(10 Hz)| within 15% of Sunde/Dwight R", &
                 abs(zinAt10Hz - rDc) < 0.15d0 * rDc, &
                 "10 Hz input impedance magnitude should approximate the DC grounding resistance")
  end block

  call test_summary()

contains

  function freqStr(f) result(s)
    real(8), intent(in) :: f
    character(len=32) :: s
    write(s, '(F0.1," Hz")') f
  end function freqStr

end program test_solve
