program example3
  !! ROADMAP.md Phase 2 milestone: the Portela-1997-parameter buried
  !! conductor (10 m, r0 = 7 mm, 0.5 m depth, soil sigma = 0.01 S/m,
  !! epsilon_r = 10), solved across a log-spaced frequency sweep and
  !! printed as an impedance-vs-frequency table.
  !!
  !! Unlike example1/example2 (smoke cases with epsilon_r = 1 soil,
  !! common/README.md), this is the actual validation-parameter case.
  !! Matching against the published Portela 1997 curve is not attempted
  !! here: no tabulated data exists for it (theory.md §9.2) — see
  !! fortran/test/test_solve.f90 for the closed-form DC-limit check
  !! (theory.md §9.1), and ROADMAP.md §7 P3 for the planned TAGS
  !! cross-validation that would supply an executable oracle for the
  !! full curve.
  use mCtes
  use mStudy
  use mNode
  use mMaterial
  use mElementLine
  implicit none

  type(tStudy) :: study
  class(tMaterial), allocatable :: mat
  class(tElement), allocatable :: elem
  real(8), parameter :: length = 10.0d0, r0 = 0.007d0, depth = 0.5d0
  real(8), parameter :: sigmaSoil = 0.01d0, epsrSoil = 10.0d0
  real(8) :: freqHz(9), omega, zinMag, zinPhaseDeg
  complex(8) :: zin
  integer :: k, idx

  ! Log-spaced, 2 points/decade, 100 Hz to 1 MHz (theory.md §9.1's range)
  freqHz = [(1.0d2 * 10.0d0**(0.5d0 * real(k-1, kind=8)), k = 1, 9)]

  print *, color_green, "Starting Example 3: Portela 1997 buried conductor validation case", color_reset
  study%title = "Example 3 - Portela 1997 buried conductor"

  call study%structure%addNode(newNode("Node_1", [0.0d0, 0.0d0, -depth]))
  call study%structure%addNode(newNode("Node_2", [length, 0.0d0, -depth]))

  mat = newMaterialLinear("copper", 1.0d0, 1.0d0, 5.96d7)
  call study%structure%addMaterial(mat)
  study%structure%soil = newMaterialLinear("soil", epsrSoil, 1.0d0, sigmaSoil)

  elem = newElementLine("Line_1", "Node_1", "Node_2", r0, 10, "copper")
  call study%structure%addElement(elem)

  print *, ""
  print '(A14,A16,A16,A16)', "f (Hz)", "|Zin| (ohm)", "phase (deg)", "Re(Zin) (ohm)"
  do k = 1, size(freqHz)
    omega = 2.0d0 * PI * freqHz(k)
    call study%run(omega, ["Node_1"], [cmplx(1.0d0, 0.0d0, kind=8)])

    idx = study%structure%findNodeIndex("Node_1")
    zin = study%mesh%voltage(idx) / cmplx(1.0d0, 0.0d0, kind=8)
    zinMag = abs(zin)
    zinPhaseDeg = atan2(aimag(zin), real(zin)) * 180.0d0 / PI

    print '(F14.1,F16.4,F16.4,F16.4)', freqHz(k), zinMag, zinPhaseDeg, real(zin)
  end do

  print *, color_green, "Example 3 completed.", color_reset
end program example3
