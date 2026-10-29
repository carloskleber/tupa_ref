program test_lumped_segment
  !! Lumped series R-L-C embedded in a wire segment (docs/marx_generator.md):
  !! a 0.25 m air wire feeds a ground rod, so at low frequency the source
  !! current crosses the wire untouched and the driving-point impedance is
  !! the rod's plus the embedded Z_lump = R + sL + 1/(sC), both on the
  !! harmonic axis (s = jω) and at a Laplace frequency (s = c + jω).
  use mCtes, only: dp, PI
  use mStudy
  use mNode
  use mMaterial
  use mElementLine
  use mElectrode, only: lumpedImpedance, tElectrode
  use mVerbosity, only: setVerbosity, VERB_QUIET
  use check
  implicit none

  type(tElectrode) :: seg
  complex(dp) :: z0, z1, s
  real(dp) :: f
  character(len=256) :: srcIds(1)

  call setVerbosity(VERB_QUIET)

  call test_init("lumpedImpedance(s)")
  seg%lumpResistance = 5.0_dp; seg%lumpInductance = 1.0d-6; seg%lumpCapacitance = 1.0d-9
  s = cmplx(1.0d4, 2.0_dp * PI * 1.0d5, kind=dp)
  call test_ok("R + sL + 1/(sC)", abs(lumpedImpedance(seg, s) - (5.0_dp + s * 1.0d-6 + 1.0_dp / (s * 1.0d-9))) < 1.0d-9, "z")
  seg%lumpResistance = 0.0_dp; seg%lumpInductance = 0.0_dp; seg%lumpCapacitance = 0.0_dp
  call test_ok("zero without a lumped element", abs(lumpedImpedance(seg, s)) == 0.0_dp, "none")

  call test_init("Embedded R in a wire segment adds R to the driving-point impedance")
  f = 10.0_dp
  z0 = zin(0.0_dp, 0.0_dp, 0.0_dp, f, 0.0_dp)
  z1 = zin(5.0_dp, 0.0_dp, 0.0_dp, f, 0.0_dp)
  call test_ok("Zin(R=5) - Zin(R=0) = 5 ohm within 1% at 10 Hz", abs((z1 - z0) - 5.0_dp) < 0.05_dp, "R")

  call test_init("Embedded C: Zin is -j/(wC) within 1% at 10 kHz")
  f = 1.0d4
  z1 = zin(0.0_dp, 0.0_dp, 1.0d-9, f, 0.0_dp)
  z0 = zin(0.0_dp, 0.0_dp, 0.0_dp, f, 0.0_dp)
  s = cmplx(0.0_dp, 2.0_dp * PI * f, kind=dp)
  call test_ok("harmonic", abs((z1 - z0) - 1.0_dp / (s * 1.0d-9)) < 1.0d-2 * abs(1.0_dp / (s * 1.0d-9)), "C")

  call test_init("Embedded L and C at a Laplace frequency")
  z1 = zin(0.0_dp, 1.0d-6, 1.0d-9, f, 2.0d3)
  z0 = zin(0.0_dp, 0.0_dp, 0.0_dp, f, 2.0d3)
  s = cmplx(2.0d3, 2.0_dp * PI * f, kind=dp)
  call test_ok("Z(s) = sL + 1/(sC) within 1%", &
               abs((z1 - z0) - (s * 1.0d-6 + 1.0_dp / (s * 1.0d-9))) < 1.0d-2 * abs(1.0_dp / (s * 1.0d-9)), "Laplace")

  call test_summary()

contains

  complex(dp) function zin(r, l, c, freqHz, damping)
    real(dp), intent(in) :: r, l, c, freqHz, damping
    type(tStudy) :: study
    class(tMaterial), allocatable :: mat
    class(tElement), allocatable :: elem
    complex(dp), allocatable :: zz(:)
    complex(dp) :: i1(1)

    call study%structure%addNode(newNode("Src",    [0.0d0, 0.0d0, 0.30d0]))
    call study%structure%addNode(newNode("Node_1", [0.0d0, 0.0d0, 0.05d0]))
    call study%structure%addNode(newNode("Node_2", [0.0d0, 0.0d0, -3.0d0]))
    mat = newMaterialLinear("copper", 1.0d0, 1.0d0, 5.96d7)
    call study%structure%addMaterial(mat)
    study%structure%soil = newMaterialLinear("soil", 10.0d0, 1.0d0, 0.01d0)
    elem = newElementLine("Wire", "Src", "Node_1", 0.005d0, 2, "copper")
    select type (elem)
    type is (tLine)
      elem%lumpSegment = 1
      elem%lumpResistance = r; elem%lumpInductance = l; elem%lumpCapacitance = c
    end select
    call study%structure%addElement(elem)
    elem = newElementLine("Rod", "Node_1", "Node_2", 0.01d0, 6, "copper")
    call study%structure%addElement(elem)
    srcIds(1) = "Src"
    i1(1) = cmplx(1.0d0, 0.0d0, kind=dp)
    if (damping > 0.0_dp) then
      call study%runSweep([freqHz], srcIds, i1, damping=damping)
    else
      call study%runSweep([freqHz], srcIds, i1)
    end if
    zz = study%inputImpedance("Src")
    zin = zz(1)
  end function zin
end program test_lumped_segment
