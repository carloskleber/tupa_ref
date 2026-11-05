program test_coating
  !! Coaxial coating layer on a buried rod (theory.md §4.3, ROADMAP Phase 13
  !! item 3): the radial layer admittance y_c = 2π(σ_c + jωε_c)/ln(r_c/r0)
  !! sits in series with the leakage path.
  !!   1. a layer of the soil's own medium reproduces the bare rod;
  !!   2. a conductive layer at low frequency adds the shell resistance
  !!      ln(r_c/r0)/(2πσ_c L) to the bare rod of radius r_c (two-shell DC form);
  !!   3. an insulating layer makes the rod a capacitor of C = 2πε_c L/ln(r_c/r0).
  use mCtes, only: dp, PI, EPSILON0
  use mStudy
  use mNode
  use mMaterial
  use mElementLine
  use mVerbosity, only: setVerbosity, VERB_QUIET
  use check
  implicit none

  real(dp), parameter :: len = 3.0_dp, r0 = 0.01_dp, rc = 0.03_dp, sigmaSoil = 0.01_dp
  complex(dp) :: zBare, zBareRc, zSame, zConc, zIns, expected
  real(dp) :: f

  call setVerbosity(VERB_QUIET)

  f = 10.0_dp
  zBare   = zin(r0, 0.0_dp, 0.0_dp, 0.0_dp, f)
  zBareRc = zin(rc, 0.0_dp, 0.0_dp, 0.0_dp, f)

  call test_init("Layer of the soil's own medium reproduces the bare rod")
  zSame = zin(r0, rc, sigmaSoil, 10.0_dp, f)
  call test_ok("|dZ|/|Z| < 3%", abs(zSame - zBare) < 0.03_dp * abs(zBare), "identity")

  call test_init("Conductive coating: shell resistance in series with the bare rod of radius r_c")
  zConc = zin(r0, rc, 0.05_dp, 10.0_dp, f)
  expected = zBareRc + log(rc / r0) / (2.0_dp * PI * 0.05_dp * len)
  call test_ok("two-shell DC form within 3%", abs(zConc - expected) < 0.03_dp * abs(expected), "two-shell")

  call test_init("Insulating coating: capacitive driving-point impedance")
  f = 1.0d3
  zIns = zin(r0, rc, 1.0d-12, 3.0_dp, f)
  expected = 1.0_dp / cmplx(0.0_dp, 2.0_dp * PI * f * 2.0_dp * PI * 3.0_dp * EPSILON0 * len / log(rc / r0), kind=dp)
  call test_ok("|Z| matches 1/(wC) within 5%", abs(abs(zIns) - abs(expected)) < 0.05_dp * abs(expected), "|Z|")
  call test_ok("phase near -90 degrees", aimag(zIns) < 0.0_dp .and. abs(aimag(zIns)) > 20.0_dp * abs(real(zIns)), "phase")

  call test_summary()

contains

  complex(dp) function zin(radius, coatR, coatSigma, coatEpsr, freqHz)
    !! Driving-point impedance of a vertical rod from just below the surface.
    real(dp), intent(in) :: radius, coatR, coatSigma, coatEpsr, freqHz
    type(tStudy) :: study
    class(tMaterial), allocatable :: mat
    class(tElement), allocatable :: elem
    complex(dp), allocatable :: zz(:)
    complex(dp) :: i1(1)
    character(len=256) :: srcIds(1)

    call study%structure%addNode(newNode("Top", [0.0d0, 0.0d0, -0.5d0]))
    call study%structure%addNode(newNode("Bot", [0.0d0, 0.0d0, -0.5d0 - len]))
    mat = newMaterialLinear("steel", 1.0d0, 1.0d0, 5.0d6)
    call study%structure%addMaterial(mat)
    mat = newMaterialLinear("coat", coatEpsr, 1.0d0, coatSigma)
    call study%structure%addMaterial(mat)
    study%structure%soil = newMaterialLinear("soil", 10.0d0, 1.0d0, sigmaSoil)
    elem = newElementLine("Rod", "Top", "Bot", radius, 15, "steel")
    select type (elem)
    type is (tLine)
      if (coatR > 0.0_dp) then
        elem%coatingRadius = coatR
        elem%idCoatingMaterial = "coat"
      end if
    end select
    call study%structure%addElement(elem)
    srcIds(1) = "Top"
    i1(1) = cmplx(1.0d0, 0.0d0, kind=dp)
    call study%runSweep([freqHz], srcIds, i1)
    zz = study%inputImpedance("Top")
    zin = zz(1)
  end function zin
end program test_coating
