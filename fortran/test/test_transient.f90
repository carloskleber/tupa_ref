program test_transient
  !! Integration test for mTransient (ROADMAP.md Phase 6 items 2-3): the
  !! excitation-spectrum -> per-frequency solve -> inverse-FFT pipeline,
  !! exercised on the same Portela-1997-parameter buried conductor already
  !! validated in test_solve.f90/test_sweep.f90.
  !!
  !! No tabulated time-domain reference waveform exists for this conductor
  !! (theory.md §9.2, same data gap as the harmonic curve), so this checks
  !! internal consistency instead: the transient GPR (ground potential rise)
  !! at the injection node must track the injected current scaled by the
  !! already-validated frequency-domain input impedance at low frequency,
  !! since a slow (250/2500 us) double-exponential surge keeps nearly all
  !! of its spectral energy inside the resistive low-frequency plateau
  !! test_solve.f90 already established.
  use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
  use mCtes, only: dp, PI
  use mStudy
  use mSignal
  use mTransient
  use mNode
  use mMaterial
  use mElementLine
  use check
  implicit none

  type(tStudy) :: study
  class(tMaterial), allocatable :: mat
  class(tElement), allocatable :: elem
  real(dp), parameter :: length = 10.0d0, r0 = 0.007d0, depth = 0.5d0
  real(dp), parameter :: sigmaSoil = 0.01d0, epsrSoil = 10.0d0
  type(tDoubleExpSignal) :: surge
  real(dp), allocatable :: t(:), injectedCurrent(:), response(:)
  complex(dp), allocatable :: zin(:)
  real(dp) :: imax, nyquistHz, freqZeroHz, ratioAtPeak, zinLowFreqMag
  integer(4), parameter :: nSamples = 1024
  integer(4) :: iPeak

  study%title = "Phase 6 transient test - buried conductor (Portela 1997 parameters)"
  call study%structure%addNode(newNode("Node_1", [0.0d0, 0.0d0, -depth]))
  call study%structure%addNode(newNode("Node_2", [length, 0.0d0, -depth]))

  mat = newMaterialLinear("copper", 1.0d0, 1.0d0, 5.96d7)
  call study%structure%addMaterial(mat)
  study%structure%soil = newMaterialLinear("soil", epsrSoil, 1.0d0, sigmaSoil)

  elem = newElementLine("Line_1", "Node_1", "Node_2", r0, 10, "copper")
  call study%structure%addElement(elem)

  imax = 1.0d3
  surge = newDoubleExpSignal(imax, "f250_2500")
  nyquistHz = 1.0d4
  freqZeroHz = 1.0d-6

  ! ----------------------------------------------------------------
  ! Pipeline runs end to end and produces finite, well-shaped output
  ! ----------------------------------------------------------------
  call test_init("transientResponse: runs end to end")

  call transientResponse(study, surge, "Node_1", "Node_1", nyquistHz, nSamples, freqZeroHz, &
                          t, injectedCurrent, response)

  call test_ok("time axis has nSamples points", size(t) == nSamples, "")
  call test_ok("response has nSamples points", size(response) == nSamples, "")
  call test_ok("no NaNs in the response", .not. any(ieee_is_nan(response)), "transient response contains NaN")
  call test_ok("time axis starts at 0", abs(t(1)) < 1.0d-12, "")
  call test_ok("time axis is increasing", all(t(2:) > t(:nSamples - 1)), "")

  ! ----------------------------------------------------------------
  ! Low-frequency scaling: response near the injected current's peak
  ! should be close to |Zin| at the lowest nonzero frequency bin (the
  ! same driving-point impedance test_solve.f90/test_sweep.f90 already
  ! validate against the Sunde/Dwight DC formula and passivity).
  ! ----------------------------------------------------------------
  call test_init("Transient GPR tracks the low-frequency input impedance")

  zin = study%inputImpedance("Node_1")
  zinLowFreqMag = abs(zin(2))  ! first bin above the freqZero substitute

  iPeak = maxloc(injectedCurrent, dim=1)
  ratioAtPeak = response(iPeak) / injectedCurrent(iPeak)

  call test_ok("Re(Zin) >= 0 across the transient's frequency axis (passivity)", &
               all(real(zin) >= -1.0d-9 * max(1.0d0, abs(zin))), &
               "input impedance must not have negative real part")
  call test_ok("response/current at the excitation peak ~= |Zin(low freq)| within 25%", &
               abs(ratioAtPeak - zinLowFreqMag) < 0.25d0 * zinLowFreqMag, &
               "transient GPR does not track the resistive low-frequency impedance")

  call test_summary()

end program test_transient
