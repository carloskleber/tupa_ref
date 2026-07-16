program example5
  !! ROADMAP.md Phase 6 milestone: transient (time-domain) ground potential
  !! rise (GPR) of the Portela-1997-parameter buried conductor (example3's
  !! geometry) under a standard 1.2/50 us double-exponential surge,
  !! obtained via the excitation-spectrum -> per-frequency solve ->
  !! inverse-FFT pipeline (mTransient, theory.md §8).
  !!
  !! As with example3, no tabulated reference waveform exists for this
  !! conductor (theory.md §9.2) — this prints the computed transient for
  !! inspection; see fortran/test/test_transient.f90 for the automated
  !! low-frequency-consistency check.
  use mCtes
  use mStudy
  use mSignal
  use mTransient
  use mNode
  use mMaterial
  use mElementLine
  implicit none

  type(tStudy) :: study
  class(tMaterial), allocatable :: mat
  class(tElement), allocatable :: elem
  real(8), parameter :: length = 10.0d0, r0 = 0.007d0, depth = 0.5d0
  real(8), parameter :: sigmaSoil = 0.01d0, epsrSoil = 10.0d0
  type(tDoubleExpSignal) :: surge
  real(8), allocatable :: t(:), injectedCurrent(:), response(:)
  real(8) :: imax, nyquistHz, freqZeroHz
  integer, parameter :: nSamples = 1024
  integer :: k, stride

  print *, color_green, "Starting Example 5: transient GPR of the Portela 1997 conductor", color_reset
  study%title = "Example 5 - transient GPR, Portela 1997 buried conductor"

  call study%structure%addNode(newNode("Node_1", [0.0d0, 0.0d0, -depth]))
  call study%structure%addNode(newNode("Node_2", [length, 0.0d0, -depth]))

  mat = newMaterialLinear("copper", 1.0d0, 1.0d0, 5.96d7)
  call study%structure%addMaterial(mat)
  study%structure%soil = newMaterialLinear("soil", epsrSoil, 1.0d0, sigmaSoil)

  elem = newElementLine("Line_1", "Node_1", "Node_2", r0, 10, "copper")
  call study%structure%addElement(elem)

  imax = 30.0d3
  surge = newDoubleExpSignal(imax, "f1_2_50")
  nyquistHz = 1.0d6
  freqZeroHz = 1.0d-6

  call transientResponse(study, surge, "Node_1", "Node_1", nyquistHz, nSamples, freqZeroHz, &
                          t, injectedCurrent, response)

  print *, ""
  print '(A14,A16,A16)', "t (us)", "i(t) (A)", "GPR v(t) (V)"
  stride = nSamples / 32
  do k = 1, nSamples, stride
    print '(F14.3,F16.2,F16.2)', t(k) * 1.0d6, injectedCurrent(k), response(k)
  end do

  print *, ""
  print '(A,F0.2,A)', "Peak injected current: ", maxval(injectedCurrent), " A"
  print '(A,F0.2,A)', "Peak GPR: ", maxval(response), " V"

  print *, color_green, "Example 5 completed.", color_reset
end program example5
