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
  real(dp), allocatable :: t(:), injectedCurrent(:), response(:,:), responseAa(:,:), antialias(:)
  complex(dp), allocatable :: zin(:)
  real(dp) :: imax, nyquistHz, freqZeroHz, ratioAtPeak, zinLowFreqMag
  integer(4), parameter :: nSamples = 1024
  integer(4) :: iPeak
  ! Phase 9 (ADR 0015 amendment 2026-09-30)
  type(tTransientOptions) :: opts
  type(tSignalSlot) :: slots(2)
  real(dp), allocatable :: w(:), respFull(:,:), respInterp(:,:), injected2(:,:), respA(:,:), respB(:,:)
  real(dp), allocatable :: resp3(:,:,:), i1Resp3(:,:,:)
  real(dp), allocatable :: respSum(:,:), respLong(:,:), respNlt(:,:), respFft(:,:), tLong(:), xk(:), yk(:)
  complex(dp), allocatable :: yq(:)
  real(dp) :: peak, errNlt, errFft
  integer(4) :: nCompare, k

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
  ! Tukey anti-aliasing response: flat to taperStart, cosine to zero at fmax
  ! ----------------------------------------------------------------
  call test_init("Tukey anti-aliasing filter")

  antialias = tukeyAntialiasFilter(101, 0.85_dp)
  call test_ok("filter has one value per one-sided bin", size(antialias) == 101, "")
  call test_ok("pass band is unity at DC", abs(antialias(1) - 1.0_dp) < 1.0d-15, "")
  call test_ok("taper starts at 0.85 fmax", abs(antialias(86) - 1.0_dp) < 1.0d-15, "")
  call test_ok("first bin above 0.85 fmax is attenuated", antialias(87) < 1.0_dp, "")
  call test_ok("taper is point-symmetric about its midpoint (bins 93/94)", &
               abs(antialias(93) + antialias(94) - 1.0_dp) < 1.0d-14, "")
  call test_ok("filter is zero at fmax", abs(antialias(101)) < 1.0d-15, "")
  call test_ok("filter is monotonically non-increasing", &
               all(antialias(2:) <= antialias(:size(antialias) - 1)), "")
  antialias = tukeyAntialiasFilter(101, 1.0_dp)
  call test_ok("taperStart = 1 is the identity", all(antialias == 1.0_dp), "")

  ! ----------------------------------------------------------------
  ! Pipeline runs end to end and produces finite, well-shaped output
  ! ----------------------------------------------------------------
  call test_init("transientResponse: runs end to end")

  call transientResponse(study, surge, "Node_1", ["Node_1"], nyquistHz, nSamples, freqZeroHz, &
                          t, injectedCurrent, response)

  call test_ok("time axis has nSamples points", size(t) == nSamples, "")
  call test_ok("response has one row per observe node", size(response, 1) == 1, "")
  call test_ok("response has nSamples points", size(response, 2) == nSamples, "")
  call test_ok("no NaNs in the response", .not. any(ieee_is_nan(response)), "transient response contains NaN")
  call test_ok("time axis starts at 0", abs(t(1)) < 1.0d-12, "")
  call test_ok("time axis is increasing", all(t(2:) > t(:nSamples - 1)), "")

  ! ----------------------------------------------------------------
  ! Anti-aliasing is opt-in: taperStart = 1 reproduces the unfiltered
  ! response exactly, a real taper changes it but keeps it finite
  ! ----------------------------------------------------------------
  call test_init("transientResponse: antialiasStart option")

  call transientResponse(study, surge, "Node_1", ["Node_1"], nyquistHz, nSamples, freqZeroHz, &
                          t, injectedCurrent, responseAa, antialiasStart=1.0_dp)
  call test_ok("antialiasStart = 1 matches the default (no filter)", &
               maxval(abs(responseAa - response)) <= 1.0d-12 * maxval(abs(response)), "")

  call transientResponse(study, surge, "Node_1", ["Node_1"], nyquistHz, nSamples, freqZeroHz, &
                          t, injectedCurrent, responseAa, antialiasStart=0.25_dp)
  call test_ok("antialiasStart = 0.25 gives a finite response", .not. any(ieee_is_nan(responseAa)), "")
  call test_ok("antialiasStart = 0.25 changes the response", &
               maxval(abs(responseAa - response)) > 0.0_dp, "")

  ! ----------------------------------------------------------------
  ! Low-frequency scaling: response near the injected current's peak
  ! should be close to |Zin| at the lowest nonzero frequency bin (the
  ! same driving-point impedance test_solve.f90/test_sweep.f90 already
  ! validate against the Sunde/Dwight DC formula and passivity).
  ! ----------------------------------------------------------------
  call test_init("Transient GPR tracks the low-frequency input impedance")

  ! `transientResponse` no longer leaves its sweep in the study (ADR 0026):
  ! solve the driving-point impedance on the same frequency axis explicitly.
  call study%runSweep(oneSidedFrequencyAxis(nyquistHz, nSamples, freqZeroHz), ["Node_1"], &
                      [cmplx(1.0_dp, 0.0_dp, kind=dp)])
  zin = study%inputImpedance("Node_1")
  zinLowFreqMag = abs(zin(2))  ! first bin above the freqZero substitute

  iPeak = maxloc(injectedCurrent, dim=1)
  ratioAtPeak = response(1, iPeak) / injectedCurrent(iPeak)

  call test_ok("Re(Zin) >= 0 across the transient's frequency axis (passivity)", &
               all(real(zin) >= -1.0d-9 * max(1.0d0, abs(zin))), &
               "input impedance must not have negative real part")
  call test_ok("response/current at the excitation peak ~= |Zin(low freq)| within 25%", &
               abs(ratioAtPeak - zinLowFreqMag) < 0.25d0 * zinLowFreqMag, &
               "transient GPR does not track the resistive low-frequency impedance")

  ! ================================================================
  ! ROADMAP Phase 9 (ADR 0015 amendment 2026-09-30)
  ! ================================================================

  call test_init("Phase 9 item 2: half-Hann window")
  w = hannHalfWindow(101)
  call test_ok("window is 1 at the first sample", abs(w(1) - 1.0_dp) < 1.0d-15, "")
  call test_ok("window is 0 at the last sample", abs(w(101)) < 1.0d-15, "")
  call test_ok("window is 1/2 at the midpoint", abs(w(51) - 0.5_dp) < 1.0d-15, "")
  call test_ok("window is monotonically non-increasing", all(w(2:) <= w(:100)), "")
  antialias = tukeyAntialiasFilter(101, 1.0d-12)
  call test_ok("spectral Hann = Tukey band-edge filter in the s -> 0 limit", &
               maxval(abs(antialias - w)) < 1.0d-9, "")

  call test_init("Phase 9 item 1: pchip interpolation (Matlab pchip port)")
  xk = [0.0_dp, 1.0_dp, 2.5_dp, 4.0_dp, 7.0_dp]
  yq = pchipInterpolate(xk, cmplx(3.0_dp * xk - 1.0_dp, -2.0_dp * xk, kind=dp), [0.3_dp, 2.0_dp, 6.9_dp])
  call test_ok("linear data are reproduced exactly (Re and Im)", &
               maxval(abs(yq - cmplx(3.0_dp * [0.3_dp, 2.0_dp, 6.9_dp] - 1.0_dp, &
                                     -2.0_dp * [0.3_dp, 2.0_dp, 6.9_dp], kind=dp))) < 1.0d-13, "")
  yk = [0.0_dp, 0.0_dp, 1.0_dp, 1.0_dp, 5.0_dp]
  yq = pchipInterpolate(xk, cmplx(yk, 0.0_dp, kind=dp), xk)
  call test_ok("knots are interpolated exactly", maxval(abs(real(yq) - yk)) < 1.0d-15, "")
  yq = pchipInterpolate(xk, cmplx(yk, 0.0_dp, kind=dp), [(0.07_dp * k, k = 0, 100)])
  call test_ok("monotone data give a monotone interpolant (no overshoot)", &
               all(real(yq(2:)) >= real(yq(:100)) - 1.0d-15) .and. minval(real(yq)) >= -1.0d-15 &
               .and. maxval(real(yq)) <= 5.0_dp + 1.0d-15, "")
  ! Reference values from SciPy's PchipInterpolator (the same Fritsch-Butland
  ! interior / one-sided end-slope algorithm as Matlab pchip, Moler NCM §3.4);
  ! the first set also follows by hand: slopes d = [5.5, 0, 0, 2.5].
  yq = pchipInterpolate([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], cmplx([1.0_dp, 4.0_dp, 2.0_dp, 3.0_dp], 0.0_dp, kind=dp), &
                        [1.5_dp, 2.5_dp, 3.5_dp])
  call test_ok("sign changes: zero interior slopes, one-sided end slopes (reference values)", &
               maxval(abs(real(yq) - [3.1875_dp, 3.0_dp, 2.1875_dp])) < 1.0d-14, "")
  yq = pchipInterpolate([0.0_dp, 1.0_dp, 3.0_dp, 4.0_dp, 7.0_dp], &
                        cmplx(0.0_dp, [0.0_dp, 2.0_dp, 3.0_dp, 7.0_dp, 8.0_dp], kind=dp), &
                        [0.5_dp, 2.0_dp, 3.5_dp, 5.5_dp, 6.9_dp])
  call test_ok("non-uniform monotone data: weighted harmonic-mean slopes (reference values, Im part)", &
               maxval(abs(aimag(yq) - [1.2053571428571428_dp, 2.471042471042471_dp, 5.032069382815652_dp, &
                                       7.76865671641791_dp, 7.999049198452184_dp])) < 1.0d-12, "")

  call test_init("Phase 9 item 1: interpolated vs full transfer function")
  call transientResponse(study, surge, "Node_1", ["Node_1", "Node_2"], nyquistHz, nSamples, freqZeroHz, &
                          t, injectedCurrent, respFull)
  opts%transferFunction = "interpolated"
  opts%scanFreqHz = logFrequencyAxis(freqZeroHz, nyquistHz, 101)
  call transientResponse(study, surge, "Node_1", ["Node_1", "Node_2"], nyquistHz, nSamples, freqZeroHz, &
                          t, injectedCurrent, respInterp, options=opts)
  peak = maxval(abs(respFull))
  call test_ok("scan-fed transient (101 log points) within 1e-4 of peak of the per-bin solve", &
               maxval(abs(respInterp - respFull)) < 1.0d-4 * peak, "")
  opts = tTransientOptions()

  call test_init("Phase 9 item 4: multiple injections superpose")
  allocate(slots(1)%sig, source=newDoubleExpSignal(0.5_dp * imax, "f250_2500"))
  allocate(slots(2)%sig, source=newDoubleExpSignal(0.5_dp * imax, "f250_2500"))
  call transientResponseSources(study, slots, ["Node_1", "Node_1"], ["Node_1", "Node_2"], nyquistHz, &
                                 nSamples, freqZeroHz, t, injected2, respSum)
  call test_ok("two half-amplitude sources at one node = one full source", &
               maxval(abs(respSum - respFull)) < 1.0d-12 * peak, "")
  call test_ok("one injected-current row per source", size(injected2, 1) == 2, "")
  deallocate(slots(2)%sig)
  allocate(slots(2)%sig, source=newSineSignal(0.2_dp * imax, 2.0d3, 30.0_dp))
  call transientResponseSources(study, slots, ["Node_1", "Node_2"], ["Node_1", "Node_2"], nyquistHz, &
                                 nSamples, freqZeroHz, t, injected2, respSum)
  call transientResponseSources(study, slots(1:1), ["Node_1"], ["Node_1", "Node_2"], nyquistHz, &
                                 nSamples, freqZeroHz, t, injected2, respA)
  call transientResponseSources(study, slots(2:2), ["Node_2"], ["Node_1", "Node_2"], nyquistHz, &
                                 nSamples, freqZeroHz, t, injected2, respB)
  call test_ok("different waveforms at different nodes = sum of single-source runs (linearity)", &
               maxval(abs(respSum - (respA + respB))) < 1.0d-10 * maxval(abs(respSum)), "")

  call test_init("ADR 0026: independent signals share transfer functions")
  ! slots(1) sine-free reference: waveform 1 at Node_1, waveform 2 at Node_2
  call transientResponseSignals(study, slots, ["Node_1", "Node_2"], ["Node_1", "Node_2"], nyquistHz, &
                                 nSamples, freqZeroHz, t, injected2, resp3, independent=.true.)
  call test_ok("one response set per signal", size(resp3, 3) == 2 .and. size(injected2, 1) == 2, "")
  call test_ok("signal 1 equals its single-source run", &
               maxval(abs(resp3(:, :, 1) - respA)) < 1.0d-12 * maxval(abs(respA)), "")
  call test_ok("signal 2 equals its single-source run (other node)", &
               maxval(abs(resp3(:, :, 2) - respB)) < 1.0d-12 * maxval(abs(respB)), "")
  call test_ok("independent responses add up to the superposed run (linearity)", &
               maxval(abs(resp3(:, :, 1) + resp3(:, :, 2) - respSum)) < 1.0d-10 * maxval(abs(respSum)), "")
  ! two different waveforms on ONE node: one terminal, one set of transfer functions
  call transientResponseSignals(study, slots, ["Node_1", "Node_1"], ["Node_1", "Node_2"], nyquistHz, &
                                 nSamples, freqZeroHz, t, injected2, resp3, ["Line_1_e1"], i1Resp3, &
                                 independent=.true.)
  call transientResponseSources(study, slots(2:2), ["Node_1"], ["Node_1", "Node_2"], nyquistHz, &
                                 nSamples, freqZeroHz, t, injected2, respB)
  call test_ok("signals sharing a node: signal 2 equals its single-source run", &
               maxval(abs(resp3(:, :, 2) - respB)) < 1.0d-12 * maxval(abs(respB)), "")
  call test_ok("electrode currents come back per signal", &
               all(shape(i1Resp3) == [1, nSamples, 2]), "")

  call test_init("Phase 9 item 2: window placements")
  opts%window = "hann"
  opts%windowPlacement = "time"
  call transientResponse(study, surge, "Node_1", ["Node_1"], nyquistHz, nSamples, freqZeroHz, &
                          t, injectedCurrent, respA, options=opts)
  call test_ok("time window multiplies the sampled excitation", &
               maxval(abs(injectedCurrent - surge%waveform(t) * tailTaper(nSamples) * hannHalfWindow(nSamples))) &
               < 1.0d-12 * imax, "")
  opts%windowPlacement = "spectral"
  call transientResponse(study, surge, "Node_1", ["Node_1"], nyquistHz, nSamples, freqZeroHz, &
                          t, injectedCurrent, respA, options=opts)
  call transientResponse(study, surge, "Node_1", ["Node_1"], nyquistHz, nSamples, freqZeroHz, &
                          t, injectedCurrent, respB, antialiasStart=1.0d-12)
  call test_ok("spectral Hann = band-edge filter with antialiasStart -> 0", &
               maxval(abs(respA - respB)) < 1.0d-8 * maxval(abs(respA)), "")
  call test_ok("spectral window leaves the excitation untouched", &
               maxval(abs(injectedCurrent - surge%waveform(t) * tailTaper(nSamples))) < 1.0d-12 * imax, "")
  opts = tTransientOptions()

  call test_init("Phase 9 item 5: Numerical Laplace Transform")
  ! Reference: plain FFT on a 16x longer record (same dt), where record
  ! wrap-around is negligible over the first part of the short record. The
  ! short record (256 samples, 12.8 ms, ~5 tail time constants of the
  ! 250/2500 us surge) is deliberately wrap-around-limited: that is where
  ! the NLT damping pays off. No window: a spectral window acts on the
  ! damped spectrum under NLT, so its smoothing is not the same as on the
  ! undamped FFT reference, and with a front only ~5 samples long that
  ! difference would dominate the comparison.
  call transientResponse(study, surge, "Node_1", ["Node_1"], nyquistHz, 4 * nSamples, freqZeroHz, &
                          tLong, injectedCurrent, respLong, options=opts)
  call transientResponse(study, surge, "Node_1", ["Node_1"], nyquistHz, nSamples / 4, freqZeroHz, &
                          t, injectedCurrent, respFft, options=opts)
  opts%transform = "nlt"
  call transientResponse(study, surge, "Node_1", ["Node_1"], nyquistHz, nSamples / 4, freqZeroHz, &
                          t, injectedCurrent, respNlt, options=opts)
  nCompare = nSamples / 8   ! first half of the short record, before its tail taper
  peak = maxval(abs(respLong(1, 1:nCompare)))
  errNlt = maxval(abs(respNlt(1, 1:nCompare) - respLong(1, 1:nCompare))) / peak
  errFft = maxval(abs(respFft(1, 1:nCompare) - respLong(1, 1:nCompare))) / peak
  print '(A,ES10.3,A,ES10.3)', "     NLT vs long-record FFT: ", errNlt, ";  FFT vs long-record FFT: ", errFft
  call test_ok("NLT response is finite", .not. any(ieee_is_nan(respNlt)), "")
  call test_ok("NLT tracks the long-record reference within 1e-4 of peak (first half)", errNlt < 1.0d-4, "")
  call test_ok("NLT suppresses record wrap-around better than the plain FFT", errNlt < errFft, "")
  call test_ok("default damping is ln(N^2)/T", &
               abs(nltDefaultDamping(nyquistHz, nSamples) - log(real(nSamples, dp)**2) &
                   / (real(nSamples, dp) / (2.0_dp * nyquistHz))) < 1.0d-12 * nltDefaultDamping(nyquistHz, nSamples), "")
  opts = tTransientOptions()

  call test_summary()

end program test_transient
