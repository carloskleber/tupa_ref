module mTransient
  !! Excitation-spectrum -> transfer-function -> inverse-FFT transient
  !! driver (theory.md §8, ROADMAP.md Phase 6 item 2).
  !!
  !! The frequency-domain solver (`tStudy%runSweep`) is reused unchanged:
  !! a unit current is injected at the source node across a linear
  !! frequency axis, giving the transfer function H(f) = V(observe)/1 A;
  !! multiplying by the excitation's own spectrum and inverse-transforming
  !! gives the time-domain response to the actual injected waveform. The
  !! FFT/IFFT pairing (one-sided spectrum, conjugate-symmetric
  !! reconstruction, DC-bin substitution) mirrors the legacy Matlab
  !! `sinais.Sinal.fourier`/`ifourier.m`/`lesinais.m` convention exactly,
  !! quirks included (the Nyquist bin is reconstructed as a conjugated
  !! copy, never used unconjugated — harmless since realistic excitation
  !! spectra carry negligible energy there).
  use mCtes, only: dp
  use mSignal, only: tSignal, tailTaper
  use mFft, only: fftForward, fftInverse, isPowerOfTwo
  use mStudy, only: tStudy
  use mError, only: raiseError
  implicit none
  private
  public :: transientResponse, sampleTimeAxis, oneSidedFrequencyAxis

contains

  function sampleTimeAxis(nyquistHz, nSamples) result(t)
    !! Linear time axis, `nSamples` points, spacing dt = 1/(2·nyquistHz)
    !! (so the record's own FFT has Nyquist frequency `nyquistHz`).
    real(dp), intent(in) :: nyquistHz
    integer(4), intent(in) :: nSamples
    real(dp), allocatable :: t(:)
    real(dp) :: dt
    integer(4) :: k

    dt = 1.0_dp / (2.0_dp * nyquistHz)
    allocate(t(nSamples))
    do k = 1, nSamples
      t(k) = real(k - 1, dp) * dt
    end do
  end function sampleTimeAxis

  function oneSidedFrequencyAxis(nyquistHz, nSamples, freqZeroHz) result(freqHz)
    !! One-sided linear axis f_k = k·df, k = 0..N/2 (N/2+1 points spanning
    !! [0, nyquistHz]), with the DC bin replaced by `freqZeroHz`: the
    !! transverse admittance of a zero-conductivity medium (e.g. the
    !! project's hardcoded air, ROADMAP.md §3 item 9) is singular at
    !! omega = 0 exactly, so the legacy `lesinais.m` solves at a small
    !! nonzero "FREQ_ZERO" instead — same convention here.
    real(dp), intent(in) :: nyquistHz
    integer(4), intent(in) :: nSamples
    real(dp), intent(in) :: freqZeroHz
    real(dp), allocatable :: freqHz(:)
    integer(4) :: nBins, k

    nBins = nSamples / 2 + 1
    allocate(freqHz(nBins))
    do k = 1, nBins
      freqHz(k) = real(k - 1, dp) * nyquistHz / real(nBins - 1, dp)
    end do
    freqHz(1) = freqZeroHz
  end function oneSidedFrequencyAxis

  subroutine transientResponse(study, signal, sourceNodeId, observeNodeId, &
                                nyquistHz, nSamples, freqZeroHz, t, injectedCurrent, response)
    !! Full excitation -> transfer-function -> response pipeline:
    !!   1. sample `signal%waveform` on a linear time axis and taper its
    !!      tail (`tailTaper`, suppresses truncation leakage);
    !!   2. forward-FFT, keep the one-sided spectrum [0, nyquistHz];
    !!   3. solve the frequency-domain system once per bin with a unit
    !!      current injected at `sourceNodeId` (`tStudy%runSweep`), giving
    !!      the transfer function H(f) = V(observeNodeId)/1 A;
    !!   4. multiply spectra, rebuild the full spectrum by conjugate
    !!      symmetry, and inverse-FFT back to the time domain.
    class(tStudy), intent(inout) :: study
    class(tSignal), intent(in) :: signal
    character(len=*), intent(in) :: sourceNodeId
    !! Node receiving the excitation current (unit current injected per
    !! frequency bin; H(f) is the response per ampere)
    character(len=*), intent(in) :: observeNodeId
    !! Node whose voltage V(t) is returned as the transient response
    real(dp), intent(in) :: nyquistHz
    !! Spectrum upper bound (Hz) — the excitation and every transfer
    !! function bin are band-limited to [0, nyquistHz]
    integer(4), intent(in) :: nSamples
    !! Number of time samples; must be a power of two (`mFft`)
    real(dp), intent(in) :: freqZeroHz
    !! Small nonzero frequency (Hz) substituted for the DC bin
    real(dp), allocatable, intent(out) :: t(:)
    !! Time axis (s), `nSamples` points, spacing 1/(2·nyquistHz)
    real(dp), allocatable, intent(out) :: injectedCurrent(:)
    !! Sampled excitation waveform i(t) (A) actually injected (post-taper)
    real(dp), allocatable, intent(out) :: response(:)
    !! Time-domain response at `observeNodeId` (V), same axis as `t`

    real(dp), allocatable :: freqHz(:), taper(:)
    complex(dp), allocatable :: excitationSpectrum(:), transferFunction(:), fullSpectrum(:)
    integer(4) :: nBins, iObs, k

    if (.not. isPowerOfTwo(nSamples)) then
      call raiseError("transientResponse: nSamples must be a power of two")
      return
    end if

    t = sampleTimeAxis(nyquistHz, nSamples)
    taper = tailTaper(nSamples)
    injectedCurrent = signal%waveform(t) * taper

    allocate(excitationSpectrum(nSamples))
    excitationSpectrum = cmplx(injectedCurrent, 0.0_dp, kind=dp)
    call fftForward(excitationSpectrum)

    nBins = nSamples / 2 + 1
    freqHz = oneSidedFrequencyAxis(nyquistHz, nSamples, freqZeroHz)

    call study%runSweep(freqHz, [sourceNodeId], [cmplx(1.0_dp, 0.0_dp, kind=dp)])
    iObs = study%structure%findNodeIndex(trim(observeNodeId))

    allocate(transferFunction(nBins))
    do k = 1, nBins
      transferFunction(k) = study%voltageResults%get(iObs, k)
    end do

    ! Conjugate-symmetric reconstruction of the full N-point spectrum from
    ! the one-sided [0, nyquistHz] product (legacy ifourier.m convention).
    allocate(fullSpectrum(nSamples))
    fullSpectrum(1:nBins - 1) = transferFunction(1:nBins - 1) * excitationSpectrum(1:nBins - 1)
    fullSpectrum(1) = cmplx(real(fullSpectrum(1), dp), 0.0_dp, kind=dp)
    do k = nBins, nSamples
      fullSpectrum(k) = conjg(transferFunction(2 * nBins - k) * excitationSpectrum(2 * nBins - k))
    end do

    call fftInverse(fullSpectrum)
    response = real(fullSpectrum, dp)
  end subroutine transientResponse

end module mTransient
