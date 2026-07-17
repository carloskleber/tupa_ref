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
    !! project's hardcoded air, ADR 0019) is singular at
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

  subroutine transientResponse(study, signal, sourceNodeId, observeNodeIds, &
                                nyquistHz, nSamples, freqZeroHz, t, injectedCurrent, &
                                nodeResponses, observeElectrodeIds, i1Responses, i2Responses)
    !! Full excitation -> transfer-function -> response pipeline:
    !!   1. sample `signal%waveform` on a linear time axis and taper its
    !!      tail (`tailTaper`, suppresses truncation leakage);
    !!   2. forward-FFT, keep the one-sided spectrum [0, nyquistHz];
    !!   3. solve the frequency-domain system once per bin with a unit
    !!      current injected at `sourceNodeId` (`tStudy%runSweep`); this
    !!      single sweep already gives the transfer function H(f) for
    !!      *every* node/electrode (`voltageResults`/`longCurrentResults`/
    !!      `transCurrentResults`), so observing more points costs no
    !!      extra `tStudy%run` calls, only more spectrum multiplies + IFFTs;
    !!   4. per requested observe point: multiply spectra, rebuild the full
    !!      spectrum by conjugate symmetry, and inverse-FFT back to the
    !!      time domain.
    class(tStudy), intent(inout) :: study
    class(tSignal), intent(in) :: signal
    character(len=*), intent(in) :: sourceNodeId
    !! Node receiving the excitation current (unit current injected per
    !! frequency bin; H(f) is the response per ampere)
    character(len=*), intent(in) :: observeNodeIds(:)
    !! Node(s) whose voltage v(t) is returned as the transient response
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
    real(dp), allocatable, intent(out) :: nodeResponses(:,:)
    !! v(t) (V) at each `observeNodeIds` entry, shape (size(observeNodeIds), nSamples)
    character(len=*), intent(in), optional :: observeElectrodeIds(:)
    !! Discretised electrode ID(s) whose longitudinal/transverse current
    !! i1(t)/i2(t) is also returned (omitted: currents are not computed)
    real(dp), allocatable, intent(out), optional :: i1Responses(:,:), i2Responses(:,:)
    !! i1(t)/i2(t) (A) at each `observeElectrodeIds` entry, shape
    !! (size(observeElectrodeIds), nSamples); allocated only if
    !! `observeElectrodeIds` is present

    real(dp), allocatable :: freqHz(:), taper(:)
    complex(dp), allocatable :: excitationSpectrum(:), transferFunction(:)
    integer(4) :: nBins, nObsNodes, nObsElectrodes, iNode, iElec, iIdx, k

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

    nObsNodes = size(observeNodeIds)
    allocate(nodeResponses(nObsNodes, nSamples))
    allocate(transferFunction(nBins))
    do iNode = 1, nObsNodes
      iIdx = study%structure%findNodeIndex(trim(observeNodeIds(iNode)))
      do k = 1, nBins
        transferFunction(k) = study%voltageResults%get(iIdx, k)
      end do
      nodeResponses(iNode, :) = spectrumToTimeSeries(transferFunction, excitationSpectrum, nBins, nSamples)
    end do

    if (present(observeElectrodeIds)) then
      nObsElectrodes = size(observeElectrodeIds)
      if (present(i1Responses)) allocate(i1Responses(nObsElectrodes, nSamples))
      if (present(i2Responses)) allocate(i2Responses(nObsElectrodes, nSamples))
      do iElec = 1, nObsElectrodes
        iIdx = electrodeIndex(study, trim(observeElectrodeIds(iElec)))
        if (present(i1Responses)) then
          do k = 1, nBins
            transferFunction(k) = study%longCurrentResults%get(iIdx, k)
          end do
          i1Responses(iElec, :) = spectrumToTimeSeries(transferFunction, excitationSpectrum, nBins, nSamples)
        end if
        if (present(i2Responses)) then
          do k = 1, nBins
            transferFunction(k) = study%transCurrentResults%get(iIdx, k)
          end do
          i2Responses(iElec, :) = spectrumToTimeSeries(transferFunction, excitationSpectrum, nBins, nSamples)
        end if
      end do
    end if
  end subroutine transientResponse

  integer(4) function electrodeIndex(study, electrodeId) result(idx)
    !! Index of `electrodeId` in `study%longCurrentResults`/`transCurrentResults`
    !! (both share the same electrode ordering — `runSweep`, Study.f90).
    class(tStudy), intent(in) :: study
    character(len=*), intent(in) :: electrodeId
    integer(4) :: i

    idx = 0
    do i = 1, study%longCurrentResults%entityCount()
      if (trim(study%longCurrentResults%entityId(i)) == electrodeId) then
        idx = i
        return
      end if
    end do
    call raiseError("transientResponse: electrode '" // electrodeId // "' not found")
  end function electrodeIndex

  function spectrumToTimeSeries(transferFunction, excitationSpectrum, nBins, nSamples) result(series)
    !! Multiply a one-sided transfer function by the excitation spectrum,
    !! rebuild the full spectrum by conjugate symmetry (legacy
    !! `ifourier.m` convention), and inverse-FFT to a real time series.
    !! Shared by every observe node/electrode in `transientResponse`.
    complex(dp), intent(in) :: transferFunction(:)
    !! One-sided transfer function H(f), size nBins
    complex(dp), intent(in) :: excitationSpectrum(:)
    !! Full N-point excitation spectrum, size nSamples
    integer(4), intent(in) :: nBins, nSamples
    real(dp) :: series(nSamples)
    complex(dp), allocatable :: fullSpectrum(:)
    integer(4) :: k

    allocate(fullSpectrum(nSamples))
    fullSpectrum(1:nBins - 1) = transferFunction(1:nBins - 1) * excitationSpectrum(1:nBins - 1)
    fullSpectrum(1) = cmplx(real(fullSpectrum(1), dp), 0.0_dp, kind=dp)
    do k = nBins, nSamples
      fullSpectrum(k) = conjg(transferFunction(2 * nBins - k) * excitationSpectrum(2 * nBins - k))
    end do

    call fftInverse(fullSpectrum)
    series = real(fullSpectrum, dp)
  end function spectrumToTimeSeries

end module mTransient
