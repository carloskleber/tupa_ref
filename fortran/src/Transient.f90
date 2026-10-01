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
  !!
  !! ROADMAP Phase 9 (ADR 0015 amendment 2026-09-30) adds, all opt-in via
  !! `tTransientOptions` so the default path is unchanged:
  !! - item 1, `transferFunction = "interpolated"`: H(f) is solved only on a
  !!   caller-supplied scan axis and interpolated onto the FFT bins by
  !!   componentwise (Re/Im) pchip (`pchipInterpolate`, Matlab `pchip`),
  !!   the legacy `imitancia.m` default mode; no extrapolation;
  !! - item 2, `window = "hann"` in `windowPlacement = "spectral"` (on the
  !!   one-sided H·X product) or `"time"` (on the sampled excitation);
  !! - item 4, several simultaneous current injections
  !!   (`transientResponseSources`): one unit-current sweep per source,
  !!   responses superposed in the frequency domain, Σ_k H_k·X_k;
  !! - item 5, `transform = "nlt"`: Numerical Laplace Transform — the
  !!   excitation is damped by e^{-ct} before the FFT, the system solved at
  !!   s = c + jω, and the inverse FFT undamped by e^{ct} (Gómez & Uribe
  !!   [17]; default c = ln(N²)/T, theory.md §8).
  use mCtes, only: dp, PI
  use mSignal, only: tSignal, tSignalSlot, tailTaper
  use mFft, only: fftForward, fftInverse, isPowerOfTwo
  use mStudy, only: tStudy
  use mError, only: raiseError
  implicit none
  private
  public :: transientResponse, transientResponseSources, tTransientOptions
  public :: sampleTimeAxis, oneSidedFrequencyAxis, tukeyAntialiasFilter, hannHalfWindow
  public :: pchipInterpolate, nltDefaultDamping

  type :: tTransientOptions
    !! Opt-in transient-pipeline options (ADR 0015 amendment 2026-09-30,
    !! ROADMAP Phase 9); the defaults reproduce the Phase 6 pipeline
    !! exactly (golden fixtures unchanged).
    real(dp) :: antialiasStart = 1.0_dp
    !! Tukey band-edge filter start (ADR 0021); 1 = off
    character(len=16) :: window = "none"
    !! Window function: "none" or "hann" (item 2)
    character(len=16) :: windowPlacement = "spectral"
    !! Where the window acts: "spectral" (one-sided H·X product) or "time"
    !! (sampled excitation record)
    character(len=16) :: transform = "fft"
    !! "fft" (s = jω) or "nlt" (s = c + jω, item 5)
    real(dp) :: nltDamping = 0.0_dp
    !! NLT damping c (1/s); 0 selects `nltDefaultDamping`
    character(len=16) :: transferFunction = "full"
    !! "full" (solve every FFT bin) or "interpolated" (solve `scanFreqHz`,
    !! pchip onto the bins, item 1)
    real(dp), allocatable :: scanFreqHz(:)
    !! Ascending scan axis (Hz) for "interpolated"; must span
    !! [freqZeroHz, nyquistHz]
  end type tTransientOptions

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

  function tukeyAntialiasFilter(nBins, taperStart) result(filter)
    !! One-sided, frequency-domain anti-aliasing filter (theory.md §8, ADR
    !! 0021). Unity up to `taperStart` times the maximum represented
    !! frequency, then the falling half of a Tukey raised-cosine window
    !! down to zero at that maximum (Nyquist):
    !!
    !!   H(x) = 1                                          x <= s
    !!          0.5 [1 + cos(pi (x - s) / (1 - s))]        s < x <= 1
    !!
    !! with x = f/f_max and s = `taperStart`. `taperStart` = 1 means no
    !! taper (all ones). Deliberately separate from `tailTaper`: that
    !! time-domain taper limits record-truncation leakage, while this
    !! filter suppresses content close to Nyquist.
    integer(4), intent(in) :: nBins
    !! Number of one-sided bins (DC through Nyquist), at least 2
    real(dp), intent(in) :: taperStart
    !! Fraction of Nyquist where the roll-off begins, in (0, 1]
    real(dp), allocatable :: filter(:)
    real(dp) :: x
    integer(4) :: k

    if (nBins < 2) then
      call raiseError("tukeyAntialiasFilter: nBins must be at least 2")
      return
    end if
    if (taperStart <= 0.0_dp .or. taperStart > 1.0_dp) then
      call raiseError("tukeyAntialiasFilter: taperStart must be in (0, 1]")
      return
    end if

    allocate(filter(nBins))
    do k = 1, nBins
      x = real(k - 1, dp) / real(nBins - 1, dp)
      if (x <= taperStart) then
        filter(k) = 1.0_dp
      else
        filter(k) = 0.5_dp * (1.0_dp + cos(PI * (x - taperStart) / (1.0_dp - taperStart)))
      end if
    end do
  end function tukeyAntialiasFilter

  function hannHalfWindow(n) result(w)
    !! Falling half of a Hann window over `n` samples (ROADMAP Phase 9 item
    !! 2): w(x) = 0.5·[1 + cos(πx)], x = (k-1)/(n-1), so w = 1 at the first
    !! sample (DC, or t = 0) and 0 at the last (Nyquist, or the record end).
    !! On the one-sided spectrum it is the Hanning data window of the NLT
    !! literature (Gómez & Uribe [17]) and the s -> 0 limit of
    !! `tukeyAntialiasFilter`.
    integer(4), intent(in) :: n
    real(dp) :: w(n)
    integer(4) :: k

    if (n < 2) then
      call raiseError("hannHalfWindow: n must be at least 2")
      w = 1.0_dp
      return
    end if
    do k = 1, n
      w(k) = 0.5_dp * (1.0_dp + cos(PI * real(k - 1, dp) / real(n - 1, dp)))
    end do
  end function hannHalfWindow

  real(dp) function nltDefaultDamping(nyquistHz, nSamples) result(c)
    !! Default NLT damping c = ln(N²)/T, T = N·Δt = N/(2·nyquistHz) the
    !! record length (Wilcox's rule as used by Gómez & Uribe [17], TAGS
    !! and PRTL; theory.md §8).
    real(dp), intent(in) :: nyquistHz
    integer(4), intent(in) :: nSamples

    c = log(real(nSamples, dp) ** 2) * 2.0_dp * nyquistHz / real(nSamples, dp)
  end function nltDefaultDamping

  ! =====================================================================
  ! Shape-preserving piecewise cubic Hermite interpolation (pchip)
  ! =====================================================================

  function pchipInterpolate(x, y, xq) result(yq)
    !! Componentwise (Re and Im separately) pchip interpolation of complex
    !! samples `y` on the strictly ascending abscissae `x` at `xq` —
    !! the legacy `imitancia.m` interpolation of H(f) onto the FFT bins
    !! (ROADMAP Phase 9 item 1). Port of Matlab's `pchip` (Fritsch–Carlson
    !! monotone slopes with the Fritsch–Butland weighted harmonic mean and
    !! the one-sided three-point end slopes, `pchipslopes`/`pchipend`).
    !! Queries outside [x(1), x(n)] are clamped to the end points: no
    !! extrapolation (callers reject such axes beforehand).
    real(dp), intent(in) :: x(:)
    complex(dp), intent(in) :: y(:)
    real(dp), intent(in) :: xq(:)
    complex(dp) :: yq(size(xq))
    real(dp), allocatable :: yr(:), yi(:), dr(:), di(:)
    real(dp) :: xc, h, sx
    integer(4) :: n, k, j

    n = size(x)
    if (n < 2 .or. size(y) /= n) then
      call raiseError("pchipInterpolate: need at least two samples and size(y) == size(x)")
      yq = (0.0_dp, 0.0_dp)
      return
    end if
    if (any(x(2:n) <= x(1:n-1))) then
      call raiseError("pchipInterpolate: abscissae must be strictly ascending")
      yq = (0.0_dp, 0.0_dp)
      return
    end if

    yr = real(y, dp)
    yi = aimag(y)
    dr = pchipSlopes(x, yr)
    di = pchipSlopes(x, yi)

    do k = 1, size(xq)
      xc = min(max(xq(k), x(1)), x(n))
      j = intervalIndex(x, xc)
      h = x(j + 1) - x(j)
      sx = xc - x(j)
      yq(k) = cmplx(hermiteEval(yr(j), yr(j + 1), dr(j), dr(j + 1), h, sx), &
                    hermiteEval(yi(j), yi(j + 1), di(j), di(j + 1), h, sx), kind=dp)
    end do
  end function pchipInterpolate

  function pchipSlopes(x, y) result(d)
    !! Derivative estimates at the knots, Matlab `pchipslopes`.
    real(dp), intent(in) :: x(:), y(:)
    real(dp) :: d(size(x))
    real(dp), allocatable :: h(:), del(:)
    real(dp) :: hs, w1, w2, dmax, dmin
    integer(4) :: n, k

    n = size(x)
    h = x(2:n) - x(1:n-1)
    del = (y(2:n) - y(1:n-1)) / h
    if (n == 2) then
      d = del(1)
      return
    end if

    d = 0.0_dp
    do k = 1, n - 2
      if (sign1(del(k)) * sign1(del(k + 1)) > 0.0_dp) then
        hs = h(k) + h(k + 1)
        w1 = (h(k) + hs) / (3.0_dp * hs)
        w2 = (hs + h(k + 1)) / (3.0_dp * hs)
        dmax = max(abs(del(k)), abs(del(k + 1)))
        dmin = min(abs(del(k)), abs(del(k + 1)))
        d(k + 1) = dmin / (w1 * (del(k) / dmax) + w2 * (del(k + 1) / dmax))
      end if
    end do
    d(1) = pchipEnd(h(1), h(2), del(1), del(2))
    d(n) = pchipEnd(h(n - 1), h(n - 2), del(n - 1), del(n - 2))
  end function pchipSlopes

  real(dp) function pchipEnd(h1, h2, del1, del2) result(d)
    !! Shape-preserving one-sided end slope, Matlab `pchipend`.
    real(dp), intent(in) :: h1, h2, del1, del2

    d = ((2.0_dp * h1 + h2) * del1 - h1 * del2) / (h1 + h2)
    if (sign1(d) /= sign1(del1)) then
      d = 0.0_dp
    else if (sign1(del1) /= sign1(del2) .and. abs(d) > abs(3.0_dp * del1)) then
      d = 3.0_dp * del1
    end if
  end function pchipEnd

  real(dp) function sign1(v)
    !! Matlab `sign`: -1, 0 or +1.
    real(dp), intent(in) :: v
    if (v > 0.0_dp) then
      sign1 = 1.0_dp
    else if (v < 0.0_dp) then
      sign1 = -1.0_dp
    else
      sign1 = 0.0_dp
    end if
  end function sign1

  real(dp) function hermiteEval(y0, y1, d0, d1, h, sx) result(v)
    !! Cubic Hermite piece on [x_j, x_j + h] at offset sx, in Matlab's
    !! `pwch`/`ppval` coefficient form and Horner order.
    real(dp), intent(in) :: y0, y1, d0, d1, h, sx
    real(dp) :: delta, b, c

    delta = (y1 - y0) / h
    c = (3.0_dp * delta - 2.0_dp * d0 - d1) / h
    b = (d0 - 2.0_dp * delta + d1) / h ** 2
    v = ((b * sx + c) * sx + d0) * sx + y0
  end function hermiteEval

  integer(4) function intervalIndex(x, xc) result(j)
    !! j with x(j) <= xc < x(j+1) (bisection); the last knot maps to the
    !! last interval.
    real(dp), intent(in) :: x(:), xc
    integer(4) :: lo, hi, mid

    lo = 1
    hi = size(x)
    do while (hi - lo > 1)
      mid = (lo + hi) / 2
      if (xc >= x(mid)) then
        lo = mid
      else
        hi = mid
      end if
    end do
    j = lo
  end function intervalIndex

  subroutine transientResponse(study, signal, sourceNodeId, observeNodeIds, &
                                nyquistHz, nSamples, freqZeroHz, t, injectedCurrent, &
                                nodeResponses, observeElectrodeIds, i1Responses, i2Responses, antialiasStart, &
                                options)
    !! Single-source transient run (ROADMAP Phase 6; ADR 0015): a thin
    !! wrapper over `transientResponseSources` with one source. Pipeline:
    !!   1. sample `signal%waveform` on a linear time axis and taper its
    !!      tail (`tailTaper`, suppresses truncation leakage);
    !!   2. forward-FFT, keep the one-sided spectrum [0, nyquistHz];
    !!   3. solve the frequency-domain system with a unit current injected
    !!      at `sourceNodeId` (`tStudy%runSweep`) — once per bin, or on a
    !!      scan axis and interpolated (`options`); this single sweep gives
    !!      the transfer function H(f) for *every* node/electrode, so more
    !!      observe points cost no extra solves;
    !!   4. per observe point: multiply spectra (and any spectral window /
    !!      band-edge filter), rebuild the full spectrum by conjugate
    !!      symmetry, and inverse-FFT back to the time domain.
    class(tStudy), intent(inout) :: study
    class(tSignal), intent(in) :: signal
    character(len=*), intent(in) :: sourceNodeId
    !! Node receiving the excitation current
    character(len=*), intent(in) :: observeNodeIds(:)
    !! Node(s) whose voltage v(t) is returned as the transient response
    real(dp), intent(in) :: nyquistHz
    !! Spectrum upper bound (Hz)
    integer(4), intent(in) :: nSamples
    !! Number of time samples; must be a power of two (`mFft`)
    real(dp), intent(in) :: freqZeroHz
    !! Small nonzero frequency (Hz) substituted for the DC bin (FFT only)
    real(dp), allocatable, intent(out) :: t(:)
    !! Time axis (s), `nSamples` points, spacing 1/(2·nyquistHz)
    real(dp), allocatable, intent(out) :: injectedCurrent(:)
    !! Sampled excitation waveform i(t) (A) actually injected (post-taper
    !! and post time-window)
    real(dp), allocatable, intent(out) :: nodeResponses(:,:)
    !! v(t) (V) at each `observeNodeIds` entry, shape (size(observeNodeIds), nSamples)
    character(len=*), intent(in), optional :: observeElectrodeIds(:)
    !! Discretised electrode ID(s) whose i1(t)/i2(t) is also returned
    real(dp), allocatable, intent(out), optional :: i1Responses(:,:), i2Responses(:,:)
    !! i1(t)/i2(t) (A), shape (size(observeElectrodeIds), nSamples)
    real(dp), intent(in), optional :: antialiasStart
    !! Band-edge filter start in (0, 1] (ADR 0021); overrides
    !! `options%antialiasStart` when given
    type(tTransientOptions), intent(in), optional :: options
    !! Phase 9 options (defaults: the Phase 6 pipeline)

    type(tSignalSlot) :: slots(1)
    type(tTransientOptions) :: opts
    real(dp), allocatable :: injectedCurrents(:,:)
    character(len=len(sourceNodeId)) :: sourceIds(1)

    if (present(options)) opts = options
    if (present(antialiasStart)) opts%antialiasStart = antialiasStart
    allocate(slots(1)%sig, source=signal)
    sourceIds(1) = sourceNodeId

    call transientResponseSources(study, slots, sourceIds, observeNodeIds, nyquistHz, nSamples, &
      freqZeroHz, t, injectedCurrents, nodeResponses, observeElectrodeIds, i1Responses, i2Responses, opts)
    if (allocated(injectedCurrents)) injectedCurrent = injectedCurrents(1, :)
  end subroutine transientResponse

  subroutine transientResponseSources(study, signals, sourceNodeIds, observeNodeIds, &
                                       nyquistHz, nSamples, freqZeroHz, t, injectedCurrents, &
                                       nodeResponses, observeElectrodeIds, i1Responses, i2Responses, options)
    !! Multi-source transient run (ROADMAP Phase 9 item 4): current
    !! injection `signals(k)` at node `sourceNodeIds(k)`. By linearity the
    !! response is the superposition Σ_k H_k(s)·X_k(s), where H_k is the
    !! transfer function of a unit current at source k (one `runSweep` per
    !! source) and X_k the spectrum of the k-th sampled, tapered excitation;
    !! the sum is formed per observe point before a single inverse FFT.
    !! With one source this is exactly the Phase 6 pipeline. See
    !! `tTransientOptions` for the interpolated transfer function, windows
    !! and the Numerical Laplace Transform.
    class(tStudy), intent(inout) :: study
    type(tSignalSlot), intent(in) :: signals(:)
    !! One waveform per source
    character(len=*), intent(in) :: sourceNodeIds(:)
    !! Injection node per source (repeats allowed: they superpose)
    character(len=*), intent(in) :: observeNodeIds(:)
    real(dp), intent(in) :: nyquistHz
    integer(4), intent(in) :: nSamples
    real(dp), intent(in) :: freqZeroHz
    real(dp), allocatable, intent(out) :: t(:)
    real(dp), allocatable, intent(out) :: injectedCurrents(:,:)
    !! Sampled excitation per source (A), shape (size(signals), nSamples)
    real(dp), allocatable, intent(out) :: nodeResponses(:,:)
    character(len=*), intent(in), optional :: observeElectrodeIds(:)
    real(dp), allocatable, intent(out), optional :: i1Responses(:,:), i2Responses(:,:)
    type(tTransientOptions), intent(in), optional :: options

    type(tTransientOptions) :: opts
    real(dp), allocatable :: taper(:), filter(:), timeWindow(:), damp(:), binFreqHz(:), solveFreqHz(:)
    complex(dp), allocatable :: excitation(:,:), transferFunction(:), sweepRow(:)
    complex(dp), allocatable :: pNode(:,:), pI1(:,:), pI2(:,:)
    integer(4), allocatable :: nodeIdx(:), elecIdx(:)
    integer(4) :: nBins, nSrc, nObsNodes, nObsElectrodes, iSrc, iObs, k
    real(dp) :: c
    logical :: nlt, interpolated, wantI1, wantI2

    if (present(options)) opts = options

    nSrc = size(signals)
    if (nSrc < 1 .or. size(sourceNodeIds) /= nSrc) then
      call raiseError("transientResponse: need at least one source and one node per source")
      return
    end if
    if (.not. isPowerOfTwo(nSamples)) then
      call raiseError("transientResponse: nSamples must be a power of two")
      return
    end if
    if (opts%antialiasStart <= 0.0_dp .or. opts%antialiasStart > 1.0_dp) then
      call raiseError("transientResponse: antialiasStart must be in (0, 1]")
      return
    end if
    select case (trim(opts%window))
    case ("none", "hann")
    case default
      call raiseError("transientResponse: unknown window '" // trim(opts%window) // "' (expected none or hann)")
      return
    end select
    select case (trim(opts%windowPlacement))
    case ("spectral", "time")
    case default
      call raiseError("transientResponse: unknown window placement '" // trim(opts%windowPlacement) // &
                      "' (expected spectral or time)")
      return
    end select
    select case (trim(opts%transform))
    case ("fft")
      nlt = .false.
    case ("nlt")
      nlt = .true.
    case default
      call raiseError("transientResponse: unknown transform '" // trim(opts%transform) // "' (expected fft or nlt)")
      return
    end select
    select case (trim(opts%transferFunction))
    case ("full")
      interpolated = .false.
    case ("interpolated")
      interpolated = .true.
    case default
      call raiseError("transientResponse: unknown transferFunction '" // trim(opts%transferFunction) // &
                      "' (expected full or interpolated)")
      return
    end select
    if (nlt .and. interpolated) then
      ! A pchip fit over a real-frequency scan is not the analytic
      ! continuation H(c + jω) the NLT needs (ADR 0015 amendment).
      call raiseError("transientResponse: transform 'nlt' cannot be combined with transferFunction 'interpolated'")
      return
    end if
    if (interpolated) then
      if (.not. allocated(opts%scanFreqHz)) then
        call raiseError("transientResponse: transferFunction 'interpolated' needs a scan axis")
        return
      end if
      if (size(opts%scanFreqHz) < 2) then
        call raiseError("transientResponse: the interpolation scan axis needs at least two points")
        return
      end if
      if (.not. scanSpans(opts%scanFreqHz, freqZeroHz, nyquistHz)) then
        call raiseError("transientResponse: the interpolation scan axis must be ascending and span " // &
                        "[freqZeroHz, nyquistHz] (no extrapolation)")
        return
      end if
    end if
    if (opts%nltDamping < 0.0_dp) then
      call raiseError("transientResponse: nltDamping must be >= 0 (0 = default ln(N^2)/T)")
      return
    end if

    nBins = nSamples / 2 + 1
    t = sampleTimeAxis(nyquistHz, nSamples)
    taper = tailTaper(nSamples)

    ! Time-domain window on the excitation record (item 2, placement "time")
    if (trim(opts%window) == "hann" .and. trim(opts%windowPlacement) == "time") then
      timeWindow = hannHalfWindow(nSamples)
    end if

    ! NLT damping e^{-ct} applied before the forward FFT (item 5)
    c = 0.0_dp
    if (nlt) then
      c = opts%nltDamping
      if (c == 0.0_dp) c = nltDefaultDamping(nyquistHz, nSamples)
      damp = exp(-c * t)
    end if

    allocate(injectedCurrents(nSrc, nSamples), excitation(nSamples, nSrc))
    do iSrc = 1, nSrc
      injectedCurrents(iSrc, :) = signals(iSrc)%sig%waveform(t) * taper
      if (allocated(timeWindow)) injectedCurrents(iSrc, :) = injectedCurrents(iSrc, :) * timeWindow
      if (nlt) then
        excitation(:, iSrc) = cmplx(injectedCurrents(iSrc, :) * damp, 0.0_dp, kind=dp)
      else
        excitation(:, iSrc) = cmplx(injectedCurrents(iSrc, :), 0.0_dp, kind=dp)
      end if
      call fftForward(excitation(:, iSrc))
    end do

    ! One-sided spectral filter: band-edge Tukey (ADR 0021) × spectral window
    filter = tukeyAntialiasFilter(nBins, opts%antialiasStart)
    if (trim(opts%window) == "hann" .and. trim(opts%windowPlacement) == "spectral") then
      filter = filter * hannHalfWindow(nBins)
    end if

    ! Synthesis axis (the FFT bins) and solve axis
    if (nlt) then
      binFreqHz = oneSidedFrequencyAxis(nyquistHz, nSamples, 0.0_dp)   ! s_0 = c: no DC substitute
    else
      binFreqHz = oneSidedFrequencyAxis(nyquistHz, nSamples, freqZeroHz)
    end if
    if (interpolated) then
      solveFreqHz = opts%scanFreqHz
    else
      solveFreqHz = binFreqHz
    end if

    ! Discretised node/electrode IDs resolve only after assembly (idempotent)
    call study%structure%assembleStructure()

    nObsNodes = size(observeNodeIds)
    allocate(nodeIdx(nObsNodes))
    do iObs = 1, nObsNodes
      nodeIdx(iObs) = study%structure%findNodeIndex(trim(observeNodeIds(iObs)))
      if (nodeIdx(iObs) == 0) then
        call raiseError("transientResponse: node '" // trim(observeNodeIds(iObs)) // "' not found")
        return
      end if
    end do
    nObsElectrodes = 0
    wantI1 = .false.
    wantI2 = .false.
    if (present(observeElectrodeIds)) then
      nObsElectrodes = size(observeElectrodeIds)
      wantI1 = present(i1Responses)
      wantI2 = present(i2Responses)
      allocate(elecIdx(nObsElectrodes))
      do iObs = 1, nObsElectrodes
        elecIdx(iObs) = electrodeIndex(study, trim(observeElectrodeIds(iObs)))
      end do
    end if

    ! Accumulated one-sided products P = Σ_k H_k·X_k per observe point
    allocate(pNode(nBins, nObsNodes))
    if (wantI1) allocate(pI1(nBins, nObsElectrodes))
    if (wantI2) allocate(pI2(nBins, nObsElectrodes))
    allocate(transferFunction(nBins), sweepRow(size(solveFreqHz)))

    do iSrc = 1, nSrc
      ! A unit current, or unit voltage, at source k (ADR 0025: across the
      ! node pair when the slot has a return node)
      if (nlt) then
        call study%runSweep(solveFreqHz, [sourceNodeIds(iSrc)], [cmplx(1.0_dp, 0.0_dp, kind=dp)], &
                            sourceIsVoltage=[signals(iSrc)%isVoltage], damping=c, &
                            returnNodeIds=[signals(iSrc)%returnNode])
      else
        call study%runSweep(solveFreqHz, [sourceNodeIds(iSrc)], [cmplx(1.0_dp, 0.0_dp, kind=dp)], &
                            sourceIsVoltage=[signals(iSrc)%isVoltage], &
                            returnNodeIds=[signals(iSrc)%returnNode])
      end if

      do iObs = 1, nObsNodes
        do k = 1, size(solveFreqHz)
          sweepRow(k) = study%voltageResults%get(nodeIdx(iObs), k)
        end do
        call toBins(sweepRow, transferFunction)
        call accumulate(pNode(:, iObs), transferFunction, excitation(:, iSrc), iSrc)
      end do
      do iObs = 1, nObsElectrodes
        if (wantI1) then
          do k = 1, size(solveFreqHz)
            sweepRow(k) = study%longCurrentResults%get(elecIdx(iObs), k)
          end do
          call toBins(sweepRow, transferFunction)
          call accumulate(pI1(:, iObs), transferFunction, excitation(:, iSrc), iSrc)
        end if
        if (wantI2) then
          do k = 1, size(solveFreqHz)
            sweepRow(k) = study%transCurrentResults%get(elecIdx(iObs), k)
          end do
          call toBins(sweepRow, transferFunction)
          call accumulate(pI2(:, iObs), transferFunction, excitation(:, iSrc), iSrc)
        end if
      end do
    end do

    allocate(nodeResponses(nObsNodes, nSamples))
    do iObs = 1, nObsNodes
      nodeResponses(iObs, :) = productToTimeSeries(pNode(:, iObs))
    end do
    if (wantI1) then
      allocate(i1Responses(nObsElectrodes, nSamples))
      do iObs = 1, nObsElectrodes
        i1Responses(iObs, :) = productToTimeSeries(pI1(:, iObs))
      end do
    end if
    if (wantI2) then
      allocate(i2Responses(nObsElectrodes, nSamples))
      do iObs = 1, nObsElectrodes
        i2Responses(iObs, :) = productToTimeSeries(pI2(:, iObs))
      end do
    end if

  contains

    subroutine toBins(row, h)
      !! Solve-axis samples -> transfer function on the FFT bins.
      complex(dp), intent(in) :: row(:)
      complex(dp), intent(out) :: h(:)
      if (interpolated) then
        h = pchipInterpolate(solveFreqHz, row, binFreqHz)
      else
        h = row
      end if
    end subroutine toBins

    subroutine accumulate(p, h, x, iSrc)
      !! p += H·X over the one-sided bins (first source assigns).
      complex(dp), intent(inout) :: p(:)
      complex(dp), intent(in) :: h(:), x(:)
      integer(4), intent(in) :: iSrc
      if (iSrc == 1) then
        p = h(1:nBins) * x(1:nBins)
      else
        p = p + h(1:nBins) * x(1:nBins)
      end if
    end subroutine accumulate

    function productToTimeSeries(p) result(series)
      !! Filter, conjugate-symmetric rebuild, inverse FFT, NLT undamping.
      complex(dp), intent(in) :: p(:)
      real(dp) :: series(nSamples)
      series = spectrumToTimeSeries(p, filter, nBins, nSamples)
      if (nlt) series = series * exp(c * t)
    end function productToTimeSeries

  end subroutine transientResponseSources

  logical function scanSpans(scanHz, freqZeroHz, nyquistHz) result(ok)
    !! True when `scanHz` is strictly ascending and covers
    !! [freqZeroHz, nyquistHz] (relative slack 1e-9 for the round-off of a
    !! log-spaced axis end point).
    real(dp), intent(in) :: scanHz(:), freqZeroHz, nyquistHz
    integer(4) :: n

    n = size(scanHz)
    ok = n >= 2
    if (.not. ok) return
    ok = all(scanHz(2:n) > scanHz(1:n-1)) &
      .and. scanHz(1) <= freqZeroHz * (1.0_dp + 1.0d-9) &
      .and. scanHz(n) >= nyquistHz * (1.0_dp - 1.0d-9)
  end function scanSpans

  integer(4) function electrodeIndex(study, electrodeId) result(idx)
    !! Index of `electrodeId` in `study%structure%electrodes` — shared
    !! ordering with `study%longCurrentResults`/`transCurrentResults`
    !! (both allocated from `structure%electrodes(i)%id`, `runSweep`,
    !! Study.f90), so it doubles as the index into either. Looked up
    !! against the structure rather than the (post-sweep) results so this
    !! also works before any sweep has run.
    !!
    !! `mTupa::validateStudyReferences` already rejects an unresolvable
    !! `signal.observeElectrodes` entry before `transientResponse` is ever
    !! called, so the `raiseError` below is a defence-in-depth backstop,
    !! not the primary check.
    class(tStudy), intent(in) :: study
    character(len=*), intent(in) :: electrodeId

    idx = study%structure%findElectrodeIndex(electrodeId)
    if (idx == 0) call raiseError("transientResponse: electrode '" // electrodeId // "' not found")
  end function electrodeIndex

  function spectrumToTimeSeries(product, filter, nBins, nSamples) result(series)
    !! Apply the one-sided spectral filter to the response spectrum
    !! P = Σ H·X, rebuild the full spectrum by conjugate symmetry (legacy
    !! `ifourier.m` convention), and inverse-FFT to a real time series.
    !! Shared by every observe node/electrode in `transientResponseSources`.
    complex(dp), intent(in) :: product(:)
    !! One-sided response spectrum H(f)·X(f) (summed over sources), size nBins
    real(dp), intent(in) :: filter(:)
    !! One-sided filter (band-edge Tukey × spectral window), size nBins
    integer(4), intent(in) :: nBins, nSamples
    real(dp) :: series(nSamples)
    complex(dp), allocatable :: fullSpectrum(:)
    integer(4) :: k, sourceBin

    allocate(fullSpectrum(nSamples))
    fullSpectrum(1:nBins - 1) = product(1:nBins - 1) * filter(1:nBins - 1)
    fullSpectrum(1) = cmplx(real(fullSpectrum(1), dp), 0.0_dp, kind=dp)
    do k = nBins, nSamples
      sourceBin = 2 * nBins - k
      fullSpectrum(k) = conjg(product(sourceBin)) * filter(sourceBin)
    end do

    call fftInverse(fullSpectrum)
    series = real(fullSpectrum, dp)
  end function spectrumToTimeSeries

end module mTransient
