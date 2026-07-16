module mSignal
  !! Time-domain excitation waveforms (ROADMAP.md Phase 6 item 1).
  !!
  !! The abstract base `tSignal` declares `waveform(t)`: the injected current
  !! i(t) (A) sampled at the given time axis. Two families are ported from
  !! the legacy Matlab `+sinais` package (the model reference of record,
  !! CLAUDE.local.md): `tHeidlerSignal` (multi-term Heidler function) and
  !! `tDoubleExpSignal` (double-exponential surge, with an optional Jones
  !! correction for a zero-slope front). The Matlab reference ships several
  !! more waveforms (single exponential, impulse/step, Portela's concave
  !! model, sine) — ported "as needed" per ROADMAP.md Phase 6 item 1; not
  !! duplicated here until a case needs them.
  use mCtes, only: dp, PI
  use mError, only: raiseError
  implicit none
  private

  public :: tSignal, tHeidlerSignal, tDoubleExpSignal
  public :: newHeidlerSignal, newDoubleExpSignal, tailTaper

  type, abstract :: tSignal
    !! Abstract base for a time-domain excitation waveform.
    real(dp) :: imax
    !! Target peak amplitude (A) — concrete waveforms normalise to this.
  contains
    procedure(waveform_interface), deferred :: waveform
  end type tSignal

  abstract interface
    function waveform_interface(this, t) result(i)
      !! i(t) (A) at every instant in `t` (s), `t` measured from the start
      !! of the record (not necessarily from the physical stroke onset).
      import :: tSignal, dp
      class(tSignal), intent(in) :: this
      real(dp), intent(in) :: t(:)
      real(dp) :: i(size(t))
    end function waveform_interface
  end interface

  type, extends(tSignal) :: tHeidlerSignal
    !! Sum-of-Heidler-terms lightning current waveform (legacy
    !! `sinais.Heidler`, "Baseada na implementacao do Tony" — no published
    !! source is recorded for this specific 6-term parameter set; ported
    !! verbatim as the project's default, not independently validated
    !! against a named reference). Each term is
    !! i_k(t) = (I0_k/η_k)·(t/τ1_k)^n_k / (1 + (t/τ1_k)^n_k)·exp(-t/τ2_k),
    !! η_k = exp[-(τ1_k/τ2_k)·(n_k·τ2_k/τ1_k)^(1/n_k)]; the sum of all six
    !! terms is rescaled so its own peak equals `imax`.
    real(dp) :: i0(6), n(6), tau1(6), tau2(6)
  contains
    procedure :: waveform => heidlerWaveform
  end type tHeidlerSignal

  type, extends(tSignal) :: tDoubleExpSignal
    !! Double-exponential surge i(t) = [imax/(k·(α-β))]·(exp(-βt) - exp(-αt))
    !! (legacy `sinais.DuplaExp`/`DuplaExpJones`), α = front decay rate, β =
    !! tail decay rate, k normalises the peak to 1 at the nominal front time
    !! `tFront`. With `jones = .true.` the front term becomes
    !! exp(-(αt)²) (legacy `sinais.DuplaExpJones`, R.D. Jones 1977 — see
    !! ROADMAP.md §8 finding 6), giving zero slope at t=0 instead of the
    !! plain double-exponential's non-physical nonzero initial di/dt.
    real(dp) :: alpha, beta, tFront
    logical :: jones = .false.
  contains
    procedure :: waveform => doubleExpWaveform
  end type tDoubleExpSignal

contains

  ! =====================================================================
  ! Constructors
  ! =====================================================================

  function newHeidlerSignal(imax) result(sig)
    !! Default 6-term Heidler waveform (legacy `sinais.Heidler` parameter
    !! table), rescaled to peak `imax` (A).
    real(dp), intent(in) :: imax
    type(tHeidlerSignal) :: sig
    sig%imax = imax
    sig%i0   = [6.0_dp, 5.0_dp, 5.0_dp, 8.0_dp, 22.0_dp, 20.0_dp]
    sig%n    = [2.0_dp, 3.0_dp, 5.0_dp, 9.0_dp, 21.0_dp, 2.0_dp]
    sig%tau1 = [3.0_dp, 3.5_dp, 4.8_dp, 6.0_dp, 7.0_dp, 70.0_dp] * 1.0d-6
    sig%tau2 = [76.0_dp, 10.0_dp, 30.0_dp, 26.0_dp, 23.2_dp, 200.0_dp] * 1.0d-6
  end function newHeidlerSignal

  function newDoubleExpSignal(imax, waveformName, jones) result(sig)
    !! Double-exponential waveform, looked up by the legacy named forms
    !! (`f1_2_5`, `f1_2_50`, `f1_2_200`, `f250_2500` — front/tail times in
    !! µs) with their pre-solved α/β decay rates (legacy `sinais.DuplaExp`
    !! constructor; α/β solve a transcendental peak-time equation offline,
    !! not reproduced here). `jones = .true.` selects the Jones-corrected
    !! front (default `.false.`, matching the plain double exponential).
    real(dp), intent(in) :: imax
    character(len=*), intent(in) :: waveformName
    logical, intent(in), optional :: jones
    type(tDoubleExpSignal) :: sig

    sig%imax = imax
    if (present(jones)) sig%jones = jones

    select case (trim(waveformName))
    case ("f1_2_5")
      sig%tFront = 1.2d-6; sig%alpha = 1.25d6;    sig%beta = 2.8736d5
    case ("f1_2_50")
      sig%tFront = 1.2d-6; sig%alpha = 2.4691d6;   sig%beta = 1.4663d4
    case ("f1_2_200")
      sig%tFront = 1.2d-6; sig%alpha = 2.6247d6;   sig%beta = 3521.1d0
    case ("f250_2500")
      sig%tFront = 250.0d-6; sig%alpha = 9615.4d0;  sig%beta = 347.58d0
    case default
      call raiseError("newDoubleExpSignal: unknown waveform '" // trim(waveformName) // &
                       "' (expected f1_2_5, f1_2_50, f1_2_200 or f250_2500)")
    end select
  end function newDoubleExpSignal

  ! =====================================================================
  ! Waveform evaluation
  ! =====================================================================

  function heidlerWaveform(this, t) result(i)
    class(tHeidlerSignal), intent(in) :: this
    real(dp), intent(in) :: t(:)
    real(dp) :: i(size(t))
    real(dp) :: eta(6), peak, tp(size(t)), ratio(size(t))
    integer(4) :: k

    eta = exp(-(this%tau1 / this%tau2) * (this%n * this%tau2 / this%tau1) ** (1.0_dp / this%n))

    ! Clamp to t >= 0 before raising to a real power: a negative base with a
    ! non-integer-typed real exponent is undefined (NaN) even when the
    ! exponent's *value* is a whole number, since the compiler picks the
    ! general exp(y*log(x)) evaluation path for real**real.
    tp = max(t, 0.0_dp)

    i = 0.0_dp
    do k = 1, 6
      ratio = (tp / this%tau1(k)) ** this%n(k)
      i = i + merge((this%i0(k) / eta(k)) * ratio / (1.0_dp + ratio) * exp(-tp / this%tau2(k)), &
                    0.0_dp, t > 0.0_dp)
    end do

    peak = maxval(abs(i))
    if (peak > 0.0_dp) i = i / peak * this%imax
  end function heidlerWaveform

  function doubleExpWaveform(this, t) result(i)
    !! The legacy `sinais.DuplaExp`/`DuplaExpJones` formula is only ever
    !! sampled on t >= 0 (their time axis starts at 0), so it never masks
    !! t < 0 explicitly. This port adds an explicit t > 0 gate (current is
    !! zero before the stroke) since the formula itself is unbounded for
    !! very negative t (exp(-α·t) grows without limit) — a harmless
    !! strengthening for callers, with no effect on the t >= 0 range the
    !! legacy code actually evaluates.
    class(tDoubleExpSignal), intent(in) :: this
    real(dp), intent(in) :: t(:)
    real(dp) :: i(size(t))
    real(dp) :: k, front(size(t)), tail(size(t)), tp(size(t))

    tp = max(t, 0.0_dp)
    tail = exp(-this%beta * tp)
    if (this%jones) then
      front = exp(-(this%alpha * tp) ** 2)
      k = (exp(-this%beta * this%tFront) - exp(-(this%alpha * this%tFront) ** 2)) / (this%alpha - this%beta)
    else
      front = exp(-this%alpha * tp)
      k = (exp(-this%beta * this%tFront) - exp(-this%alpha * this%tFront)) / (this%alpha - this%beta)
    end if

    i = merge(this%imax / (k * (this%alpha - this%beta)) * (tail - front), 0.0_dp, t > 0.0_dp)
  end function doubleExpWaveform

  ! =====================================================================
  ! Pre-transform windowing
  ! =====================================================================

  function tailTaper(n) result(w)
    !! Sigmoid (complementary error function) taper applied to the last
    !! ~20% of a sampled record before an FFT, to suppress the spectral
    !! leakage of an abruptly truncated tail (legacy `sinais.Sinal.sinalt0Pad`).
    !! `w` multiplies the sampled waveform elementwise; index 1 maps to the
    !! first sample.
    integer(4), intent(in) :: n
    real(dp) :: w(n)
    real(dp) :: pos, deltan
    integer(4) :: k

    pos = 0.8_dp * real(n, dp)
    deltan = real(n, dp) / 20.0_dp
    do k = 1, n
      w(k) = 0.5_dp * erfc((real(k, dp) - pos) / deltan)
    end do
  end function tailTaper

end module mSignal
