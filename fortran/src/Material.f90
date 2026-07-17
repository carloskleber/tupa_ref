module mMaterial
  !! Electrical material models for conductors and soil media.
  !!
  !! The abstract base type `tMaterial` declares the common interface:
  !! every material must be able to compute its complex propagation constant
  !! γ = α + jβ (stored in `propagationConstant`) at a given angular frequency
  !! ω.  Two concrete types are provided:
  !!
  !! - `tLinear`: constant-parameter medium (air, simple soil, copper, …).
  !! - `tPortelaSoil`: frequency-dependent soil using Portela's minimum-phase
  !!   power-law model (currently a placeholder — full implementation is
  !!   planned). Per ADR 0007, this is the first of several `tMaterial`
  !!   dispersive-soil subtypes; further models (e.g. `tLongmireSmithSoil`)
  !!   are added as siblings, each named after its original reference —
  !!   never folded into this one under a generic name.
  !! - `tVisacroAlipioSoil`: frequency-dependent soil using the causal
  !!   (minimum-phase) model of Alipio & Visacro (theory.md §7, references.md
  !!   [14]), *mean* parameter set.
  use mCtes, only: PI, MU0, EPSILON0, newl
  implicit none
  private

  public :: newMaterialLinear, newMaterialPortela, newMaterialVisacroAlipio

  ! ------------------------------------------------------------------
  ! Abstract base type
  ! ------------------------------------------------------------------

  type, abstract, public :: tMaterial
    !! Abstract base for all electrical material models.
    character(len=256) :: id
    !! Material identifier string
    real(8) :: mur
    !! Relative permeability μr (dimensionless)
    complex(8) :: propagationConstant
    !! Complex propagation constant γ = α + jβ (rad/m) at the last computed ω.
    !! Updated by `calcPropagationConstant`.
  contains
    procedure(admittance_interface), deferred :: admittance
    !! Complex immittance W(ω) = σ(ω) + jωε(ω) (theory.md §7). The one
    !! quantity that differs between material models — every model shares
    !! the same γ = √(jωμW(ω)) relation, computed once by
    !! `calcPropagationConstant` below.
    procedure :: calcPropagationConstant => calcPropagationConstant_base
    !! Compute and store `propagationConstant` for the given ω, via `admittance`.
    procedure(print_interface), deferred :: report
    !! Append a human-readable description to the accumulator string `str`.
  end type tMaterial

  abstract interface
    function admittance_interface(this, omega) result(w)
      !! Interface for the frequency-dependent complex immittance W(ω).
      import :: tMaterial
      class(tMaterial), intent(in) :: this
      real(8), intent(in) :: omega
      !! Angular frequency ω (rad/s)
      complex(8) :: w
      !! W(ω) = σ(ω) + jωε(ω) (S/m)
    end function admittance_interface
  end interface

  abstract interface
    subroutine print_interface(this, str)
      !! Interface for building a human-readable report string.
      import :: tMaterial
      class(tMaterial), intent(in) :: this
      character(:), allocatable, intent(inout) :: str
      !! Accumulator string — text is appended (not replaced)
    end subroutine print_interface
  end interface

  ! ------------------------------------------------------------------
  ! Concrete types
  ! ------------------------------------------------------------------

  type, extends(tMaterial), public :: tLinear
    !! Constant-parameter (frequency-independent) isotropic medium.
    !!
    !! The propagation constant is γ = √(jωμ(σ + jωε)).
    real(8) :: epsilonr
    !! Relative permittivity εr (dimensionless)
    real(8) :: sigma
    !! Electrical conductivity σ (S/m)
  contains
    procedure :: admittance => admittance_linear
    procedure :: report     => report_linear
  end type tLinear

  type, extends(tMaterial), public :: tPortelaSoil
    !! Frequency-dependent soil model — Lima–Portela minimum-phase power-law
    !! formulation, ADR 0007, reference frequency ω₀ = 2π·1 MHz
    !! (theory.md §7). Other dispersion models (Longmire-Smith,
    !! Visacro-Alipio, ...) are separate `tMaterial` subtypes, not variants
    !! of this one.
    real(8) :: sigma0
    !! Low-frequency (DC) conductivity σ₀ (S/m)
    real(8) :: alpha0
    !! Exponent parameter α₀ for the power-law frequency dependence
    real(8) :: kr
    !! Dispersion magnitude Δᵢ at ω₀ = 2π·1 MHz (S/m) — legacy Matlab `kr`
    !! values are referenced to ω₀ = 1 rad/s and must be converted before
    !! reuse (ADR 0007), never copied verbatim.
  contains
    procedure :: admittance => admittance_freq
    procedure :: report     => report_freq
  end type tPortelaSoil

  type, extends(tMaterial), public :: tVisacroAlipioSoil
    !! Frequency-dependent soil model — causal (minimum-phase) model of
    !! Alipio & Visacro (theory.md §7, references.md [14]), *mean*
    !! parameter set. The only free parameter is `sigma0`; the dispersion
    !! exponent, high-frequency permittivity and the h(sigma0) fit are the
    !! fixed constants of the mean curve (the *relatively conservative* /
    !! *conservative* bounding curves of [14] are not implemented).
    real(8) :: sigma0
    !! Low-frequency (100 Hz) conductivity σ₀ (S/m)
  contains
    procedure :: admittance => admittance_alipio
    procedure :: report     => report_alipio
  end type tVisacroAlipioSoil

contains

  ! ------------------------------------------------------------------
  ! Constructors
  ! ------------------------------------------------------------------

  function newMaterialLinear(id, epsilonr, mur, sigma) result(this)
    !! Construct a `tLinear` material with the given constant parameters.
    character(len=*), intent(in) :: id
    !! Material identifier
    real(8), intent(in) :: epsilonr
    !! Relative permittivity εr
    real(8), intent(in) :: mur
    !! Relative permeability μr
    real(8), intent(in) :: sigma
    !! Conductivity σ (S/m)
    type(tLinear) :: this

    this%id                  = id
    this%epsilonr            = epsilonr
    this%mur                 = mur
    this%sigma               = sigma
    this%propagationConstant = cmplx(0.0d0, 0.0d0, kind=8)
  end function newMaterialLinear

  function newMaterialPortela(id, mur, sigma0, alpha0, kr) result(this)
    !! Construct a `tPortelaSoil` material with Lima–Portela parameters
    !! (ADR 0007, ω₀ = 2π·1 MHz).
    character(len=*), intent(in) :: id
    !! Material identifier
    real(8), intent(in) :: mur
    !! Relative permeability μr
    real(8), intent(in) :: sigma0
    !! Low-frequency conductivity σ₀ (S/m)
    real(8), intent(in) :: alpha0
    !! Power-law frequency exponent α₀
    real(8), intent(in) :: kr
    !! Dispersion magnitude Δᵢ at ω₀ = 2π·1 MHz (S/m)
    type(tPortelaSoil) :: this

    this%id                  = id
    this%mur                 = mur
    this%sigma0              = sigma0
    this%alpha0              = alpha0
    this%kr                  = kr
    this%propagationConstant = cmplx(0.0d0, 0.0d0, kind=8)
  end function newMaterialPortela

  function newMaterialVisacroAlipio(id, mur, sigma0) result(this)
    !! Construct a `tVisacroAlipioSoil` material (Alipio & Visacro *mean*
    !! parameter set, theory.md §7) from the 100 Hz conductivity alone.
    character(len=*), intent(in) :: id
    !! Material identifier
    real(8), intent(in) :: mur
    !! Relative permeability μr
    real(8), intent(in) :: sigma0
    !! Low-frequency (100 Hz) conductivity σ₀ (S/m)
    type(tVisacroAlipioSoil) :: this

    this%id                  = id
    this%mur                 = mur
    this%sigma0              = sigma0
    this%propagationConstant = cmplx(0.0d0, 0.0d0, kind=8)
  end function newMaterialVisacroAlipio

  ! ------------------------------------------------------------------
  ! calcPropagationConstant: shared by every tMaterial subtype
  ! ------------------------------------------------------------------

  subroutine calcPropagationConstant_base(this, omega)
    !! Compute γ = √(jωμ·W(ω)), Re γ ≥ 0 (theory.md §2, engineering
    !! convention e^{+jωt}). W(ω) comes from the deferred `admittance`
    !! function, so this formula is shared by every material model —
    !! only W(ω) differs between a linear medium and a dispersive one.
    class(tMaterial), intent(inout) :: this
    real(8), intent(in) :: omega
    !! Angular frequency ω (rad/s)

    this%propagationConstant = sqrt(cmplx(0.0d0, omega, kind=8) &
      * this%mur * MU0 * this%admittance(omega))
  end subroutine calcPropagationConstant_base

  ! ------------------------------------------------------------------
  ! admittance implementations: W(omega) = sigma(omega) + j*omega*epsilon(omega)
  ! ------------------------------------------------------------------

  function admittance_linear(this, omega) result(w)
    !! W(ω) = σ + jωε for a constant-parameter medium (theory.md §2).
    class(tLinear), intent(in) :: this
    real(8), intent(in) :: omega
    !! Angular frequency ω (rad/s)
    complex(8) :: w

    w = cmplx(this%sigma, omega * this%epsilonr * EPSILON0, kind=8)
  end function admittance_linear

  function admittance_freq(this, omega) result(w)
    !! W(ω) for the Lima–Portela dispersive soil model (ADR 0007,
    !! theory.md §7), reference frequency ω₀ = 2π·1 MHz:
    !!
    !!     W(ω) = σ₀ + kr·[cot(πα₀/2) + j]·(ω/ω₀)^α₀
    !!
    !! As ω → 0, the power-law term vanishes and W → σ₀, i.e. this
    !! converges to a purely resistive `tLinear(epsilonr=0, sigma=σ₀)`
    !! medium (ADR 0007's required regression, pinned in test_material.f90).
    class(tPortelaSoil), intent(in) :: this
    real(8), intent(in) :: omega
    !! Angular frequency ω (rad/s)
    complex(8) :: w
    real(8), parameter :: OMEGA0 = 2.0d0 * PI * 1.0d6

    w = cmplx(this%sigma0, 0.0d0, kind=8) &
      + this%kr * cmplx(1.0d0 / tan(0.5d0 * PI * this%alpha0), 1.0d0, kind=8) &
        * (omega / OMEGA0) ** this%alpha0
  end function admittance_freq

  function admittance_alipio(this, omega) result(w)
    !! W(ω) for the Alipio-Visacro causal dispersive soil model (theory.md
    !! §7, references.md [14]), *mean* parameter set, reference frequency
    !! f0 = 1 MHz:
    !!
    !!     Delta_sigma(f) = sigma0 * h * (f/f0)^xi,  f = omega/(2*pi)
    !!     W(omega) = sigma0 + Delta_sigma(f)*[1 + j*tan(pi*xi/2)]
    !!                + j*omega*epsilon0*epsr_inf
    !!
    !! Written this way (rather than computing epsilon_r(f) as a standalone
    !! quantity and multiplying by omega afterwards) to avoid the spurious
    !! f -> 0 singularity of epsilon_r(f) alone: the reactive term
    !! omega*epsilon0*epsilon_r(f) is finite in this limit (Delta_sigma(f)
    !! -> 0 as f -> 0 since xi > 0), even though epsilon_r(f) itself diverges
    !! there. As omega -> 0, W -> sigma0, i.e. this converges to a purely
    !! resistive `tLinear(epsilonr=0, sigma=sigma0)` medium (ADR 0007's
    !! required regression, pinned in test_material.f90).
    class(tVisacroAlipioSoil), intent(in) :: this
    real(8), intent(in) :: omega
    !! Angular frequency ω (rad/s)
    complex(8) :: w
    real(8), parameter :: F0 = 1.0d6
    !! Reference frequency (Hz)
    real(8), parameter :: XI = 0.54d0
    !! Dispersion exponent (mean parameter set, [14])
    real(8), parameter :: EPSR_INF = 12.0d0
    !! High-frequency relative permittivity (mean parameter set, [14])
    real(8) :: f, h, dsigma

    f = omega / (2.0d0 * PI)
    h = 1.26d0 * (1.0d3 * this%sigma0) ** (-0.73d0)
    dsigma = this%sigma0 * h * (f / F0) ** XI

    w = cmplx(this%sigma0 + dsigma, &
              omega * EPSILON0 * EPSR_INF + dsigma * tan(0.5d0 * PI * XI), kind=8)
  end function admittance_alipio

  ! ------------------------------------------------------------------
  ! report implementations
  ! ------------------------------------------------------------------

  subroutine report_linear(this, str)
    !! Append a one-line description of the linear material to `str`.
    class(tLinear), intent(in) :: this
    character(:), allocatable, intent(inout) :: str
    !! Accumulator string — text is appended

    str = str // "linear material" // newl
  end subroutine report_linear

  subroutine report_freq(this, str)
    !! Append a one-line description of the Lima–Portela dispersive soil to `str`.
    class(tPortelaSoil), intent(in) :: this
    character(:), allocatable, intent(inout) :: str
    !! Accumulator string — text is appended
    character(len=256) :: line

    write(line, '("Lima-Portela dispersive soil ",A,": sigma0=",ES10.3, &
      &" S/m, alpha0=",F0.4,", kr=",ES10.3," S/m, mur=",F0.3)') &
      trim(this%id), this%sigma0, this%alpha0, this%kr, this%mur
    str = str // trim(line) // newl
  end subroutine report_freq

  subroutine report_alipio(this, str)
    !! Append a one-line description of the Alipio-Visacro dispersive soil to `str`.
    class(tVisacroAlipioSoil), intent(in) :: this
    character(:), allocatable, intent(inout) :: str
    !! Accumulator string — text is appended
    character(len=256) :: line

    write(line, '("Alipio-Visacro dispersive soil ",A,": sigma0=",ES10.3, &
      &" S/m (mean parameter set), mur=",F0.3)') &
      trim(this%id), this%sigma0, this%mur
    str = str // trim(line) // newl
  end subroutine report_alipio

end module mMaterial
