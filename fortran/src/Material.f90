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
  use mCtes, only: PI, MU0, EPSILON0, newl
  implicit none
  private

  public :: newMaterialLinear, newMaterialPortela

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
    procedure(material_interface), deferred :: calcPropagationConstant
    !! Compute and store `propagationConstant` for the given ω.
    procedure(print_interface), deferred :: report
    !! Append a human-readable description to the accumulator string `str`.
  end type tMaterial

  abstract interface
    subroutine material_interface(this, omega)
      !! Interface for frequency-dependent propagation-constant update.
      import :: tMaterial
      class(tMaterial), intent(inout) :: this
      real(8), intent(in) :: omega
      !! Angular frequency ω (rad/s)
    end subroutine material_interface
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
    procedure :: calcPropagationConstant => calcPropagationConstant_linear
    procedure :: report                  => report_linear
  end type tLinear

  type, extends(tMaterial), public :: tPortelaSoil
    !! Frequency-dependent soil model (Portela's minimum-phase power-law
    !! formulation — ADR 0007). Other dispersion models (Longmire-Smith,
    !! Visacro-Alipio, ...) are separate `tMaterial` subtypes, not variants
    !! of this one.
    !!
    !! Full implementation is pending; `calcPropagationConstant` is currently
    !! a placeholder that returns zero.
    real(8) :: alpha0
    !! Exponent parameter α₀ for the power-law frequency dependence
    real(8) :: kr
    !! Scaling factor kr for the loss tangent
  contains
    procedure :: calcPropagationConstant => calcPropagationConstant_freq
    procedure :: report                  => report_freq
  end type tPortelaSoil

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

  function newMaterialPortela(id, mur, alpha0, kr) result(this)
    !! Construct a `tPortelaSoil` material with Portela power-law parameters.
    character(len=*), intent(in) :: id
    !! Material identifier
    real(8), intent(in) :: mur
    !! Relative permeability μr
    real(8), intent(in) :: alpha0
    !! Power-law frequency exponent α₀
    real(8), intent(in) :: kr
    !! Loss-tangent scaling factor kr
    type(tPortelaSoil) :: this

    this%id                  = id
    this%mur                 = mur
    this%alpha0              = alpha0
    this%kr                  = kr
    this%propagationConstant = cmplx(0.0d0, 0.0d0, kind=8)
  end function newMaterialPortela

  ! ------------------------------------------------------------------
  ! calcPropagationConstant implementations
  ! ------------------------------------------------------------------

  subroutine calcPropagationConstant_linear(this, omega)
    !! Compute γ = √(μr·μ₀·(σ + jω·εr·ε₀)·jω) for a linear medium.
    class(tLinear), intent(inout) :: this
    real(8), intent(in) :: omega
    !! Angular frequency ω (rad/s)

    this%propagationConstant = sqrt(cmplx( &
      this%mur * MU0 * this%epsilonr * EPSILON0 * omega * omega, &
      this%mur * MU0 * this%sigma * omega, kind=8))
  end subroutine calcPropagationConstant_linear

  subroutine calcPropagationConstant_freq(this, omega)
    !! Compute γ for frequency-dependent soil (Portela power-law, ADR 0007).
    !!
    !! **Not yet implemented** — currently initialises to zero.
    class(tPortelaSoil), intent(inout) :: this
    real(8), intent(in) :: omega
    !! Angular frequency ω (rad/s)
    real(8) :: ki

    ki = this%kr * tan(0.5d0 * PI * this%alpha0)

    this%propagationConstant = cmplx(0.0d0, 0.0d0, kind=8) ! TODO: implement Portela power-law
  end subroutine calcPropagationConstant_freq

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
    !! Append a one-line description of the frequency-dependent material to `str`.
    class(tPortelaSoil), intent(in) :: this
    character(:), allocatable, intent(inout) :: str
    !! Accumulator string — text is appended

    str = str // "frequency-dependent material" // newl
  end subroutine report_freq

end module mMaterial
