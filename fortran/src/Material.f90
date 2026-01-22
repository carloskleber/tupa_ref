module mMaterial
  use mCtes, only: PI, MU0, EPSILON0, newl
  implicit none
  private

  public :: newMaterialLinear, newMaterialFreq

  type, abstract, public :: tMaterial
    character(len=256) id
    !! Material identifier
    real(8) mur
    !! Relative permeability
    complex(8) propagationConstant
  contains
    procedure(material_interface), deferred :: calcPropagationConstant
    procedure(print_interface), deferred :: report
  end type tMaterial

  abstract interface
    subroutine material_interface(this, omega)
      import :: tMaterial
      class(tMaterial), intent(inout) :: this
      real(8), intent(in) :: omega
    end subroutine material_interface
  end interface

  abstract interface
    subroutine print_interface(this, str)
      import :: tMaterial
      class(tMaterial), intent(in) :: this
      character(:), allocatable, intent(inout) :: str
    end subroutine print_interface
  end interface

  type, extends(tMaterial), public :: tLinear
    real(8) epsilonr
    !! Relative permittivity
    real(8) sigma
    !! Conductivity (S/m)
  contains
    procedure :: calcPropagationConstant => calcPropagationConstant_linear
    procedure :: report => report_linear
  end type tLinear

  type, extends(tMaterial), public :: tSoilFreq
    real(8) alpha0
    real(8) kr
  contains
    procedure :: calcPropagationConstant => calcPropagationConstant_freq
    procedure :: report => report_freq
  end type tSoilFreq
contains
  function newMaterialLinear(id, epsilonr, mur, sigma) result(this)
    !! Constructor function to create a tLinear material instance
    !! @param[in] epsilonr relative Permittivity
    !! @param[in] mur relative Permeability
    !! @param[in] sigma Conductivity (S/m)
    !! @return Instantiated tLinear object
    character(len=*), intent(in) :: id
    real(8), intent(in) :: epsilonr, mur, sigma
    type(tLinear) :: this

    this%id = id
    this%epsilonr = epsilonr
    this%mur = mur
    this%sigma = sigma
    this%propagationConstant = cmplx(0.0d0, 0.0d0, kind=8)
  end function newMaterialLinear

  function newMaterialFreq(id, mur, alpha0, kr) result(this)
    !! Constructor function to create a tSoilFreq material instance
    !! @param[in] mur Relative permeability
    !! @param[in] alpha0 Frequency-dependent parameter
    !! @param[in] kr Frequency-dependent parameter
    !! @return Instantiated tSoilFreq object
    character(len=*), intent(in) :: id
    real(8), intent(in) :: mur, alpha0, kr
    type(tSoilFreq) :: this

    this%id = id
    this%mur = mur
    this%alpha0 = alpha0
    this%kr = kr
    this%propagationConstant = cmplx(0.0d0, 0.0d0, kind=8)
  end function newMaterialFreq

  subroutine calcPropagationConstant_linear(this, omega)
    !! Calculate the propagation constant for linear materials
    class(tLinear), intent(inout) :: this
    real(8), intent(in) :: omega
    this%propagationConstant = sqrt(cmplx(this%mur * MU0 * this%epsilonr * EPSILON0 * omega * omega, &
      this%mur * MU0 * this%sigma * omega, kind=8))
  end subroutine calcPropagationConstant_linear

  subroutine calcPropagationConstant_freq(this, omega)
    !! Calculate the propagation constant for frequency-dependent soil materials
    class(tSoilFreq), intent(inout) :: this
    real(8), intent(in) :: omega
    real(8) :: ki

    ki = this%kr * tan(0.5 * PI * this%alpha0)

    this%propagationConstant = cmplx(0, 0, kind=8) !! TODO temporary initialization
  end subroutine calcPropagationConstant_freq

  subroutine report_linear(this, str)
    class(tLinear), intent(in) :: this
    character(:), allocatable, intent(inout) :: str

    str = str // "linear material" // newl
  end subroutine report_linear

  subroutine report_freq(this, str)
    class(tSoilFreq), intent(in) :: this
    character(:), allocatable, intent(inout) :: str

    str = str // "frequency-dependent material" // newl
  end subroutine report_freq

end module mMaterial
