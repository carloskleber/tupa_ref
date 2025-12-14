module mMaterial
  implicit none
  private

  type, abstract, public :: tMaterial
    real(8) epsilon
    real(8) mu
    real(8) sigma
    complex(8) propagationConstant
  contains
    procedure(material_interface), deferred :: calcPropagationConstant
  end type tMaterial

  abstract interface
    subroutine material_interface(this, omega)
      import :: tMaterial
      class(tMaterial), intent(inout) :: this
      real(8), intent(in) :: omega
    end subroutine material_interface
  end interface

  type, extends(tMaterial), public :: tLinear
  contains
    procedure :: calcPropagationConstant => calcPropagationConstant_linear
  end type tLinear

  type, extends(tMaterial), public :: tSoilFreq
  contains
    procedure :: calcPropagationConstant => calcPropagationConstant_freq
  end type tSoilFreq

contains

  subroutine calcPropagationConstant_linear(this, omega)
    class(tLinear), intent(inout) :: this
    real(8), intent(in) :: omega
    this%propagationConstant = sqrt(cmplx(this%mu * this%epsilon * omega * omega, this%mu * this%sigma * omega, kind=8))
  end subroutine calcPropagationConstant_linear

  subroutine calcPropagationConstant_freq(this, omega)
    class(tSoilFreq), intent(inout) :: this
    real(8), intent(in) :: omega
    this%propagationConstant = sqrt(cmplx(this%mu * this%epsilon * omega * omega, this%mu * this%sigma * omega, kind=8))
  end subroutine calcPropagationConstant_freq

end module mMaterial
