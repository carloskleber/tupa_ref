module mResult
  use mNode
  use mElement
  use mElectrode
  implicit none

  type, abstract :: tResult
    !! Base type for different result types
    private
    character(256) :: id
    type(tNode), pointer :: nodes(:)
    !! Pointer to array of nodes for which results are stored
    class(tElement), pointer :: elements(:)
    !! Pointer to array of elements for which results are stored
    type(tElectrode), pointer :: electrodes(:)
    !! Pointer to array of electrodes for which results are stored
    real(8), pointer :: omega(:)
    !! Pointer to array of angular frequencies
  contains
    procedure(alloc_interface), deferred :: alloc
  end type tResult

  abstract interface
    subroutine alloc_interface(this, nodes, elements, electrodes, omega)
      import :: tResult, tNode, tElement, tElectrode
      class(tResult), intent(inout) :: this
      type(tNode), pointer, intent(inout) :: nodes(:)
      class(tElement), pointer, intent(inout) :: elements(:)
      type(tElectrode), pointer, intent(inout) :: electrodes(:)
      real(8), pointer, intent(inout) :: omega(:)
    end subroutine alloc_interface
  end interface

  type, extends(tResult) :: tVoltages
    private
    complex(8), allocatable :: voltages(:,:)
  contains
    procedure :: alloc => allocVoltages
  end type tVoltages

  interface tVoltages
    module procedure newResultVoltages
  end interface

  type, extends(tResult) :: tLongCurrents
    private
    complex(8), allocatable :: currents(:,:)
  contains
    procedure :: alloc => allocLongCurrents
  end type tLongCurrents

  interface tLongCurrents
    module procedure newResultLongCurrents
  end interface

  type, extends(tResult) :: tTransCurrents
    private
    complex(8), allocatable :: currents(:,:)
  contains
    procedure :: alloc => allocTransCurrents
  end type tTransCurrents

  interface tTransCurrents
    module procedure newResultTransCurrents
  end interface

contains
  function newResultVoltages(id) result(this)
    !! Constructor for tVoltages result type
    character(len=*), intent(in) :: id
    type(tVoltages) :: this

    this%id = id
  end function newResultVoltages

  function newResultLongCurrents(id) result(this)
    !! Constructor for tLongCurrents result type
    character(len=*), intent(in) :: id
    type(tLongCurrents) :: this

    this%id = id
  end function newResultLongCurrents

  function newResultTransCurrents(id) result(this)
    !! Constructor for tTransCurrents result type
    character(len=*), intent(in) :: id
    type(tTransCurrents) :: this

    this%id = id
  end function newResultTransCurrents

  subroutine allocVoltages(this, nodes, elements, electrodes, omega)
    !! Allocate arrays for voltages result
    class(tVoltages), intent(inout) :: this
    type(tNode), pointer, intent(inout) :: nodes(:)
    class(tElement), pointer, intent(inout) :: elements(:)
    type(tElectrode), pointer, intent(inout) :: electrodes(:)
    real(8), pointer, intent(inout) :: omega(:)

    allocate(this%voltages(size(nodes), size(omega)))
  end subroutine allocVoltages

  subroutine allocLongCurrents(this, nodes, elements, electrodes, omega)
    !! Allocate arrays for longitudinal currents result
    class(tLongCurrents), intent(inout) :: this
    type(tNode), pointer, intent(inout) :: nodes(:)
    class(tElement), pointer, intent(inout) :: elements(:)
    type(tElectrode), pointer, intent(inout) :: electrodes(:)
    real(8), pointer, intent(inout) :: omega(:)

    allocate(this%currents(size(electrodes), size(omega)))
  end subroutine allocLongCurrents

  subroutine allocTransCurrents(this, nodes, elements, electrodes, omega)
    !! Allocate arrays for transverse currents result
    class(tTransCurrents), intent(inout) :: this
    type(tNode), pointer, intent(inout) :: nodes(:)
    class(tElement), pointer, intent(inout) :: elements(:)
    type(tElectrode), pointer, intent(inout) :: electrodes(:)
    real(8), pointer, intent(inout) :: omega(:)

    allocate(this%currents(size(electrodes), size(omega)))
  end subroutine allocTransCurrents
end module mResult
