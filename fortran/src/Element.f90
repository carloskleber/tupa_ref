module mElement
  use mNode
  use mMaterial
  use mElectrode
  implicit none
  private

  type, abstract, public :: tElement
    character(len=256) id
    !! Element identifier
    class(tMaterial), allocatable :: material
    integer(4) nNodes
    integer(4) nSegments
    real(8) radius
    !! Default radius for each segment
    contains
      procedure(element_interface), deferred :: init
      procedure(element_interface), deferred :: assemble
  end type

  abstract interface
    subroutine element_interface(this)
      import :: tElement
      class(tElement), intent(inout) :: this
      ! Additional arguments can be defined in specific implementations
    end subroutine element_interface
  end interface

  type, extends(tElement), public :: tLine
    !! Line element - straight conductor between two nodes with a number of equally spaced electrodes.
    type(tNode), pointer :: nodeStart => null()
    !! Pointer to start node
    type(tNode), pointer :: nodeEnd => null()
    !! Pointer to end node
  contains
    procedure :: init => initLine
    procedure :: assemble => assembleLine
  end type

contains

  subroutine initLine(this, nodeStart, nodeEnd, radius, nSegments, material, id)
    class(tLine), intent(inout) :: this
    type(tNode), target :: nodeStart, nodeEnd
    real(8), intent(in) :: radius
    integer(4), intent(in) :: nSegments
    class(tMaterial), intent(in) :: material
    character(len=*), intent(in) :: id

    real(8) :: dx, dy, dz

    this%radius = radius
    this%nSegments = nSegments
    this%nNodes = 2
    this%material = material
    this%id = id

    ! Set node pointers
    this%nodeStart => nodeStart
    this%nodeEnd => nodeEnd
  end subroutine initLine

  subroutine assembleLine(this, nodes, electrodes, idNodeStart, idNodeEnd, mainNodes, materials)
    class(tLine), intent(inout) :: this
    type(tNode), allocatable, intent(inout) :: nodes(:)
    type(tElectrode), allocatable, intent(inout) :: electrodes(:)
    integer(4), intent(in) :: idNodeStart
    integer(4), intent(in) :: idNodeEnd
    type(tNode), allocatable, intent(inout) :: mainNodes(:)
    class(tMaterial), allocatable, intent(inout) :: materials(:)
  end subroutine assembleLine

end module
