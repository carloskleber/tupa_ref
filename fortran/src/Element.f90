module mElement
  use mNode
  use mMaterial
  use mElectrode
  implicit none
  private

  public :: tElement, tLine

  type, abstract :: tElement
    character(len=256) id
    !! Element identifier
    class(tMaterial), allocatable :: material
    type(tNode), allocatable :: nodes(:)
    type(tElectrode), allocatable :: electrodes(:)
    integer(4) nNodes
    !! Number of nodes (main and internal ones)
    integer(4) nElectrodes
    !! Number of electrodes
    real(8) radius
    !! Default radius for each segment (m)
    contains
      procedure(assemble_interface), deferred :: assemble
  end type

  abstract interface  
    subroutine assemble_interface(this, nodes, electrodes, materials)
      import :: tElement, tNode, tElectrode, tMaterial
      class(tElement), intent(inout) :: this
      type(tNode), pointer, intent(inout) :: nodes(:)
      !! Node array from Structure
      type(tElectrode), pointer, intent(inout) :: electrodes(:)
      !! Electrode array from Structure
      class(tMaterial), pointer, intent(in) :: materials(:)
      !! Materials array from Structure
    end subroutine assemble_interface
  end interface

  type, extends(tElement), public :: tLine
    !! Line element - straight conductor between two nodes with a number of equally spaced electrodes.
    character(256) :: idNodeStart, idNodeEnd, idMaterial
    type(tNode), pointer :: nodeStart => null()
    !! Pointer to start node
    type(tNode), pointer :: nodeEnd => null()
    !! Pointer to end node
  contains
    procedure :: assemble => assembleLine
  end type
contains
  !! Each derived type should have a specific constructor, and a general interface to link the pointers and assemble.

  function newElementLine(id, idNodeStart, idNodeEnd, radius, nElectrodes, idMaterial) result(this)
    !! Initialize a tLine element. 
    !! At the moment all external references are by string ids. After initialization, the Structure object will link the pointers.
    implicit none
    class(tLine), allocatable :: this
    character(len=*), intent(in) :: id
    character(len=*), intent(in) :: idNodeStart, idNodeEnd, idMaterial
    !! Identifiers to start and end nodes
    real(8), intent(in) :: radius
    integer(4), intent(in) :: nElectrodes

    allocate(this)
    this%radius = radius
    this%nElectrodes = nElectrodes
    this%nNodes = nElectrodes + 1
    this%id = id
    this%idNodeStart = idNodeStart
    this%idNodeEnd = idNodeEnd
    this%idMaterial = idMaterial
  end function newElementLine

  subroutine assembleLine(this, nodes, electrodes, materials)
    !! Assemble the line element: create electrodes and link nodes.
    implicit none
    class(tLine), intent(inout) :: this
    type(tNode), pointer, intent(inout) :: nodes(:)
    type(tElectrode), pointer, intent(inout) :: electrodes(:)
    class(tMaterial), pointer, intent(in) :: materials(:)
  end subroutine assembleLine

end module
