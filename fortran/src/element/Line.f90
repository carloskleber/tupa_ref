module mElementLine
  use mElement
  use mStructure
  use mNode
  implicit none
  private

  public :: newElementLine

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
    class(tElement), allocatable :: this
    type(tLine), allocatable :: line
    character(len=*), intent(in) :: id
    character(len=*), intent(in) :: idNodeStart, idNodeEnd, idMaterial
    !! Identifiers to start and end nodes
    real(8), intent(in) :: radius
    integer(4), intent(in) :: nElectrodes

    allocate(line)
    line%radius = radius
    line%nElectrodes = nElectrodes
    line%nNodes = nElectrodes + 1
    line%id = id
    line%idNodeStart = idNodeStart
    line%idNodeEnd = idNodeEnd
    line%idMaterial = idMaterial
    call move_alloc(line, this)
  end function newElementLine

  subroutine assembleLine(this, structure)
    !! Assemble the line element: create electrodes and link nodes.
    implicit none
    class(tLine), intent(inout) :: this
    class(*), intent(inout) :: structure

    select type (structure)
    type is (tStructure)
      !...
    end select
  end subroutine assembleLine

end module
