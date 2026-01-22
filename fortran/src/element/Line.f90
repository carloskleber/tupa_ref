module mElementLine
  use mElement
  use mStructure
  use mNode
  use mMaterial
  use mElectrode
  use mCtes, only: newl
  implicit none
  private

  public :: newElementLine

  type, extends(tElement), public :: tLine
    !! Line element - straight conductor between two nodes with a number of equally spaced electrodes.
    character(len=256) :: idNodeStart, idNodeEnd, idMaterial
    type(tNode), pointer :: nodeStart => null()
    !! Pointer to start node
    type(tNode), pointer :: nodeEnd => null()
    !! Pointer to end node
  contains
    procedure :: assemble => assembleLine
    procedure :: report => reportLine
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

  subroutine reportLine(this, str)
    !! Generate a string report of the line element.
    implicit none
    class(tLine), intent(in) :: this
    character(:), allocatable, intent(inout) :: str
    character(len=128) :: buf
    integer :: i

    ! Header line
    str = str // "Element ID: " // trim(this%id) // &
      ", Material: " // trim(this%idMaterial)

    write(buf, '(I0)') this%nNodes
    str = str // ", Nodes: " // trim(buf)

    write(buf, '(F0.3)') this%radius
    str = str // ", Radius: " // trim(buf) // " m" // newl

    ! Electrodes
    str = str // new_line('a') // "  Electrodes:"
    if (allocated(this%electrodes)) then
      do i = 1, this%nElectrodes
        write(buf, '(I0)') i
        str = str // newl // "    Electrode " // trim(buf) // &
          ": ID = " // trim(this%electrodes(i)%id)
      end do
    else
      str = str // " None" // newl
    end if
  end subroutine reportLine
end module
