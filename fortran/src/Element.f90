module mElement
  use stdlib_strings
  use mNode
  implicit none

  type :: tElement
    type(string_type) id
    !! Element identifier
    integer(4) nNodes
    integer(4) nSegments
    real(8) radius
    !! Default radius for each segment
  end type

  type, extends(tElement) :: tLine
    !! Line element
    type(tNode), pointer :: nodeStart => null()
    !! Pointer to start node
    type(tNode), pointer :: nodeEnd => null()
    !! Pointer to end node
  contains
    procedure :: init => initLine
  end type

contains

  subroutine initLine(this, nodeStart, nodeEnd, radius, nSegments, id)
    class(tLine), intent(inout) :: this
    type(tNode), target :: nodeStart, nodeEnd
    real(8), intent(in) :: radius
    integer(4), intent(in) :: nSegments
    character(len=*), intent(in), optional :: id

    real(8) :: dx, dy, dz

    ! Set basic properties
    this%radius = radius
    this%nSegments = nSegments
    this%nNodes = 2

    ! Set node pointers
    this%nodeStart => nodeStart
    this%nodeEnd => nodeEnd

    ! Calculate length from node coordinates
    dx = nodeEnd%x - nodeStart%x
    dy = nodeEnd%y - nodeStart%y
    dz = nodeEnd%z - nodeStart%z
    this%length = sqrt(dx*dx + dy*dy + dz*dz)

    ! Set identifier
    if (present(id)) then
      this%id = id
    end if
  end subroutine initLine

end module
