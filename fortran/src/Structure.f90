module mStructure
  use mNode
  use mMaterial
  implicit none
  private
  public :: tStructure

  type :: tStructure
    type(tNode), allocatable :: nodes(:)
    class(tMaterial), allocatable :: soil

    !! Dynamic array of nodes
    integer :: nodeCount = 0
    !! Current number of nodes
  contains
    procedure :: addNode => addNodeToStructure
    procedure :: getNodeCount => getNodeCountStructure
  end type tStructure

contains

  subroutine addNodeToStructure(this, node)
    class(tStructure), intent(inout) :: this
    type(tNode), intent(in) :: node
    type(tNode), allocatable :: temp(:)
    integer :: newSize

    if (.not. allocated(this%nodes)) then
      ! First allocation: initial capacity of 10
      allocate(this%nodes(10))
      this%nodeCount = 0
    end if

    if (this%nodeCount >= size(this%nodes)) then
      ! Array is full, expand it (double the size)
      newSize = size(this%nodes) * 2
      allocate(temp(newSize))
      temp(1:this%nodeCount) = this%nodes(1:this%nodeCount)
      deallocate(this%nodes)
      allocate(this%nodes(newSize))
      this%nodes(1:this%nodeCount) = temp(1:this%nodeCount)
      deallocate(temp)
    end if

    ! Add new node
    this%nodeCount = this%nodeCount + 1
    this%nodes(this%nodeCount) = node
  end subroutine addNodeToStructure

  function getNodeCountStructure(this) result(count)
    class(tStructure), intent(in) :: this
    integer :: count

    if (allocated(this%nodes)) then
      count = this%nodeCount
    else
      count = 0
    end if
  end function getNodeCountStructure

end module mStructure
