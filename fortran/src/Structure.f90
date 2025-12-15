module mStructure
  use mNode
  use mMaterial
  use mElement
  implicit none
  private
  public :: tStructure

  type :: tStructure
    type(tNode), allocatable :: nodes(:)
    type(tElement), allocatable :: elements(:)
    class(tMaterial), allocatable :: materials(:)
    class(tMaterial), allocatable :: soil

    !! Dynamic array of nodes
    integer :: nodeCount = 0
    !! Current number of nodes
    integer :: elementCount = 0
    !! Current number of elements
    integer :: materialCount = 0
    !! Current number of materials
  contains
    procedure :: addNode => addNodeToStructure
    procedure :: getNodeCount => getNodeCountStructure
    procedure :: addElement => addElementToStructure
    procedure :: getElementCount => getElementCountStructure
    procedure :: addMaterial => addMaterialToStructure
    procedure :: getMaterialCount => getMaterialCountStructure
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

  subroutine addElementToStructure(this, element)
    class(tStructure), intent(inout) :: this
    type(tElement), intent(in) :: element
    type(tElement), allocatable :: temp(:)
    integer :: newSize

    if (.not. allocated(this%elements)) then
      ! First allocation: initial capacity of 10
      allocate(this%elements(10))
      this%elementCount = 0
    end if

    if (this%elementCount >= size(this%elements)) then
      ! Array is full, expand it (double the size)
      newSize = size(this%elements) * 2
      allocate(temp(newSize))
      temp(1:this%elementCount) = this%elements(1:this%elementCount)
      deallocate(this%elements)
      allocate(this%elements(newSize))
      this%elements(1:this%elementCount) = temp(1:this%elementCount)
      deallocate(temp)
    end if

    ! Add new element
    this%elementCount = this%elementCount + 1
    this%elements(this%elementCount) = element
  end subroutine addElementToStructure

  function getElementCountStructure(this) result(count)
    class(tStructure), intent(in) :: this
    integer :: count

    if (allocated(this%elements)) then
      count = this%elementCount
    else
      count = 0
    end if
  end function getElementCountStructure

  subroutine addMaterialToStructure(this, material)
    class(tStructure), intent(inout) :: this
    class(tMaterial), intent(in) :: material
    class(tMaterial), allocatable :: temp(:)
    integer :: newSize

    if (.not. allocated(this%materials)) then
      ! First allocation: initial capacity of 10
      allocate(this%materials(10), source=material)
      this%materialCount = 0
    end if

    if (this%materialCount >= size(this%materials)) then
      ! Array is full, expand it (double the size)
      newSize = size(this%materials) * 2
      allocate(temp(newSize), source=material)
      temp(1:this%materialCount) = this%materials(1:this%materialCount)
      deallocate(this%materials)
      allocate(this%materials(newSize), source=material)
      this%materials(1:this%materialCount) = temp(1:this%materialCount)
      deallocate(temp)
    end if

    ! Add new material
    this%materialCount = this%materialCount + 1
    this%materials(this%materialCount) = material
  end subroutine addMaterialToStructure

  function getMaterialCountStructure(this) result(count)
    class(tStructure), intent(in) :: this
    integer :: count

    if (allocated(this%materials)) then
      count = this%materialCount
    else
      count = 0
    end if
  end function getMaterialCountStructure

end module mStructure

