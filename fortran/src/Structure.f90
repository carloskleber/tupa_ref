module mStructure
  use mNode
  use mElectrode
  use mMaterial
  use mElement
  implicit none
  private

  type :: tElementNode
    !! Linked list node for elements
    class(tElement), allocatable :: elem
    type(tElementNode), pointer :: next => null()
  end type

  type, public :: tStructure
    type(tNode), allocatable :: nodes(:)
    !! Array of main nodes
    type(tElectrode), allocatable :: electrodes(:)
    !! Array of electrodes
    type(tElementNode), pointer :: elements => null()
    !! Array of elements
    class(tMaterial), allocatable :: materials(:)
    !! Array of conductive materials to be applied in elements
    class(tMaterial), allocatable :: soil
    !! Soil electrical properties.
    type(tLinear) :: air

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
    procedure :: getElement => getElementFromStructure
    procedure :: getElementCount => getElementCountStructure
    procedure :: addMaterial => addMaterialToStructure
    procedure :: getMaterialCount => getMaterialCountStructure
    final :: finalizeStructure
  end type tStructure

contains

  subroutine addNodeToStructure(this, node)
    class(tStructure), intent(inout) :: this
    type(tNode), intent(in) :: node
    type(tNode), allocatable :: temp(:)
    integer :: newSize

    if (.not. allocated(this%nodes)) then
      ! Preallocation: initial capacity of 10
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

    count = this%nodeCount
  end function getNodeCountStructure

  subroutine addElementToStructure(this, element)
    class(tStructure), intent(inout) :: this
    class(tElement), allocatable, intent(inout) :: element
    type(tElementNode), pointer :: node

    allocate(node)
    call move_alloc(element, node%elem)

    node%next => this%elements
    this%elements => node
    this%elementCount = this%elementCount + 1
  end subroutine addElementToStructure

  function getElementCountStructure(this) result(count)
    class(tStructure), intent(in) :: this
    integer :: count

    count = this%elementCount
  end function getElementCountStructure

  function getElementFromStructure(this, index) result(element)
    !! Retrieve pointer to element by index
    class(tStructure), intent(in) :: this
    integer, intent(in) :: index
    class(tElement), pointer :: element
    type(tElementNode), pointer :: p
    integer :: i

    p => this%elements
    do i = 1, index - 1
      if (associated(p)) then
        p => p%next
      else
        exit
      end if
    end do

    if (associated(p)) then
      element => p%elem
    else
      nullify(element)
    end if
  end function getElementFromStructure

  subroutine addMaterialToStructure(this, material)
    class(tStructure), intent(inout) :: this
    class(tMaterial), intent(in) :: material
    class(tMaterial), allocatable :: temp(:)
    integer :: newSize
    integer :: i

    if (.not. allocated(this%materials)) then
      ! First allocation: initial capacity of 10
      allocate(this%materials(10), source=material)
      this%materialCount = 0
    end if

    if (this%materialCount >= size(this%materials)) then
      ! Array is full, expand it (double the size)
      newSize = size(this%materials) * 2
      allocate(temp(newSize), source=material)
      do i = 1, this%materialCount
        allocate(temp(i), source=this%materials(i))
      end do
      deallocate(this%materials)
      allocate(this%materials(newSize), source=material)
      do i = 1, this%materialCount
        allocate(this%materials(i), source=temp(i))
      end do
      deallocate(temp)
    end if

    ! Add new material
    this%materialCount = this%materialCount + 1
    allocate(this%materials(this%materialCount), source=material)
  end subroutine addMaterialToStructure

  function getMaterialCountStructure(this) result(count)
    class(tStructure), intent(in) :: this
    integer :: count

    count = this%materialCount
  end function getMaterialCountStructure

  subroutine assembleStructure(this)
    !! Link main nodes and materials in each element. Create secondary nodes and electrodes.
    class(tStructure), intent(inout) :: this
    type(tElementNode), pointer :: p

    p => this%elements
    do while (associated(p))
      call p%elem%assemble(this)
      p => p%next
    end do
  end subroutine assembleStructure

  subroutine finalizeStructure(this)
    type(tStructure), intent(inout) :: this
    type(tElementNode), pointer :: p, next

    if (allocated(this%nodes)) then
      deallocate(this%nodes)
    end if
    if (allocated(this%electrodes)) then
      deallocate(this%electrodes)
    end if
    p => this%elements
    do while (associated(p))
      next => p%next
      deallocate(p%elem)
      deallocate(p)
      p => next
    end do
    nullify(this%elements)
    if (allocated(this%materials)) then
      deallocate(this%materials)
    end if
    this%materialCount = 0
    this%nodeCount = 0
    this%elementCount = 0
  end subroutine finalizeStructure
end module mStructure
