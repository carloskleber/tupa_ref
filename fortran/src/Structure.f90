module mStructure
  !! Container for the geometric structure: nodes, elements, materials, and electrodes.
  !!
  !! `tStructure` manages all main (boundary) and secondary (internal) nodes,
  !! all elements (line, catenary, tower, …), all conductor materials, and the
  !! soil medium. Elements are stored as a linked list to permit dynamic addition.
  !! After all elements are added, call `assembleStructure` to discretise them
  !! into nodes and electrode segments that form the mesh.
  use mNode
  use mElectrode
  use mMaterial
  use mElement
  implicit none
  private

  type :: tElementNode
    !! Linked list node wrapping a polymorphic tElement pointer.
    class(tElement), allocatable :: elem
    type(tElementNode), pointer :: next => null()
  end type tElementNode

  type :: tMaterialNode
    !! Linked list node wrapping a polymorphic tMaterial pointer.
    class(tMaterial), allocatable :: material
    type(tMaterialNode), pointer :: next => null()
  end type tMaterialNode

  type, public :: tStructure
    !! Geometric structure: all nodes, elements, materials, and the soil medium.
    type(tNode), allocatable :: nodes(:)
    !! Array of boundary and internal nodes (expanded dynamically by addNode)
    type(tElectrode), allocatable :: electrodes(:)
    !! Array of all electrode segments (populated during assembly)
    type(tElementNode), pointer :: elements => null()
    !! Linked list of geometric elements waiting to be assembled
    class(tMaterialNode), pointer :: materials => null()
    !! Linked list of conductor materials
    class(tMaterial), allocatable :: soil
    !! Soil electrical properties (fixed across all frequencies)
    type(tLinear) :: air
    !! Air electrical properties (permittivity, conductivity, permeability)

    integer :: nodeCount = 0
    !! Current number of nodes in the `nodes` array
    integer :: elementCount = 0
    !! Current number of elements in the linked list
    integer :: materialCount = 0
    !! Current number of materials in the linked list
  contains
    procedure :: addNode           => addNodeToStructure
    !! Append a new node to the `nodes` array (with dynamic expansion)
    procedure :: getNodeCount      => getNodeCountStructure
    !! Return the current number of nodes
    procedure :: addElement        => addElementToStructure
    !! Append an element to the linked list
    procedure :: getElement        => getElementFromStructure
    !! Retrieve pointer to element by 1-based linked-list index
    procedure :: getElementCount   => getElementCountStructure
    !! Return the current number of elements
    procedure :: addMaterial       => addMaterialToStructure
    !! Append a material to the linked list
    procedure :: getMaterial       => getMaterialFromStructure
    !! Retrieve pointer to material by 1-based linked-list index
    procedure :: getMaterialCount  => getMaterialCountStructure
    !! Return the current number of materials
    final :: finalizeStructure
    !! Destructor: deallocate all linked lists and arrays
  end type tStructure

contains

  ! =====================================================================
  ! Node management
  ! =====================================================================

  subroutine addNodeToStructure(this, node)
    !! Append a tNode to the `nodes` array, expanding if necessary.
    !!
    !! Uses dynamic growth: preallocates 10 on first call, then doubles
    !! whenever full (doubling strategy amortizes expansion cost).
    class(tStructure), intent(inout) :: this
    type(tNode), intent(in) :: node
    !! Node to append
    type(tNode), allocatable :: temp(:)
    integer :: newSize

    if (.not. allocated(this%nodes)) then
      allocate(this%nodes(10))
      this%nodeCount = 0
    end if

    if (this%nodeCount >= size(this%nodes)) then
      newSize = size(this%nodes) * 2
      allocate(temp(newSize))
      temp(1:this%nodeCount) = this%nodes(1:this%nodeCount)
      deallocate(this%nodes)
      allocate(this%nodes(newSize))
      this%nodes(1:this%nodeCount) = temp(1:this%nodeCount)
      deallocate(temp)
    end if

    this%nodeCount = this%nodeCount + 1
    this%nodes(this%nodeCount) = node
  end subroutine addNodeToStructure

  function getNodeCountStructure(this) result(count)
    !! Return the current number of nodes.
    class(tStructure), intent(in) :: this
    integer :: count

    count = this%nodeCount
  end function getNodeCountStructure

  ! =====================================================================
  ! Element management (linked list)
  ! =====================================================================

  subroutine addElementToStructure(this, element)
    !! Append an element to the linked list.
    class(tStructure), intent(inout) :: this
    class(tElement), allocatable, intent(inout) :: element
    !! Element to add (moved, not copied)
    type(tElementNode), pointer :: node

    allocate(node)
    call move_alloc(element, node%elem)

    node%next => this%elements
    this%elements => node
    this%elementCount = this%elementCount + 1
  end subroutine addElementToStructure

  function getElementCountStructure(this) result(count)
    !! Return the current number of elements.
    class(tStructure), intent(in) :: this
    integer :: count

    count = this%elementCount
  end function getElementCountStructure

  function getElementFromStructure(this, index) result(element)
    !! Retrieve a pointer to the element at 1-based linked-list position `index`.
    !!
    !! Returns null if `index` is out of bounds.
    class(tStructure), intent(in) :: this
    integer, intent(in) :: index
    !! 1-based position in the linked list
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

  ! =====================================================================
  ! Material management (linked list)
  ! =====================================================================

  subroutine addMaterialToStructure(this, material)
    !! Append a material to the linked list.
    class(tStructure), intent(inout) :: this
    class(tMaterial), allocatable, intent(inout) :: material
    !! Material to add (moved, not copied)
    type(tMaterialNode), pointer :: node

    allocate(node)
    call move_alloc(material, node%material)

    node%next => this%materials
    this%materials => node
    this%materialCount = this%materialCount + 1
  end subroutine addMaterialToStructure

  function getMaterialFromStructure(this, index) result(material)
    !! Retrieve a pointer to the material at 1-based linked-list position `index`.
    !!
    !! Returns null if `index` is out of bounds.
    class(tStructure), intent(in) :: this
    integer, intent(in) :: index
    !! 1-based position in the linked list
    class(tMaterial), pointer :: material
    type(tMaterialNode), pointer :: p
    integer :: i

    p => this%materials
    do i = 1, index - 1
      if (associated(p)) then
        p => p%next
      else
        exit
      end if
    end do

    if (associated(p)) then
      material => p%material
    else
      nullify(material)
    end if
  end function getMaterialFromStructure

  function getMaterialCountStructure(this) result(count)
    !! Return the current number of materials.
    class(tStructure), intent(in) :: this
    integer :: count

    count = this%materialCount
  end function getMaterialCountStructure

  ! =====================================================================
  ! Assembly and finalisation
  ! =====================================================================

  subroutine assembleStructure(this)
    !! Discretise all elements: resolve pointers and create nodes and electrodes.
    !!
    !! Iterates through the linked list of elements and calls each element's
    !! `assemble` method, passing the structure itself so the element can
    !! register its nodes, electrodes, and material references.
    class(tStructure), intent(inout) :: this
    type(tElementNode), pointer :: p

    p => this%elements
    do while (associated(p))
      call p%elem%assemble(this)
      p => p%next
    end do
  end subroutine assembleStructure

  subroutine finalizeStructure(this)
    !! Destructor: deallocate all linked lists and arrays.
    !!
    !! Called automatically when the tStructure object goes out of scope
    !! (declared with `type(tStructure) :: x` not `allocate`).
    type(tStructure), intent(inout) :: this
    type(tElementNode), pointer :: pElem, nextElem
    type(tMaterialNode), pointer :: pMat, nextMat

    if (allocated(this%nodes)) then
      deallocate(this%nodes)
    end if
    if (allocated(this%electrodes)) then
      deallocate(this%electrodes)
    end if

    pElem => this%elements
    do while (associated(pElem))
      nextElem => pElem%next
      deallocate(pElem%elem)
      deallocate(pElem)
      pElem => nextElem
    end do
    nullify(this%elements)

    pMat => this%materials
    do while (associated(pMat))
      nextMat => pMat%next
      deallocate(pMat%material)
      deallocate(pMat)
      pMat => nextMat
    end do
    nullify(this%materials)

    this%materialCount = 0
    this%nodeCount     = 0
    this%elementCount  = 0
  end subroutine finalizeStructure

end module mStructure
