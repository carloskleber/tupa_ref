module mElement
  !! Abstract base type for all geometric elements in the solver.
  !!
  !! An `tElement` represents a high-level geometric primitive (line, catenary,
  !! circumference, tower, …) that is discretised into nodes and electrodes by
  !! the `assemble` method. Each element carries a material reference and is
  !! responsible for creating its own secondary nodes and electrode segments
  !! during assembly.
  use mNode
  use mMaterial
  use mElectrode
  implicit none
  private

  type, abstract, public :: tElement
    !! Abstract base for all geometric elements.
    character(len=256) :: id
    !! Element identifier string
    class(tMaterial), allocatable :: material
    !! Electrical material (conductor or soil) — allocated during assembly
    type(tNode), allocatable :: nodes(:)
    !! All nodes (main and internal) created by this element
    type(tElectrode), allocatable :: electrodes(:)
    !! All electrode segments created by this element
    integer(4) :: nNodes
    !! Number of nodes (main + internal interpolation points)
    integer(4) :: nElectrodes
    !! Number of electrode segments
    real(8) :: radius
    !! Default cylindrical radius for each electrode segment (m)
  contains
    procedure(assemble_interface), deferred :: assemble
    !! Discretise this element: create nodes and electrodes, link to structure.
    procedure(print_interface), deferred :: report
    !! Append a human-readable description to the accumulator string `str`.
  end type tElement

  abstract interface
    subroutine assemble_interface(this, structure)
      !! Discretise the element and link its nodes and materials to the parent structure.
      !!
      !! Implementation must:
      !! 1. Resolve any string identifiers (node IDs, material IDs) to pointers
      !!    using the parent tStructure's lookup methods.
      !! 2. Create internal nodes (if needed) and allocate `this%nodes`.
      !! 3. Create electrode segments connecting the nodes and allocate `this%electrodes`.
      !! 4. Call `structure%addNode()` and/or other structure methods to register
      !!    the new geometry so that the parent can build the global mesh.
      import :: tElement
      class(tElement), intent(inout), target :: this
      class(*), intent(inout) :: structure
      !! Parent tStructure — intended use is `select type (structure); type is (tStructure)`.
    end subroutine assemble_interface
  end interface

  abstract interface
    subroutine print_interface(this, str)
      !! Build a human-readable report of the element and append it to `str`.
      import :: tElement
      class(tElement), intent(in) :: this
      character(:), allocatable, intent(inout) :: str
      !! Accumulator string — text is appended (not replaced)
    end subroutine print_interface
  end interface

end module mElement
