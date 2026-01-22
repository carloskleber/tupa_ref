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
    procedure(print_interface), deferred :: report
  end type

  abstract interface
    subroutine assemble_interface(this, structure)
      !! Common structure to assemble Elements
      import :: tElement
      class(tElement), intent(inout) :: this
      class(*), intent(inout) :: structure
    end subroutine assemble_interface
  end interface
  
  abstract interface
    subroutine print_interface(this, str)
      import :: tElement
      class(tElement), intent(in) :: this
      character(:), allocatable, intent(inout) :: str
    end subroutine print_interface
  end interface
end module
