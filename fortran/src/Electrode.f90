module mElectrode
  !! Line segment (electrode) connecting two tNode instances.
  !!
  !! In the HEM formulation each physical conductor is discretised into a set of
  !! cylindrical electrodes. Every electrode carries a longitudinal current `il`
  !! (along its axis) and a transverse current `it` (leaking radially to earth).
  !! Both currents are complex-valued frequency-domain quantities solved by
  !! `Mesh.injetaSinalF`.
  use mNode
  implicit none
  private

  type, public :: tElectrode
    !! Cylindrical conductor segment between two nodes.
    character(256) :: id
    !! Identifier string — only required for key (named) electrodes
    integer(4) :: nodeIndices(2)
    !! 1-based indices into the parent tStructure%nodes array
    type(tNode), pointer :: node1 => null()
    !! Pointer to the start node (set by Structure%assemble)
    type(tNode), pointer :: node2 => null()
    !! Pointer to the end node (set by Structure%assemble)
    complex(8) :: il
    !! Longitudinal current through the electrode (A)
    complex(8) :: it
    !! Transverse (leakage) current of the electrode (A)
  end type tElectrode

contains

  function newElectrode(id, nodeIndex1, nodeIndex2) result(this)
    !! Construct a tElectrode connecting two nodes by their array indices.
    !!
    !! Node pointers are not set here; they must be resolved by the enclosing
    !! tStructure after all nodes have been added.
    character(len=*), intent(in) :: id
    !! Identifier string for this electrode
    integer(4), intent(in) :: nodeIndex1
    !! 1-based index of the start node in tStructure%nodes
    integer(4), intent(in) :: nodeIndex2
    !! 1-based index of the end node in tStructure%nodes
    type(tElectrode) :: this

    this%id             = id
    this%nodeIndices(1) = nodeIndex1
    this%nodeIndices(2) = nodeIndex2
    this%il             = cmplx(0.0d0, 0.0d0, kind=8)
    this%it             = cmplx(0.0d0, 0.0d0, kind=8)
  end function newElectrode

  function center(this) result(point)
    !! Return the geometric midpoint of the electrode segment (m).
    !!
    !! Returns the origin [0,0,0] if either node pointer is not yet associated.
    class(tElectrode), intent(in) :: this
    real(8) :: point(3)

    if (.not. associated(this%node1) .or. .not. associated(this%node2)) then
      point = [0.0d0, 0.0d0, 0.0d0]
      return
    end if

    point = 0.5d0 * (this%node1%p + this%node2%p)
  end function center

end module mElectrode
