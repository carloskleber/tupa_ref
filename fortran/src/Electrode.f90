module mElectrode
  use mNode
  implicit none
  private

  type, public :: tElectrode
    !! An electrode - usually a metallic cylinder connecting two nodes, with radius >> length.
    character(256) id
    !! Electrode identifier (optional - used only in key electrodes).
    integer(4) nodeIndices(2)
    !! Indices of nodes that make up the electrode
    type(tNode), pointer :: node1 => null()
    type(tNode), pointer :: node2 => null()
    !! Pointers to the nodes
    complex(8) il
    !! Longitudinal current through the electrode (A)
    complex(8) it
    !! Transverse current through the electrode (A)
  end type tElectrode
contains
  function newElectrode(id, nodeIndex1, nodeIndex2) result(this)
    !! Constructor function to create a tElectrode instance
    !! @param[in] id Electrode identifier
    !! @param[in] nodeIndex1 Index of the first node
    !! @param[in] nodeIndex2 Index of the second node
    !! @return Instantiated tElectrode object
    character(len=*), intent(in) :: id
    integer(4), intent(in) :: nodeIndex1, nodeIndex2
    type(tElectrode) :: this

    this%id = id
    this%nodeIndices(1) = nodeIndex1
    this%nodeIndices(2) = nodeIndex2
    this%il = cmplx(0.0d0, 0.0d0, kind=8)
    this%it = cmplx(0.0d0, 0.0d0, kind=8)
  end function newElectrode

  function center(this) result(point)
    !! Calculate the center point of the electrode
    class(tElectrode), intent(in) :: this
    real(8) point(3)

    if (.not. associated(this%node1) .or. .not. associated(this%node2)) then
      point = [0.0d0, 0.0d0, 0.0d0]
      return
    end if

    point = 0.5d0 * (this%node1%p + this%node2%p)
  end function center
end module mElectrode
