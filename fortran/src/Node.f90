module mNode
  implicit none

  type tNode
    character(len=256) :: id
    !! Node identifier
    real(8) x
    real(8) y
    real(8) z
    complex(8) voltage
  end type
contains

  function createNode(id, x, y, z) result(node)
    !! Constructor function to create a tNode instance
    !! @param[in] id Node identifier
    !! @param[in] x X coordinate
    !! @param[in] y Y coordinate
    !! @param[in] z Z coordinate
    !! @return Instantiated tNode object
    character(len=*), intent(in) :: id
    real(8), intent(in) :: x, y, z
    type(tNode) :: node

    node%id = id
    node%x = x
    node%y = y
    node%z = z
    node%voltage = cmplx(0.0d0, 0.0d0, kind=8)
  end function createNode

end module
