module mNode
  implicit none

  type, public ::  tNode
    character(256) id
    !! Node identifier
    real(8) p(3)
    !! Position in 3D space [x, y, z] (m)
    complex(8) voltage
    !! Voltage at the node (V)
  end type
contains

  function newNode(id, p) result(this)
    !! Constructor function to create a tNode instance
    !! @param[in] id Node identifier
    !! @param[in] p Position vector [x, y, z] (m)
    character(len=*), intent(in) :: id
    real(8), intent(in) :: p(3)
    type(tNode) :: this

    this%id = id
    this%p = p
    this%voltage = cmplx(0.0d0, 0.0d0, kind=8)
  end function newNode
end module
