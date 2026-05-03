module mNode
  !! Geometric point with an associated complex voltage in the frequency domain.
  !!
  !! A `tNode` represents one vertex of the conductor network. Nodes are created
  !! by the user (main or boundary nodes) and also internally by element assembly
  !! (internal interpolation nodes). Voltages are computed by `Mesh.injetaSinalF`
  !! and stored back here after each frequency solve.
  implicit none

  type, public :: tNode
    !! Discretisation node: spatial position + complex voltage.
    character(256) :: id
    !! User-assigned string identifier (unique within a tStructure)
    real(8) :: p(3)
    !! Position vector [x, y, z] in metres
    complex(8) :: voltage
    !! Complex voltage at this node for the current frequency (V)
  end type

contains

  function newNode(id, p) result(this)
    !! Construct a tNode at position `p` with voltage initialised to zero.
    character(len=*), intent(in) :: id
    !! User-assigned identifier string
    real(8), intent(in) :: p(3)
    !! Position vector [x, y, z] (m)
    type(tNode) :: this

    this%id      = id
    this%p       = p
    this%voltage = cmplx(0.0d0, 0.0d0, kind=8)
  end function newNode

end module mNode
