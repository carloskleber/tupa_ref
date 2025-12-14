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
end module
