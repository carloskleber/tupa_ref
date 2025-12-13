module mNode
  use stdlib_strings
  implicit none

  type tNode
    type(string_type) id
    !! Node identifier
    real(8) x
    real(8) y
    real(8) z
    complex(8) voltage
  end type
contains
end module
