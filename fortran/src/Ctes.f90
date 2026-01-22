module mCtes
    !! Main physical constants used in the simulations
  implicit none
  private
  public :: PI, MU0, EPSILON0, SQRT2, IMAG_I, newl, &
            color_red, color_green, color_yellow, color_blue, color_reset

  real(8), parameter :: PI = acos(-1.0d0) !! Pi constant
  real(8), parameter :: MU0 = 4.0d-7 * PI !! Permeability of free space (H/m)
  real(8), parameter :: C = 299792458.0d0 !! Speed of light in vacuum (m/s)
  real(8), parameter :: EPSILON0 = 1.0d0 / (MU0 * C * C) !! Permittivity of free space (F/m)
  real(8), parameter :: SQRT2 = sqrt(2.0d0) !! Square root of 2
  complex(8), parameter :: IMAG_I = (0.0d0, 1.0d0) !! Imaginary unit

  character(len=*), parameter :: newl = new_line('a') !! New line (LF)

  ! Helper strings for color output in terminal
  character(len=*), parameter :: esc = char(27) !! Escape character for terminal colors
  character(len=*), parameter :: color_red = esc // "[31m"
  character(len=*), parameter :: color_green = esc // "[32m"
  character(len=*), parameter :: color_yellow = esc // "[33m"
  character(len=*), parameter :: color_blue = esc // "[34m"
  character(len=*), parameter :: color_reset = esc // "[0m"
end module mCtes
