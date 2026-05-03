module mCtes
  !! Physical constants and utility parameters for the TUPÃ solver.
  !!
  !! All real constants use `kind=8` (double precision). Complex constants use
  !! the same kind. Import only the symbols you need via `use mCtes, only: ...`.
  implicit none
  private
  public :: PI, FOUR_PI, MU0, EPSILON0, SQRT2, IMAG_I, ZERO_CPLX, ONE_CPLX, newl, &
            color_red, color_green, color_yellow, color_blue, color_reset

  real(8), parameter :: PI = acos(-1.0d0)
  !! Pi (π ≈ 3.14159…)
  real(8), parameter :: FOUR_PI = 4.0d0 * acos(-1.0d0)
  !! 4π — frequently used in electromagnetic potential kernels

  real(8), parameter :: MU0 = 4.0d-7 * PI
  !! Permeability of free space μ₀ (H/m)
  real(8), parameter :: C = 299792458.0d0
  !! Speed of light in vacuum (m/s)
  real(8), parameter :: EPSILON0 = 1.0d0 / (MU0 * C * C)
  !! Permittivity of free space ε₀ (F/m)
  real(8), parameter :: SQRT2 = sqrt(2.0d0)
  !! √2 — used in RMS conversions

  complex(8), parameter :: IMAG_I = (0.0d0, 1.0d0)
  !! Imaginary unit j (i.e. √−1)
  complex(8), parameter :: ZERO_CPLX = (0.0d0, 0.0d0)
  !! Complex zero (0 + 0j)
  complex(8), parameter :: ONE_CPLX = (1.0d0, 0.0d0)
  !! Complex one (1 + 0j)

  character(len=*), parameter :: newl = new_line('a')
  !! Line-feed character (LF, ASCII 10) for portable string assembly

  ! ANSI escape sequences for coloured terminal output
  character(len=*), parameter :: esc = char(27)
  !! ESC character (ASCII 27) used to build ANSI colour codes
  character(len=*), parameter :: color_red    = esc // "[31m"
  !! ANSI red foreground
  character(len=*), parameter :: color_green  = esc // "[32m"
  !! ANSI green foreground
  character(len=*), parameter :: color_yellow = esc // "[33m"
  !! ANSI yellow foreground
  character(len=*), parameter :: color_blue   = esc // "[34m"
  !! ANSI blue foreground
  character(len=*), parameter :: color_reset  = esc // "[0m"
  !! ANSI reset — restores default terminal colour
end module mCtes
