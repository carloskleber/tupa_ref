program test_finite_math
  !! Regression for ROADMAP near-term item 2: the closed-form parallel geometry
  !! factor must not depend on NaN/Inf detection of its result, so it behaves
  !! identically under -ffinite-math-only (part of -ffast-math).
  !! Run it under the release flags:
  !!   fpm test test_finite_math --flag "-O3 -ffast-math -ffinite-math-only"
  use mGeometry, only: mutualGeometryFactor
  use check
  implicit none

  real(8) :: a1(3), a2(3), b1(3), b2(3), gFast, gQuad, la, lb
  integer :: k
  character(len=48) :: label

  ! Collinear pairs on the x axis: touching, gapped and overlapping-adjacent;
  ! parallel offset pairs. Each is compared with the quadrature path.
  do k = 1, 6
    a1 = [0.0d0, 0.0d0, 0.0d0]
    a2 = [2.0d0, 0.0d0, 0.0d0]
    la = 2.0d0; lb = 3.0d0
    select case (k)
    case (1)  ! touching, same direction
      b1 = [2.0d0, 0.0d0, 0.0d0]; b2 = [5.0d0, 0.0d0, 0.0d0]; label = "collinear touching"
    case (2)  ! touching, b reversed
      b1 = [5.0d0, 0.0d0, 0.0d0]; b2 = [2.0d0, 0.0d0, 0.0d0]; label = "collinear touching, reversed"
    case (3)  ! gap
      b1 = [3.0d0, 0.0d0, 0.0d0]; b2 = [6.0d0, 0.0d0, 0.0d0]; label = "collinear with gap"
    case (4)  ! parallel offset
      b1 = [0.5d0, 1.0d0, 0.0d0]; b2 = [3.5d0, 1.0d0, 0.0d0]; label = "parallel offset"
    case (5)  ! parallel, staggered, reversed
      b1 = [4.0d0, 0.3d0, 0.1d0]; b2 = [1.0d0, 0.3d0, 0.1d0]; label = "parallel staggered, reversed"
    case (6)  ! tiny offset (log arguments close to degenerate)
      b1 = [0.0d0, 1.0d-4, 0.0d0]; b2 = [3.0d0, 1.0d-4, 0.0d0]; label = "parallel, 1e-4 offset"
    end select
    call mutualGeometryFactor(a1, a2, b1, b2, gFast)
    call mutualGeometryFactor(a1, a2, b1, b2, gQuad, forceNumeric=.true.)
    call test_ok(trim(label) // ": closed form finite and equals quadrature", &
                 gFast > 0.0d0 .and. gFast < huge(1.0d0) .and. abs(gFast - gQuad) < 1.0d-5 * abs(gQuad))
  end do

  call test_summary()
  call test_close()
end program test_finite_math
