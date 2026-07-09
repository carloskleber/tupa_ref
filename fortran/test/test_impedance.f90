program test_impedance
  use mImpedance
  use check
  use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
  implicit none

  ! Test suite variables
  real(8) :: result, expected, tolerance
  real(8) :: za1(3), zb1(3), zva(3), zvb(3), zla, zlb
  real(8) :: x, y
  integer :: i
  real(8) :: result1, result2

  ! ----------------------------------------------------------------
  ! Test 1: inverseDistanceIntegrand - distance between parallel points
  ! ----------------------------------------------------------------
  call test_init("inverseDistanceIntegrand function tests")

  ! Point A travels along the x axis from the origin; point B fixed at the origin.
  ! zva must be non-zero so the distance varies with X (previous bug:
  ! zva=zvb=[0,0,0] made inverseDistanceIntegrand(X,Y) constant, breaking the
  ! monotonicity test below).
  za1 = [0.0d0, 0.0d0, 0.0d0]
  zb1 = [0.0d0, 0.0d0, 0.0d0]
  zva = [1.0d0, 0.0d0, 0.0d0]
  zvb = [0.0d0, 0.0d0, 0.0d0]
  zla = 1.0d0
  zlb = 1.0d0

  ! Set the common block for the test
  call set_params(za1, zb1, zva, zvb, zlb)

  x = 1.0d0
  y = 0.0d0

  call test_ok("inverseDistanceIntegrand returns a positive value", &
               inverseDistanceIntegrand(x, y) > 0.0d0, &
               "Expected a positive value")

  call test_ok("inverseDistanceIntegrand decreases with distance", &
               inverseDistanceIntegrand(1.0d0, 0.0d0) > inverseDistanceIntegrand(2.0d0, 0.0d0), &
               "The larger x, the larger the distance, the smaller the value")

  ! ----------------------------------------------------------------
  ! Test 2: lowerLimit - always returns 0
  ! ----------------------------------------------------------------
  call test_init("lowerLimit function tests")

  call test_ok("lowerLimit(0.0) = 0.0", &
               lowerLimit(0.0d0) == 0.0d0, &
               "Must return zero")

  call test_ok("lowerLimit(5.0) = 0.0", &
               lowerLimit(5.0d0) == 0.0d0, &
               "Must return zero regardless of the argument")

  call test_ok("lowerLimit(-3.0) = 0.0", &
               lowerLimit(-3.0d0) == 0.0d0, &
               "Must return zero for negative arguments")

  ! ----------------------------------------------------------------
  ! Test 3: upperLimit - returns lb from the common block
  ! ----------------------------------------------------------------
  call test_init("upperLimit function tests")

  za1 = [0.0d0, 0.0d0, 0.0d0]
  zb1 = [1.0d0, 0.0d0, 0.0d0]
  zva = [0.0d0, 0.0d0, 0.0d0]
  zvb = [0.0d0, 0.0d0, 0.0d0]
  zla = 2.0d0
  zlb = 3.0d0

  call set_params(za1, zb1, zva, zvb, zlb)

  call test_ok("upperLimit returns lb", &
               abs(upperLimit(0.0d0) - 3.0d0) < 1.0d-15, &
               "Must return 3.0")

  call test_ok("upperLimit is constant", &
               abs(upperLimit(1.0d0) - upperLimit(2.0d0)) < 1.0d-15, &
               "Must be independent of the argument")

  ! ----------------------------------------------------------------
  ! Test 4: 1D integration - Gauss-Kronrod
  ! ----------------------------------------------------------------
  call test_init("1D Integration tests")

  tolerance = 1.0d-10

  ! Integral of x^2 from 0 to 1
  expected = 1.0d0/3.0d0
  result = integrate_1d(quadratic, 0.0d0, 1.0d0, 0.0d0, 1.0d-12)
  call test_ok("integral_0^1 x^2 dx = 1/3", &
               abs(result - expected) < tolerance, &
               "Result: " // trim(adjustl(real_to_str(result))))

  ! Integral of sin(x) from 0 to pi
  expected = 2.0d0
  result = integrate_1d(sin_func, 0.0d0, acos(-1.0d0), 0.0d0, 1.0d-12)
  call test_ok("integral_0^pi sin(x) dx = 2", &
               abs(result - expected) < tolerance, &
               "Result: " // trim(adjustl(real_to_str(result))))

  ! ----------------------------------------------------------------
  ! Test 5: 2D integration - TWODQ
  ! ----------------------------------------------------------------
  call test_init("2D Integration (TWODQ) tests")

  tolerance = 1.0d-6

  ! Integral of x*y over the [0,1]x[0,1] rectangle
  expected = 0.25d0
  call test_twodq_simple(xy_func, constant_0, constant_1, &
                         0.0d0, 1.0d0, 0.0d0, 1.0d-6, &
                         result, "integral_0^1 integral_0^1 x*y dy dx = 0.25")
  call test_ok("integral_0^1 integral_0^1 x*y dy dx = 0.25", &
               abs(result - expected) < tolerance, &
               "Result: " // trim(adjustl(real_to_str(result))))

  ! Integral of x^2 + y^2 over the [0,1]x[0,2] rectangle
  expected = 10.0d0/3.0d0  ! = 3.333...
  call test_twodq_simple(sum_squares, constant_0, constant_2, &
                         0.0d0, 1.0d0, 0.0d0, 1.0d-6, &
                         result, "integral_0^1 integral_0^2 (x^2+y^2) dy dx = 10/3")
  call test_ok("integral_0^1 integral_0^2 (x^2+y^2) dy dx = 10/3", &
               abs(result - expected) < tolerance, &
               "Result: " // trim(adjustl(real_to_str(result))))

  ! ----------------------------------------------------------------
  ! Test 6: geometryFactor2D - simple geometry cases
  ! ----------------------------------------------------------------
  call test_init("geometryFactor2D geometry tests")

  tolerance = 1.0d-4

  ! Case 1: two parallel unit segments, separated by distance d=1
  za1 = [0.0d0, 0.0d0, 0.0d0]
  zb1 = [1.0d0, 0.0d0, 0.0d0]
  zva = [1.0d0, 0.0d0, 0.0d0]
  zvb = [1.0d0, 0.0d0, 0.0d0]
  zla = 1.0d0
  zlb = 1.0d0

  call geometryFactor2D(za1, zva, zla, zb1, zvb, zlb, result)
  call test_ok("geometryFactor2D - parallel unit segments", &
               result > 0.0d0, &
               "Positive result: " // trim(adjustl(real_to_str(result))))

  ! Reasonableness check (an analytic reference value could be computed)
  call test_ok("geometryFactor2D - reasonable value", &
               result > 0.1d0 .and. result < 10.0d0, &
               "Result must be finite and positive")

  ! Case 2: exactly coincident segments — genuinely divergent integral
  ! (int 1/|x-y| over the square's diagonal is not finite without a finite
  ! radius offset). geometryFactor2D must not be used for the self term;
  ! that is the role of mGeometry%selfGeometryFactor (closed-form formula
  ! with an axis-to-surface offset r0), tested in test_geometry.f90.
  za1 = [0.0d0, 0.0d0, 0.0d0]
  zb1 = [0.0d0, 0.0d0, 0.0d0]
  zva = [1.0d0, 0.0d0, 0.0d0]
  zvb = [1.0d0, 0.0d0, 0.0d0]
  zla = 1.0d0
  zlb = 1.0d0

  call geometryFactor2D(za1, zva, zla, zb1, zvb, zlb, result)
  call test_ok("geometryFactor2D on exactly coincident segments diverges (expected)", &
               ieee_is_nan(result) .or. result > 1.0d6, &
               "without a finite radius the integral diverges; the self term uses mGeometry%selfGeometryFactor")

  ! Case 3: distant segments
  za1 = [0.0d0, 0.0d0, 0.0d0]
  zb1 = [0.0d0, 0.0d0, 10.0d0]
  zva = [1.0d0, 0.0d0, 0.0d0]
  zvb = [1.0d0, 0.0d0, 0.0d0]
  zla = 1.0d0
  zlb = 1.0d0

  call geometryFactor2D(za1, zva, zla, zb1, zvb, zlb, result)
  call test_ok("geometryFactor2D - distant segments", &
               result < 1.0d0, &
               "Small value for large separation: " // trim(adjustl(real_to_str(result))))

  ! Case 4: perpendicular segments
  za1 = [0.0d0, 0.0d0, 0.0d0]
  zb1 = [1.0d0, 0.0d0, 0.0d0]
  zva = [1.0d0, 0.0d0, 0.0d0]
  zvb = [0.0d0, 1.0d0, 0.0d0]
  zla = 1.0d0
  zlb = 1.0d0

  call geometryFactor2D(za1, zva, zla, zb1, zvb, zlb, result)
  call test_ok("geometryFactor2D - perpendicular segments", &
               result > 0.0d0 .and. result < 10.0d0, &
               "Finite result: " // trim(adjustl(real_to_str(result))))

  ! ----------------------------------------------------------------
  ! Test 7: consistency and symmetry
  ! ----------------------------------------------------------------
  call test_init("geometryFactor2D symmetry tests")

  tolerance = 1.0d-6

  za1 = [0.0d0, 0.0d0, 0.0d0]
  zb1 = [2.0d0, 1.0d0, 0.0d0]
  zva = [1.0d0, 0.0d0, 0.0d0]
  zvb = [0.0d0, 1.0d0, 0.0d0]
  zla = 2.0d0
  zlb = 3.0d0

  ! geometryFactor2D must be symmetric
  call geometryFactor2D(za1, zva, zla, zb1, zvb, zlb, result1)
  call geometryFactor2D(zb1, zvb, zlb, za1, zva, zla, result2)

  call test_ok("geometryFactor2D is symmetric", &
               abs(result1 - result2) < tolerance, &
               "Result1: " // trim(adjustl(real_to_str(result1))) // &
               " Result2: " // trim(adjustl(real_to_str(result2))))

  ! ----------------------------------------------------------------
  ! Test 8: stress tests - non-rectangular domains
  ! ----------------------------------------------------------------
  call test_init("TWODQ non-rectangular domain tests")

  tolerance = 1.0d-6

  ! Integral with a variable upper limit: integral_0^1 integral_0^x 1 dy dx = 0.5
  expected = 0.5d0
  call test_twodq_simple(const_1, constant_0, identity_func, &
                         0.0d0, 1.0d0, 0.0d0, 1.0d-12, &
                         result, "integral_0^1 integral_0^x 1 dy dx = 0.5")
  call test_ok("integral_0^1 integral_0^x 1 dy dx = 0.5", &
               abs(result - expected) < tolerance, &
               "Result: " // trim(adjustl(real_to_str(result))))

  ! Integral with variable limits: integral_0^1 integral_{-x}^x x*y dy dx
  expected = 0.0d0  ! Odd function in y
  call test_twodq_simple(xy_func, neg_identity, identity_func, &
                         0.0d0, 1.0d0, 0.0d0, 1.0d-12, &
                         result, "integral_0^1 integral_{-x}^x x*y dy dx = 0")
  call test_ok("integral_0^1 integral_{-x}^x x*y dy dx = 0", &
               abs(result - expected) < 1.0d-8, &
               "Result: " // trim(adjustl(real_to_str(result))))

  ! Final summary
  call test_summary()

contains

  ! Helper: Set common block params
  subroutine set_params(a1, b1, va, vb, lb)
    real(8), intent(in) :: a1(3), b1(3), va(3), vb(3), lb
    real(8) :: common_a1(3), common_b1(3), common_va(3), common_vb(3), common_lb
    common /params/ common_a1, common_b1, common_va, common_vb, common_lb

    common_a1 = a1
    common_b1 = b1
    common_va = va
    common_vb = vb
    common_lb = lb
  end subroutine set_params

  ! Test function for 1D integration
  real(8) function quadratic(x)
    real(8), intent(in) :: x
    quadratic = x * x
  end function quadratic

  real(8) function sin_func(x)
    real(8), intent(in) :: x
    sin_func = sin(x)
  end function sin_func

  ! Test functions for 2D integration
  real(8) function xy_func(x, y)
    real(8), intent(in) :: x, y
    xy_func = x * y
  end function xy_func

  real(8) function sum_squares(x, y)
    real(8), intent(in) :: x, y
    sum_squares = x*x + y*y
  end function sum_squares

  real(8) function const_1(x, y)
    real(8), intent(in) :: x, y
    const_1 = 1.0d0
  end function const_1

  ! Boundary functions
  real(8) function constant_0(x)
    real(8), intent(in) :: x
    constant_0 = 0.0d0
  end function constant_0

  real(8) function constant_1(x)
    real(8), intent(in) :: x
    constant_1 = 1.0d0
  end function constant_1

  real(8) function constant_2(x)
    real(8), intent(in) :: x
    constant_2 = 2.0d0
  end function constant_2

  real(8) function identity_func(x)
    real(8), intent(in) :: x
    identity_func = x
  end function identity_func

  real(8) function neg_identity(x)
    real(8), intent(in) :: x
    neg_identity = -x
  end function neg_identity

  ! Wrapper to integrate 1D functions using the module's integrator
  real(8) function integrate_1d(f, a, b, epsabs, epsrel)
    real(8), external :: f
    real(8), intent(in) :: a, b, epsabs, epsrel
    real(8) :: err

    call dqag_k15(f, a, b, epsabs, epsrel, integrate_1d, err)
  end function integrate_1d

  ! Wrapper for TWODQ testing with simple functions
  subroutine test_twodq_simple(f, g, h, a, b, epsabs, epsrel, result, description)
    interface
      real(8) function f(x, y)
        real(8), intent(in) :: x, y
      end function f
      real(8) function g(x)
        real(8), intent(in) :: x
      end function g
      real(8) function h(x)
        real(8), intent(in) :: x
      end function h
    end interface
    real(8), intent(in) :: a, b, epsabs, epsrel
    real(8), intent(out) :: result
    character(len=*), intent(in) :: description
    real(8) :: errest

    call TWODQ(f, a, b, g, h, epsabs, epsrel, result, errest)
  end subroutine test_twodq_simple

  ! Convert real(8) to string for output
  function real_to_str(val) result(str)
    real(8), intent(in) :: val
    character(len=20) :: str
    write(str, '(ES20.12)') val
    str = adjustl(str)
  end function real_to_str

end program test_impedance
