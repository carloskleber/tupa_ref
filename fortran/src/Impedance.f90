module mImpedance
  !! Routines for computing electrode self- and mutual impedances.
  !!
  !! This module implements adaptive double integration using 15-point Gauss-Kronrod
  !! quadrature to evaluate the mutual impedance integral between line segments
  !! (two cylindrical electrodes). The integral is:
  !!
  !!     Z_ij = (1/4π) ∫∫ (1/r_ij) ds_i ds_j
  !!
  !! where r_ij is the distance between points on the two line segments, and the
  !! integration is over the lengths of both segments.
  !!
  !! **Note on COMMON blocks**: The module uses legacy Fortran COMMON blocks to
  !! pass integration parameters between nested functions. This avoids allocatable
  !! component issues in older compilers and preserves historical code structure.
  !! A modern refactoring could use module-level pointers instead.
  use mCtes, only: PI, MU0
  use mError, only: raiseError
  implicit none

  ! Explicit interface for SLATEC ZBESI (complex modified Bessel function I)
  interface
    subroutine zbesi(zr, zi, fnu, kode, n, cyr, cyi, nz, ierr)
      real(8), intent(in) :: zr, zi, fnu
      integer, intent(in) :: kode, n
      real(8), intent(out) :: cyr(n), cyi(n)
      integer, intent(out) :: nz, ierr
    end subroutine zbesi
  end interface

  ! =====================================================================
  ! Abstract interfaces for function pointers
  ! =====================================================================

  abstract interface
    real(8) function func2d(x, y)
      !! 2D integrand: f(x,y)
      real(8), intent(in) :: x, y
    end function func2d
    real(8) function func1d(x)
      !! 1D integrand: f(x)
      real(8), intent(in) :: x
    end function func1d
  end interface

  ! =====================================================================
  ! Module-level integration state (for nested integration)
  ! =====================================================================

  procedure(func2d), pointer :: pF => null()
  !! Pointer to the 2D integrand function
  procedure(func1d), pointer :: pG => null()
  !! Pointer to the lower limit function for the inner integral
  procedure(func1d), pointer :: pH => null()
  !! Pointer to the upper limit function for the inner integral
  real(8) :: m_epsabs, m_epsrel, m_a, m_b
  !! Tolerance and integration bounds stored for use by nested routines

  ! =====================================================================
  ! Gauss-Kronrod quadrature rule: 15-point Kronrod + nested 7-point Gauss
  ! =====================================================================

  integer, parameter :: maxint = 500
  !! Maximum number of subintervals in adaptive quadrature
  real(8), parameter :: xgk(15) = [ &
       -0.9914553711208126D+00, -0.9491079123427585D+00, &
       -0.8648644233597691D+00, -0.7415311855993944D+00, &
       -0.5860872354676911D+00, -0.4058451513773972D+00, &
       -0.2077849550078985D+00,  0.0000000000000000D+00, &
        0.2077849550078985D+00,  0.4058451513773972D+00, &
        0.5860872354676911D+00,  0.7415311855993944D+00, &
        0.8648644233597691D+00,  0.9491079123427585D+00, &
        0.9914553711208126D+00 ]
  !! Abscissae for the 15-point Kronrod rule (on [-1,1])
  real(8), parameter :: wgk(15) = [ &
        0.02293532201052922D+00,  0.06309209262997855D+00, &
        0.1047900103222502D+00,   0.1406532597155259D+00, &
        0.1690047266392679D+00,   0.1903505780647854D+00, &
        0.2044329400752989D+00,   0.2094821410847278D+00, &
        0.2044329400752989D+00,   0.1903505780647854D+00, &
        0.1690047266392679D+00,   0.1406532597155259D+00, &
        0.1047900103222502D+00,   0.06309209262997855D+00, &
        0.02293532201052922D+00 ]
  !! Weights for the 15-point Kronrod rule
  real(8), parameter :: wg(7) = [ &
        0.06309209262997855D+00,  0.1406532597155259D+00, &
        0.1903505780647854D+00,   0.2094821410847278D+00, &
        0.1903505780647854D+00,   0.1406532597155259D+00, &
        0.06309209262997855D+00 ]
  !! Weights for the nested 7-point Gauss rule (subset of Kronrod nodes)
  integer, parameter :: igauss(7) = [2, 4, 6, 8, 10, 12, 14]
  !! Indices in xgk/wgk that correspond to the 7-point Gauss rule

  public :: geometryFactor2D, inverseDistanceIntegrand, lowerLimit, upperLimit, TWODQ, internalImpedance

contains

  ! =====================================================================
  ! Internal (skin-effect) impedance of a solid cylindrical conductor
  ! =====================================================================

  complex(8) function internalImpedance(radius, length, omega, sigma, mur) result(Zint)
    !! Internal impedance of a solid cylindrical conductor (theory.md §4.3):
    !!
    !!     z_int = sqrt(j*omega*mu/sigma) / (2*pi*r0) * I0(rho) / I1(rho)
    !!     rho   = r0 * sqrt(j*omega*mu*sigma),  Zint = z_int * l
    !!
    !! I0, I1 are modified Bessel functions of the first kind (complex
    !! argument), evaluated via SLATEC ZBESI. For |rho| > 500 the ratio is
    !! taken as 1 (its asymptotic limit) to avoid unnecessary/unstable
    !! evaluation at large argument, matching the original implementation.
    real(8), intent(in) :: radius
    !! Conductor radius r0 (m)
    real(8), intent(in) :: length
    !! Segment length l (m)
    real(8), intent(in) :: omega
    !! Angular frequency omega (rad/s)
    real(8), intent(in) :: sigma
    !! Conductor conductivity sigma_c (S/m)
    real(8), intent(in) :: mur
    !! Conductor relative permeability mu_r
    complex(8) :: rho, ratio, zPerLength
    real(8) :: cyr(2), cyi(2)
    integer :: nz, ierr
    character(len=8) :: ierrStr

    rho = radius * sqrt(cmplx(0.0d0, omega, kind=8) * mur * MU0 * sigma)

    if (abs(rho) > 500.0d0) then
      ratio = cmplx(1.0d0, 0.0d0, kind=8)
    else
      call zbesi(real(rho, kind=8), aimag(rho), 0.0d0, 1, 2, cyr, cyi, nz, ierr)
      if (ierr /= 0 .and. ierr /= 3) then
        write(ierrStr, '(I0)') ierr
        call raiseError("internalImpedance: ZBESI failed with IERR=" // trim(ierrStr))
      end if
      ratio = cmplx(cyr(1), cyi(1), kind=8) / cmplx(cyr(2), cyi(2), kind=8)
    end if

    zPerLength = sqrt(cmplx(0.0d0, omega, kind=8) * mur * MU0 / sigma) / (2.0d0 * PI * radius) * ratio
    Zint = zPerLength * length
  end function internalImpedance

  ! =====================================================================
  ! Public entry point for mutual impedance
  ! =====================================================================

  subroutine geometryFactor2D(za1, zva, zla, zb1, zvb, zlb, res)
    !! Compute the geometry factor g(a,b) between two line segments via double
    !! integration (theory.md §4.2, "general position, 2-D").
    !!
    !! The integrand is 1/r, where r is the distance between a point on the first
    !! segment and a point on the second. Uses adaptive Gauss-Kronrod quadrature.
    real(8) :: za1(:)
    !! Starting point of the first segment (m)
    real(8) :: zva(3)
    !! Direction vector of the first segment (normalised or scaled by length)
    real(8) :: zla
    !! Length of the first segment (m)
    real(8) :: zb1(:)
    !! Starting point of the second segment (m)
    real(8) :: zvb(3)
    !! Direction vector of the second segment
    real(8) :: zlb
    !! Length of the second segment (m)
    real(8) :: res
    !! Result: the geometry factor g(a,b) (dimensionless distance integral)
    real(8) :: a1(3), b1(3), va(3), vb(3), la, lb
    integer :: irule
    common /params/ a1, b1, va, vb, lb
    real(8) :: errabs, errrel, errest

    a1 = za1
    b1 = zb1
    va = zva
    vb = zvb
    la = zla
    lb = zlb
    errabs = 0.0d0
    errrel = dmin1(la, lb) * 1.0d-6
    call TWODQ(inverseDistanceIntegrand, 0.0d0, la, lowerLimit, upperLimit, errabs, errrel, res, errest)
  end subroutine geometryFactor2D

  ! =====================================================================
  ! Integration kernel (integrand and integration limits)
  ! =====================================================================

  real(8) function inverseDistanceIntegrand(X, Y)
    !! 2D integrand: 1 / distance between points on two line segments.
    !!
    !! The distance r(x,y) is computed as the Euclidean distance between:
    !!   P_a(x) = a1 + x·va
    !!   P_b(y) = b1 + y·vb
    real(8), intent(in) :: X, Y
    real(8) :: a1(3), b1(3), va(3), vb(3), lb
    real(8) :: a, b, c, z
    common /params/ a1, b1, va, vb, lb
    integer :: j

    z = 0.0d0
    do j = 1, 3
      a = a1(j) + va(j) * X
      b = b1(j) + vb(j) * Y
      c = b - a
      z = z + c * c
    end do
    inverseDistanceIntegrand = 1.0d0 / dsqrt(z)
  end function inverseDistanceIntegrand

  real(8) function lowerLimit(x)
    !! Lower integration limit for the inner integral: always 0.
    real(8), intent(in) :: x

    lowerLimit = 0.0d0
  end function lowerLimit

  real(8) function upperLimit(x)
    !! Upper integration limit for the inner integral: always lb (length of segment 2).
    real(8), intent(in) :: x
    real(8) :: a1(3), b1(3), va(3), vb(3), la, lb
    common /params/ a1, b1, va, vb, lb

    upperLimit = lb
  end function upperLimit

  ! =====================================================================
  ! Double integration: wrapper for nested 1D integration
  ! =====================================================================

  subroutine TWODQ(F, A, B, G, H, ERRABS, ERRREL, RESULT, ERREST)
    !! Adaptive double integration of F(x,y) over x∈[A,B], y∈[G(x),H(x)].
    !!
    !! Stores function pointers and integration parameters in module-level
    !! variables so they can be accessed by nested integration routines.
    procedure(func2d) :: F
    !! 2D integrand
    procedure(func1d) :: G, H
    !! Lower and upper limit functions for the inner integral
    real(8), intent(in)  :: A, B
    !! Outer integration bounds
    real(8), intent(in)  :: ERRABS, ERRREL
    !! Absolute and relative tolerance
    real(8), intent(out) :: RESULT, ERREST
    !! Result and error estimate

    pF => F
    pG => G
    pH => H
    m_epsabs = ERRABS
    m_epsrel = ERRREL
    m_a = A
    m_b = B

    call dqag_k15(outer_fcn, A, B, ERRABS, ERRREL, RESULT, ERREST)
  end subroutine TWODQ

  ! =====================================================================
  ! Nested integration: outer integral and its inner integrand
  ! =====================================================================

  real(8) function outer_fcn(x)
    !! Outer integrand: inner integral result as a function of x.
    real(8), intent(in) :: x
    real(8) :: inner_epsabs, inner_result, inner_err

    inner_epsabs = 0.5d0 * m_epsabs / max(1.0d0, (m_b - m_a))
    call dqag_k15(inner_wrap, pG(x), pH(x), inner_epsabs, m_epsrel, &
                  inner_result, inner_err)
    outer_fcn = inner_result

  contains
    real(8) function inner_wrap(y)
      !! Wrapper to evaluate pF(x,y) where x is from the enclosing scope.
      real(8), intent(in) :: y
      inner_wrap = pF(x, y)
    end function inner_wrap
  end function outer_fcn

  ! =====================================================================
  ! Adaptive Gauss-Kronrod quadrature (1D)
  ! =====================================================================

  subroutine dqag_k15(f, a, b, epsabs, epsrel, result, abserr)
    !! Adaptive 1D integration using 15-point Gauss-Kronrod rule.
    !!
    !! Subdivides intervals recursively, prioritising those with largest errors.
    !! Halts when either the error tolerance is satisfied or `maxint` subintervals
    !! are used.
    procedure(func1d) :: f
    !! 1D integrand
    real(8), intent(in)  :: a, b
    !! Integration bounds
    real(8), intent(in)  :: epsabs, epsrel
    !! Absolute and relative tolerance
    real(8), intent(out) :: result, abserr
    !! Result and error estimate

    real(8) :: alist(maxint), blist(maxint), rlist(maxint), elist(maxint)
    integer :: nint, i, maxind
    real(8) :: a1, b1, c, area1, err1, area2, err2, total_result, total_error
    logical :: converged

    ! Initial interval
    nint = 1
    alist(1) = a
    blist(1) = b
    call qk15(f, alist(1), blist(1), rlist(1), elist(1))
    total_result = rlist(1)
    total_error  = elist(1)
    converged = (total_error <= max(epsabs, epsrel*abs(total_result)))

    ! Adaptive refinement loop
    do while (.not. converged .and. nint < maxint)
      ! Find the subinterval with the largest error
      maxind = 1
      do i = 2, nint
        if (elist(i) > elist(maxind)) maxind = i
      end do

      a1 = alist(maxind)
      b1 = blist(maxind)
      c  = 0.5d0 * (a1 + b1)

      ! Evaluate both halves
      call qk15(f, a1, c, area1, err1)
      call qk15(f, c, b1, area2, err2)

      ! Update total by removing old interval and adding two new ones
      total_result = total_result - rlist(maxind)
      total_error  = total_error  - elist(maxind)

      ! Replace old interval with left half
      alist(maxind) = a1
      blist(maxind) = c
      rlist(maxind) = area1
      elist(maxind) = err1

      ! Add right half as a new interval
      nint = nint + 1
      if (nint > maxint) exit
      alist(nint) = c
      blist(nint) = b1
      rlist(nint) = area2
      elist(nint) = err2

      total_result = total_result + area1 + area2
      total_error  = total_error  + err1  + err2

      converged = (total_error <= max(epsabs, epsrel*abs(total_result)))
    end do

    result = total_result
    abserr = total_error
  end subroutine dqag_k15

  ! =====================================================================
  ! Gauss-Kronrod quadrature node rule (15-point)
  ! =====================================================================

  subroutine qk15(f, a, b, result, abserr)
    !! 15-point Gauss-Kronrod quadrature on interval [a,b].
    !!
    !! Evaluates the integrand at all 15 Kronrod nodes and computes both
    !! the 15-point Kronrod estimate and the nested 7-point Gauss estimate.
    !! The difference is used as an error indicator.
    procedure(func1d) :: f
    !! 1D integrand
    real(8), intent(in)  :: a, b
    !! Integration interval
    real(8), intent(out) :: result, abserr
    !! Integral value and error estimate

    real(8) :: center, hlgth
    real(8) :: fv(15)
    real(8) :: resk, resg
    integer :: j

    center = 0.5d0 * (a + b)
    hlgth  = 0.5d0 * (b - a)

    ! Evaluate function at all 15 Kronrod nodes (mapped to [a,b])
    do j = 1, 15
      fv(j) = f(center + hlgth * xgk(j))
    end do

    ! 15-point Kronrod rule
    resk = sum(wgk * fv)
    ! 7-point nested Gauss rule (subset of Kronrod nodes)
    resg = sum(wg * fv(igauss))

    ! Scale to interval [a,b] and estimate error
    result = resk * hlgth
    abserr = abs(resk - resg) * hlgth
  end subroutine qk15

end module mImpedance
