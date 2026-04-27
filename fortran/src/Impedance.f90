module mImpedance
  !! Routines related to electrodes´ impedance calculations (self and mutual)
  implicit none
    abstract interface
    real(8) function func2d(x, y)
      real(8), intent(in) :: x, y
    end function func2d
    real(8) function func1d(x)
      real(8), intent(in) :: x
    end function func1d
  end interface

  ! Module variables used for nested integration
  procedure(func2d), pointer :: pF => null()
  procedure(func1d), pointer :: pG => null()
  procedure(func1d), pointer :: pH => null()
  real(8) :: m_epsabs, m_epsrel, m_a, m_b

  ! Private constants for Gauss-Kronrod integration
  integer, parameter :: maxint = 500               ! max number of subintervals
  real(8), parameter :: xgk(15) = [ &
       -0.9914553711208126D+00, -0.9491079123427585D+00, &
       -0.8648644233597691D+00, -0.7415311855993944D+00, &
       -0.5860872354676911D+00, -0.4058451513773972D+00, &
       -0.2077849550078985D+00,  0.0000000000000000D+00, &
        0.2077849550078985D+00,  0.4058451513773972D+00, &
        0.5860872354676911D+00,  0.7415311855993944D+00, &
        0.8648644233597691D+00,  0.9491079123427585D+00, &
        0.9914553711208126D+00 ]
  real(8), parameter :: wgk(15) = [ &
        0.02293532201052922D+00,  0.06309209262997855D+00, &
        0.1047900103222502D+00,   0.1406532597155259D+00, &
        0.1690047266392679D+00,   0.1903505780647854D+00, &
        0.2044329400752989D+00,   0.2094821410847278D+00, &
        0.2044329400752989D+00,   0.1903505780647854D+00, &
        0.1690047266392679D+00,   0.1406532597155259D+00, &
        0.1047900103222502D+00,   0.06309209262997855D+00, &
        0.02293532201052922D+00 ]
  ! 7-point Gauss weights (nested subset of Kronrod rule)
  ! indices in xgk: 2,4,6,8,10,12,14
  real(8), parameter :: wg(7) = [ &
        0.06309209262997855D+00,  0.1406532597155259D+00, &
        0.1903505780647854D+00,   0.2094821410847278D+00, &
        0.1903505780647854D+00,   0.1406532597155259D+00, &
        0.06309209262997855D+00 ]
  integer, parameter :: igauss(7) = [2, 4, 6, 8, 10, 12, 14]

  public :: IMPMUTUA, FUNCBARRA, LIMINF, LIMSUP, TWODQ

contains
  subroutine IMPMUTUA (za1, zva, zla, zb1, zvb, zlb, res)
    !!
    implicit none
    real(8) za1(3), zb1(3), zva(3), zvb(3), zla, zlb, res
    real(8) a1(3), b1(3), va(3), vb(3), la, lb
    integer irule
    common /params/ a1, b1, va, vb, lb
    real(8) errabs, errrel, errest

    a1 = za1
    b1 = zb1
    va = zva
    vb = zvb
    la = zla
    lb = zlb
    errabs = 0.0
    errrel = dmin1(la, lb) * 1e-6
    call TWODQ(FUNCBARRA, 0.0d0, la, LIMINF, LIMSUP, errabs, errrel, res, errest)
    return
  end subroutine IMPMUTUA

  real(8) function FUNCBARRA (X, Y)
    !!
    !! Integrand 1 / distance between points on two line segments
    real(8), intent(in) :: X, Y
    real(8) a1(3), b1(3), va(3), vb(3), lb
    real(8) a, b, c, z
    common /params/ a1, b1, va, vb, lb
    integer j

    z = 0.0
    do j = 1,3
      a = a1(j) + va(j) * X
      b = b1(j) + vb(j) * Y
      c = b - a
      z = z + c * c
    end do
    FUNCBARRA = 1 / dsqrt(z)
    return
  end function FUNCBARRA

  real(8) function LIMINF (x)
    !!
    !! Lower limit of inner integral (constant 0)
    real(8), intent(in) :: x

    LIMINF = 0.0d0
    return
  end function LIMINF

  real(8) function LIMSUP (x)
    !!
    !! Upper limit of inner integral (constant lb)

    real(8), intent(in) :: x
    real(8) a1(3), b1(3), va(3), vb(3), la, lb
    common /params/ a1, b1, va, vb, lb

    LIMSUP = lb
    return
  end function LIMSUP

    ! Replacement for IMSL TWODQ
  subroutine TWODQ(F, A, B, G, H, ERRABS, ERRREL, RESULT, ERREST)
    ! Double integration of F(X,Y) over X in [A,B], Y in [G(X),H(X)]
    procedure(func2d) :: F
    procedure(func1d) :: G, H
    real(8), intent(in)  :: A, B, ERRABS, ERRREL
    real(8), intent(out) :: RESULT, ERREST

    pF => F
    pG => G
    pH => H
    m_epsabs = ERRABS
    m_epsrel = ERRREL
    m_a = A
    m_b = B

    call dqag_k15(outer_fcn, A, B, ERRABS, ERRREL, RESULT, ERREST)
  end subroutine TWODQ


  real(8) function outer_fcn(x)
    !!
    !! Outer function: returns inner integral for a given X
    real(8), intent(in) :: x
    real(8) :: inner_epsabs, inner_result, inner_err

    ! Heuristic: apportion absolute tolerance equally over the X-length
    inner_epsabs = 0.5d0 * m_epsabs / max(1.0d0, (m_b - m_a))
    call dqag_k15(inner_wrap, pG(x), pH(x), inner_epsabs, m_epsrel, &
                  inner_result, inner_err)
    outer_fcn = inner_result

  contains
    real(8) function inner_wrap(y)
      real(8), intent(in) :: y
      inner_wrap = pF(x, y)
    end function inner_wrap
  end function outer_fcn


  subroutine dqag_k15(f, a, b, epsabs, epsrel, result, abserr)
    !!
    !! Adaptive 1D integration using 15-point Gauss-Kronrod rule
    procedure(func1d) :: f
    real(8), intent(in)  :: a, b, epsabs, epsrel
    real(8), intent(out) :: result, abserr

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

    do while (.not. converged .and. nint < maxint)
      ! Locate interval with largest error
      maxind = 1
      do i = 2, nint
        if (elist(i) > elist(maxind)) maxind = i
      end do

      a1 = alist(maxind)
      b1 = blist(maxind)
      c  = 0.5d0 * (a1 + b1)

      ! Compute both halves
      call qk15(f, a1, c, area1, err1)
      call qk15(f, c, b1, area2, err2)

      ! Remove old contribution
      total_result = total_result - rlist(maxind)
      total_error  = total_error  - elist(maxind)

      ! Replace old interval with left half
      alist(maxind) = a1
      blist(maxind) = c
      rlist(maxind) = area1
      elist(maxind) = err1

      ! Add right half as a new interval
      nint = nint + 1
      if (nint > maxint) exit      ! exceed storage – stop subdividing
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


  subroutine qk15(f, a, b, result, abserr)
    !!
    !! 15-point Gauss-Kronrod quadrature on [a,b]

    procedure(func1d) :: f
    real(8), intent(in)  :: a, b
    real(8), intent(out) :: result, abserr

    real(8) :: center, hlgth
    real(8) :: fv(15), resk, resg
    integer :: j

    center = 0.5d0 * (a + b)
    hlgth  = 0.5d0 * (b - a)

    ! Evaluate function at all 15 Kronrod nodes
    forall(j = 1:15) fv(j) = f(center + hlgth * xgk(j))

    resk = sum(wgk * fv)                   ! 15-point value
    resg = sum(wg  * fv(igauss))           !  7-point Gauss value (nested)

    result = resk * hlgth
    abserr = abs(resk - resg) * hlgth      ! simple error estimate
  end subroutine qk15
end module mImpedance
