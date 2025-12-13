module mImpedance
  !! Routines related to electrodes´ impedance calculations (self and mutual)
  implicit none

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
    irule = 2
    ! Função original do IMSL, mas segue a mesma forma do SLATEC
    call TWODQ(FUNCBARRA, 0.0, la, LIMINF, LIMSUP, errabs, errrel, irule, res, errest)
    return

  end subroutine IMPMUTUA

  real(8) function FUNCBARRA (X, Y)
    !!
    real(8) X, Y
    real(8) a1(3), b1(3), va(3), vb(3), lb
    real(8) a, b, c, z
    common /params/ a1, b1, va, vb, lb
    integer j
    intrinsic dsqrt

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
    real(8) x

    LIMINF = 0.0
    return
  end function LIMINF

  real(8) function LIMSUP (x)
    real(8) x
    real(8) a1(3), b1(3), va(3), vb(3), la, lb
    common /params/ a1, b1, va, vb, lb

    LIMSUP = lb
    return
  end function LIMSUP


end module
