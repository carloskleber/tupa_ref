program test_bessel
  !! Pins `mBessel` (ADR 0031): I0(z)/I1(z) against the AMOS reference table
  !! `test/data/bessel_ratio_reference.csv` (regenerate with
  !! `gen_bessel_reference.py`), the switch between the series and the
  !! asymptotic expansion, the small-argument limit, and the z = 0 guard.
  use mCtes, only: dp
  use mBessel
  use check
  use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
  implicit none

  character(len=*), parameter :: REF_FILE = "test/data/bessel_ratio_reference.csv"
  integer :: unit, ios, n, nBad
  character(len=256) :: header
  real(dp) :: re, im, rr, ri, err, worst
  complex(dp) :: z, ref, got, below, above

  call test_init("besselI0OverI1 vs AMOS reference table (ADR 0031)")

  open(newunit=unit, file=REF_FILE, status="old", action="read", iostat=ios)
  call test_ok("reference table opens", ios == 0, "cannot open " // REF_FILE // " (run from fortran/)")
  if (ios == 0) then
    read(unit, '(A)') header
    n = 0
    nBad = 0
    worst = 0.0_dp
    do
      read(unit, *, iostat=ios) re, im, rr, ri
      if (ios /= 0) exit
      z = cmplx(re, im, kind=dp)
      ref = cmplx(rr, ri, kind=dp)
      got = besselI0OverI1(z)
      err = abs(got - ref) / abs(ref)
      if (.not. (err <= 1.0e-13_dp)) nBad = nBad + 1
      if (err > worst) worst = err
      n = n + 1
    end do
    close(unit)
    call test_ok("reference table has the expected size", n > 1000, "too few rows")
    call test_ok("all points within 1e-13 relative", nBad == 0, "points outside 1e-13 of AMOS")
    print '(A,I0,A,ES9.2)', "  rows: ", n, "   worst relative error: ", worst
  end if

  call test_init("besselI0OverI1 limits and the series/asymptotic switch")

  ! Both branches agree where they meet (arg = 45°)
  z = cmplx(24.99_dp, 0.0_dp, kind=dp) * exp(cmplx(0.0_dp, 0.25_dp * atan(1.0_dp) * 4.0_dp, kind=dp))
  below = besselI0OverI1(z)
  above = besselI0OverI1(z * (25.01_dp / 24.99_dp))
  call test_ok("continuous across the switch |z| = 25 (45°)", abs(below - above) / abs(below) < 1.0e-3_dp)

  ! Small argument: I0/I1 -> 2/z
  z = cmplx(1.0e-3_dp, 1.0e-3_dp, kind=dp)
  got = besselI0OverI1(z)
  ref = 2.0_dp / z
  call test_ok("small argument: ratio -> 2/z", abs(got - ref) / abs(ref) < 1.0e-5_dp)

  ! Large argument: ratio -> 1 + 1/(2z) + 3/(8z²) + 3/(8z³)
  z = cmplx(2000.0_dp, 0.0_dp, kind=dp)
  got = besselI0OverI1(z)
  ref = 1.0_dp + 1.0_dp / (2.0_dp * z) + 3.0_dp / (8.0_dp * z**2) + 3.0_dp / (8.0_dp * z**3)
  call test_ok("large argument: asymptotic expansion", abs(got - ref) / abs(ref) < 1.0e-13_dp)

  ! Known value, real argument: I0(1)/I1(1) = 1.2660658777520082/0.5651591039924851
  got = besselI0OverI1(cmplx(1.0_dp, 0.0_dp, kind=dp))
  call test_ok("I0(1)/I1(1)", abs(real(got) - 1.2660658777520082_dp / 0.5651591039924851_dp) < 1.0e-14_dp)

  call test_ok("z = 0 gives NaN, not a silent value", ieee_is_nan(real(besselI0OverI1(cmplx(0.0_dp, 0.0_dp, kind=dp)))))

  call test_close()
end program test_bessel
