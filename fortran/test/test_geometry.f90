program test_geometry
  !! Tests for the geometry-factor layer (mGeometry) and internal impedance
  !! (mImpedance%internalImpedance), per ROADMAP.md Phase 1.
  use mCtes
  use mGeometry
  use mImpedance, only: internalImpedance
  use check
  implicit none

  real(8) :: l, r0, d, h, gClosed, gQuad
  real(8) :: a1(3), a2(3), b1(3), b2(3)
  real(8) :: p1(10,3), p2(10,3), radius(10)
  real(8) :: G(10,10), Gi(10,10), Rbar(10,10), Rbari(10,10), cosTheta(10,10), cosThetaI(10,10)
  real(8) :: gDirect
  integer :: i, j
  complex(8) :: Zint, ZintExpectedDC, ZintExpectedHF
  real(8) :: omega, sigma, mur, length

  ! ----------------------------------------------------------------
  ! Self geometry factor vs independent quadrature oracle.
  !
  ! g_self(l,r0) is defined (theory.md §4.2) as the axis-to-surface integral
  ! int_0^l int_0^l dx dy / sqrt((x-y)^2 + r0^2). That same integral is what
  ! mutualGeometryFactor computes for two PARALLEL, aligned segments offset
  ! by r0 (a genuinely independent code path: adaptive 2D quadrature via
  ! IMPMUTUA, vs. the closed-form selfGeometryFactor). This also directly
  ! disproves the original C++ `fatorGeometriaPropria` formula (missing a
  ! factor of 2) noted in ROADMAP.md gap 8.
  !
  ! forceNumeric=.true. is required here: a and b are parallel, so
  ! mutualGeometryFactor would otherwise take its own closed-form fast path
  ! (parallelGeometryFactor) instead of the quadrature oracle this test needs.
  ! ----------------------------------------------------------------
  call test_init("selfGeometryFactor vs quadrature oracle (theory.md §4.2)")

  l  = 10.0d0
  r0 = 0.01d0
  a1 = [0.0d0, 0.0d0, 0.0d0]
  a2 = [l, 0.0d0, 0.0d0]
  b1 = [0.0d0, r0, 0.0d0]
  b2 = [l, r0, 0.0d0]
  call mutualGeometryFactor(a1, a2, b1, b2, gQuad, forceNumeric=.true.)
  gClosed = selfGeometryFactor(l, r0)

  call test_ok("g_self(10, 0.01) matches quadrature to 1e-6 relative", &
               abs(gClosed - gQuad) < 1.0d-6 * abs(gQuad), &
               "closed form and quadrature disagree")

  ! ----------------------------------------------------------------
  ! Same closed form, general (non-self) aligned-parallel-segments case:
  ! the plan's "closed-form parallel-segments factor vs quadrature" test.
  ! ----------------------------------------------------------------
  call test_init("Aligned-parallel-segments closed form vs quadrature")

  l = 5.0d0
  d = 2.0d0
  a1 = [0.0d0, 0.0d0, 0.0d0]
  a2 = [l, 0.0d0, 0.0d0]
  b1 = [0.0d0, 0.0d0, -d]
  b2 = [l, 0.0d0, -d]
  call mutualGeometryFactor(a1, a2, b1, b2, gQuad, forceNumeric=.true.)
  gClosed = selfGeometryFactor(l, d)

  call test_ok("g(l=5, offset=2) matches quadrature to 1e-6 relative", &
               abs(gClosed - gQuad) < 1.0d-6 * abs(gQuad), &
               "closed form and quadrature disagree for a genuine (non-self) pair")

  ! ----------------------------------------------------------------
  ! Parallel-segment closed-form fast path (mutualGeometryFactor's default
  ! path), ported from the Matlab reference barraquad.m's posparal. Cases
  ! mirror mom_matlab/test/testesIntegralxAnalitica.m.
  !
  ! Touching/consecutive collinear cases (1-2) are NOT compared against
  ! forced quadrature: that is exactly the near-singular, slow-to-converge
  ! configuration the closed form exists to avoid (ROADMAP.md §5). Instead
  ! they check a property quadrature can't easily confirm anyway: g(a,b) is
  ! invariant to which way each segment's direction vector happens to point.
  ! ----------------------------------------------------------------
  call test_init("Parallel-segment closed form (posparal port) vs quadrature and self-consistency")

  ! Case 1/2 (testesIntegralxAnalitica.m): collinear, consecutive (touching),
  ! same case with segment a's direction vector reversed.
  a1 = [150.0d0, 0.0d0, 30.0d0]
  a2 = [150.0d0, 0.0d0, 27.5d0]
  b1 = [150.0d0, 0.0d0, 27.5d0]
  b2 = [150.0d0, 0.0d0, 25.0d0]
  call mutualGeometryFactor(a1, a2, b1, b2, gClosed)

  block
    real(8) :: gFlipped
    call mutualGeometryFactor(a2, a1, b1, b2, gFlipped)
    call test_ok("collinear touching: g invariant to segment a's direction", &
                 abs(gClosed - gFlipped) < 1.0d-9 * abs(gClosed), &
                 "reversing a's parametrisation must not change the mutual geometry factor")
  end block

  ! Case 3: collinear, non-consecutive (2.5 m gap) -- safe for quadrature.
  a1 = [150.0d0, 0.0d0, 30.0d0]
  a2 = [150.0d0, 0.0d0, 27.5d0]
  b1 = [150.0d0, 0.0d0, 25.0d0]
  b2 = [150.0d0, 0.0d0, 22.5d0]
  call mutualGeometryFactor(a1, a2, b1, b2, gClosed)
  call mutualGeometryFactor(a1, a2, b1, b2, gQuad, forceNumeric=.true.)
  call test_ok("collinear, non-consecutive: closed form matches quadrature", &
               abs(gClosed - gQuad) < 1.0d-4 * abs(gQuad), &
               "closed form and quadrature disagree for a collinear, non-touching pair")

  ! Case 4: parallel, offset = length (10 m) -- safe for quadrature.
  a1 = [0.0d0, 0.0d0, 1.0d0]
  a2 = [10.0d0, 0.0d0, 1.0d0]
  b1 = [0.0d0, 10.0d0, 1.0d0]
  b2 = [10.0d0, 10.0d0, 1.0d0]
  call mutualGeometryFactor(a1, a2, b1, b2, gClosed)
  call mutualGeometryFactor(a1, a2, b1, b2, gQuad, forceNumeric=.true.)
  call test_ok("parallel, far offset: closed form matches quadrature", &
               abs(gClosed - gQuad) < 1.0d-4 * abs(gQuad), &
               "closed form and quadrature disagree for a widely-offset parallel pair")

  ! Case 6/7: parallel, offset = length/100 (0.1 m over 10 m) -- moderately
  ! close but not touching; safe for quadrature. Case 7 reverses b's
  ! direction vector and swaps its endpoints, so it must give the same g.
  a1 = [0.0d0, 0.0d0, 1.0d0]
  a2 = [10.0d0, 0.0d0, 1.0d0]
  b1 = [0.0d0, 0.1d0, 1.0d0]
  b2 = [10.0d0, 0.1d0, 1.0d0]
  call mutualGeometryFactor(a1, a2, b1, b2, gClosed)
  call mutualGeometryFactor(a1, a2, b1, b2, gQuad, forceNumeric=.true.)
  call test_ok("parallel, offset = length/100: closed form matches quadrature", &
               abs(gClosed - gQuad) < 1.0d-3 * abs(gQuad), &
               "closed form and quadrature disagree for a close parallel pair")

  block
    real(8) :: gOpposite
    call mutualGeometryFactor(a1, a2, b2, b1, gOpposite)
    call test_ok("parallel, offset = length/100: g invariant to b's direction", &
                 abs(gClosed - gOpposite) < 1.0d-9 * abs(gClosed), &
                 "reversing b's parametrisation must not change the mutual geometry factor")
  end block

  ! forceNumeric must actually route through IMPMUTUA: a non-parallel pair
  ! only ever uses quadrature, so it must agree with itself regardless of
  ! the flag (sanity check that the flag doesn't corrupt the non-parallel path).
  a1 = [0.0d0, 0.0d0, 0.0d0]
  a2 = [1.0d0, 0.0d0, 0.0d0]
  b1 = [0.0d0, 1.0d0, 0.0d0]
  b2 = [0.0d0, 1.0d0, 1.0d0]
  call mutualGeometryFactor(a1, a2, b1, b2, gClosed)
  call mutualGeometryFactor(a1, a2, b1, b2, gQuad, forceNumeric=.true.)
  call test_ok("non-parallel pair: forceNumeric is a no-op (both paths are quadrature)", &
               abs(gClosed - gQuad) < 1.0d-12 * abs(gQuad), &
               "a non-parallel pair must give the identical result regardless of forceNumeric")

  ! ----------------------------------------------------------------
  ! Direction cosines, including image (z-flip)
  ! ----------------------------------------------------------------
  call test_init("directionCosine and imageVector (theory.md §4-5)")

  call test_ok("parallel vectors: cos = 1", &
               abs(directionCosine([1.0d0,0.0d0,0.0d0], [2.0d0,0.0d0,0.0d0]) - 1.0d0) < 1.0d-14, &
               "cos(theta) should be 1 for parallel vectors")
  call test_ok("antiparallel vectors: cos = -1", &
               abs(directionCosine([1.0d0,0.0d0,0.0d0], [-1.0d0,0.0d0,0.0d0]) + 1.0d0) < 1.0d-14, &
               "cos(theta) should be -1 for antiparallel vectors")
  call test_ok("perpendicular vectors: cos = 0", &
               abs(directionCosine([1.0d0,0.0d0,0.0d0], [0.0d0,1.0d0,0.0d0])) < 1.0d-14, &
               "cos(theta) should be 0 for perpendicular vectors")
  call test_ok("horizontal direction: image cosine = +1", &
               abs(directionCosine([1.0d0,0.0d0,0.0d0], imageVector([1.0d0,0.0d0,0.0d0])) - 1.0d0) < 1.0d-14, &
               "a horizontal segment's image direction must equal its own direction")
  call test_ok("vertical direction: image cosine = -1", &
               abs(directionCosine([0.0d0,0.0d0,-1.0d0], imageVector([0.0d0,0.0d0,-1.0d0])) + 1.0d0) < 1.0d-14, &
               "a vertical segment's image direction must be reversed")

  ! ----------------------------------------------------------------
  ! Image mean distance for a buried horizontal wire: Rbari = 2h (theory.md §5)
  ! ----------------------------------------------------------------
  call test_init("Image mean distance for a buried horizontal wire (theory.md §5)")

  h = 0.5d0
  p1(1,:) = [0.0d0, 0.0d0, -h]
  p2(1,:) = [10.0d0, 0.0d0, -h]
  radius(1) = 0.007d0
  call buildGeometryMatrices(p1(1:1,:), p2(1:1,:), radius(1:1), 1, &
                             G(1:1,1:1), Gi(1:1,1:1), Rbar(1:1,1:1), Rbari(1:1,1:1), &
                             cosTheta(1:1,1:1), cosThetaI(1:1,1:1))

  call test_ok("Rbari(1,1) == 2h == 1.0", abs(Rbari(1,1) - 1.0d0) < 1.0d-12, &
               "self-image mean distance must be twice the burial depth")
  call test_ok("cosThetaI(1,1) == 1 (horizontal self-image)", &
               abs(cosThetaI(1,1) - 1.0d0) < 1.0d-12, &
               "horizontal segment's self-image direction cosine must be +1")

  ! ----------------------------------------------------------------
  ! Full 10 m / 10-segment buried line: matrix symmetry and per-pair wiring
  ! (ROADMAP.md Phase 1 exit criterion)
  ! ----------------------------------------------------------------
  call test_init("buildGeometryMatrices: 10 m line, 10 segments")

  do i = 1, 10
    p1(i,:) = [real(i-1, kind=8), 0.0d0, -h]
    p2(i,:) = [real(i, kind=8),   0.0d0, -h]
    radius(i) = 0.007d0
  end do

  call buildGeometryMatrices(p1, p2, radius, 10, G, Gi, Rbar, Rbari, cosTheta, cosThetaI)

  call test_ok("G is symmetric", all(abs(G - transpose(G)) < 1.0d-12), "G(i,j) /= G(j,i)")
  call test_ok("Gi is symmetric", all(abs(Gi - transpose(Gi)) < 1.0d-12), "Gi(i,j) /= Gi(j,i)")
  call test_ok("Rbar is symmetric", all(abs(Rbar - transpose(Rbar)) < 1.0d-12), "Rbar(i,j) /= Rbar(j,i)")

  ! Cross-check three representative pairs against standalone quadrature calls
  ! on the exact same endpoints (catches indexing/wiring bugs in the fill loop).
  block
    integer :: pairs(3,2)
    pairs = reshape([1,1,3, 2,10,7], [3,2])
    do i = 1, 3
      call mutualGeometryFactor(p1(pairs(i,1),:), p2(pairs(i,1),:), &
                                 p1(pairs(i,2),:), p2(pairs(i,2),:), gDirect)
      call test_ok("G matrix entry matches standalone call for pair", &
                   abs(G(pairs(i,1), pairs(i,2)) - gDirect) < 1.0d-12 * abs(gDirect), &
                   "buildGeometryMatrices fill loop disagrees with a direct call")
    end do
  end block

  call test_ok("G diagonal matches selfGeometryFactor", &
               abs(G(1,1) - selfGeometryFactor(1.0d0, 0.007d0)) < 1.0d-12, &
               "self entries must use the closed form, not quadrature")

  ! ----------------------------------------------------------------
  ! Internal (skin-effect) impedance: DC and high-frequency limits (theory.md §4.3)
  ! ----------------------------------------------------------------
  call test_init("internalImpedance DC and high-frequency limits")

  r0 = 0.005d0
  sigma = 5.8d7   ! copper
  mur = 1.0d0
  length = 1.0d0

  ! DC limit: rho -> 0, I0/I1 -> 2/rho, so Zint/length -> 1/(sigma*pi*r0^2) (real)
  omega = 1.0d-6
  Zint = internalImpedance(r0, length, omega, sigma, mur)
  ZintExpectedDC = cmplx(1.0d0 / (sigma * PI * r0 * r0), 0.0d0, kind=8)
  call test_ok("Zint -> DC resistance as omega -> 0", &
               abs(Zint - ZintExpectedDC) < 1.0d-6 * abs(ZintExpectedDC), &
               "internal impedance does not reduce to 1/(sigma*pi*r0^2) at DC")

  ! High-frequency limit: rho >> 1, ratio -> 1, Zint/length -> sqrt(j*omega*mu/sigma)/(2*pi*r0)
  omega = 1.0d12
  Zint = internalImpedance(r0, length, omega, sigma, mur)
  ZintExpectedHF = sqrt(cmplx(0.0d0, omega, kind=8) * mur * MU0 / sigma) / (2.0d0 * PI * r0)
  call test_ok("Zint -> surface-impedance asymptote at high omega", &
               abs(Zint - ZintExpectedHF) < 1.0d-3 * abs(ZintExpectedHF), &
               "internal impedance does not reduce to the high-frequency asymptote")

  call test_ok("Re(Zint) > 0 (passive, resistive)", real(Zint) > 0.0d0, &
               "internal impedance must be dissipative")

  call test_summary()

end program test_geometry
