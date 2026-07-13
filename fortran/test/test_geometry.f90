program test_geometry
  !! Tests for the geometry-factor layer (mGeometry) and internal impedance
  !! (mImpedance%internalImpedance), per ROADMAP.md Phase 1.
  use mCtes
  use mGeometry
  use mImpedance, only: internalImpedance, setQuadEpsRel, getQuadEpsRel
  use mGeometryCache, only: geomCacheClear, geomCacheStats, geomCacheSetEnabled
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
  ! geometryFactor2D, vs. the closed-form selfGeometryFactor). This also directly
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
  ! mirror all 8 cases of mom_matlab/test/testesIntegralxAnalitica.m (its
  ! Cases 8a/8b/8c are the X/Y/Z-aligned repeats of one offset ratio).
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

  ! Case 5: parallel, offset = length/10 (1 m over 10 m) -- safe for quadrature.
  a1 = [0.0d0, 0.0d0, 1.0d0]
  a2 = [10.0d0, 0.0d0, 1.0d0]
  b1 = [0.0d0, 1.0d0, 1.0d0]
  b2 = [10.0d0, 1.0d0, 1.0d0]
  call mutualGeometryFactor(a1, a2, b1, b2, gClosed)
  call mutualGeometryFactor(a1, a2, b1, b2, gQuad, forceNumeric=.true.)
  call test_ok("parallel, offset = length/10: closed form matches quadrature", &
               abs(gClosed - gQuad) < 1.0d-4 * abs(gQuad), &
               "closed form and quadrature disagree for an offset = length/10 parallel pair")

  ! Cases 8a/8b/8c: parallel, offset = length/1000 (0.1 m over 100 m), with
  ! the pair oriented along each of the three axes in turn -- checks that the
  ! closed form (and its (xi1,xi2,d..) bookkeeping) is axis-orientation
  ! invariant, not just correct for the X-aligned case already exercised above.
  a1 = [0.0d0, 0.0d0, 1.0d0]
  a2 = [100.0d0, 0.0d0, 1.0d0]
  b1 = [0.0d0, 0.1d0, 1.0d0]
  b2 = [100.0d0, 0.1d0, 1.0d0]
  call mutualGeometryFactor(a1, a2, b1, b2, gClosed)
  call mutualGeometryFactor(a1, a2, b1, b2, gQuad, forceNumeric=.true.)
  call test_ok("parallel along X, offset = length/1000: closed form matches quadrature", &
               abs(gClosed - gQuad) < 1.0d-3 * abs(gQuad), &
               "closed form and quadrature disagree for an X-aligned offset = length/1000 parallel pair")

  a1 = [0.0d0, 0.0d0, 1.0d0]
  a2 = [0.0d0, 100.0d0, 1.0d0]
  b1 = [0.1d0, 0.0d0, 1.0d0]
  b2 = [0.1d0, 100.0d0, 1.0d0]
  call mutualGeometryFactor(a1, a2, b1, b2, gClosed)
  call mutualGeometryFactor(a1, a2, b1, b2, gQuad, forceNumeric=.true.)
  call test_ok("parallel along Y, offset = length/1000: closed form matches quadrature", &
               abs(gClosed - gQuad) < 1.0d-3 * abs(gQuad), &
               "closed form and quadrature disagree for a Y-aligned offset = length/1000 parallel pair")

  a1 = [0.0d0, 0.0d0, 1.0d0]
  a2 = [0.0d0, 0.0d0, 101.0d0]
  b1 = [0.0d0, 0.1d0, 1.0d0]
  b2 = [0.0d0, 0.1d0, 101.0d0]
  call mutualGeometryFactor(a1, a2, b1, b2, gClosed)
  call mutualGeometryFactor(a1, a2, b1, b2, gQuad, forceNumeric=.true.)
  call test_ok("parallel along Z, offset = length/1000: closed form matches quadrature", &
               abs(gClosed - gQuad) < 1.0d-3 * abs(gQuad), &
               "closed form and quadrature disagree for a Z-aligned offset = length/1000 parallel pair")

  ! forceNumeric must actually route through geometryFactor2D: a non-parallel pair
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
  ! Numeric quadrature tolerance sweep vs closed form, mirroring
  ! testesIntegralxAnalitica.m's roda() helper (which sweeps INT_TOL over
  ! 1e-2, 1e-3, 1e-4 and prints numeric vs analytic side by side). Here the
  ! knob is geometryFactor2D's epsrel (setQuadEpsRel/getQuadEpsRel), and the
  ! "analytic" reference is the closed-form parallelGeometryFactor. Loosening
  ! epsrel only visibly degrades quadrature accuracy on a close (sharply
  ! peaked) pair; the far Case-4-style pair above is already accurate to
  ! machine precision even at epsrel=1e-1, so it is not repeated here.
  ! ----------------------------------------------------------------
  call test_init("Quadrature tolerance sweep vs closed form (testesIntegralxAnalitica.m INT_TOL sweep)")

  block
    real(8) :: epsrel(4), err(4), gTol
    integer :: k

    ! Case 6 config (offset = length/100): moderately close pair.
    epsrel = [1.0d-1, 5.0d-2, 2.0d-2, 1.0d-2]
    a1 = [0.0d0, 0.0d0, 1.0d0]
    a2 = [10.0d0, 0.0d0, 1.0d0]
    b1 = [0.0d0, 0.1d0, 1.0d0]
    b2 = [10.0d0, 0.1d0, 1.0d0]
    call mutualGeometryFactor(a1, a2, b1, b2, gClosed)

    do k = 1, 4
      call setQuadEpsRel(epsrel(k))
      call mutualGeometryFactor(a1, a2, b1, b2, gTol, forceNumeric=.true.)
      err(k) = abs(gTol - gClosed) / abs(gClosed)
    end do
    call setQuadEpsRel(1.0d-6)

    call test_ok("offset = length/100: relative error shrinks monotonically as epsrel tightens", &
                 err(1) >= err(2) .and. err(2) >= err(3) .and. err(3) >= err(4), &
                 "loosening epsrel should not make the quadrature agree better with the closed form")
    call test_ok("offset = length/100: loosest epsrel (1e-1) is visibly inaccurate", &
                 err(1) > 1.0d-2, &
                 "epsrel=1e-1 was expected to show a visible (percent-level) discrepancy here")
    call test_ok("offset = length/100: tightest epsrel (1e-2) matches closed form to 1e-6 relative", &
                 err(4) < 1.0d-6, &
                 "epsrel=1e-2 quadrature strayed more than 1e-6 relative from the closed form")

    ! Case 8a config (offset = length/1000): closer pair, harder integrand --
    ! needs tighter epsrel before the sweep converges.
    epsrel = [1.0d-1, 1.0d-2, 1.0d-3, 1.0d-4]
    a1 = [0.0d0, 0.0d0, 1.0d0]
    a2 = [100.0d0, 0.0d0, 1.0d0]
    b1 = [0.0d0, 0.1d0, 1.0d0]
    b2 = [100.0d0, 0.1d0, 1.0d0]
    call mutualGeometryFactor(a1, a2, b1, b2, gClosed)

    do k = 1, 4
      call setQuadEpsRel(epsrel(k))
      call mutualGeometryFactor(a1, a2, b1, b2, gTol, forceNumeric=.true.)
      err(k) = abs(gTol - gClosed) / abs(gClosed)
    end do
    call setQuadEpsRel(1.0d-6)

    call test_ok("offset = length/1000: relative error shrinks monotonically as epsrel tightens", &
                 err(1) >= err(2) .and. err(2) >= err(3) .and. err(3) >= err(4), &
                 "loosening epsrel should not make the quadrature agree better with the closed form")
    call test_ok("offset = length/1000: loosest epsrel (1e-1) is grossly inaccurate", &
                 err(1) > 1.0d0, &
                 "epsrel=1e-1 was expected to show a gross (>100% relative) discrepancy for this closer pair")
    call test_ok("offset = length/1000: tightest epsrel (1e-4) matches closed form to 1e-8 relative", &
                 err(4) < 1.0d-8, &
                 "epsrel=1e-4 quadrature strayed more than 1e-8 relative from the closed form")

    call test_ok("getQuadEpsRel restored to default 1e-6 after the sweep", &
                 getQuadEpsRel() == 1.0d-6, &
                 "the sweep must leave global quadrature state as it found it for later tests")
  end block

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
  ! Determinism (ROADMAP.md Phase 3 item 4 / P6): the fill loop's write
  ! pattern (each matrix entry (r,c) is written by exactly one outer
  ! iteration i = min(r,c)) is race-free and OpenMP-ready, but the loop is
  ! NOT parallelised yet — mImpedance's quadrature fallback keeps
  ! non-reentrant module-level state (ARCHITECTURE.md §7), so parallelising
  ! today would race whenever a pair takes that path. This test pins
  ! today's serial determinism as a baseline, to be re-run once OpenMP
  ! actually lands (after the mImpedance reentrancy fix).
  ! ----------------------------------------------------------------
  call test_init("buildGeometryMatrices is deterministic across repeated calls")

  block
    real(8) :: G2(10,10), Gi2(10,10), Rbar2(10,10), Rbari2(10,10), cosTheta2(10,10), cosThetaI2(10,10)

    call buildGeometryMatrices(p1, p2, radius, 10, G2, Gi2, Rbar2, Rbari2, cosTheta2, cosThetaI2)

    call test_ok("G bit-identical across two calls", all(G == G2), "geometry factor matrix is nondeterministic")
    call test_ok("Gi bit-identical across two calls", all(Gi == Gi2), "image geometry factor matrix is nondeterministic")
    call test_ok("Rbar bit-identical across two calls", all(Rbar == Rbar2), "mean distance matrix is nondeterministic")
    call test_ok("Rbari bit-identical across two calls", all(Rbari == Rbari2), "image mean distance matrix is nondeterministic")
    call test_ok("cosTheta bit-identical across two calls", all(cosTheta == cosTheta2), "direction cosine matrix is nondeterministic")
    call test_ok("cosThetaI bit-identical across two calls", all(cosThetaI == cosThetaI2), "image direction cosine matrix is nondeterministic")
  end block

  ! ----------------------------------------------------------------
  ! Quadrature memo table (mGeometryCache): congruent pairs must hit the
  ! cache and return the identical value; non-congruent pairs that share
  ! the four cross endpoint distances but differ in segment length must
  ! NOT collide (the lengths are part of the key precisely for this).
  ! ----------------------------------------------------------------
  call test_init("Geometry-factor quadrature cache (mGeometryCache)")

  block
    real(8) :: gFirst, gCongruent, gUncached, gShort, t(3)
    integer(8) :: hits0, hits1, misses0, misses1
    integer :: entries

    ! Non-parallel (perpendicular) pair: always takes the quadrature path.
    a1 = [0.0d0, 0.0d0, 0.0d0]
    a2 = [1.0d0, 0.0d0, 0.0d0]
    b1 = [2.0d0, 1.0d0, 0.0d0]
    b2 = [2.0d0, 1.0d0, 1.0d0]

    call geomCacheClear()
    call mutualGeometryFactor(a1, a2, b1, b2, gFirst)
    call geomCacheStats(hits0, misses0, entries)

    ! Congruent copy: translated by (5, -3, 2), with segment a's endpoint
    ! order reversed — the canonical key must absorb both.
    t = [5.0d0, -3.0d0, 2.0d0]
    call mutualGeometryFactor(a2 + t, a1 + t, b1 + t, b2 + t, gCongruent)
    call geomCacheStats(hits1, misses1, entries)

    call test_ok("congruent (translated + reversed) pair hits the cache", &
                 hits1 == hits0 + 1, &
                 "a rigid copy of an already-integrated pair must not re-run TWODQ")
    call test_ok("cache hit returns the bit-identical value", gCongruent == gFirst, &
                 "cached geometry factor differs from the first computation")

    ! The cached value must agree with an independent (cache-disabled) run.
    call geomCacheSetEnabled(.false.)
    call mutualGeometryFactor(a2 + t, a1 + t, b1 + t, b2 + t, gUncached)
    call geomCacheSetEnabled(.true.)
    call test_ok("cache-disabled quadrature agrees with the cached value", &
                 abs(gUncached - gFirst) < 1.0d-9 * abs(gFirst), &
                 "memoised value disagrees with a fresh quadrature of the congruent pair")

    ! Same four cross distances, different lengths: la = lb = 1 with b at
    ! y = 1 gives all cross distances sqrt(0.25 + 1 + 0.25) = sqrt(1.5);
    ! halving a to la = 0.5 and moving b to y = sqrt(1.1875) keeps all four
    ! at sqrt(0.0625 + 1.1875 + 0.25) = sqrt(1.5) while g clearly changes.
    call geomCacheClear()
    a1 = [-0.5d0, 0.0d0, 0.0d0]
    a2 = [ 0.5d0, 0.0d0, 0.0d0]
    b1 = [0.0d0, 1.0d0, -0.5d0]
    b2 = [0.0d0, 1.0d0,  0.5d0]
    call mutualGeometryFactor(a1, a2, b1, b2, gFirst)
    a1 = [-0.25d0, 0.0d0, 0.0d0]
    a2 = [ 0.25d0, 0.0d0, 0.0d0]
    b1 = [0.0d0, sqrt(1.1875d0), -0.5d0]
    b2 = [0.0d0, sqrt(1.1875d0),  0.5d0]
    call mutualGeometryFactor(a1, a2, b1, b2, gShort)
    call geomCacheStats(hits1, misses1, entries)

    call test_ok("equal cross distances, different lengths: no false hit", &
                 hits1 == 0 .and. misses1 == 2, &
                 "two non-congruent pairs sharing the four cross distances collided in the cache")
    call test_ok("the two configurations indeed differ", &
                 abs(gShort - gFirst) > 1.0d-2 * abs(gFirst), &
                 "test premise broken: the two configurations should have distinct g")
  end block

  ! ----------------------------------------------------------------
  ! Quadrature tolerance control (CLI --epsrel -> setQuadEpsRel)
  ! ----------------------------------------------------------------
  call test_init("setQuadEpsRel controls the geometryFactor2D tolerance")

  block
    real(8) :: gTight, gLoose
    integer(8) :: hits1, misses1
    integer :: entries

    a1 = [0.0d0, 0.0d0, 0.0d0]
    a2 = [1.0d0, 0.0d0, 0.0d0]
    b1 = [2.0d0, 1.0d0, 0.0d0]
    b2 = [2.0d0, 1.0d0, 1.0d0]

    call setQuadEpsRel(1.0d-6)
    call mutualGeometryFactor(a1, a2, b1, b2, gTight)
    call setQuadEpsRel(1.0d-2)
    call geomCacheStats(hits1, misses1, entries)
    call test_ok("changing epsrel clears the cache", entries == 0, &
                 "cached values computed at the old tolerance must be dropped")
    call mutualGeometryFactor(a1, a2, b1, b2, gLoose)
    call setQuadEpsRel(1.0d-6)

    call test_ok("getQuadEpsRel returns the restored default", &
                 getQuadEpsRel() == 1.0d-6, "setter/getter round trip failed")
    call test_ok("loose-tolerance result stays within its own tolerance", &
                 abs(gLoose - gTight) < 1.0d-2 * abs(gTight), &
                 "epsrel = 1e-2 quadrature strayed more than 1e-2 from the tight result")
  end block

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
