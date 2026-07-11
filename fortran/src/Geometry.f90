module mGeometry
  !! Geometry-factor layer: mean/image distances, real geometry factors, and
  !! direction cosines for pairs of straight cylindrical segments
  !! (theory.md §4-5, ADR 0004). Frequency-independent; computed once per
  !! geometry and reused by the per-frequency impedance fill (Phase 2).
  !!
  !! This module has no dependency on the object model (`tStructure`,
  !! `tElectrode`) — it operates on plain segment endpoints and radii, so it
  !! can be tested and reasoned about in isolation.
  use mImpedance, only: geometryFactor2D
  use mGeometryCache, only: geomCacheKey, geomCacheGet, geomCachePut, &
                            geomCacheClear, geomCacheIsEnabled
  implicit none
  private

  public :: segmentVector, selfGeometryFactor, mutualGeometryFactor, &
            directionCosine, imageVector, meanDistance, buildGeometryMatrices

  real(8), parameter :: PARALLEL_TOL = 1.0d-20
  !! Threshold on |va x vb| below which two unit direction vectors are
  !! treated as parallel (matches the Matlab reference `barraquad`'s
  !! `cruz < 1e-20` check). Tight on purpose: only trips the closed-form
  !! fast path for genuinely (near-machine-precision) parallel segments,
  !! not merely near-parallel ones, since the formula assumes exact
  !! parallelism.
  real(8), parameter :: NUMP = 1.0d-6
  !! Collinearity/touching-endpoint tolerance used inside the closed-form
  !! parallel-segment formula, matching the Matlab reference's default
  !! `NUMP` global (`inicvars.m`).

contains

  ! =====================================================================
  ! Segment decomposition
  ! =====================================================================

  subroutine segmentVector(p1, p2, dir, length)
    !! Decompose a segment into a unit direction vector and its length.
    real(8), intent(in)  :: p1(:), p2(:)
    !! Segment endpoints (m)
    real(8), intent(out) :: dir(:)
    !! Unit direction vector from p1 to p2
    real(8), intent(out) :: length
    !! Segment length (m)
    real(8) :: d(3)

    d = p2 - p1
    length = norm2(d)
    dir = d / length
  end subroutine segmentVector

  ! =====================================================================
  ! Geometry factors
  ! =====================================================================

  real(8) function selfGeometryFactor(l, r0) result(g)
    !! Coincident (self) geometry factor, axis-to-surface (theory.md §4.2):
    !!
    !!     g_self = 2 [ l*ln((l+h)/r0) - h + r0 ],   h = sqrt(l^2 + r0^2)
    !!
    !! Derived directly from g_self = 2*integral_0^l (l-u)/sqrt(u^2+r0^2) du,
    !! the reduction of int_0^l int_0^l dx dy / sqrt((x-y)^2 + r0^2) (axis
    !! point x against surface point at perpendicular offset r0 from axis
    !! point y). Differs from the original C++ `fatorGeometriaPropria`
    !! (`r0 - h + l*log((1+h)/r0)`, missing the factor 2 and using "1" instead
    !! of "l" in the log) — that formula does not satisfy the defining
    !! integral; see ROADMAP.md gap 8 and test_geometry.f90, which
    !! checks this formula against the same integral evaluated by quadrature.
    real(8), intent(in) :: l, r0
    !! Segment length and conductor radius (m)
    real(8) :: h

    h = sqrt(l*l + r0*r0)
    g = 2.0d0 * (l * log((l + h) / r0) - h + r0)
  end function selfGeometryFactor

  subroutine mutualGeometryFactor(a1, a2, b1, b2, g, forceNumeric)
    !! General mutual geometry factor g(a,b) = integral(dl_a dl_b / R_ab)
    !! (theory.md §4.2). Segments must not be coincident or touching (use
    !! `selfGeometryFactor` for the diagonal).
    !!
    !! Dispatches to the closed-form `parallelGeometryFactor` fast path
    !! (theory.md §4.2 "Parallel segments"; ported from the Matlab
    !! reference `barraquad.m`'s `posparal`) whenever `a` and `b` are
    !! parallel, falling back to adaptive 2D quadrature (`geometryFactor2D`) for
    !! non-parallel pairs, or if the closed form hits a degenerate
    !! (NaN/Inf) edge case, exactly as the reference does.
    !!
    !! The quadrature path is memoised through `mGeometryCache`: congruent
    !! pairs (same lengths and cross endpoint distances — ubiquitous in
    !! regular meshes) reuse the first pair's quadrature result instead of
    !! re-running TWODQ. The cheap closed-form path is deliberately not
    !! cached.
    real(8), intent(in)  :: a1(:), a2(:)
    !! Endpoints of segment a (m)
    real(8), intent(in)  :: b1(:), b2(:)
    !! Endpoints of segment b (m)
    real(8), intent(out) :: g
    !! Geometry factor (m)
    logical, intent(in), optional :: forceNumeric
    !! When present and .true., always use quadrature, even for parallel
    !! segments (matches `barraquad`'s `numerico` flag). Useful for
    !! testing the closed form against its quadrature oracle.
    real(8) :: va(3), vb(3), la, lb, key(6)
    logical :: tryClosedForm, ok, useCache

    call segmentVector(a1, a2, va, la)
    call segmentVector(b1, b2, vb, lb)

    tryClosedForm = .true.
    if (present(forceNumeric)) tryClosedForm = .not. forceNumeric
    if (tryClosedForm) tryClosedForm = (norm2(crossProduct(va, vb)) < PARALLEL_TOL)

    if (tryClosedForm) then
      call parallelGeometryFactor(a1, a2, la, va, b1, b2, lb, vb, g, ok)
      if (ok) return
    end if

    useCache = geomCacheIsEnabled()
    if (useCache) then
      key = geomCacheKey(a1, a2, la, b1, b2, lb)
      if (geomCacheGet(key, g)) return
    end if
    call geometryFactor2D(a1, va, la, b1, vb, lb, g)
    if (useCache) call geomCachePut(key, g)
  end subroutine mutualGeometryFactor

  function crossProduct(u, v) result(w)
    !! 3-vector cross product.
    real(8), intent(in) :: u(3), v(3)
    real(8) :: w(3)

    w(1) = u(2) * v(3) - u(3) * v(2)
    w(2) = u(3) * v(1) - u(1) * v(3)
    w(3) = u(1) * v(2) - u(2) * v(1)
  end function crossProduct

  subroutine parallelGeometryFactor(a1, a2, la, va, b1, b2, lb, vb, g, ok)
    !! Closed-form mutual geometry factor for two PARALLEL straight
    !! segments (theory.md §4.2), ported from the Matlab reference
    !! `barraquad.m`'s `posparal` (mom_matlab, 2003 dissertation). Callers
    !! must already know `a` and `b` are parallel (`mutualGeometryFactor`
    !! checks this via `crossProduct`).
    !!
    !! `ok = .false.` signals a degenerate (NaN/Inf) result from a
    !! near-machine-precision edge case in the log terms — the caller
    !! should then fall back to `geometryFactor2D` quadrature, exactly as the
    !! reference does (its `isnan(fg)` check in `barraquad`).
    real(8), intent(in)  :: a1(:), a2(:), la, va(3)
    real(8), intent(in)  :: b1(:), b2(:), lb, vb(3)
    real(8), intent(out) :: g
    logical, intent(out) :: ok
    real(8) :: alignment, da1b1, da1b2, da2b1, da2b2, x2
    real(8) :: xi1, xi2, d11, d12, d21, d22, l11, l21, l22, y

    alignment = dot_product(va, vb)
    da1b1 = norm2(a1 - b1)
    da1b2 = norm2(a1 - b2)
    da2b1 = norm2(a2 - b1)
    da2b2 = norm2(a2 - b2)
    x2 = la

    if (alignment > 0.0d0) then
      if (da1b2 > da2b1) then
        xi1 = dot_product(b1 - a1, va)
        xi2 = xi1 + lb
        d11 = da1b1; d12 = da1b2; d21 = da2b1; d22 = da2b2
      else
        x2 = lb
        xi1 = dot_product(a1 - b1, vb)
        xi2 = xi1 + la
        d11 = da1b1; d12 = da2b1; d21 = da1b2; d22 = da2b2
      end if
    else
      if (da2b2 > da1b1) then
        xi1 = dot_product(b2 - a1, vb)
        xi2 = xi1 + lb
        d11 = da2b1; d12 = da2b2; d21 = da1b1; d22 = da1b2
      else
        x2 = lb
        xi1 = dot_product(b2 - a1, va)
        xi2 = xi1 + la
        d11 = da1b2; d12 = da1b1; d21 = da2b2; d22 = da2b1
      end if
    end if

    l11 = xi1
    l21 = xi1 - x2
    l22 = xi2 - x2
    y = sqrt(max(0.0d0, d11 * d11 - l11 * l11)) / la

    if (y < NUMP) then
      if (abs(xi1 - x2) < NUMP) then
        g = xi1 * log(-(xi1 - xi2) / xi1) + xi2 * log(-xi2 / (xi1 - xi2))
      else
        g = x2 * log((x2 - xi2) / (x2 - xi1)) + xi1 * log(-(x2 - xi1) / xi1) &
          + xi2 * log(-xi2 / (x2 - xi2))
      end if
    else
      g = d11 - d12 - d21 + d22 + x2 * log((d22 + l22) / (d21 + l21)) &
        + xi1 * log((d11 - xi1) / (d21 - l21)) + xi2 * log((d22 - l22) / (d12 - xi2))
    end if

    ok = (g == g) .and. (abs(g) < huge(1.0d0))
    !! NaN never equals itself; Inf fails the huge() bound. Mirrors
    !! barraquad's isnan(fg) fallback-to-quadrature check.
  end subroutine parallelGeometryFactor

  ! =====================================================================
  ! Distances and direction cosines
  ! =====================================================================

  real(8) function meanDistance(p1, p2) result(Rbar)
    !! Distance between two points (segment midpoints), theory.md §4.1.
    real(8), intent(in) :: p1(:), p2(:)

    Rbar = norm2(p2 - p1)
  end function meanDistance

  function imageVector(d) result(di)
    !! Mirror a position or direction vector through the z=0 air-soil
    !! interface (theory.md §5): the image reverses the sign of the
    !! z-component.
    real(8), intent(in) :: d(:)
    real(8) :: di(3)

    di = [d(1), d(2), -d(3)]
  end function imageVector

  real(8) function directionCosine(d1, d2) result(c)
    !! cos(theta) between two direction vectors (theory.md §4).
    !!
    !! Returns 0 if either vector has zero length (degenerate segment).
    real(8), intent(in) :: d1(:), d2(:)
    real(8) :: n1, n2

    n1 = norm2(d1)
    n2 = norm2(d2)
    if (n1 <= 0.0d0 .or. n2 <= 0.0d0) then
      c = 0.0d0
      return
    end if
    c = dot_product(d1, d2) / (n1 * n2)
  end function directionCosine

  ! =====================================================================
  ! Full geometry-matrix assembly
  ! =====================================================================

  subroutine buildGeometryMatrices(p1, p2, radius, n, G, Gi, Rbar, Rbari, cosTheta, cosThetaI, forceNumeric)
    !! Build the full set of n x n geometry matrices for n straight segments
    !! (theory.md §4-5): real geometry factor G, image geometry factor Gi,
    !! mean distance Rbar, image mean distance Rbari, direction cosine
    !! cosTheta, and image direction cosine cosThetaI. All matrices are
    !! symmetric; only the upper triangle is computed and then mirrored.
    !!
    !! Diagonal (self) entries use `selfGeometryFactor` for G, with
    !! Rbar(i,i) = radius(i) and cosTheta(i,i) = 1 (theory.md §4.3: the
    !! exterior self term is evaluated at the surface, R = r0). Image
    !! entries (including the diagonal, segment vs. its own image) are
    !! always computed by quadrature against the mirrored segment, which
    !! requires the segment's height/depth above/below z=0 to be nonzero.
    integer(4), intent(in)  :: n
    !! Number of segments
    real(8), intent(in)  :: p1(n,3), p2(n,3)
    !! Segment endpoints (m)
    real(8), intent(in)  :: radius(n)
    !! Segment radii (m)
    real(8), intent(out) :: G(n,n), Gi(n,n), Rbar(n,n), Rbari(n,n)
    real(8), intent(out) :: cosTheta(n,n), cosThetaI(n,n)
    logical, intent(in), optional :: forceNumeric
    !! Forwarded to `mutualGeometryFactor` (see there); always use
    !! quadrature even for parallel segment pairs.
    real(8) :: dir(n,3), len(n), mid(n,3)
    real(8) :: p1i(3), p2i(3), midi(3), diri(3), gij
    integer :: i, j

    ! Start each build with an empty memo table so statistics are per-build
    ! and no memory is carried across studies (entries would still be valid —
    ! keys are pure congruence invariants — but a fresh table keeps growth
    ! bounded by this geometry).
    call geomCacheClear()

    do i = 1, n
      call segmentVector(p1(i,:), p2(i,:), dir(i,:), len(i))
      mid(i,:) = 0.5d0 * (p1(i,:) + p2(i,:))
    end do

    ! NOT parallelised (ROADMAP.md Phase 3 item 4 / P6, deliberately deferred):
    ! the write pattern itself is race-free (for any physical entry (r,c),
    ! iteration i = min(r,c) is the sole writer, since j only ever ranges
    ! over [i,n]), but `mutualGeometryFactor`'s quadrature fallback
    ! (`geometryFactor2D`/`TWODQ` in Impedance.f90) stores integration state
    ! in module-level procedure pointers and a `COMMON /params/` block —
    ! non-reentrant, so concurrent calls from different segment pairs would
    ! corrupt each other's integration (ARCHITECTURE.md §7). The
    ! mGeometryCache memo table added around that fallback is shared mutable
    ! state too, so it joins the list of things needing a reentrancy story
    ! (per-thread tables or a critical section) before OpenMP lands here.
    ! Only pairs that take the closed-form parallel-segment path are
    ! actually safe today; real (non-parallel) geometries hit the
    ! quadrature. OpenMP here needs that reentrancy fix first, not just
    ! this loop's own write pattern.
    do i = 1, n
      do j = i, n
        if (i == j) then
          G(i,i)        = selfGeometryFactor(len(i), radius(i))
          Rbar(i,i)     = radius(i)
          cosTheta(i,i) = 1.0d0
        else
          call mutualGeometryFactor(p1(i,:), p2(i,:), p1(j,:), p2(j,:), gij, forceNumeric)
          G(i,j) = gij;                       G(j,i) = gij
          Rbar(i,j) = meanDistance(mid(i,:), mid(j,:))
          Rbar(j,i) = Rbar(i,j)
          cosTheta(i,j) = directionCosine(dir(i,:), dir(j,:))
          cosTheta(j,i) = cosTheta(i,j)
        end if

        ! Image term: segment i against the mirror image of segment j
        p1i  = imageVector(p1(j,:))
        p2i  = imageVector(p2(j,:))
        midi = imageVector(mid(j,:))
        diri = imageVector(dir(j,:))
        call mutualGeometryFactor(p1(i,:), p2(i,:), p1i, p2i, gij, forceNumeric)
        Gi(i,j) = gij;                        Gi(j,i) = gij
        Rbari(i,j) = meanDistance(mid(i,:), midi)
        Rbari(j,i) = Rbari(i,j)
        cosThetaI(i,j) = directionCosine(dir(i,:), diri)
        cosThetaI(j,i) = cosThetaI(i,j)
      end do
    end do
  end subroutine buildGeometryMatrices

end module mGeometry
