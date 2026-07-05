module mGeometry
  !! Geometry-factor layer: mean/image distances, real geometry factors, and
  !! direction cosines for pairs of straight cylindrical segments
  !! (theory.md §4-5, ADR 0004). Frequency-independent; computed once per
  !! geometry and reused by the per-frequency impedance fill (Phase 2).
  !!
  !! This module has no dependency on the object model (`tStructure`,
  !! `tElectrode`) — it operates on plain segment endpoints and radii, so it
  !! can be tested and reasoned about in isolation.
  use mImpedance, only: IMPMUTUA
  implicit none
  private

  public :: segmentVector, selfGeometryFactor, mutualGeometryFactor, &
            directionCosine, imageVector, meanDistance, buildGeometryMatrices

contains

  ! =====================================================================
  ! Segment decomposition
  ! =====================================================================

  subroutine segmentVector(p1, p2, dir, length)
    !! Decompose a segment into a unit direction vector and its length.
    real(8), intent(in)  :: p1(3), p2(3)
    !! Segment endpoints (m)
    real(8), intent(out) :: dir(3)
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

  subroutine mutualGeometryFactor(a1, a2, b1, b2, g)
    !! General mutual geometry factor g(a,b) = integral(dl_a dl_b / R_ab) via
    !! adaptive 2D quadrature (theory.md §4.2). Segments must not be
    !! coincident or touching (use `selfGeometryFactor` for the diagonal).
    real(8), intent(in)  :: a1(3), a2(3)
    !! Endpoints of segment a (m)
    real(8), intent(in)  :: b1(3), b2(3)
    !! Endpoints of segment b (m)
    real(8), intent(out) :: g
    !! Geometry factor (m)
    real(8) :: va(3), vb(3), la, lb

    call segmentVector(a1, a2, va, la)
    call segmentVector(b1, b2, vb, lb)
    call IMPMUTUA(a1, va, la, b1, vb, lb, g)
  end subroutine mutualGeometryFactor

  ! =====================================================================
  ! Distances and direction cosines
  ! =====================================================================

  real(8) function meanDistance(p1, p2) result(Rbar)
    !! Distance between two points (segment midpoints), theory.md §4.1.
    real(8), intent(in) :: p1(3), p2(3)

    Rbar = norm2(p2 - p1)
  end function meanDistance

  function imageVector(d) result(di)
    !! Mirror a position or direction vector through the z=0 air-soil
    !! interface (theory.md §5): the image reverses the sign of the
    !! z-component.
    real(8), intent(in) :: d(3)
    real(8) :: di(3)

    di = [d(1), d(2), -d(3)]
  end function imageVector

  real(8) function directionCosine(d1, d2) result(c)
    !! cos(theta) between two direction vectors (theory.md §4).
    !!
    !! Returns 0 if either vector has zero length (degenerate segment).
    real(8), intent(in) :: d1(3), d2(3)
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

  subroutine buildGeometryMatrices(p1, p2, radius, n, G, Gi, Rbar, Rbari, cosTheta, cosThetaI)
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
    real(8) :: dir(n,3), len(n), mid(n,3)
    real(8) :: p1i(3), p2i(3), midi(3), diri(3), gij
    integer :: i, j

    do i = 1, n
      call segmentVector(p1(i,:), p2(i,:), dir(i,:), len(i))
      mid(i,:) = 0.5d0 * (p1(i,:) + p2(i,:))
    end do

    do i = 1, n
      do j = i, n
        if (i == j) then
          G(i,i)        = selfGeometryFactor(len(i), radius(i))
          Rbar(i,i)     = radius(i)
          cosTheta(i,i) = 1.0d0
        else
          call mutualGeometryFactor(p1(i,:), p2(i,:), p1(j,:), p2(j,:), gij)
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
        call mutualGeometryFactor(p1(i,:), p2(i,:), p1i, p2i, gij)
        Gi(i,j) = gij;                        Gi(j,i) = gij
        Rbari(i,j) = meanDistance(mid(i,:), midi)
        Rbari(j,i) = Rbari(i,j)
        cosThetaI(i,j) = directionCosine(dir(i,:), diri)
        cosThetaI(j,i) = cosThetaI(i,j)
      end do
    end do
  end subroutine buildGeometryMatrices

end module mGeometry
