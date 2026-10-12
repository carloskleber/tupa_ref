module mIncident
  !! Total voltage at a node: the scalar potential plus the vertical
  !! vector-potential integral of the lightning channel (theory.md §4.6.2–§4.6.3,
  !! ADR 0028).
  !!
  !! The nodal unknown of the HEM is the scalar potential φ at the node,
  !! relative to remote earth. The voltage that the induced-voltage literature
  !! reports at a point of an overhead line is the *total* voltage
  !!
  !!     u(x, y) = −∫₀^h E_z dz = φ(x, y, h) + ∫₀^h ∂A_z^i/∂t dz,
  !!
  !! the line integral of the electric field along the vertical from the ground
  !! to the node (φ is zero on a perfect ground), with A^i the vector potential
  !! of the *incident* sources — the lightning channel and its image. This
  !! module supplies the second term in the frequency domain, s ∫₀^h A_z^i dz,
  !! from the solved segment currents. The kernel is the impedance fill's own
  !! Z_ℓ term (calcZMutual) with the field point moved off the conductor:
  !!
  !!     s A_z(P) = Σ_b cM · I_b · u_z,b · ( e^{−γR_b} g_b(P) − Γ e^{−γR_bi} g_bi(P) ),
  !!
  !! summed over the channel (series-loaded) segments, with I_b = (i1 − i2)/2
  !! the mean current along the segment, u_z,b the z component of its
  !! direction, g_b(P) = ∫ dℓ/|P − r(ℓ)| (the closed form of
  !! `segmentPotentialFactor`) and Γ the image coefficient (−1 for the ideal
  !! ground). The image segment is the mirror of the direct one, so its
  !! direction carries the opposite z component, which is the minus sign.
  !!
  !! Everything geometric is frequency independent and is built once
  !! (`buildIncidentPath`); per frequency `incidentDrop` evaluates the sum.
  !! The vertical path is integrated with 4-point Gauss–Legendre panels of at
  !! most `PANEL_MAX` metres. The kernel is smooth on that scale when the
  !! channel is farther from the path than a few segment lengths; a path that
  !! grazes the channel (closer than a segment) is outside the method.
  use mStructure, only: tStructure
  use mMesh, only: tMesh
  implicit none
  private

  public :: tIncidentPath, buildIncidentPath, incidentDrop, segmentPotentialFactor

  real(8), parameter :: PANEL_MAX = 10.0d0
  !! Longest Gauss–Legendre panel along the vertical path (m)
  real(8), parameter :: GL_X(4) = [-0.8611363115940526d0, -0.3399810435848563d0, &
                                    0.3399810435848563d0, 0.8611363115940526d0]
  real(8), parameter :: GL_W(4) = [0.3478548451374538d0, 0.6521451548625461d0, &
                                    0.6521451548625461d0, 0.3478548451374538d0]

  type :: tIncidentPath
    !! Frequency-independent geometry of the vertical paths and the channel
    !! segments that act on them.
    integer :: nObs = 0
    !! Number of observed nodes (paths)
    integer :: nQ = 0
    !! Quadrature points per path (padded with zero weights)
    integer :: nSrc = 0
    !! Channel segments with a vertical component
    integer, allocatable :: srcSeg(:)
    !! Electrode index of each source segment
    real(8), allocatable :: uz(:)
    !! z component of the unit direction (node 1 → node 2) of each source
    real(8), allocatable :: wq(:,:)
    !! Quadrature weights (nQ, nObs), m; zero for padding and for a node at
    !! or below the ground (no vertical path in air)
    real(8), allocatable :: gD(:,:,:), gI(:,:,:)
    !! Direct and image geometry factors g(P_q) (nSrc, nQ, nObs)
    real(8), allocatable :: rD(:,:,:), rI(:,:,:)
    !! Distances from P_q to the direct and image segment midpoints (m)
    logical, allocatable :: active(:)
    !! A path exists (the node is above the ground)
  end type tIncidentPath

contains

  pure function segmentPotentialFactor(a, b, p, radius) result(g)
    !! Geometry factor of a straight segment seen from a point,
    !! g = ∫ dℓ / |P − r(ℓ)| over the segment from `a` to `b` (theory.md
    !! §3.1; the closed form of §4.2 for a field point off the axis).
    !!
    !! With s1, s2 the axial coordinates of P relative to the two ends and ρ
    !! its distance to the axis, g = asinh(s1/ρ) − asinh(s2/ρ). A point closer
    !! to the axis than the conductor radius is treated as lying on the
    !! surface (ρ = `radius`), the §4.3 regularisation of the self terms.
    real(8), intent(in) :: a(3), b(3), p(3)
    !! Segment end points and field point (m)
    real(8), intent(in) :: radius
    !! Conductor radius (m): floor of the distance to the axis
    real(8) :: g
    real(8) :: l, u(3), s1, s2, rho

    l  = norm2(b - a)
    u  = (b - a) / l
    s1 = dot_product(p - a, u)
    s2 = s1 - l
    rho = sqrt(max(dot_product(p - a, p - a) - s1 * s1, 0.0d0))
    rho = max(rho, radius)
    g = asinh(s1 / rho) - asinh(s2 / rho)
  end function segmentPotentialFactor

  subroutine buildIncidentPath(structure, geomPos, geomRadius, nodeRows, path)
    !! Geometry of the vertical paths above the nodes `nodeRows` and of the
    !! channel segments that couple to them. The structure must be assembled.
    type(tStructure), intent(in) :: structure
    integer(4), intent(in) :: geomPos(:)
    !! Medium of each electrode (1 = air), `tStudy%geomPos`
    real(8), intent(in) :: geomRadius(:)
    !! Radius of each electrode (m), `tStudy%geomRadius`
    integer(4), intent(in) :: nodeRows(:)
    !! 1-based node indices whose total voltage is wanted
    type(tIncidentPath), intent(out) :: path
    integer :: nseg, b, k, i, q, ip, np, iq, nQmax
    real(8) :: p1(3), p2(3), q1(3), q2(3), pt(3), za, zb, zn, mid(3), midi(3)
    integer, allocatable :: list(:)

    nseg = structure%getElectrodeCount()
    allocate(list(nseg))
    path%nSrc = 0
    do b = 1, nseg
      if (.not. structure%electrodes(b)%loaded) cycle
      if (geomPos(b) /= 1) cycle
      p1 = structure%nodes(structure%electrodes(b)%nodeIndices(1))%p
      p2 = structure%nodes(structure%electrodes(b)%nodeIndices(2))%p
      if (abs(p2(3) - p1(3)) <= 1.0d-12 * max(norm2(p2 - p1), tiny(1.0d0))) cycle
      path%nSrc = path%nSrc + 1
      list(path%nSrc) = b
    end do
    path%nObs = size(nodeRows)
    allocate(path%srcSeg(path%nSrc), path%uz(path%nSrc), path%active(path%nObs))
    path%srcSeg = list(1:path%nSrc)
    do k = 1, path%nSrc
      b  = path%srcSeg(k)
      p1 = structure%nodes(structure%electrodes(b)%nodeIndices(1))%p
      p2 = structure%nodes(structure%electrodes(b)%nodeIndices(2))%p
      path%uz(k) = (p2(3) - p1(3)) / norm2(p2 - p1)
    end do

    nQmax = 4
    do i = 1, path%nObs
      zn = structure%nodes(nodeRows(i))%p(3)
      nQmax = max(nQmax, 4 * max(1, ceiling(zn / PANEL_MAX)))
    end do
    path%nQ = nQmax
    allocate(path%wq(nQmax, path%nObs))
    allocate(path%gD(path%nSrc, nQmax, path%nObs), path%gI(path%nSrc, nQmax, path%nObs))
    allocate(path%rD(path%nSrc, nQmax, path%nObs), path%rI(path%nSrc, nQmax, path%nObs))
    path%wq = 0.0d0
    path%gD = 0.0d0
    path%gI = 0.0d0
    path%rD = 0.0d0
    path%rI = 0.0d0

    do i = 1, path%nObs
      pt = structure%nodes(nodeRows(i))%p
      zn = pt(3)
      path%active(i) = zn > 0.0d0 .and. path%nSrc > 0
      if (.not. path%active(i)) cycle
      np = max(1, ceiling(zn / PANEL_MAX))
      iq = 0
      do ip = 1, np
        za = zn * real(ip - 1, 8) / real(np, 8)
        zb = zn * real(ip, 8) / real(np, 8)
        do q = 1, 4
          iq = iq + 1
          path%wq(iq, i) = 0.5d0 * (zb - za) * GL_W(q)
          pt(3) = 0.5d0 * (za + zb) + 0.5d0 * (zb - za) * GL_X(q)
          do k = 1, path%nSrc
            b  = path%srcSeg(k)
            p1 = structure%nodes(structure%electrodes(b)%nodeIndices(1))%p
            p2 = structure%nodes(structure%electrodes(b)%nodeIndices(2))%p
            q1 = [p1(1), p1(2), -p1(3)]
            q2 = [p2(1), p2(2), -p2(3)]
            mid  = 0.5d0 * (p1 + p2)
            midi = 0.5d0 * (q1 + q2)
            path%gD(k, iq, i) = segmentPotentialFactor(p1, p2, pt, geomRadius(b))
            path%gI(k, iq, i) = segmentPotentialFactor(q1, q2, pt, geomRadius(b))
            path%rD(k, iq, i) = norm2(pt - mid)
            path%rI(k, iq, i) = norm2(pt - midi)
          end do
        end do
      end do
    end do
  end subroutine buildIncidentPath

  function incidentDrop(path, mesh, i1, i2) result(du)
    !! s ∫₀^h A_z^i dz at every observed node, for one solution of the system
    !! (the study's currents `i1`, `i2` at the frequency `mesh` was filled
    !! for). Zero where the node has no vertical path.
    type(tIncidentPath), intent(in) :: path
    type(tMesh), intent(in) :: mesh
    !! Medium constants at the solved frequency (`cMAir`, `propAir`, `gammaAir`)
    complex(8), intent(in) :: i1(:), i2(:)
    !! End currents of every electrode (A)
    complex(8) :: du(path%nObs)
    integer :: i, q, k, b
    complex(8) :: acc, cur, gam, img

    du = (0.0d0, 0.0d0)
    gam = mesh%propAir
    img = mesh%gammaAir
    do i = 1, path%nObs
      if (.not. path%active(i)) cycle
      acc = (0.0d0, 0.0d0)
      do q = 1, path%nQ
        if (path%wq(q, i) == 0.0d0) cycle
        do k = 1, path%nSrc
          b = path%srcSeg(k)
          cur = 0.5d0 * (i1(b) - i2(b))
          acc = acc + path%wq(q, i) * cur * path%uz(k) * &
                (exp(-gam * path%rD(k, q, i)) * path%gD(k, q, i) &
                 - img * exp(-gam * path%rI(k, q, i)) * path%gI(k, q, i))
        end do
      end do
      du(i) = mesh%cMAir * acc
    end do
  end function incidentDrop

end module mIncident
