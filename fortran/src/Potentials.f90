module mPotentials
  !! Potential post-processing at observation points (ROADMAP Phase 11,
  !! theory.md §3.1, ADR 0027): ground potential rise, surface potentials,
  !! touch and step voltages from the transversal currents of a solved sweep.
  !!
  !! Everything here is read-only on the `tStudy`: the solved currents
  !! `I_t = i1 + i2` are reused, no new unknowns appear. The kernel is the
  !! impedance fill's own — the Z_t row of §4.1 with the field point moved off
  !! the conductor,
  !!
  !!     ψ(P) = Σ_b cE · I_t,b / l_b · ( e^{-γ R_b} g_b(P) + Γ e^{-γ R_bi} g_bi(P) ),
  !!
  !! summed over the segments of the medium that contains P (mixed-media
  !! coupling is neglected exactly as in `calcZMutual`, theory.md §5), with
  !! the medium constants and image coefficient Γ(ω) of `mMesh%calcParamW`.
  use mStudy
  use mObservation
  use mResult
  use mMesh, only: tMesh, calcParamW
  use mCtes, only: PI, MU0
  use mError, only: raiseError
  implicit none
  private

  public :: computeObservations, potentialsAt, segmentPotentialFactor

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

  subroutine potentialsAt(study, pts, psi)
    !! ψ at every point of `pts` for every frequency of the study's stored
    !! sweep (theory.md §3.1). Points at z <= 0 are in soil, above it in air
    !! (the segment-midpoint rule of §2).
    class(tStudy), intent(in) :: study
    real(8), intent(in) :: pts(:,:)
    !! Points (3, M), m
    complex(8), allocatable, intent(out) :: psi(:,:)
    !! Potentials (M, nFrequencies), V

    integer :: nf, nseg, nM, k, b, m, med
    real(8), allocatable :: omega(:), p1(:,:), p2(:,:), q1(:,:), q2(:,:), mid(:,:), midi(:,:)
    complex(8), allocatable :: cE(:,:), gam(:,:), gImg(:,:), it(:,:), acc(:)
    type(tMesh) :: mesh
    real(8) :: muAir, muSoil, g, gi, r, ri, pt(3)
    complex(8) :: w

    nf   = study%voltageResults%frequencyCount()
    nseg = study%structure%getElectrodeCount()
    nM   = size(pts, 2)
    allocate(psi(nM, nf))
    psi = (0.0d0, 0.0d0)

    allocate(omega(nf), cE(2, nf), gam(2, nf), gImg(2, nf), it(nseg, nf))
    muAir  = study%structure%air%mur * MU0
    muSoil = study%structure%soil%mur * MU0
    mesh%imageModel = study%imageModel
    do k = 1, nf
      omega(k) = study%voltageResults%frequency(k)
      call calcParamW(mesh, omega(k), muAir, study%structure%air%admittance(omega(k)), &
                      muSoil, study%structure%soil%admittance(omega(k)))
      cE(1, k)   = mesh%cEAir;     cE(2, k)   = mesh%cESoil
      gam(1, k)  = mesh%propAir;   gam(2, k)  = mesh%propSoil
      gImg(1, k) = mesh%gammaAir;  gImg(2, k) = mesh%gammaSoil
      do b = 1, nseg
        it(b, k) = study%longCurrentResults%get(b, k) + study%transCurrentResults%get(b, k)
      end do
    end do

    allocate(p1(3, nseg), p2(3, nseg), q1(3, nseg), q2(3, nseg), mid(3, nseg), midi(3, nseg))
    do b = 1, nseg
      p1(:, b) = study%structure%nodes(study%structure%electrodes(b)%nodeIndices(1))%p
      p2(:, b) = study%structure%nodes(study%structure%electrodes(b)%nodeIndices(2))%p
      q1(:, b) = [p1(1, b), p1(2, b), -p1(3, b)]
      q2(:, b) = [p2(1, b), p2(2, b), -p2(3, b)]
      mid(:, b)  = 0.5d0 * (p1(:, b) + p2(:, b))
      midi(:, b) = 0.5d0 * (q1(:, b) + q2(:, b))
    end do

    !$omp parallel default(shared) private(m, b, k, med, g, gi, r, ri, pt, w, acc)
    allocate(acc(nf))
    !$omp do schedule(dynamic)
    do m = 1, nM
      pt  = pts(:, m)
      med = 2
      if (pt(3) > 0.0d0) med = 1
      acc = (0.0d0, 0.0d0)
      do b = 1, nseg
        if (study%geomPos(b) /= med) cycle
        g  = segmentPotentialFactor(p1(:, b), p2(:, b), pt, study%geomRadius(b))
        gi = segmentPotentialFactor(q1(:, b), q2(:, b), pt, study%geomRadius(b))
        r  = norm2(pt - mid(:, b))
        ri = norm2(pt - midi(:, b))
        do k = 1, nf
          w = exp(-gam(med, k) * r) * g + gImg(med, k) * exp(-gam(med, k) * ri) * gi
          acc(k) = acc(k) + cE(med, k) * it(b, k) * w / study%geomLength(b)
        end do
      end do
      psi(m, :) = acc
    end do
    !$omp end do
    deallocate(acc)
    !$omp end parallel
  end subroutine potentialsAt

  subroutine computeObservations(study)
    !! Evaluate the study's `observation` request on its stored sweep and
    !! fill `study%observationResults` (theory.md §3.1): ψ at the points and
    !! grid, GPR and touch voltage per touch site, Δψ per step pair and the
    !! step map of the grid. A no-op when no observation was requested.
    class(tStudy), intent(inout) :: study

    type(tObservation) :: obs
    integer :: nf, nS, nT, nSt, nDir, nG, nPts, M, i, j, k, iNode, o, oTouch, oStep, oMap, first
    real(8), allocatable :: pts(:,:), omega(:)
    complex(8), allocatable :: psi(:,:)
    character(256), allocatable :: ids(:), tIds(:), stIds(:)
    character(256) :: id
    real(8) :: p(3), ang, worst, lenStep
    complex(8) :: u

    obs = study%observation
    if (obs%isEmpty()) return
    if (study%voltageResults%frequencyCount() == 0) then
      call raiseError("mPotentials: observation needs a solved harmonic sweep (sources + frequencies)")
      return
    end if
    if (study%sweepDamping /= 0.0d0) then
      call raiseError("mPotentials: observation is not defined for a Numerical Laplace Transform sweep")
      return
    end if

    nf = study%voltageResults%frequencyCount()
    allocate(omega(nf))
    do k = 1, nf
      omega(k) = study%voltageResults%frequency(k)
    end do

    nS = obs%siteCount()
    nT = 0
    if (allocated(obs%touch)) nT = size(obs%touch)
    nSt = 0
    if (allocated(obs%steps)) nSt = size(obs%steps)
    nPts = 0
    if (allocated(obs%points)) nPts = size(obs%points)
    nG = 0
    nDir = 0
    if (obs%hasGrid) then
      nG = obs%grid%nx * obs%grid%ny
      if (obs%grid%step) nDir = obs%grid%stepDirections
    end if

    ! One flat list of evaluation points: sites, touch circles, step pairs,
    ! step-map neighbours — so a single pass over the segments serves all
    oTouch = nS
    M = nS
    do i = 1, nT
      M = M + obs%touch(i)%nPoints
    end do
    oStep = M
    M = M + 2 * nSt
    oMap = M
    M = M + nG * nDir
    allocate(pts(3, M), ids(nS))
    if (allocated(study%observationResults%sitePos)) deallocate(study%observationResults%sitePos)
    allocate(study%observationResults%sitePos(3, nS))

    do i = 1, nS
      call obs%site(i, id, p)
      ids(i) = id
      pts(:, i) = p
      study%observationResults%sitePos(:, i) = p
    end do

    o = oTouch
    do i = 1, nT
      iNode = study%structure%findNodeIndex(trim(obs%touch(i)%node))
      if (iNode == 0) then
        call raiseError("mPotentials: observation.touch references unknown node '" // &
                        trim(obs%touch(i)%node) // "'")
        return
      end if
      do j = 1, obs%touch(i)%nPoints
        ang = 2.0d0 * PI * real(j - 1, 8) / real(obs%touch(i)%nPoints, 8)
        o = o + 1
        pts(:, o) = [study%structure%nodes(iNode)%p(1) + obs%touch(i)%radius * sin(ang), &
                     study%structure%nodes(iNode)%p(2) + obs%touch(i)%radius * cos(ang), &
                     obs%touch(i)%z]
      end do
    end do

    do i = 1, nSt
      pts(:, oStep + 2 * i - 1) = obs%steps(i)%from
      pts(:, oStep + 2 * i)     = obs%steps(i)%to
    end do

    if (nDir > 0) then
      lenStep = obs%grid%stepLength
      do i = 1, nG
        do j = 1, nDir
          ang = 2.0d0 * PI * real(j - 1, 8) / real(nDir, 8)
          pts(:, oMap + (i - 1) * nDir + j) = pts(:, nPts + i) + lenStep * [cos(ang), sin(ang), 0.0d0]
        end do
      end do
    end if

    call potentialsAt(study, pts, psi)

    call study%observationResults%potentials%alloc(ids, omega)
    do k = 1, nf
      do i = 1, nS
        call study%observationResults%potentials%set(i, k, psi(i, k))
      end do
    end do

    allocate(tIds(nT), stIds(nSt))
    do i = 1, nT
      tIds(i) = obs%touch(i)%id
    end do
    do i = 1, nSt
      stIds(i) = obs%steps(i)%id
    end do
    call study%observationResults%gpr%alloc(tIds, omega)
    call study%observationResults%touch%alloc(tIds, omega)
    call study%observationResults%steps%alloc(stIds, omega)

    first = oTouch
    do i = 1, nT
      iNode = study%structure%findNodeIndex(trim(obs%touch(i)%node))
      do k = 1, nf
        u = study%voltageResults%get(iNode, k)
        worst = 0.0d0
        do j = 1, obs%touch(i)%nPoints
          worst = max(worst, abs(psi(first + j, k) - u))
        end do
        call study%observationResults%gpr%set(i, k, u)
        call study%observationResults%touch%set(i, k, worst)
      end do
      first = first + obs%touch(i)%nPoints
    end do

    do i = 1, nSt
      do k = 1, nf
        call study%observationResults%steps%set(i, k, psi(oStep + 2 * i, k) - psi(oStep + 2 * i - 1, k))
      end do
    end do

    if (nDir > 0) then
      block
        character(256), allocatable :: gIds(:)
        allocate(gIds(nG))
        gIds = ids(nPts + 1:nS)
        call study%observationResults%stepMap%alloc(gIds, omega)
      end block
      do i = 1, nG
        do k = 1, nf
          worst = 0.0d0
          do j = 1, nDir
            worst = max(worst, abs(psi(nPts + i, k) - psi(oMap + (i - 1) * nDir + j, k)))
          end do
          call study%observationResults%stepMap%set(i, k, worst)
        end do
      end do
    end if

    study%observationResults%freqHz = study%sweepFreqHz
  end subroutine computeObservations

end module mPotentials
