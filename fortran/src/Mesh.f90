module mMesh
  !! Mesh configuration: topology matrices, impedance matrices, and frequency-domain solution.
  !!
  !! The `tMesh` type stores the discretised mesh and all matrices required for
  !! frequency-domain electromagnetic field computation using the Method of Moments.
  !! Key responsibilities:
  !!
  !! 1. **Topology matrices** (A, B, C, D): Relate node voltages to electrode currents.
  !! 2. **Impedance matrices** (Zlong, Ztrans): Self and mutual impedances of electrodes.
  !! 3. **Frequency-domain solve**: Assemble full matrix Zeq and solve for voltages/currents
  !!    via ZGESV (LU factorisation).
  !!
  !! All indices (nodes, segments) are 1-based, in the ordinary Fortran convention.
  !! Sign and propagation conventions follow theory.md §2, §5, §6 (ADR 0008); this
  !! module must not be edited to match any single paper's convention in isolation.
  !!
  !! **References:**
  !! - Portela 1997 (IEEE EMC) — HEM formulation foundation
  !! - Visacro & Soares 2005 (IEEE Trans. Power Del.) — validation
  use mCtes
  implicit none

  ! Explicit interface for LAPACK ZGESV (double complex linear solve)
  interface
    subroutine zgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
      integer, intent(in) :: n
      integer, intent(in) :: nrhs
      integer, intent(in) :: lda
      integer, intent(in) :: ldb
      integer, intent(out) :: ipiv(*)
      integer, intent(out) :: info
      complex(8), intent(inout) :: a(lda, *)
      complex(8), intent(inout) :: b(ldb, *)
    end subroutine zgesv
  end interface

  type :: tMesh
    !! Discretised electromagnetic mesh for frequency-domain HEM solution.

    ! Solution vectors — the augmented unknown x = [u, i1, i2] of theory.md §6
    complex(8), allocatable :: voltage(:)
    !! Complex node voltages u (V), size nno
    complex(8), allocatable :: current1(:)
    !! End currents i1 at node n1 of each segment (A), positive INTO the
    !! segment (theory.md §2), size nseg
    complex(8), allocatable :: current2(:)
    !! End currents i2 at node n2 of each segment (A), positive INTO the
    !! segment, size nseg. The physical currents are derived quantities:
    !! longitudinal Il = (i1 - i2)/2, transversal (leakage) It = i1 + i2.

    ! Topology matrices
    complex(8), allocatable :: A(:,:)
    !! Topology matrix A: nseg × nno, relating voltages to long. currents
    complex(8), allocatable :: B(:,:)
    !! Topology matrix B: nseg × nno, relating voltages to trans. currents
    complex(8), allocatable :: C(:,:)
    !! Topology matrix C: nno × nseg, relating trans. currents to voltages
    complex(8), allocatable :: D(:,:)
    !! Topology matrix D: nno × nseg, relating long. currents to voltages

    ! Impedance matrices
    complex(8), allocatable :: Ztrans(:,:)
    !! Transversal impedance matrix: nseg × nseg (self and mutual)
    complex(8), allocatable :: Zlong(:,:)
    !! Longitudinal impedance matrix: nseg × nseg (self and mutual)

    ! Full system matrix
    complex(8), allocatable :: Zeq(:,:)
    !! Augmented impedance matrix: (nno + 2*nseg) × (nno + 2*nseg)
    !! Assembled from A, B, C, D, Zlong, Ztrans via calcFreq2

    ! Frequency-dependent medium constants (theory.md §5: c_E, c_M)
    complex(8) :: cEAir
    !! Electric constant for air: 1/(4π·(σ_air + jω·ε_air))
    complex(8) :: cESoil
    !! Electric constant for soil: 1/(4π·(σ_soil + jω·ε_soil))
    complex(8) :: cMAir
    !! Magnetic constant for air: jω·μ_air/(4π)
    complex(8) :: cMSoil
    !! Magnetic constant for soil: jω·μ_soil/(4π)
    complex(8) :: propAir
    !! Propagation constant for air: γ = sqrt(jωμ(σ+jωε)), Re γ ≥ 0 (theory.md §2)
    complex(8) :: propSoil
    !! Propagation constant for soil

    ! Problem dimensions
    integer(4) :: nno
    !! Number of nodes
    integer(4) :: nseg
    !! Number of electrode segments
  end type tMesh

contains

  ! =====================================================================
  ! Memory allocation and initialisation
  ! =====================================================================

  subroutine initMesh(mesh, nn, ns)
    !! Initialise a tMesh for nn nodes and ns segments (allocates all components).
    type(tMesh), intent(inout) :: mesh
    integer(4), intent(in), value :: nn, ns
    !! Number of nodes and segments

    mesh%nno  = nn
    mesh%nseg = ns

    allocate(mesh%voltage(nn))
    allocate(mesh%current1(ns))
    allocate(mesh%current2(ns))
    allocate(mesh%A(ns, nn))
    allocate(mesh%B(ns, nn))
    allocate(mesh%C(nn, ns))
    allocate(mesh%D(nn, ns))
    allocate(mesh%Ztrans(ns, ns))
    allocate(mesh%Zlong(ns, ns))
    allocate(mesh%Zeq(nn + 2*ns, nn + 2*ns))

    mesh%A    = ZERO_CPLX
    mesh%B    = ZERO_CPLX
    mesh%C    = ZERO_CPLX
    mesh%D    = ZERO_CPLX
  end subroutine initMesh

  ! =====================================================================
  ! Topology matrix assembly
  ! =====================================================================

  subroutine calcTopology(mesh, ns, n1, n2)
    !! Assemble topology matrices A, B, C, D from node index arrays (theory.md §6).
    !!
    !! Row j (segment j) of A has -1 at column n1(j), +1 at n2(j); row j of B has
    !! -1/2 at both n1(j) and n2(j); column j of C has +1 at row n1(j); column j
    !! of D has +1 at row n2(j).
    integer(4), intent(in), value :: ns
    integer(4), intent(in) :: n1(ns)
    !! 1-based node index for the start of each segment
    integer(4), intent(in) :: n2(ns)
    !! 1-based node index for the end of each segment
    type(tMesh), intent(inout) :: mesh
    integer(4) :: i1

    do i1 = 1, ns
      mesh%A(i1, n1(i1)) = cmplx(-1.0d0, 0.0d0, kind=8)
      mesh%A(i1, n2(i1)) = ONE_CPLX
      mesh%B(i1, n1(i1)) = cmplx(-0.5d0, 0.0d0, kind=8)
      mesh%B(i1, n2(i1)) = cmplx(-0.5d0, 0.0d0, kind=8)
      mesh%C(n1(i1), i1) = ONE_CPLX
      mesh%D(n2(i1), i1) = ONE_CPLX
    end do
  end subroutine calcTopology

  ! =====================================================================
  ! Frequency-dependent medium parameters
  ! =====================================================================

  subroutine calcParam(mesh, omega, epsAir, muAir, sigmaAir, epsSoil, muSoil, sigmaSoil)
    !! Compute frequency-dependent medium constants for a given angular frequency ω,
    !! for constant-parameter (linear) air/soil media.
    !!
    !! Updates `cE*`, `cM*`, and `prop*` fields of the mesh based on
    !! the permittivity, permeability, and conductivity of air and soil.
    !! Thin wrapper over `calcParamW` for the linear-medium immittance
    !! W = sigma + j*omega*eps; dispersive media (ROADMAP Phase 4) call
    !! `calcParamW` directly with their own W(omega).
    real(8), intent(in), value :: omega
    !! Angular frequency ω (rad/s)
    real(8), intent(in), value :: epsAir, muAir, sigmaAir
    !! Air permittivity (F/m), permeability (H/m), conductivity (S/m)
    real(8), intent(in), value :: epsSoil, muSoil, sigmaSoil
    !! Soil permittivity (F/m), permeability (H/m), conductivity (S/m)
    type(tMesh), intent(inout) :: mesh

    call calcParamW(mesh, omega, muAir, cmplx(sigmaAir, omega * epsAir, kind=8), &
                     muSoil, cmplx(sigmaSoil, omega * epsSoil, kind=8))
  end subroutine calcParam

  subroutine calcParamW(mesh, omega, muAir, Wair, muSoil, Wsoil)
    !! Compute frequency-dependent medium constants from the complex
    !! immittance W(ω) = σ(ω) + jωε(ω) of each medium directly (theory.md
    !! §2, §7), rather than from separate real ε/σ — the general form that
    !! also supports dispersive soil models (`mMaterial%admittance`,
    !! ROADMAP Phase 4). `calcParam` is the linear-medium special case,
    !! implemented on top of this routine.
    real(8), intent(in), value :: omega
    !! Angular frequency ω (rad/s)
    real(8), intent(in), value :: muAir, muSoil
    !! Air/soil permeability (H/m)
    complex(8), intent(in), value :: Wair, Wsoil
    !! Air/soil complex immittance W(ω) = σ(ω) + jωε(ω) (S/m)
    type(tMesh), intent(inout) :: mesh

    mesh%cEAir  = 1.0d0 / (FOUR_PI * Wair)
    mesh%cESoil = 1.0d0 / (FOUR_PI * Wsoil)
    mesh%cMAir  = cmplx(0.0d0, omega * muAir / FOUR_PI, kind=8)
    mesh%cMSoil = cmplx(0.0d0, omega * muSoil / FOUR_PI, kind=8)
    ! theory.md §2: gamma = sqrt(j*omega*mu*W), Re(gamma) >= 0
    mesh%propAir  = sqrt(cmplx(0.0d0, omega, kind=8) * muAir  * Wair)
    mesh%propSoil = sqrt(cmplx(0.0d0, omega, kind=8) * muSoil * Wsoil)
  end subroutine calcParamW

  ! =====================================================================
  ! Impedance matrix element setting
  ! =====================================================================

  subroutine setZ(mesh, i, j, zl, zt)
    !! Set longitudinal and transversal impedance matrix elements (i,j) and (j,i).
    !!
    !! Uses symmetry: if i ≠ j, also sets the transposed elements.
    integer(4), intent(in), value :: i, j
    type(tMesh), intent(inout) :: mesh
    complex(8), intent(in), value :: zl, zt
    !! Longitudinal and transversal impedance values

    mesh%Zlong(i, j)  = zl
    mesh%Ztrans(i, j) = zt

    if (i /= j) then
      mesh%Zlong(j, i)  = zl
      mesh%Ztrans(j, i) = zt
    end if
  end subroutine setZ

  ! =====================================================================
  ! Self-impedance calculation (using image theory)
  ! =====================================================================

  subroutine calcZSelf(mesh, i, pos, d, di, l, zint, g, gi, cosThetaI)
    !! Compute the self-impedance of a cylindrical segment with its own image
    !! (theory.md §4.3, §5; ADR 0009). Sets `mesh%Zlong(i,i)` and
    !! `mesh%Ztrans(i,i)`:
    !!
    !!     Ztrans(i,i) = cE * (e^{-γd} g ± e^{-γdi} gi) / l²
    !!     Zlong(i,i)  = cM * (e^{-γd} g ± cosThetaI e^{-γdi} gi) + zint
    !!
    !! All theory factors (propagation at the mean distances, direction cosine
    !! of the image, 1/l² length normalisation) are applied HERE; callers pass
    !! the raw outputs of mGeometry%buildGeometryMatrices. The direct-term
    !! direction cosine is identically 1 for a segment against itself.
    !! Image sign: "-" in air, "+" in soil (theory.md §5).
    integer(4), intent(in) :: i, pos
    !! 1-based segment index and position (1 = air, 2 = soil)
    real(8), intent(in) :: d
    !! Direct mean distance: the conductor radius r0 (field point on the
    !! surface, theory.md §4.3)
    real(8), intent(in) :: di
    !! Image mean distance: twice the height/depth of the segment centre (m)
    real(8), intent(in) :: l
    !! Segment length (m)
    real(8), intent(in) :: g, gi
    !! Direct and image geometry factors (m)
    real(8), intent(in) :: cosThetaI
    !! Direction cosine between the segment and its own image (+1 horizontal,
    !! -1 vertical)
    complex(8), intent(in) :: zint
    !! Internal (skin-effect) impedance of the segment
    type(tMesh), intent(inout) :: mesh
    complex(8) :: prop, cE, cM, fprop, fpropi
    real(8) :: s

    if (pos == 1) then
      prop = mesh%propAir; cE = mesh%cEAir;  cM = mesh%cMAir;  s = -1.0d0
    else
      prop = mesh%propSoil; cE = mesh%cESoil; cM = mesh%cMSoil; s = +1.0d0
    end if
    fprop  = exp(-d  * prop)
    fpropi = exp(-di * prop)

    mesh%Ztrans(i, i) = cE * (fprop * g + s * fpropi * gi) / (l * l)
    mesh%Zlong(i, i)  = cM * (fprop * g + s * cosThetaI * fpropi * gi) + zint
  end subroutine calcZSelf

  ! =====================================================================
  ! Mutual impedance calculation
  ! =====================================================================

  subroutine calcZMutual(mesh, i, j, pos1, pos2, d, di, la, lb, g, gi, cosTheta, cosThetaI)
    !! Compute the mutual impedance between two segments with image theory
    !! (theory.md §4.1, §5; ADR 0009). Sets `mesh%Zlong(i,j)` and
    !! `mesh%Ztrans(i,j)` plus their symmetric counterparts:
    !!
    !!     Ztrans(i,j) = cE * (e^{-γd} g ± e^{-γdi} gi) / (la·lb)
    !!     Zlong(i,j)  = cM * (cosTheta e^{-γd} g ± cosThetaI e^{-γdi} gi)
    !!
    !! All theory factors (propagation at the mean distances, direction
    !! cosines, 1/(la·lb) length normalisation) are applied HERE; callers pass
    !! the raw outputs of mGeometry%buildGeometryMatrices. Image sign: "-"
    !! both in air, "+" both in soil; mixed-media pairs are neglected (zero),
    !! per theory.md §5 / ADR 0005.
    integer(4), intent(in), value :: i, j, pos1, pos2
    !! 1-based segment indices, positions (1 = air, 2 = soil)
    real(8), intent(in), value :: d, di
    !! Mean distances between segment midpoints, direct and image (m)
    real(8), intent(in), value :: la, lb
    !! Segment lengths (m)
    real(8), intent(in), value :: g, gi
    !! Direct and image geometry factors (m)
    real(8), intent(in), value :: cosTheta, cosThetaI
    !! Direction cosines, direct and against the image of segment j
    type(tMesh), intent(inout) :: mesh
    complex(8) :: prop, cE, cM, fprop, fpropi, zt, zl
    real(8) :: s

    if (pos1 == pos2) then
      if (pos1 == 1) then
        prop = mesh%propAir; cE = mesh%cEAir;  cM = mesh%cMAir;  s = -1.0d0
      else
        prop = mesh%propSoil; cE = mesh%cESoil; cM = mesh%cMSoil; s = +1.0d0
      end if
      fprop  = exp(-d  * prop)
      fpropi = exp(-di * prop)
      zt = cE * (fprop * g + s * fpropi * gi) / (la * lb)
      zl = cM * (cosTheta * fprop * g + s * cosThetaI * fpropi * gi)
    else
      ! Mixed media: coupling neglected (theory.md §5)
      zt = ZERO_CPLX
      zl = ZERO_CPLX
    end if
    mesh%Ztrans(i, j) = zt
    mesh%Ztrans(j, i) = zt
    mesh%Zlong(i, j)  = zl
    mesh%Zlong(j, i)  = zl
  end subroutine calcZMutual

  ! =====================================================================
  ! Full system matrix assembly
  ! =====================================================================

  subroutine calcFreq2(mesh)
    !! Assemble the full system matrix Zeq from topology and impedance matrices
    !! (theory.md §6).
    !!
    !! Builds the augmented (nno + 2*nseg) × (nno + 2*nseg) matrix:
    !!
    !!     ┌─────────────────────────────────┐
    !!     │  A   | Zlong/2  | -Zlong/2    │
    !!     ├─────────────────────────────────┤
    !!     │  B   | Ztrans   | Ztrans      │
    !!     ├─────────────────────────────────┤
    !!     │  0   | C        | D           │
    !!     └─────────────────────────────────┘
    !!
    !! This matrix is then solved by ZGESV in `injectSignal`.
    type(tMesh), intent(inout) :: mesh
    integer :: nn, ns, n

    nn = mesh%nno
    ns = mesh%nseg
    n  = nn + 2*ns

    mesh%Zeq(1:ns,           1:nn)           = mesh%A
    mesh%Zeq(1:ns,           (nn+1):(nn+ns)) = mesh%Zlong * 0.5d0
    mesh%Zeq(1:ns,           (nn+ns+1):n)    = mesh%Zlong * (-0.5d0)
    mesh%Zeq((ns+1):(2*ns),  1:nn)           = mesh%B
    mesh%Zeq((ns+1):(2*ns),  (nn+1):(nn+ns)) = mesh%Ztrans
    mesh%Zeq((ns+1):(2*ns),  (nn+ns+1):n)    = mesh%Ztrans
    mesh%Zeq((2*ns+1):n,     1:nn)           = ZERO_CPLX
    mesh%Zeq((2*ns+1):n,     (nn+1):(nn+ns)) = mesh%C
    mesh%Zeq((2*ns+1):n,     (nn+ns+1):n)    = mesh%D
  end subroutine calcFreq2

  ! =====================================================================
  ! Signal injection and solving
  ! =====================================================================

  integer(4) function injectSignal(mesh, nsig, pos, sig)
    !! Solve for node voltages and electrode currents given source current injections.
    !!
    !! Sets up the right-hand side vector with source currents at specified nodes,
    !! then calls ZGESV to solve Zeq·x = b, where x contains node voltages
    !! and both longitudinal and transversal electrode currents.
    !!
    !! Returns ZGESV INFO code (0 = success).
    type(tMesh), intent(inout) :: mesh
    !! Mesh element
    integer(4), intent(in), value :: nsig
    !! Number of source injections
    integer(4), intent(in) :: pos(nsig)
    !! 1-based node indices where sources are injected
    complex(8), intent(in) :: sig(nsig)
    !! Current magnitudes at each injection point (A)
    complex(8), allocatable :: y(:)
    integer :: INFO, nn, ns, n
    integer, allocatable :: IPIV(:)

    nn = mesh%nno
    ns = mesh%nseg
    n  = 2*ns + nn
    allocate(IPIV(n))
    allocate(y(n))
    y = cmplx(0.0d0, 0.0d0, kind=8)
    y(2*ns + pos) = sig

    call zgesv(n, 1, mesh%Zeq, n, IPIV, y, n, INFO)
    if (INFO /= 0) then
      injectSignal = INFO
      return
    end if

    mesh%voltage  = y(1:nn)
    mesh%current1 = y((nn+1):(nn+ns))
    mesh%current2 = y((nn+ns+1):n)
    injectSignal = 0
  end function injectSignal

  ! =====================================================================
  ! Output retrieval
  ! =====================================================================

  subroutine getOutputs(mesh, nn, ns, v, i1, i2)
    !! Copy mesh solution vectors (voltages and currents) to output arrays.
    integer(4), intent(in), value :: nn, ns
    complex(8), intent(out) :: v(nn)
    !! Output node voltages
    complex(8), intent(out) :: i1(ns)
    !! Output longitudinal electrode currents
    complex(8), intent(out) :: i2(ns)
    !! Output transversal electrode currents
    type(tMesh), intent(in) :: mesh

    v = mesh%voltage
    i1 = mesh%current1
    i2 = mesh%current2
  end subroutine getOutputs

  ! =====================================================================
  ! Debug output
  ! =====================================================================

  subroutine printM(desc, m, n, a)
    !! Print a complex matrix for debugging: real and imaginary parts on separate lines.
    character*(*) :: desc
    integer :: m, n
    complex(8) :: a(m, n)
    integer :: i, j

    write (*,*)
    write (*,*) desc
    do i = 1, m
      write(*, "(1x,*(g10.3))") (real(a(i,j)), " ", j=1,n)
      write(*, "(1x,*(g10.3))") (aimag(a(i,j)), " ", j=1,n)
    end do
  end subroutine printM

end module mMesh
