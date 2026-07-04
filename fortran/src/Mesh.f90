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
  !! Portuguese variable names are retained per the project standard (legacy from MATLAB).
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

    ! Solution vectors
    complex(8), allocatable :: tensao(:)
    !! Complex node voltages V (V), size nno
    complex(8), allocatable :: corrente1(:)
    !! Longitudinal electrode currents (A), size nseg
    complex(8), allocatable :: corrente2(:)
    !! Transversal (leakage) electrode currents (A), size nseg

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

    ! Frequency-dependent medium constants
    complex(8) :: cteEletAr
    !! Electric constant for air: 1/(4π·(σ_air + jω·ε_air))
    complex(8) :: cteEletSolo
    !! Electric constant for soil: 1/(4π·(σ_soil + jω·ε_soil))
    complex(8) :: cteMagAr
    !! Magnetic constant for air: jω·μ_air/(4π)
    complex(8) :: cteMagSolo
    !! Magnetic constant for soil: jω·μ_soil/(4π)
    complex(8) :: propAr
    !! Propagation constant for air: γ = sqrt(jωμ(σ+jωε)), Re γ ≥ 0 (theory.md §2)
    complex(8) :: propSolo
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

    allocate(mesh%tensao(nn))
    allocate(mesh%corrente1(ns))
    allocate(mesh%corrente2(ns))
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

  subroutine calcTopologia(mesh, ns, n1, n2)
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
  end subroutine calcTopologia

  ! =====================================================================
  ! Base matrix computation (placeholder)
  ! =====================================================================

  subroutine calcBase(mesh)
    !! **Placeholder**: compute base geometry matrices from the structure.
    !!
    !! Not yet implemented. When done, this will obtain the geometric connectivity
    !! and construct the topology matrices A, B, C, D.
    type(tMesh), intent(inout) :: mesh
  end subroutine calcBase

  ! =====================================================================
  ! Frequency-dependent medium parameters
  ! =====================================================================

  subroutine calcParam(mesh, omega, epsAr, muAr, sigmaAr, epsSolo, muSolo, sigmaSolo)
    !! Compute frequency-dependent medium constants for a given angular frequency ω.
    !!
    !! Updates `cteElet*`, `cteMag*`, and `prop*` fields of the mesh based on
    !! the permittivity, permeability, and conductivity of air and soil.
    real(8), intent(in), value :: omega
    !! Angular frequency ω (rad/s)
    real(8), intent(in), value :: epsAr, muAr, sigmaAr
    !! Air permittivity (F/m), permeability (H/m), conductivity (S/m)
    real(8), intent(in), value :: epsSolo, muSolo, sigmaSolo
    !! Soil permittivity (F/m), permeability (H/m), conductivity (S/m)
    type(tMesh), intent(inout) :: mesh

    mesh%cteEletAr   = 1.0d0 / (FOUR_PI * cmplx(sigmaAr, omega * epsAr, kind=8))
    mesh%cteEletSolo = 1.0d0 / (FOUR_PI * cmplx(sigmaSolo, omega * epsSolo, kind=8))
    mesh%cteMagAr    = cmplx(0.0d0, omega * muAr / FOUR_PI, kind=8)
    mesh%cteMagSolo  = cmplx(0.0d0, omega * muSolo / FOUR_PI, kind=8)
    ! theory.md §2: gamma = sqrt(j*omega*mu*(sigma + j*omega*eps)), Re(gamma) >= 0
    mesh%propAr      = sqrt(cmplx(0.0d0, omega, kind=8) * muAr &
                             * cmplx(sigmaAr, omega * epsAr, kind=8))
    mesh%propSolo    = sqrt(cmplx(0.0d0, omega, kind=8) * muSolo &
                             * cmplx(sigmaSolo, omega * epsSolo, kind=8))
  end subroutine calcParam

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

  subroutine calcZPropria(mesh, i, pos, h, zint, fgl, fgli, fgt, fgti)
    !! Compute self-impedance of a cylindrical segment using image theory.
    !!
    !! Accounts for proximity to the air–soil interface. Sets both
    !! `mesh%Zlong(i, i)` and `mesh%Ztrans(i, i)`.
    integer(4), intent(in) :: i, pos
    !! 1-based segment index and position (1 = air, 2 = soil, 0 = boundary)
    real(8), intent(in) :: h, fgl, fgli, fgt, fgti
    !! 2× height, Sommerfeld integrals (direct and image contributions)
    complex(8), intent(in) :: zint
    !! Internal impedance of the segment
    type(tMesh), intent(inout) :: mesh
    complex(8) :: fpropi

    if (pos == 1) then
      ! Segment in air: use air propagation constant. Image sign is "-" (theory.md §5)
      fpropi = exp(-cmplx(h, 0.0d0, kind=8) * mesh%propAr)
      mesh%Ztrans(i, i) = mesh%cteEletAr * &
        (cmplx(fgt, 0.0d0, kind=8) - fpropi * cmplx(fgti, 0.0d0, kind=8))
      mesh%Zlong(i, i) = mesh%cteMagAr * &
        (cmplx(fgl, 0.0d0, kind=8) - fpropi * cmplx(fgli, 0.0d0, kind=8)) + zint
    else
      ! Segment in soil: use soil propagation constant. Image sign is "+" (theory.md §5)
      fpropi = exp(-cmplx(h, 0.0d0, kind=8) * mesh%propSolo)
      mesh%Ztrans(i, i) = mesh%cteEletSolo * &
        (cmplx(fgt, 0.0d0, kind=8) + fpropi * cmplx(fgti, 0.0d0, kind=8))
      mesh%Zlong(i, i) = mesh%cteMagSolo * &
        (cmplx(fgl, 0.0d0, kind=8) + fpropi * cmplx(fgli, 0.0d0, kind=8)) + zint
    end if
  end subroutine calcZPropria

  ! =====================================================================
  ! Mutual impedance calculation
  ! =====================================================================

  subroutine calcZMutua(mesh, i, j, pos1, pos2, d, di, fgl, fgli, fgt, fgti)
    !! Compute mutual impedance between two segments using image theory.
    !!
    !! Accounts for different segment positions (air or soil) and proximity
    !! to the air–soil interface. Sets both `mesh%Zlong(i,j)` and
    !! `mesh%Ztrans(i,j)`, using symmetry.
    integer(4), intent(in), value :: i, j, pos1, pos2
    !! 1-based segment indices, positions (1 = air, 2 = soil)
    real(8), intent(in), value :: d, di, fgl, fgli, fgt, fgti
    !! Distances (direct and image), geometry factors
    type(tMesh), intent(inout) :: mesh
    complex(8) :: fprop, fpropi, zt, zl

    if (pos1 == 1 .and. pos2 == 1) then
      ! Both in air. Image sign is "-" for both Zt and Zl (theory.md §5)
      fprop  = exp(-cmplx(d, 0.0d0, kind=8) * mesh%propAr)
      fpropi = exp(-cmplx(di, 0.0d0, kind=8) * mesh%propAr)
      zt = mesh%cteEletAr * &
        (fprop * cmplx(fgt, 0.0d0, kind=8) - fpropi * cmplx(fgti, 0.0d0, kind=8))
      zl = mesh%cteMagAr * &
        (fprop * cmplx(fgl, 0.0d0, kind=8) - fpropi * cmplx(fgli, 0.0d0, kind=8))
    else if (pos1 == 2 .and. pos2 == 2) then
      ! Both in soil. Image sign is "+" for both Zt and Zl (theory.md §5)
      fprop  = exp(-cmplx(d, 0.0d0, kind=8) * mesh%propSolo)
      fpropi = exp(-cmplx(di, 0.0d0, kind=8) * mesh%propSolo)
      zt = mesh%cteEletSolo * &
        (fprop * cmplx(fgt, 0.0d0, kind=8) + fpropi * cmplx(fgti, 0.0d0, kind=8))
      zl = mesh%cteMagSolo * &
        (fprop * cmplx(fgl, 0.0d0, kind=8) + fpropi * cmplx(fgli, 0.0d0, kind=8))
    else
      ! Mixed media: coupling neglected (theory.md §5)
      zt = cmplx(0.0d0, 0.0d0, kind=8)
      zl = cmplx(0.0d0, 0.0d0, kind=8)
    end if
    mesh%Ztrans(i, j) = zt
    mesh%Ztrans(j, i) = zt
    mesh%Zlong(i, j)  = zl
    mesh%Zlong(j, i)  = zl
  end subroutine calcZMutua

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
    !! This matrix is then solved by ZGESV in `injetaSinalF`.
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

  integer(4) function injetaSinalF(mesh, nsig, pos, sig)
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
      injetaSinalF = INFO
      return
    end if

    mesh%tensao    = y(1:nn)
    mesh%corrente1 = y((nn+1):(nn+ns))
    mesh%corrente2 = y((nn+ns+1):n)
    injetaSinalF = 0
  end function injetaSinalF

  ! =====================================================================
  ! Output retrieval
  ! =====================================================================

  subroutine getSaidas(mesh, nn, ns, v, i1, i2)
    !! Copy mesh solution vectors (voltages and currents) to output arrays.
    integer(4), intent(in), value :: nn, ns
    complex(8), intent(out) :: v(nn)
    !! Output node voltages
    complex(8), intent(out) :: i1(ns)
    !! Output longitudinal electrode currents
    complex(8), intent(out) :: i2(ns)
    !! Output transversal electrode currents
    type(tMesh), intent(in) :: mesh

    v = mesh%tensao
    i1 = mesh%corrente1
    i2 = mesh%corrente2
  end subroutine getSaidas

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
      write(*, "(1x,*(g10.3))") ((real(a(i,j)), " ", j=1,n), j=1,n)
      write(*, "(1x,*(g10.3))") ((aimag(a(i,j)), " ", j=1,n), j=1,n)
    end do
  end subroutine printM

end module mMesh
