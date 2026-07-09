module mStudy
  !! Top-level orchestration object for a complete electromagnetic study.
  !!
  !! `tStudy` contains all data needed to define and execute one complete
  !! simulation: geometry (structure), mesh, materials, loads, and results.
  !! It serves as the container passed between I/O (JSON parsing) and the
  !! frequency-domain solver.
  !!
  !! Typical workflow:
  !! 1. JSON parsing creates and populates a tStudy instance
  !! 2. Study calls `structure%assembleStructure()` to discretise elements
  !! 3. Study calls `mesh%calcTopology()` and `mesh%calcFreq2(ω)` to solve
  !! 4. Study stores results from `mesh%getOutputs()`
  !! 5. I/O writes results to CSV or JSON
  use mMesh
  use mStructure
  use mElement
  use mMaterial
  use mResult
  use mGeometry, only: buildGeometryMatrices
  use mImpedance, only: internalImpedance
  use mError, only: raiseError
  use mCtes, only: newl, EPSILON0, MU0, ZERO_CPLX
  implicit none

  type :: tStudy
    !! Container for a complete electromagnetic field study.
    !!
    !! Manages the geometric structure, mesh, and collection of frequency-domain
    !! results. All inputs (nodes, elements, materials, loads) are stored in
    !! the `structure` component; all computed outputs are stored in the
    !! `results` array.
    character(len=256) :: title
    !! User-assigned name for the study
    type(tStructure) :: structure
    !! Geometric model: nodes, elements, materials, and soil/air media
    type(tMesh) :: mesh
    !! Frequency-domain mesh and solver: topology matrices and impedance system
    class(tElement), pointer :: element => null()
    !! Temporary pointer for iteration during element management
    class(tMaterial), pointer :: mat => null()
    !! Temporary pointer for iteration during material management
    class(tResult), allocatable :: results(:)
    !! Array of frequency-domain results: voltages, longitudinal currents, transverse currents

    logical :: prepared = .false.
    !! Set once assembly and geometry-factor computation have run (theory.md
    !! §4.1: these are frequency-independent and computed only once, even
    !! across a `run` call per frequency in a sweep)
    real(8), allocatable :: geomG(:,:), geomGi(:,:)
    !! Cached direct/image geometry factors (mGeometry%buildGeometryMatrices)
    real(8), allocatable :: geomRbar(:,:), geomRbari(:,:)
    !! Cached direct/image mean distances
    real(8), allocatable :: geomCosTheta(:,:), geomCosThetaI(:,:)
    !! Cached direct/image direction cosines
    real(8), allocatable :: geomLength(:), geomRadius(:)
    !! Cached per-electrode segment length and radius
    integer(4), allocatable :: geomPos(:)
    !! Cached per-electrode medium (1 = air, 2 = soil), from the sign of the
    !! segment midpoint's z (theory.md §2: air z>0, soil z<0)
  contains
    procedure :: report
    !! Print a human-readable summary of the study contents
    procedure :: run
    !! Execute the full simulation pipeline (discretisation, solving, extraction)
  end type tStudy

contains

  ! =====================================================================
  ! One-time preparation: assembly + frequency-independent geometry factors
  ! =====================================================================

  subroutine prepareStudy(this)
    !! Discretise the structure and compute the geometry-factor matrices
    !! once (theory.md §4.1). Guarded by `this%prepared` so repeated `run`
    !! calls across a frequency sweep do not redo the assembly or the O(n²)
    !! quadrature.
    class(tStudy), intent(inout) :: this
    integer(4) :: nno, nseg, i
    integer(4), allocatable :: n1(:), n2(:)
    real(8), allocatable :: p1(:,:), p2(:,:)

    call this%structure%assembleStructure()

    nno  = this%structure%getNodeCount()
    nseg = this%structure%getElectrodeCount()

    allocate(n1(nseg), n2(nseg), p1(nseg,3), p2(nseg,3))
    allocate(this%geomRadius(nseg), this%geomLength(nseg), this%geomPos(nseg))

    do i = 1, nseg
      n1(i) = this%structure%electrodes(i)%nodeIndices(1)
      n2(i) = this%structure%electrodes(i)%nodeIndices(2)
      p1(i,:) = this%structure%nodes(n1(i))%p
      p2(i,:) = this%structure%nodes(n2(i))%p
      this%geomRadius(i) = this%structure%electrodes(i)%radius
      this%geomLength(i) = norm2(p2(i,:) - p1(i,:))
      if (0.5d0 * (p1(i,3) + p2(i,3)) > 0.0d0) then
        this%geomPos(i) = 1 ! air
      else
        this%geomPos(i) = 2 ! soil
      end if
    end do

    allocate(this%geomG(nseg,nseg),        this%geomGi(nseg,nseg))
    allocate(this%geomRbar(nseg,nseg),     this%geomRbari(nseg,nseg))
    allocate(this%geomCosTheta(nseg,nseg), this%geomCosThetaI(nseg,nseg))

    call buildGeometryMatrices(p1, p2, this%geomRadius, nseg, &
      this%geomG, this%geomGi, this%geomRbar, this%geomRbari, &
      this%geomCosTheta, this%geomCosThetaI)

    call initMesh(this%mesh, nno, nseg)
    call calcTopology(this%mesh, nseg, n1, n2)

    this%prepared = .true.
  end subroutine prepareStudy

  ! =====================================================================
  ! Per-segment internal (skin-effect) impedance
  ! =====================================================================

  complex(8) function segmentInternalImpedance(this, i, omega) result(zint)
    !! Internal impedance of electrode `i`'s conductor material at `omega`
    !! (theory.md §4.3). Only `tLinear` conductor materials are supported;
    !! dispersive conductor models are not part of the current object model.
    class(tStudy), intent(in) :: this
    integer(4), intent(in) :: i
    real(8), intent(in) :: omega

    select type (mat => this%structure%electrodes(i)%material)
    type is (tLinear)
      zint = internalImpedance(this%geomRadius(i), this%geomLength(i), omega, mat%sigma, mat%mur)
    class default
      call raiseError("tStudy%run: internal impedance requires a tLinear conductor material")
      zint = ZERO_CPLX
    end select
  end function segmentInternalImpedance

  ! =====================================================================
  ! Study execution and reporting
  ! =====================================================================

  subroutine run(this, omega, sourceNodeIds, sourceCurrents)
    !! Solve the study at one angular frequency ω, injecting the given
    !! currents at the given nodes (ADR 0010: current-injection sources).
    !!
    !! First call: discretises the structure and computes the geometry-factor
    !! matrices (`prepareStudy`, done once). Every call: resolves medium
    !! constants from `structure%air`/`structure%soil`, fills `Zlong`/`Ztrans`
    !! from the cached geometry matrices (ADR 0009 — `calcZSelf`/
    !! `calcZMutual` apply every theory factor internally), assembles `Zeq`
    !! and solves. The solution is left in `this%mesh%voltage`/`current1`/
    !! `current2` for the caller to read (e.g. input impedance at the
    !! injection node); a frequency sweep is driven by calling `run` in a
    !! loop, one call per ω (ROADMAP Phase 3 formalises sweep storage).
    class(tStudy), intent(inout) :: this
    real(8), intent(in) :: omega
    !! Angular frequency ω (rad/s) for this solve
    character(len=*), intent(in) :: sourceNodeIds(:)
    !! User-assigned IDs of the nodes receiving current injection
    complex(8), intent(in) :: sourceCurrents(:)
    !! Complex current injected at each corresponding node in `sourceNodeIds` (A)
    integer(4) :: nseg, i, j, k
    integer(4), allocatable :: sourcePos(:)
    complex(8) :: zint
    real(8) :: epsAir, muAir, sigmaAir, epsSoil, muSoil, sigmaSoil
    integer(4) :: info

    if (.not. this%prepared) call prepareStudy(this)

    epsAir   = this%structure%air%epsilonr * EPSILON0
    muAir    = this%structure%air%mur * MU0
    sigmaAir = this%structure%air%sigma

    select type (soil => this%structure%soil)
    type is (tLinear)
      epsSoil   = soil%epsilonr * EPSILON0
      muSoil    = soil%mur * MU0
      sigmaSoil = soil%sigma
    class default
      call raiseError("tStudy%run: dispersive soil is not supported until ROADMAP Phase 4 (ADR 0007)")
      return
    end select

    call calcParam(this%mesh, omega, epsAir, muAir, sigmaAir, epsSoil, muSoil, sigmaSoil)

    nseg = this%structure%getElectrodeCount()
    do i = 1, nseg
      do j = i, nseg
        if (i == j) then
          zint = segmentInternalImpedance(this, i, omega)
          call calcZSelf(this%mesh, i, this%geomPos(i), &
            this%geomRbar(i,i), this%geomRbari(i,i), this%geomLength(i), &
            zint, this%geomG(i,i), this%geomGi(i,i), this%geomCosThetaI(i,i))
        else
          call calcZMutual(this%mesh, i, j, this%geomPos(i), this%geomPos(j), &
            this%geomRbar(i,j), this%geomRbari(i,j), &
            this%geomLength(i), this%geomLength(j), &
            this%geomG(i,j), this%geomGi(i,j), &
            this%geomCosTheta(i,j), this%geomCosThetaI(i,j))
        end if
      end do
    end do

    call calcFreq2(this%mesh)

    allocate(sourcePos(size(sourceNodeIds)))
    do k = 1, size(sourceNodeIds)
      sourcePos(k) = this%structure%findNodeIndex(trim(sourceNodeIds(k)))
      if (sourcePos(k) == 0) then
        call raiseError("tStudy%run: source node '" // trim(sourceNodeIds(k)) // "' not found")
        return
      end if
    end do

    info = injectSignal(this%mesh, size(sourceNodeIds), sourcePos, sourceCurrents)
    if (info /= 0) then
      call raiseError("tStudy%run: injectSignal failed (ZGESV INFO /= 0)")
    end if
  end subroutine run

  subroutine report(this)
    !! Print a formatted text report of the study geometry and properties.
    !!
    !! Outputs:
    !! - Study title
    !! - Node count, material count, element count
    !! - Detailed list of all nodes with coordinates
    !! - Detailed list of all materials with properties
    !! - Detailed list of all elements with their parameters
    class(tStudy), intent(in) :: this
    character(:), allocatable :: str
    character(len=256) :: line
    integer :: i
    class(tElement), pointer :: element => null()
    class(tMaterial), pointer :: mat => null()

    str = "=========================================" // newl // &
          "Example Study Initialization" // newl // &
          "=========================================" // newl
    str = str // "Study Title: " // trim(this%title) // newl
    write(line,'("Number of Nodes: ",I0)') this%structure%getNodeCount()
    str = str // trim(line) // newl
    write(line,'("Number of Materials: ",I0)') this%structure%getMaterialCount()
    str = str // trim(line) // newl
    write(line,'("Number of Elements: ",I0)') this%structure%getElementCount()
    str = str // trim(line) // newl
    str = str // "Nodes:" // newl
    do i = 1, this%structure%getNodeCount()
      write(line,'("  ",A," at (",F0.2,", ",F0.2,", ",F0.2,")")') &
        trim(this%structure%nodes(i)%id), &
        this%structure%nodes(i)%p(1), this%structure%nodes(i)%p(2), &
        this%structure%nodes(i)%p(3)
      str = str // trim(line) // newl
    end do
    str = str // "Materials:" // newl
    do i = 1, this%structure%getMaterialCount()
      mat => this%structure%getMaterial(i)
      call mat%report(str)
    end do
    str = str // "Elements:" // newl
    do i = 1, this%structure%getElementCount()
      element => this%structure%getElement(i)
      call element%report(str)
    end do
    str = str // "=========================================" // newl
    print *, str
  end subroutine report

end module mStudy
