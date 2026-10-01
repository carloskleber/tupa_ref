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
  use mObservation, only: tObservation, tObservationResults
  use mGeometry, only: buildGeometryMatrices, setGeometryKernel, getGeometryKernel
  use mGeometryCache, only: geomCacheStats
  use mImpedance, only: internalImpedance, internalImpedanceLaplace, warmUpMachineConstants
  use mError, only: raiseError
  use mCtes, only: newl, PI, EPSILON0, MU0, ZERO_CPLX
  use mVerbosity
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
    type(tVoltages) :: voltageResults
    !! Node voltages V(ω) across the last `runSweep` call
    type(tLongCurrents) :: longCurrentResults
    !! Longitudinal electrode currents I_long(ω) across the last `runSweep` call
    type(tTransCurrents) :: transCurrentResults
    !! Transverse (leakage) electrode currents I_trans(ω) across the last `runSweep` call
    real(8), allocatable :: sweepFreqHz(:)
    !! Frequency axis (Hz) of the last `runSweep` call
    real(8) :: sweepDamping = 0.0d0
    !! Damping c (1/s) of the last `runSweep` call: results were solved at
    !! s = c + jω (0 = ordinary harmonic sweep; > 0 only for the Numerical
    !! Laplace Transform driver, ROADMAP Phase 9 item 5)
    character(256), allocatable :: sweepSourceIds(:)
    !! Source node IDs of the last `runSweep` call (for `inputImpedance`)
    character(256), allocatable :: sweepReturnIds(:)
    !! Return node IDs of the last `runSweep` call (blank = none); allocated
    !! only when the call passed `returnNodeIds`
    complex(8), allocatable :: sweepSourceCurrents(:)
    !! Source values corresponding to `sweepSourceIds` as given by the
    !! caller: currents (A), or voltages (V) where flagged as voltage
    !! sources (ADR 0016)
    complex(8), allocatable :: lastSourceCurrents(:)
    !! Effective currents (A) actually injected by the last `run` call —
    !! equal to the given currents for current sources; the solved
    !! equivalent injections for voltage sources (ADR 0010/0016)
    complex(8), allocatable :: sweepSourceCurrentsFreq(:,:)
    !! Effective injected currents per source and frequency of the last
    !! `runSweep` call, shape (nSources, nFreq) — frequency-dependent for
    !! voltage sources, constant columns for current sources

    integer :: imageModel = 0
    !! Image reflection model (`IMAGE_FREQ_DEPENDENT` / `IMAGE_IDEAL` of
    !! mMesh; ROADMAP Phase 10 item 2). 0 = the process default
    !! (`mMesh%getDefaultImageModel`, frequency-dependent unless `--image-model`).
    integer :: geometryKernel = 0
    !! Geometry-factor quadrature (`GEOM_KERNEL_SINGLE`/`_DOUBLE` of
    !! mGeometry; ROADMAP Phase 10 item 1). 0 = the process default
    !! (`mGeometry%getGeometryKernel`, the single-integral form unless
    !! `--kernel double`).
    type(tObservation) :: observation
    !! Parsed `"observation"` block (ROADMAP Phase 11, ADR 0027); empty when
    !! the case asks for no potentials
    type(tObservationResults) :: observationResults
    !! Potentials, GPR, touch and step voltages of the last
    !! `mPotentials::computeObservations` call
    real(8) :: maxSegmentLength = 0.0d0
    !! Per-study segment-length target (m), ROADMAP Phase 10 item 3; 0 = none
    !! (each element keeps its own `segments` count). Applied while loading.

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
    procedure :: runSweep
    !! Execute `run` across a frequency sweep, storing results (ROADMAP Phase 3)
    procedure :: runSweepUnits
    !! Transfer functions of several unit terminals from one factorisation
    !! per frequency (transient driver, ADR 0026)
    procedure :: inputImpedance
    !! Driving-point impedance Zin(ω) at a sweep source node
    procedure :: maxVoltageMagnitude
    !! Per-frequency maximum |V| across all nodes (e.g. ground-potential-rise check)
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
    integer(4) :: nno, nseg, i, kernelDefault
    integer(4), allocatable :: n1(:), n2(:)
    real(8), allocatable :: p1(:,:), p2(:,:)

    if (verbosityLevel() .eq. VERB_VERBOSE) print *, "Assembling structure and computing geometry factors..."
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

    ! The study's own kernel, if it states one, applies only to its geometry
    ! build: the process default is restored afterwards so studies do not leak
    ! into each other.
    kernelDefault = getGeometryKernel()
    if (this%geometryKernel /= 0) call setGeometryKernel(this%geometryKernel)
    call buildGeometryMatrices(p1, p2, this%geomRadius, nseg, &
      this%geomG, this%geomGi, this%geomRbar, this%geomRbari, &
      this%geomCosTheta, this%geomCosThetaI, pos=this%geomPos)
    if (this%geometryKernel /= 0) call setGeometryKernel(kernelDefault)

    if (verbosityLevel() .eq. VERB_VERBOSE) then
      block
        integer(8) :: cacheHits, cacheMisses
        integer :: cacheEntries
        call geomCacheStats(cacheHits, cacheMisses, cacheEntries)
        print '(A,I0,A,I0,A,I0,A)', " Geometry-factor quadrature cache: ", &
          cacheHits, " hits, ", cacheMisses, " misses (", cacheEntries, " entries)"
      end block
    end if

    call initMesh(this%mesh, nno, nseg)
    call calcTopology(this%mesh, nseg, n1, n2)

    this%prepared = .true.
  end subroutine prepareStudy

  ! =====================================================================
  ! Per-segment internal (skin-effect) impedance
  ! =====================================================================

  subroutine checkConductorMaterials(this)
    !! Raise the "requires a tLinear conductor material" error up front, so a
    !! threaded sweep never has to raise it from inside a parallel region.
    class(tStudy), intent(in) :: this
    integer(4) :: i

    do i = 1, this%structure%getElectrodeCount()
      if (this%structure%electrodes(i)%loaded) cycle
      select type (mat => this%structure%electrodes(i)%material)
      type is (tLinear)
      class default
        call raiseError("tStudy%run: internal impedance requires a tLinear conductor material")
      end select
    end do
  end subroutine checkConductorMaterials

  complex(8) function segmentInternalImpedance(this, i, omega) result(zint)
    !! Internal impedance of electrode `i`'s conductor material at `omega`
    !! (theory.md §4.3). Only `tLinear` conductor materials are supported;
    !! dispersive conductor models are not part of the current object model.
    !! A series-loaded segment (lightning channel, theory.md §4.5) takes
    !! (R' + jωL')·l instead.
    class(tStudy), intent(in) :: this
    integer(4), intent(in) :: i
    real(8), intent(in) :: omega

    if (this%structure%electrodes(i)%loaded) then
      zint = cmplx(this%structure%electrodes(i)%loadResistance, &
                   omega * this%structure%electrodes(i)%loadInductance, kind=8) * this%geomLength(i)
      return
    end if
    select type (mat => this%structure%electrodes(i)%material)
    type is (tLinear)
      zint = internalImpedance(this%geomRadius(i), this%geomLength(i), omega, mat%sigma, mat%mur)
    class default
      call raiseError("tStudy%run: internal impedance requires a tLinear conductor material")
      zint = ZERO_CPLX
    end select
  end function segmentInternalImpedance

  complex(8) function segmentInternalImpedanceLaplace(this, i, sLap) result(zint)
    !! `segmentInternalImpedance` at a complex frequency s = c + jω.
    class(tStudy), intent(in) :: this
    integer(4), intent(in) :: i
    complex(8), intent(in) :: sLap

    if (this%structure%electrodes(i)%loaded) then
      zint = (this%structure%electrodes(i)%loadResistance + &
              sLap * this%structure%electrodes(i)%loadInductance) * this%geomLength(i)
      return
    end if
    select type (mat => this%structure%electrodes(i)%material)
    type is (tLinear)
      zint = internalImpedanceLaplace(this%geomRadius(i), this%geomLength(i), sLap, mat%sigma, mat%mur)
    class default
      call raiseError("tStudy%run: internal impedance requires a tLinear conductor material")
      zint = ZERO_CPLX
    end select
  end function segmentInternalImpedanceLaplace

  ! =====================================================================
  ! Study execution and reporting
  ! =====================================================================

  subroutine run(this, omega, sourceNodeIds, sourceCurrents, sourceIsVoltage, damping, returnNodeIds)
    !! Solve the study at one angular frequency ω, injecting the given
    !! sources at the given nodes (ADR 0010: current-injection sources).
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
    !!
    !! Sources flagged in the optional `sourceIsVoltage` are ideal voltage
    !! sources: their `sourceCurrents` entry is read as a complex voltage
    !! (V) and converted to an equivalent current injection by
    !! unit-injection superposition (ADR 0016 — the solver kernel sees only
    !! currents, per ADR 0010). The effective injected currents of every
    !! source are left in `this%lastSourceCurrents`.
    !!
    !! With a nonzero `damping` c the system is solved at the complex
    !! frequency s = c + jω instead of jω (Numerical Laplace Transform,
    !! ROADMAP Phase 9 item 5, theory.md §8): media immittances come from
    !! `admittanceLaplace` and every jω factor becomes s. Absent or zero,
    !! the real-ω path runs exactly as before.
    !!
    !! A source with a nonblank entry in `returnNodeIds` is a two-node source
    !! (ROADMAP Phase 10b item 2, ADR 0025): a current source pushes +I into
    !! its node and -I into the return node; a voltage source fixes
    !! u(node) - u(returnNode). Its unit pattern is the ±1 dipole.
    class(tStudy), intent(inout) :: this
    real(8), intent(in) :: omega
    !! Angular frequency ω (rad/s) for this solve
    character(len=*), intent(in) :: sourceNodeIds(:)
    !! User-assigned IDs of the nodes receiving the injection
    complex(8), intent(in) :: sourceCurrents(:)
    !! Source values, one per node in `sourceNodeIds`: injected current (A),
    !! or source voltage (V) where `sourceIsVoltage` is true
    logical, intent(in), optional :: sourceIsVoltage(:)
    !! Marks entries of `sourceCurrents` as voltage sources (default: all
    !! current sources)
    real(8), intent(in), optional :: damping
    !! Damping c (1/s) of the complex frequency s = c + jω (default 0)
    character(len=*), intent(in), optional :: returnNodeIds(:)
    !! Return node per source (blank = none: the source acts against remote earth)
    integer(4), allocatable :: sourcePos(:), retPos(:)
    complex(8), allocatable :: lastSrc(:)
    integer(4) :: k, info
    logical :: anyVoltage, laplace

    laplace = .false.
    if (present(damping)) laplace = damping /= 0.0d0

    if (.not. this%prepared) call prepareStudy(this)

    call resolveSources(this, "run", sourceNodeIds, sourceCurrents, sourceIsVoltage, returnNodeIds, &
                        sourcePos, retPos, anyVoltage)
    if (.not. allocated(sourcePos)) return

    call solveAtFrequency(this, this%mesh, omega, sourcePos, retPos, sourceCurrents, sourceIsVoltage, anyVoltage, &
                          damping, laplace, lastSrc, info)
    if (info /= 0) then
      call raiseError("tStudy%run: linear solve failed (ZGESV INFO /= 0)")
      return
    end if
    this%lastSourceCurrents = lastSrc
  end subroutine run

  subroutine resolveSources(this, who, sourceNodeIds, sourceCurrents, sourceIsVoltage, returnNodeIds, &
                            sourcePos, retPos, anyVoltage)
    !! Validate the source arguments of `run`/`runSweep` and resolve the
    !! node IDs to 1-based node indices (`retPos` = 0 where a source has no
    !! return node). Leaves `sourcePos` unallocated after raising an error.
    !! Done once, before any (possibly threaded) solve.
    class(tStudy), intent(in) :: this
    character(len=*), intent(in) :: who
    character(len=*), intent(in) :: sourceNodeIds(:)
    complex(8), intent(in) :: sourceCurrents(:)
    logical, intent(in), optional :: sourceIsVoltage(:)
    character(len=*), intent(in), optional :: returnNodeIds(:)
    integer(4), allocatable, intent(out) :: sourcePos(:), retPos(:)
    logical, intent(out) :: anyVoltage
    integer(4), allocatable :: pos(:), ret(:)
    integer(4) :: k

    anyVoltage = .false.
    allocate(pos(size(sourceNodeIds)), ret(size(sourceNodeIds)))
    ret = 0
    do k = 1, size(sourceNodeIds)
      pos(k) = this%structure%findNodeIndex(trim(sourceNodeIds(k)))
      if (pos(k) == 0) then
        call raiseError("tStudy%" // who // ": source node '" // trim(sourceNodeIds(k)) // "' not found")
        return
      end if
    end do

    if (present(returnNodeIds)) then
      if (size(returnNodeIds) /= size(sourceNodeIds)) then
        call raiseError("tStudy%" // who // ": returnNodeIds must have one entry per source")
        return
      end if
      do k = 1, size(returnNodeIds)
        if (len_trim(returnNodeIds(k)) == 0) cycle
        ret(k) = this%structure%findNodeIndex(trim(returnNodeIds(k)))
        if (ret(k) == 0) then
          call raiseError("tStudy%" // who // ": return node '" // trim(returnNodeIds(k)) // "' not found")
          return
        end if
        if (ret(k) == pos(k)) then
          call raiseError("tStudy%" // who // ": source node '" // trim(sourceNodeIds(k)) // &
                          "' and its return node are the same node")
          return
        end if
      end do
    end if

    if (present(sourceIsVoltage)) then
      if (size(sourceIsVoltage) /= size(sourceNodeIds)) then
        call raiseError("tStudy%" // who // ": sourceIsVoltage must have one entry per source")
        return
      end if
      anyVoltage = any(sourceIsVoltage)
    end if
    call move_alloc(pos, sourcePos)
    call move_alloc(ret, retPos)
  end subroutine resolveSources

  subroutine injectionPatterns(sourcePos, retPos, nodes, patterns)
    !! Right-hand-side patterns of the sources: `nodes` lists every distinct
    !! node a source touches, and column k of `patterns` is source k's unit
    !! injection over those nodes — +1 at its node, -1 at its return node
    !! (ADR 0025). Merging shared nodes here also keeps the vector-subscript
    !! assignment of `injectSignal(s)` free of repeated indices.
    integer(4), intent(in) :: sourcePos(:), retPos(:)
    integer(4), allocatable, intent(out) :: nodes(:)
    complex(8), allocatable, intent(out) :: patterns(:,:)
    integer(4), allocatable :: list(:)
    integer(4) :: ns, k, n, j

    ns = size(sourcePos)
    allocate(list(2 * ns))
    n = 0
    do k = 1, ns
      call addNode(sourcePos(k))
      if (retPos(k) /= 0) call addNode(retPos(k))
    end do
    allocate(nodes(n), patterns(n, ns))
    nodes = list(1:n)
    patterns = ZERO_CPLX
    do k = 1, ns
      patterns(indexOf(sourcePos(k)), k) = cmplx(1.0d0, 0.0d0, kind=8)
      if (retPos(k) /= 0) patterns(indexOf(retPos(k)), k) = cmplx(-1.0d0, 0.0d0, kind=8)
    end do

  contains

    subroutine addNode(idx)
      integer(4), intent(in) :: idx
      if (n > 0) then
        if (any(list(1:n) == idx)) return
      end if
      n = n + 1
      list(n) = idx
    end subroutine addNode

    integer(4) function indexOf(idx) result(r)
      integer(4), intent(in) :: idx
      do j = 1, n
        if (list(j) == idx) then
          r = j
          return
        end if
      end do
      r = 0
    end function indexOf

  end subroutine injectionPatterns

  subroutine solveAtFrequency(this, mesh, omega, sourcePos, retPos, sourceValues, sourceIsVoltage, anyVoltage, &
                              damping, laplace, lastSrc, info)
    !! One frequency's fill + solve on the caller's `mesh` (ROADMAP Phase 10
    !! item 4): the study is read-only here (geometry matrices, media), every
    !! write goes to `mesh` and the returned `lastSrc`, so concurrent calls
    !! with distinct meshes — one per thread in `runSweep` — are safe.
    !! Contains the former body of `run`: medium constants, the `calcZSelf`/
    !! `calcZMutual` fill (ADR 0009), `calcFreq2`, then the current- or
    !! voltage-source solve (ADR 0010/0016). `info` is the LAPACK status.
    class(tStudy), intent(in) :: this
    type(tMesh), intent(inout) :: mesh
    real(8), intent(in) :: omega
    integer(4), intent(in) :: sourcePos(:), retPos(:)
    complex(8), intent(in) :: sourceValues(:)
    logical, intent(in), optional :: sourceIsVoltage(:)
    logical, intent(in) :: anyVoltage
    real(8), intent(in), optional :: damping
    logical, intent(in) :: laplace
    complex(8), allocatable, intent(out) :: lastSrc(:)
    integer(4), intent(out) :: info
    integer(4), allocatable :: injNodes(:)
    complex(8), allocatable :: patterns(:,:)

    info = 0
    call fillAtFrequency(this, mesh, omega, damping, laplace)

    call injectionPatterns(sourcePos, retPos, injNodes, patterns)
    if (anyVoltage) then
      call solveWithVoltageSources(mesh, sourcePos, retPos, injNodes, patterns, sourceValues, sourceIsVoltage, &
                                   lastSrc, info)
    else
      info = injectSignal(mesh, size(injNodes), injNodes, matmul(patterns, sourceValues))
      lastSrc = sourceValues
    end if
  end subroutine solveAtFrequency

  subroutine fillAtFrequency(this, mesh, omega, damping, laplace)
    !! The frequency-dependent half of `solveAtFrequency`: medium constants,
    !! the `calcZSelf`/`calcZMutual` fill (ADR 0009) and `calcFreq2`, leaving
    !! `mesh%Zeq` assembled and ready to be factorised. Shared by the
    !! single-injection sweep and the multi-pattern transfer-function sweep
    !! (`runSweepUnits`); the study is read-only, every write goes to `mesh`.
    class(tStudy), intent(in) :: this
    type(tMesh), intent(inout) :: mesh
    real(8), intent(in) :: omega
    real(8), intent(in), optional :: damping
    logical, intent(in) :: laplace
    integer(4) :: nseg, i, j
    complex(8) :: zint, sLap
    real(8) :: muAir, muSoil

    if (laplace) sLap = cmplx(damping, omega, kind=8)

    muAir  = this%structure%air%mur * MU0
    muSoil = this%structure%soil%mur * MU0

    ! this%structure%soil is class(tMaterial): admittance() dispatches to
    ! whichever concrete model (tLinear, tPortelaSoil, ...) is stored, so any
    ! dispersive soil (ROADMAP Phase 4, ADR 0007) works here without a
    ! type-specific branch.
    mesh%imageModel = this%imageModel
    if (laplace) then
      call calcParamLaplace(mesh, sLap, muAir, this%structure%air%admittanceLaplace(sLap), &
                             muSoil, this%structure%soil%admittanceLaplace(sLap))
    else
      call calcParamW(mesh, omega, muAir, this%structure%air%admittance(omega), &
                       muSoil, this%structure%soil%admittance(omega))
    end if

    nseg = this%structure%getElectrodeCount()
    do i = 1, nseg
      do j = i, nseg
        if (i == j) then
          if (laplace) then
            zint = segmentInternalImpedanceLaplace(this, i, sLap)
          else
            zint = segmentInternalImpedance(this, i, omega)
          end if
          call calcZSelf(mesh, i, this%geomPos(i), &
            this%geomRbar(i,i), this%geomRbari(i,i), this%geomLength(i), &
            zint, this%geomG(i,i), this%geomGi(i,i), this%geomCosThetaI(i,i))
        else
          call calcZMutual(mesh, i, j, this%geomPos(i), this%geomPos(j), &
            this%geomRbar(i,j), this%geomRbari(i,j), &
            this%geomLength(i), this%geomLength(j), &
            this%geomG(i,j), this%geomGi(i,j), &
            this%geomCosTheta(i,j), this%geomCosThetaI(i,j))
        end if
      end do
    end do

    call calcFreq2(mesh)
  end subroutine fillAtFrequency

  subroutine solveWithVoltageSources(mesh, sourcePos, retPos, injNodes, patterns, sourceValues, isVoltage, &
                                     lastSrc, info)
    !! Convert ideal voltage sources to equivalent current injections by
    !! unit-injection superposition (ADR 0016, implementing ADR 0010's
    !! study-layer conversion), then superpose the full solution.
    !!
    !! One multi-RHS solve (`injectSignals`) with a unit current at each
    !! source node gives every source-node voltage per unit injection —
    !! the transfer-impedance matrix restricted to the source nodes. The
    !! unknown injections at the voltage-source nodes then satisfy the
    !! small dense system
    !!     Σ_k I_k · Vunit(pos_j, k) = U_j   for every voltage source j,
    !! with the current-source injections I_k fixed by the caller. The
    !! full field solution is the same superposition applied to the unit
    !! solutions, left in `mesh%voltage`/`current1`/`current2` exactly as
    !! `injectSignal` would; the effective injections (fixed + solved) are
    !! returned in `lastSrc`.
    !!
    !! `mesh%Zeq` holds LU factors afterwards, same as the plain-current
    !! path — `solveAtFrequency` reassembles it (`calcFreq2`) on every call.
    !!
    !! A two-node source (ADR 0025) uses the ±1 dipole as its unit pattern
    !! and constrains u(node) - u(returnNode); the equations above hold with
    !! `Vunit(pos_j, k)` read as that difference.
    type(tMesh), intent(inout) :: mesh
    integer(4), intent(in) :: sourcePos(:), retPos(:)
    !! 1-based node indices of every source and of its return node (0 = none)
    integer(4), intent(in) :: injNodes(:)
    !! Distinct nodes touched by the sources
    complex(8), intent(in) :: patterns(:,:)
    !! Unit injection pattern of each source over `injNodes`
    complex(8), intent(in) :: sourceValues(:)
    !! Current (A) or voltage (V) per source, per `isVoltage`
    logical, intent(in) :: isVoltage(:)
    !! Which entries of `sourceValues` are voltages
    complex(8), allocatable, intent(out) :: lastSrc(:)
    !! Effective injected currents per source
    integer(4), intent(out) :: info
    !! LAPACK status of the failing solve (0 = success)
    complex(8), allocatable :: vUnit(:,:), i1Unit(:,:), i2Unit(:,:)
    complex(8), allocatable :: a(:,:), rhs(:), ieff(:)
    integer(4), allocatable :: vIdx(:), ipiv(:)
    integer(4) :: ns, nV, j, k, l

    ns = size(sourcePos)

    info = injectSignals(mesh, size(injNodes), injNodes, patterns, vUnit, i1Unit, i2Unit)
    if (info /= 0) return

    nV = count(isVoltage)
    allocate(vIdx(nV))
    j = 0
    do k = 1, ns
      if (isVoltage(k)) then
        j = j + 1
        vIdx(j) = k
      end if
    end do

    ! Constraint system A·Iv = rhs over the voltage-source injections
    allocate(a(nV, nV), rhs(nV), ipiv(nV))
    do j = 1, nV
      do l = 1, nV
        a(j, l) = gap(vIdx(j), vIdx(l))
      end do
      rhs(j) = sourceValues(vIdx(j))
      do k = 1, ns
        if (.not. isVoltage(k)) rhs(j) = rhs(j) - sourceValues(k) * gap(vIdx(j), k)
      end do
    end do

    call zgesv(nV, 1, a, nV, ipiv, rhs, nV, info)
    if (info /= 0) return

    allocate(ieff(ns))
    do k = 1, ns
      ieff(k) = sourceValues(k)
    end do
    do j = 1, nV
      ieff(vIdx(j)) = rhs(j)
    end do

    mesh%voltage  = matmul(vUnit,  ieff)
    mesh%current1 = matmul(i1Unit, ieff)
    mesh%current2 = matmul(i2Unit, ieff)
    lastSrc = ieff

  contains

    complex(8) function gap(j, k) result(v)
      !! Voltage across source j's terminals for a unit pattern k.
      integer(4), intent(in) :: j, k
      v = vUnit(sourcePos(j), k)
      if (retPos(j) /= 0) v = v - vUnit(retPos(j), k)
    end function gap

  end subroutine solveWithVoltageSources

  ! =====================================================================
  ! Frequency sweep, result storage, and convenience queries (ROADMAP Phase 3)
  ! =====================================================================

  function logFrequencyAxis(freqMinHz, freqMaxHz, nPoints) result(freqHz)
    !! Default log-spaced frequency axis (ROADMAP.md Phase 3 item 1;
    !! CONVENTIONS.md: log spacing for harmonic sweeps, linear for
    !! transients). `nPoints` points from `freqMinHz` to `freqMaxHz`
    !! inclusive; pass a different axis to `runSweep` directly to override.
    real(8), intent(in) :: freqMinHz, freqMaxHz
    !! Endpoints of the sweep (Hz), both > 0
    integer(4), intent(in) :: nPoints
    !! Number of frequency points (>= 2)
    real(8), allocatable :: freqHz(:)
    real(8) :: logMin, logMax
    integer(4) :: k

    if (nPoints < 2) then
      call raiseError("logFrequencyAxis: nPoints must be >= 2")
      return
    end if

    allocate(freqHz(nPoints))
    logMin = log10(freqMinHz)
    logMax = log10(freqMaxHz)
    do k = 1, nPoints
      freqHz(k) = 10.0d0 ** (logMin + (logMax - logMin) * real(k - 1, kind=8) / real(nPoints - 1, kind=8))
    end do
  end function logFrequencyAxis

  subroutine runSweep(this, freqHz, sourceNodeIds, sourceCurrents, sourceIsVoltage, damping, returnNodeIds)
    !! Solve the study once per frequency in `freqHz` (ROADMAP.md Phase 3
    !! items 1-2), storing node voltages and electrode currents in
    !! `this%voltageResults`/`longCurrentResults`/`transCurrentResults`,
    !! and the effective injected currents in
    !! `this%sweepSourceCurrentsFreq` (frequency-dependent for voltage
    !! sources, ADR 0016). Geometry factors are cached after the first
    !! `run` call (theory.md §4.1), so only the per-frequency fill+solve
    !! repeats. Use `logFrequencyAxis` to build a default log-spaced axis,
    !! or pass any user-chosen `freqHz`. A nonzero `damping` c solves every
    !! point at s = c + 2πj·f (see `run`); the stored axis stays real f.
    class(tStudy), intent(inout) :: this
    real(8), intent(in) :: freqHz(:)
    !! Frequency axis (Hz), in the order results are stored
    character(len=*), intent(in) :: sourceNodeIds(:)
    !! User-assigned IDs of the nodes receiving the injection
    complex(8), intent(in) :: sourceCurrents(:)
    !! Source values, one per node in `sourceNodeIds`: injected current (A),
    !! or source voltage (V) where `sourceIsVoltage` is true (ADR 0016)
    logical, intent(in), optional :: sourceIsVoltage(:)
    !! Marks entries of `sourceCurrents` as voltage sources (default: all
    !! current sources)
    real(8), intent(in), optional :: damping
    !! Damping c (1/s) of the complex frequency s = c + jω (default 0)
    character(len=*), intent(in), optional :: returnNodeIds(:)
    !! Return node per source (blank = none), see `run`
    real(8), allocatable :: omegaAxis(:)
    character(256), allocatable :: nodeIds(:), electrodeIds(:)
    integer(4) :: nf, nno, nseg, i, k, info, solveInfo
    integer(4), allocatable :: sourcePos(:), retPos(:)
    complex(8), allocatable :: lastSrc(:)
    logical :: anyVoltage, laplace
    type(tMesh) :: meshLocal

    if (.not. this%prepared) call prepareStudy(this)

    nf = size(freqHz)
    omegaAxis = 2.0d0 * PI * freqHz

    nno  = this%structure%getNodeCount()
    nseg = this%structure%getElectrodeCount()

    allocate(nodeIds(nno))
    do i = 1, nno
      nodeIds(i) = this%structure%nodes(i)%id
    end do
    allocate(electrodeIds(nseg))
    do i = 1, nseg
      electrodeIds(i) = this%structure%electrodes(i)%id
    end do

    call this%voltageResults%alloc(nodeIds, omegaAxis)
    call this%longCurrentResults%alloc(electrodeIds, omegaAxis)
    call this%transCurrentResults%alloc(electrodeIds, omegaAxis)

    if (allocated(this%sweepSourceCurrentsFreq)) deallocate(this%sweepSourceCurrentsFreq)
    allocate(this%sweepSourceCurrentsFreq(size(sourceNodeIds), nf))

    call resolveSources(this, "runSweep", sourceNodeIds, sourceCurrents, sourceIsVoltage, returnNodeIds, &
                        sourcePos, retPos, anyVoltage)
    if (.not. allocated(sourcePos)) return
    laplace = .false.
    if (present(damping)) laplace = damping /= 0.0d0
    call checkConductorMaterials(this)

    ! Frequencies are independent solves of the same read-only geometry
    ! (ROADMAP Phase 10 item 4, P6), so the loop parallelises over k with one
    ! thread-private mesh each — no shared mutable state: `this` is only read
    ! inside `solveAtFrequency`, the result arrays are written at disjoint
    ! columns k, and the geometry factors were built before the loop. The
    ! quadrature kernel needs no cache or module state at this point
    ! (`geometryFactor1D` is re-entrant and runs only in `prepareStudy`).
    ! Each frequency runs the identical serial operations, so results are
    ! bit-identical for any thread count. Without -fopenmp the directives are
    ! comments and the loop is the former serial one.
    solveInfo = 0
    call warmUpMachineConstants()
    !$omp parallel default(shared) private(k, lastSrc, info, meshLocal)
    meshLocal = this%mesh
    block
      !$omp do schedule(dynamic)
      do k = 1, nf
        if (verbosityLevel() .eq. VERB_VERBOSE) write(*, '("f = ",EN0.1E2," Hz")') freqHz(k)
        call solveAtFrequency(this, meshLocal, omegaAxis(k), sourcePos, retPos, sourceCurrents, sourceIsVoltage, &
                              anyVoltage, damping, laplace, lastSrc, info)
        if (info /= 0) then
          !$omp critical (tupaSolveInfo)
          solveInfo = info
          !$omp end critical (tupaSolveInfo)
          cycle
        end if

        do i = 1, nno
          call this%voltageResults%set(i, k, meshLocal%voltage(i))
        end do
        do i = 1, nseg
          call this%longCurrentResults%set(i, k, meshLocal%current1(i))
          call this%transCurrentResults%set(i, k, meshLocal%current2(i))
        end do
        this%sweepSourceCurrentsFreq(:, k) = lastSrc
      end do
      !$omp end do
    end block
    !$omp end parallel
    if (solveInfo /= 0) then
      call raiseError("tStudy%runSweep: linear solve failed (ZGESV INFO /= 0)")
      return
    end if
    ! Leave the study's own mesh and last-source state as the serial loop
    ! did: those of the final frequency (for callers reading `this%mesh`).
    call this%run(omegaAxis(nf), sourceNodeIds, sourceCurrents, sourceIsVoltage, damping, returnNodeIds)

    this%sweepFreqHz = freqHz
    this%sweepDamping = 0.0d0
    if (present(damping)) this%sweepDamping = damping
    this%sweepSourceIds = sourceNodeIds
    this%sweepSourceCurrents = sourceCurrents
    if (present(returnNodeIds)) then
      this%sweepReturnIds = returnNodeIds
    else if (allocated(this%sweepReturnIds)) then
      deallocate(this%sweepReturnIds)
    end if
  end subroutine runSweep

  subroutine runSweepUnits(this, freqHz, termNodeIds, termReturnIds, termIsVoltage, nodeRows, electrodeRows, &
                           hNode, hI1, hI2, damping)
    !! Transfer functions of several independent unit terminals at once
    !! (transient driver, ADR 0026). Terminal m is a unit current (A) or unit
    !! voltage (V) at node `termNodeIds(m)` — across the pair when
    !! `termReturnIds(m)` is nonblank (ADR 0025). Per frequency the system is
    !! filled and factorised once and all terminals are back-substituted
    !! together (`injectSignals`, NRHS = number of terminals), so each extra
    !! terminal costs one triangular solve instead of a whole sweep. A unit
    !! voltage terminal is the unit-current solution scaled by 1/(voltage
    !! across its terminals), the one-source case of ADR 0016.
    !!
    !! Only the requested rows are kept: `hNode(i, k, m)` is the voltage at
    !! node `nodeRows(i)`, `hI1`/`hI2(i, k, m)` the end currents of electrode
    !! `electrodeRows(i)`, at `freqHz(k)` for terminal m. The study's own
    !! sweep storage (`voltageResults` etc.) is not touched. A nonzero
    !! `damping` c solves at s = c + 2πj·f as in `runSweep`.
    class(tStudy), intent(inout) :: this
    real(8), intent(in) :: freqHz(:)
    character(len=*), intent(in) :: termNodeIds(:), termReturnIds(:)
    logical, intent(in) :: termIsVoltage(:)
    integer(4), intent(in) :: nodeRows(:), electrodeRows(:)
    !! 1-based node / electrode indices to keep
    complex(8), allocatable, intent(out) :: hNode(:,:,:), hI1(:,:,:), hI2(:,:,:)
    real(8), intent(in), optional :: damping
    real(8), allocatable :: omegaAxis(:)
    integer(4), allocatable :: sourcePos(:), retPos(:), injNodes(:)
    complex(8), allocatable :: patterns(:,:), unitValues(:)
    integer(4) :: nf, nT, k, solveInfo
    logical :: anyVoltage, laplace
    type(tMesh) :: meshLocal
    complex(8), allocatable :: vU(:,:), i1U(:,:), i2U(:,:)
    complex(8) :: scale
    integer(4) :: m, info

    if (.not. this%prepared) call prepareStudy(this)

    nf = size(freqHz)
    nT = size(termNodeIds)
    allocate(hNode(size(nodeRows), nf, nT), hI1(size(electrodeRows), nf, nT), hI2(size(electrodeRows), nf, nT))
    allocate(unitValues(nT))
    unitValues = cmplx(1.0d0, 0.0d0, kind=8)

    call resolveSources(this, "runSweepUnits", termNodeIds, unitValues, termIsVoltage, termReturnIds, &
                        sourcePos, retPos, anyVoltage)
    if (.not. allocated(sourcePos)) return
    laplace = .false.
    if (present(damping)) laplace = damping /= 0.0d0
    call checkConductorMaterials(this)

    omegaAxis = 2.0d0 * PI * freqHz
    call injectionPatterns(sourcePos, retPos, injNodes, patterns)

    ! Same threading scheme as `runSweep`: independent frequencies, one
    ! thread-private mesh each, disjoint writes, bit-identical for any thread count.
    solveInfo = 0
    call warmUpMachineConstants()
    !$omp parallel default(shared) private(meshLocal, vU, i1U, i2U, scale, m, info)
    meshLocal = this%mesh
    block
      !$omp do schedule(dynamic)
      do k = 1, nf
        if (verbosityLevel() .eq. VERB_VERBOSE) write(*, '("f = ",EN0.1E2," Hz")') freqHz(k)
        call fillAtFrequency(this, meshLocal, omegaAxis(k), damping, laplace)
        info = injectSignals(meshLocal, size(injNodes), injNodes, patterns, vU, i1U, i2U)
        if (info /= 0) then
          !$omp critical (tupaSolveInfo)
          solveInfo = info
          !$omp end critical (tupaSolveInfo)
          cycle
        end if
        do m = 1, nT
          scale = cmplx(1.0d0, 0.0d0, kind=8)
          if (termIsVoltage(m)) then
            scale = vU(sourcePos(m), m)
            if (retPos(m) /= 0) scale = scale - vU(retPos(m), m)
            scale = cmplx(1.0d0, 0.0d0, kind=8) / scale
          end if
          hNode(:, k, m) = scale * vU(nodeRows, m)
          hI1(:, k, m)   = scale * i1U(electrodeRows, m)
          hI2(:, k, m)   = scale * i2U(electrodeRows, m)
        end do
      end do
      !$omp end do
    end block
    !$omp end parallel
    if (solveInfo /= 0) call raiseError("tStudy%runSweepUnits: linear solve failed (ZGESV INFO /= 0)")
  end subroutine runSweepUnits

  function inputImpedance(this, nodeId) result(zin)
    !! Driving-point impedance Zin(ω) = V(nodeId)/I(nodeId) across the
    !! frequency axis of the last `runSweep` call (ROADMAP.md Phase 3 item
    !! 2), using the *effective* injected current at each frequency — so
    !! it is correct for voltage sources too (ADR 0016), where the
    !! injection varies with frequency. `nodeId` must be one of the
    !! sweep's source node IDs.
    class(tStudy), intent(in) :: this
    character(len=*), intent(in) :: nodeId
    complex(8), allocatable :: zin(:)
    integer(4) :: iNode, iRet, iSrc, k, nf

    iSrc = 0
    if (allocated(this%sweepSourceIds)) then
      do k = 1, size(this%sweepSourceIds)
        if (trim(this%sweepSourceIds(k)) == trim(nodeId)) then
          iSrc = k
          exit
        end if
      end do
    end if
    if (iSrc == 0) then
      call raiseError("tStudy%inputImpedance: '" // trim(nodeId) // "' was not a runSweep source node")
      return
    end if

    iNode = this%structure%findNodeIndex(trim(nodeId))
    ! A two-node source (ADR 0025): the impedance seen between its terminals
    iRet = 0
    if (allocated(this%sweepReturnIds)) then
      if (len_trim(this%sweepReturnIds(iSrc)) > 0) iRet = this%structure%findNodeIndex(trim(this%sweepReturnIds(iSrc)))
    end if
    nf = this%voltageResults%frequencyCount()
    allocate(zin(nf))
    do k = 1, nf
      zin(k) = this%voltageResults%get(iNode, k)
      if (iRet /= 0) zin(k) = zin(k) - this%voltageResults%get(iRet, k)
      zin(k) = zin(k) / this%sweepSourceCurrentsFreq(iSrc, k)
    end do
  end function inputImpedance

  function maxVoltageMagnitude(this) result(vmax)
    !! Per-frequency maximum |V| across all nodes (ROADMAP.md Phase 3 item
    !! 2) — e.g. a quick ground-potential-rise check across the sweep.
    class(tStudy), intent(in) :: this
    real(8), allocatable :: vmax(:)
    integer(4) :: nf, nno, i, k

    nf  = this%voltageResults%frequencyCount()
    nno = this%voltageResults%entityCount()
    allocate(vmax(nf))
    do k = 1, nf
      vmax(k) = 0.0d0
      do i = 1, nno
        vmax(k) = max(vmax(k), abs(this%voltageResults%get(i, k)))
      end do
    end do
  end function maxVoltageMagnitude

  subroutine report(this)
    !! Print a formatted text report of the study geometry and properties.
    !!
    !! Outputs:
    !! - Study title
    !! - Node count, material count, element count
    !! - Detailed list of all nodes with coordinates
    !! - Detailed list of all materials with properties
    !! - Detailed list of all elements with their parameters
    !!
    !! Suppressed under `mVerbosity`'s `VERB_QUIET` (`-q`/`--quiet`).
    class(tStudy), intent(in) :: this
    character(:), allocatable :: str
    character(len=256) :: line
    integer :: i
    class(tElement), pointer :: element => null()
    class(tMaterial), pointer :: mat => null()

    if (verbosityLevel() < VERB_NORMAL) return

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
    write(*, '(A)') str
  end subroutine report

end module mStudy
