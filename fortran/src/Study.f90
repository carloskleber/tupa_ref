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
  use mGeometryCache, only: geomCacheStats
  use mImpedance, only: internalImpedance
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
    character(256), allocatable :: sweepSourceIds(:)
    !! Source node IDs of the last `runSweep` call (for `inputImpedance`)
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
    integer(4) :: nno, nseg, i
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

    call buildGeometryMatrices(p1, p2, this%geomRadius, nseg, &
      this%geomG, this%geomGi, this%geomRbar, this%geomRbari, &
      this%geomCosTheta, this%geomCosThetaI, pos=this%geomPos)

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

  subroutine run(this, omega, sourceNodeIds, sourceCurrents, sourceIsVoltage)
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
    integer(4) :: nseg, i, j, k
    integer(4), allocatable :: sourcePos(:)
    complex(8) :: zint
    real(8) :: muAir, muSoil
    integer(4) :: info
    logical :: anyVoltage

    if (.not. this%prepared) call prepareStudy(this)

    muAir  = this%structure%air%mur * MU0
    muSoil = this%structure%soil%mur * MU0

    ! this%structure%soil is class(tMaterial): admittance() dispatches to
    ! whichever concrete model (tLinear, tPortelaSoil, ...) is stored, so any
    ! dispersive soil (ROADMAP Phase 4, ADR 0007) works here without a
    ! type-specific branch.
    call calcParamW(this%mesh, omega, muAir, this%structure%air%admittance(omega), &
                     muSoil, this%structure%soil%admittance(omega))

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

    anyVoltage = .false.
    if (present(sourceIsVoltage)) then
      if (size(sourceIsVoltage) /= size(sourceNodeIds)) then
        call raiseError("tStudy%run: sourceIsVoltage must have one entry per source")
        return
      end if
      anyVoltage = any(sourceIsVoltage)
    end if

    if (anyVoltage) then
      call solveWithVoltageSources(this, sourcePos, sourceCurrents, sourceIsVoltage)
    else
      info = injectSignal(this%mesh, size(sourceNodeIds), sourcePos, sourceCurrents)
      if (info /= 0) then
        call raiseError("tStudy%run: injectSignal failed (ZGESV INFO /= 0)")
      end if
      this%lastSourceCurrents = sourceCurrents
    end if
  end subroutine run

  subroutine solveWithVoltageSources(this, sourcePos, sourceValues, isVoltage)
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
    !! solutions, left in `this%mesh%voltage`/`current1`/`current2`
    !! exactly as `injectSignal` would; the effective injections (fixed +
    !! solved) are stored in `this%lastSourceCurrents`.
    !!
    !! `mesh%Zeq` holds LU factors afterwards, same as the plain-current
    !! path — `run` reassembles it (`calcFreq2`) on every call.
    class(tStudy), intent(inout) :: this
    integer(4), intent(in) :: sourcePos(:)
    !! 1-based node indices of every source
    complex(8), intent(in) :: sourceValues(:)
    !! Current (A) or voltage (V) per source, per `isVoltage`
    logical, intent(in) :: isVoltage(:)
    !! Which entries of `sourceValues` are voltages
    complex(8), allocatable :: unitSigs(:,:), vUnit(:,:), i1Unit(:,:), i2Unit(:,:)
    complex(8), allocatable :: a(:,:), rhs(:), ieff(:)
    integer(4), allocatable :: vIdx(:), ipiv(:)
    integer(4) :: ns, nV, j, k, l, info

    ns = size(sourcePos)
    allocate(unitSigs(ns, ns))
    unitSigs = ZERO_CPLX
    do k = 1, ns
      unitSigs(k, k) = cmplx(1.0d0, 0.0d0, kind=8)
    end do

    info = injectSignals(this%mesh, ns, sourcePos, unitSigs, vUnit, i1Unit, i2Unit)
    if (info /= 0) then
      call raiseError("tStudy%run: unit-injection solve failed (ZGESV INFO /= 0)")
      return
    end if

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
        a(j, l) = vUnit(sourcePos(vIdx(j)), vIdx(l))
      end do
      rhs(j) = sourceValues(vIdx(j))
      do k = 1, ns
        if (.not. isVoltage(k)) rhs(j) = rhs(j) - sourceValues(k) * vUnit(sourcePos(vIdx(j)), k)
      end do
    end do

    call zgesv(nV, 1, a, nV, ipiv, rhs, nV, info)
    if (info /= 0) then
      call raiseError("tStudy%run: voltage-source constraint solve failed (ZGESV INFO /= 0)")
      return
    end if

    allocate(ieff(ns))
    do k = 1, ns
      ieff(k) = sourceValues(k)
    end do
    do j = 1, nV
      ieff(vIdx(j)) = rhs(j)
    end do

    this%mesh%voltage  = matmul(vUnit,  ieff)
    this%mesh%current1 = matmul(i1Unit, ieff)
    this%mesh%current2 = matmul(i2Unit, ieff)
    this%lastSourceCurrents = ieff
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

  subroutine runSweep(this, freqHz, sourceNodeIds, sourceCurrents, sourceIsVoltage)
    !! Solve the study once per frequency in `freqHz` (ROADMAP.md Phase 3
    !! items 1-2), storing node voltages and electrode currents in
    !! `this%voltageResults`/`longCurrentResults`/`transCurrentResults`,
    !! and the effective injected currents in
    !! `this%sweepSourceCurrentsFreq` (frequency-dependent for voltage
    !! sources, ADR 0016). Geometry factors are cached after the first
    !! `run` call (theory.md §4.1), so only the per-frequency fill+solve
    !! repeats. Use `logFrequencyAxis` to build a default log-spaced axis,
    !! or pass any user-chosen `freqHz`.
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
    real(8), allocatable :: omegaAxis(:)
    character(256), allocatable :: nodeIds(:), electrodeIds(:)
    integer(4) :: nf, nno, nseg, i, k

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

    do k = 1, nf
      if (verbosityLevel() .eq. VERB_VERBOSE) write(*, '("f = ",EN0.1E2," Hz")') freqHz(k)
      call this%run(omegaAxis(k), sourceNodeIds, sourceCurrents, sourceIsVoltage)

      do i = 1, nno
        call this%voltageResults%set(i, k, this%mesh%voltage(i))
      end do
      do i = 1, nseg
        call this%longCurrentResults%set(i, k, this%mesh%current1(i))
        call this%transCurrentResults%set(i, k, this%mesh%current2(i))
      end do
      this%sweepSourceCurrentsFreq(:, k) = this%lastSourceCurrents
    end do

    this%sweepFreqHz = freqHz
    this%sweepSourceIds = sourceNodeIds
    this%sweepSourceCurrents = sourceCurrents
  end subroutine runSweep

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
    integer(4) :: iNode, iSrc, k, nf

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
    nf = this%voltageResults%frequencyCount()
    allocate(zin(nf))
    do k = 1, nf
      zin(k) = this%voltageResults%get(iNode, k) / this%sweepSourceCurrentsFreq(iSrc, k)
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
    print *, str
  end subroutine report

end module mStudy
