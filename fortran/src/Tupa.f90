module tupa
  !! High-level I/O module that orchestrates JSON parsing and study execution.
  !!
  !! This module provides the entry points for loading electromagnetic studies
  !! from JSON files and executing the full simulation pipeline. It bridges the
  !! gap between file-based input format and the internal object model.
  !!
  !! **Entry points:**
  !! - `loadStudy(filename, study)` — parse JSON, populate tStudy object
  !! - `runFromFile(filename)` — parse, run, and report (convenience wrapper)
  !!
  !! **JSON Input Format:**
  !! The JSON file must contain:
  !! - `"title"` (string) — study name
  !! - `"soil"` (object) — soil properties. Optional `"type"` selects the
  !!   dispersion model (default `"linear"`): `"linear"` takes `permittivity`,
  !!   `permeability`, `conductivity`; `"portela"` (Lima-Portela, ADR 0007)
  !!   takes `permeability`, `sigma0`, `alpha0`, `kr`; `"alipio-visacro"`
  !!   (theory.md §7, references.md [14], mean parameter set) takes
  !!   `permeability`, `sigma0`.
  !! - `"nodes"` (array of objects) — boundary nodes with `id` and `position` (3D)
  !! - `"materials"` (array of objects, optional) — conductor materials with `id`, `epsilonr`, `mur`, `sigma`
  !! - `"elements"` (array of objects) — geometric elements with type-specific parameters
  !!
  !! **Element Types:**
  !! - `"line"` — straight conductor with parameters: `id`, `from`, `to`, `radius`, `segments`, `material`
  !! - `"mesh"` — rectangular grounding grid (`mElementMesh`), composite of
  !!   `"line"` bars on an axis-aligned grid: `id`, `position` (grid corner,
  !!   3D), `lengthX`, `lengthY`, `rowsX`, `rowsY`, `radius`, `segments`,
  !!   `material`. Plants its own main nodes, named `"<id>-<row><col>"`
  !!   (2-digit zero-padded, 0-based) — externally referenceable by
  !!   `sources[].node` or another element's `from`/`to`.
  !! - `"catenary"` — parabolic sagging span (`mElementCatenary`, ADR 0023):
  !!   the `"line"` fields plus `sag` (midspan drop below the chord, m).
  !!
  !! - `"channel"` — lightning return-stroke channel in air
  !!   (`mElementChannel`, ADR 0025, theory.md §4.5): `id`, `strike` (node),
  !!   `length`, `radius`, optional `incidence`/`azimuth` (degrees), loading
  !!   `speed` (m/s, number or `[{upTo, value}]` profile) or `inductance`
  !!   (H/m), `resistance` (Ω/m, number or profile), `calibrate`, and either
  !!   `segments` (uniform) or `maxSegment` with optional `firstSegment` and
  !!   `growth` (graded from the foot). Plants `<id>-base` (separate from
  !!   the strike node), `<id>_n<k>` and `<id>-top`.
  !!
  !! - `"observation"` (optional, ADR 0027): potentials, GPR, touch and step
  !!   voltages from the solved sweep — `points`, `grid`, `touch`, `steps`
  !!   (`mObservation`, evaluated by `mPotentials`; written to
  !!   `<case>_potentials.csv/.json`).
  !!
  !! Future versions will add tCircumference, tTower.
  use mStudy
  use mObservation, only: tObservation, tObservationPoint, tGridSpec, tTouchSpec, tStepSpec
  use mPotentials, only: computeObservations
  use mMesh, only: IMAGE_FREQ_DEPENDENT, IMAGE_IDEAL
  use mGeometry, only: GEOM_KERNEL_SINGLE, GEOM_KERNEL_DOUBLE
  use mNode
  use mMaterial
  use mElementLine
  use mElementMesh
  use mElementCatenary
  use mElementChannel
  use mChannelCalibration, only: calibrateChannels
  use mJsonParser
  use mSignal, only: tSignal, tSignalSlot, newHeidlerSignal, newHeidlerSignalTerms, newDoubleExpSignal, &
                     newPortelaSignal, newSineSignal
  use mTransient, only: transientResponseSources, transientResponseSignals, tTransientOptions
  use mResultsWriter, only: writeResultsCsv, writeResultsJson, &
                             writeTransientResultsCsv, writeTransientResultsJson, &
                             writeTransientSignalsCsv, writeTransientSignalsJson, &
                             writeObservationCsv, writeObservationJson
  use mError, only: raiseError
  use mVerbosity
  implicit none
  private

  public :: loadStudy, runFromFile, runStudyFromFile, validateStudyReferences

contains

  ! =====================================================================
  ! Segmentation helpers (ROADMAP Phase 10 item 3)
  ! =====================================================================

  function nodeDistance(study, idA, idB) result(d)
    !! Chord length between two already-loaded boundary nodes (m); 0 when
    !! either is unknown (the element's own reference check reports that).
    type(tStudy), intent(in) :: study
    character(len=*), intent(in) :: idA, idB
    real(8) :: d
    integer :: ia, ib

    d = 0.0d0
    ia = study%structure%findNodeIndex(trim(idA))
    ib = study%structure%findNodeIndex(trim(idB))
    if (ia == 0 .or. ib == 0) return
    d = norm2(study%structure%nodes(ia)%p - study%structure%nodes(ib)%p)
  end function nodeDistance

  integer function readSegments(elemObj, maxLen, length) result(n)
    !! Segment count of one element: the explicit `segments` field (default
    !! 1 when absent), raised to ceil(length / maxLen) when the study carries
    !! a `numerics.maxSegmentLength` target — the target only ever refines.
    type(tJsonValue), pointer, intent(in) :: elemObj
    real(8), intent(in) :: maxLen
    !! Study segment-length target (m); 0 = none
    real(8), intent(in) :: length
    !! Element length (m)

    n = 1
    if (json_has(elemObj, "segments")) n = json_int(elemObj, "segments")
    if (maxLen > 0.0d0 .and. length > 0.0d0) n = max(n, ceiling(length / maxLen - 1.0d-9))
  end function readSegments

  subroutine readPiecewise(obj, key, p, ok)
    !! Read a profile along the channel: a number (uniform) or an array of
    !! `{upTo, value}` pieces in ascending `upTo` (the last piece extends
    !! beyond its break). `p` stays unallocated when `key` is absent.
    type(tJsonValue), pointer, intent(in) :: obj
    character(len=*), intent(in) :: key
    type(tPiecewise), intent(out) :: p
    logical, intent(out) :: ok
    type(tJsonValue), pointer :: child, item
    integer :: i, n

    ok = .true.
    if (.not. json_has(obj, key)) return
    child => json_child(obj, key)
    if (json_value_type(child) == JSON_ARRAY) then
      n = json_size(child)
      if (n < 1) then
        call raiseError("mTupa: channel '" // key // "' profile must hold at least one piece")
        ok = .false.
        return
      end if
      allocate(p%upTo(n), p%value(n))
      do i = 1, n
        item => json_item(child, i)
        p%upTo(i)  = json_real(item, "upTo")
        p%value(i) = json_real(item, "value")
        if (i > 1) then
          if (p%upTo(i) <= p%upTo(i - 1)) then
            call raiseError("mTupa: channel '" // key // "' profile breaks must be ascending")
            ok = .false.
            return
          end if
        end if
      end do
    else
      allocate(p%upTo(1), p%value(1))
      p%upTo(1)  = huge(1.0d0)
      p%value(1) = json_value_real(child)
    end if
  end subroutine readPiecewise

  ! =====================================================================
  ! JSON parsing and study loading
  ! =====================================================================

  subroutine loadStudy(filename, study, sourceNodeIds, sourceCurrents, sourceIsVoltage, freqHz, &
                        outputNodeIds, outputElectrodeIds, outputQuantities, &
                        signal, signalSourceNode, signalObserveNodeIds, signalObserveElectrodeIds, &
                        signalNyquistHz, signalFftPoints, signalFreqZeroHz, signalAntialiasStart, &
                        signalSources, signalSourceNodeIds, signalOptions, sourceReturnNodeIds, signalNames)
    !! Parse a JSON study file and populate all fields of a tStudy object.
    !!
    !! Performs the following steps:
    !! 1. Call `parseJsonFile()` to read and parse the JSON file into a tree
    !! 2. Extract study title from the "title" field
    !! 3. Parse "soil" object to define the soil medium
    !! 4. Parse "nodes" array to create boundary nodes
    !! 5. Parse "materials" array (if present) to define conductor materials
    !! 6. Parse "elements" array to create geometric elements (line segments, catenaries, etc.)
    !! 7. If present (ADR 0013, ROADMAP Phase 5), parse the optional
    !!    "sources"/"frequencies"/"outputs" blocks into the corresponding
    !!    optional output arguments; a structure-only case file (no such
    !!    blocks) leaves them unallocated.
    !! 8. If present (ADR 0015), parse the optional "signal" block into the
    !!    corresponding optional output arguments — including the ROADMAP
    !!    Phase 9 fields (ADR 0015 amendment 2026-09-30): `sources[]`
    !!    (several injections, each with its own waveform), `window`,
    !!    `transferFunction` ("interpolated" reuses the case's
    !!    `frequencies` axis as the scan grid) and `transform`/`nltDamping`.
    !!
    !! After this call, `study%structure` is fully populated and ready for assembly.
    !! Call `study%structure%assembleStructure()` to discretise elements into nodes
    !! and electrodes.
    character(len=*), intent(in)  :: filename
    !! Path to the JSON study file to parse
    type(tStudy),     intent(out) :: study
    !! Output study object (all fields populated)
    character(len=256), allocatable, intent(out), optional :: sourceNodeIds(:)
    !! Node IDs from the "sources" block (ADR 0013), one per injection
    complex(8), allocatable, intent(out), optional :: sourceCurrents(:)
    !! Source values corresponding to `sourceNodeIds`: injected current (A),
    !! or source voltage (V) where `sourceIsVoltage` is true (ADR 0016)
    logical, allocatable, intent(out), optional :: sourceIsVoltage(:)
    !! True where the source object carries a "voltage" field instead of
    !! "current" (ADR 0016); allocated together with `sourceNodeIds`
    real(8), allocatable, intent(out), optional :: freqHz(:)
    !! Log-spaced frequency axis (Hz) built from the "frequencies" block
    !! (`min`/`max`/`pointsPerDecade`, ADR 0013)
    character(len=256), allocatable, intent(out), optional :: outputNodeIds(:)
    !! Node IDs from "outputs.nodes" (ADR 0013); unallocated means "all nodes"
    character(len=256), allocatable, intent(out), optional :: outputElectrodeIds(:)
    !! Electrode IDs from "outputs.electrodes"; unallocated means "all electrodes"
    character(len=256), allocatable, intent(out), optional :: outputQuantities(:)
    !! Quantity names from "outputs.quantities"; unallocated means "all quantities"
    class(tSignal), allocatable, intent(out), optional :: signal
    !! Excitation waveform from the "signal" block (ADR 0015); unallocated
    !! means the case file has no transient run to perform
    character(len=256), intent(out), optional :: signalSourceNode
    !! "signal.sourceNode" — node receiving the excitation current
    character(len=256), allocatable, intent(out), optional :: signalObserveNodeIds(:)
    !! "signal.observeNodes" — node(s) whose v(t) is computed
    character(len=256), allocatable, intent(out), optional :: signalObserveElectrodeIds(:)
    !! "signal.observeElectrodes" (optional in the JSON); unallocated means
    !! no electrode current is computed for this run
    real(8), intent(out), optional :: signalNyquistHz
    !! "signal.nyquistHz" — spectrum upper bound (Hz)
    integer, intent(out), optional :: signalFftPoints
    !! "signal.fftPoints" — number of time/FFT samples (power of two)
    real(8), intent(out), optional :: signalFreqZeroHz
    !! "signal.freqZeroHz", default 1.0e-6 if absent from the JSON
    real(8), intent(out), optional :: signalAntialiasStart
    !! "signal.antialiasStart" (ADR 0021), in (0, 1]; default 1.0 (no
    !! anti-aliasing filter) if absent from the JSON
    type(tSignalSlot), allocatable, intent(out), optional :: signalSources(:)
    !! Every injection's waveform: "signal.sources[]", or the single
    !! top-level waveform. `signal` holds a copy of the first entry.
    character(len=256), allocatable, intent(out), optional :: signalSourceNodeIds(:)
    !! Injection node per `signalSources` entry ("sources[].node", or
    !! "sourceNode"); `signalSourceNode` holds the first
    character(len=64), allocatable, intent(out), optional :: signalNames(:)
    !! "signal.signals[].name" (ADR 0026): allocated only when the case lists
    !! independent signals; `signalSources`/`signalSourceNodeIds` then hold one
    !! entry per signal and each drives its own response set. Unallocated for
    !! the single-waveform and `sources` (superposition) forms.
    type(tTransientOptions), intent(out), optional :: signalOptions
    !! Phase 9 transient options, including `antialiasStart` and, for
    !! "transferFunction": "interpolated", the scan axis
    character(len=256), allocatable, intent(out), optional :: sourceReturnNodeIds(:)
    !! "sources[].returnNode" (ADR 0025), one per injection, blank where the
    !! source has none; allocated together with `sourceNodeIds`. Transient
    !! sources carry theirs in `signalSources(:)%returnNode`.

    type(tJsonValue), target  :: root
    !! Root of the parsed JSON tree (must be TARGET for child pointers)
    type(tJsonValue), pointer :: soil_obj, nodes_arr, mats_arr, elems_arr
    !! Pointers to major JSON objects
    type(tJsonValue), pointer :: node_obj, mat_obj, elem_obj, pos_arr, pos_item
    !! Pointers to individual JSON objects and array items
    type(tJsonValue), pointer :: sources_arr, src_obj, current_obj
    type(tJsonValue), pointer :: freq_obj, outputs_obj, strArr
    !! Pointers for the sources/frequencies/outputs blocks (ADR 0013)
    type(tJsonValue), pointer :: signal_obj, numerics_obj
    !! Pointer for the "signal" block (ADR 0015)
    class(tMaterial), allocatable :: mat
    !! Temporary material object for adding to structure
    class(tElement),  allocatable :: elem
    !! Temporary element object for adding to structure
    integer :: i, n, nseg, rowsX, rowsY
    !! Loop indices, segment count, and "mesh" row/column counts
    character(len=256) :: id, from_id, to_id, mat_id, elem_type
    !! String fields from JSON: identifiers and element type
    real(8) :: x, y, z, radius, sigma, epsr, mur_val, lengthX, lengthY, segLen, sag
    integer :: idxA, idxB
    !! Geometric and material parameters

    call parseJsonFile(filename, root)

    study%title = json_str(root, "title")

    soil_obj => json_child(root, "soil")
    if (json_has(soil_obj, "type")) then
      elem_type = json_str(soil_obj, "type")
    else
      elem_type = "linear"
    end if
    select case (trim(elem_type))
    case ("linear")
      study%structure%soil = newMaterialLinear("soil", &
        json_real(soil_obj, "permittivity"), &
        json_real(soil_obj, "permeability"), &
        json_real(soil_obj, "conductivity"))
    case ("portela")
      study%structure%soil = newMaterialPortela("soil", &
        json_real(soil_obj, "permeability"), &
        json_real(soil_obj, "sigma0"), &
        json_real(soil_obj, "alpha0"), &
        json_real(soil_obj, "kr"))
    case ("alipio-visacro")
      study%structure%soil = newMaterialVisacroAlipio("soil", &
        json_real(soil_obj, "permeability"), &
        json_real(soil_obj, "sigma0"))
    case default
      call raiseError("mTupa: unknown soil.type '" // trim(elem_type) // &
                       "' (expected linear, portela or alipio-visacro)")
      return
    end select

    ! Optional numerics block (ADR 0024, ROADMAP Phase 10): kernel, image
    ! model and the segment-length target. Parsed before the elements, whose
    ! segment counts the target can raise.
    if (json_has(root, "numerics")) then
      numerics_obj => json_child(root, "numerics")
      if (json_has(numerics_obj, "kernel")) then
        elem_type = json_str(numerics_obj, "kernel")
        select case (trim(elem_type))
        case ("single")
          study%geometryKernel = GEOM_KERNEL_SINGLE
        case ("double")
          study%geometryKernel = GEOM_KERNEL_DOUBLE
        case default
          call raiseError("mTupa: unknown numerics.kernel '" // trim(elem_type) // "' (expected single or double)")
          return
        end select
      end if
      if (json_has(numerics_obj, "imageModel")) then
        elem_type = json_str(numerics_obj, "imageModel")
        select case (trim(elem_type))
        case ("frequency-dependent")
          study%imageModel = IMAGE_FREQ_DEPENDENT
        case ("ideal")
          study%imageModel = IMAGE_IDEAL
        case default
          call raiseError("mTupa: unknown numerics.imageModel '" // trim(elem_type) // &
                          "' (expected frequency-dependent or ideal)")
          return
        end select
      end if
      if (json_has(numerics_obj, "maxSegmentLength")) then
        study%maxSegmentLength = json_real(numerics_obj, "maxSegmentLength")
        if (study%maxSegmentLength <= 0.0d0) then
          call raiseError("mTupa: numerics.maxSegmentLength must be positive")
          return
        end if
      end if
    end if

    if (json_has(root, "nodes")) then
      nodes_arr => json_child(root, "nodes")
      n = json_size(nodes_arr)
      do i = 1, n
        node_obj => json_item(nodes_arr, i)
        id       = json_str(node_obj, "id")
        pos_arr  => json_child(node_obj, "position")
        pos_item => json_item(pos_arr, 1); x = json_value_real(pos_item)
        pos_item => json_item(pos_arr, 2); y = json_value_real(pos_item)
        pos_item => json_item(pos_arr, 3); z = json_value_real(pos_item)
        call study%structure%addNode(newNode(trim(id), [x, y, z]))
      end do
    end if

    if (json_has(root, "materials")) then
      mats_arr => json_child(root, "materials")
      n = json_size(mats_arr)
      do i = 1, n
        mat_obj => json_item(mats_arr, i)
        id      = json_str(mat_obj, "id")
        epsr    = json_real(mat_obj, "epsilonr")
        mur_val = json_real(mat_obj, "mur")
        sigma   = json_real(mat_obj, "sigma")
        mat = newMaterialLinear(trim(id), epsr, mur_val, sigma)
        call study%structure%addMaterial(mat)
      end do
    end if

    if (json_has(root, "elements")) then
      elems_arr => json_child(root, "elements")
      n = json_size(elems_arr)
      do i = 1, n
        elem_obj  => json_item(elems_arr, i)
        elem_type = json_str(elem_obj, "type")
        select case (trim(elem_type))
        case ("line")
          id      = json_str(elem_obj, "id")
          from_id = json_str(elem_obj, "from")
          to_id   = json_str(elem_obj, "to")
          radius  = json_real(elem_obj, "radius")
          nseg    = readSegments(elem_obj, study%maxSegmentLength, nodeDistance(study, from_id, to_id))
          mat_id  = json_str(elem_obj, "material")
          elem = newElementLine(trim(id), trim(from_id), trim(to_id), &
                                radius, nseg, trim(mat_id))
          call study%structure%addElement(elem)
        case ("catenary")
          id      = json_str(elem_obj, "id")
          from_id = json_str(elem_obj, "from")
          to_id   = json_str(elem_obj, "to")
          radius  = json_real(elem_obj, "radius")
          sag     = json_real(elem_obj, "sag")
          segLen  = nodeDistance(study, from_id, to_id)
          ! parabolic arc length: c + 8 s² / (3 c) (theory.md §4.4)
          if (segLen > 0.0d0) segLen = segLen + 8.0d0 * sag * sag / (3.0d0 * segLen)
          nseg    = readSegments(elem_obj, study%maxSegmentLength, segLen)
          mat_id  = json_str(elem_obj, "material")
          elem = newElementCatenary(trim(id), trim(from_id), trim(to_id), &
                                    sag, radius, nseg, trim(mat_id))
          call study%structure%addElement(elem)
        case ("channel")
          block
            class(tElement), allocatable :: chElem
            type(tPiecewise) :: spd, res
            real(8), allocatable :: brk(:)
            real(8) :: chLen, chFirst, chGrowth, chMax, incDeg, azDeg
            logical :: okProfile
            integer :: nUniform, kBrk

            id      = json_str(elem_obj, "id")
            from_id = json_str(elem_obj, "strike")
            radius  = json_real(elem_obj, "radius")
            chLen   = json_real(elem_obj, "length")
            if ((len_trim(from_id) == 0) .eqv. (.not. json_has(elem_obj, "position"))) then
              call raiseError("mTupa: channel '" // trim(id) // "' needs exactly one of strike (node) and position")
              return
            end if
            incDeg  = 0.0d0
            azDeg   = 0.0d0
            if (json_has(elem_obj, "incidence")) incDeg = json_real(elem_obj, "incidence")
            if (json_has(elem_obj, "azimuth"))   azDeg  = json_real(elem_obj, "azimuth")
            if (chLen <= 0.0d0 .or. radius <= 0.0d0) then
              call raiseError("mTupa: channel '" // trim(id) // "' needs positive length and radius")
              return
            end if
            if (json_has(elem_obj, "speed") .and. json_has(elem_obj, "inductance")) then
              call raiseError("mTupa: channel '" // trim(id) // "': give either speed or inductance, not both")
              return
            end if
            call readPiecewise(elem_obj, "speed", spd, okProfile)
            if (.not. okProfile) return
            call readPiecewise(elem_obj, "resistance", res, okProfile)
            if (.not. okProfile) return

            if (json_has(elem_obj, "segments")) then
              ! Uniform chain; the study target can only refine it
              nUniform = readSegments(elem_obj, study%maxSegmentLength, chLen)
              allocate(brk(nUniform + 1))
              do kBrk = 0, nUniform
                brk(kBrk + 1) = chLen * real(kBrk, kind=8) / real(nUniform, kind=8)
              end do
            else
              chMax = huge(1.0d0)
              if (json_has(elem_obj, "maxSegment")) chMax = json_real(elem_obj, "maxSegment")
              if (study%maxSegmentLength > 0.0d0) chMax = min(chMax, study%maxSegmentLength)
              if (chMax >= huge(1.0d0) .or. chMax <= 0.0d0) then
                call raiseError("mTupa: channel '" // trim(id) // &
                                "' needs segments, maxSegment or numerics.maxSegmentLength")
                return
              end if
              chFirst = chMax
              if (json_has(elem_obj, "firstSegment")) chFirst = json_real(elem_obj, "firstSegment")
              chGrowth = 1.0d0
              if (json_has(elem_obj, "growth")) chGrowth = json_real(elem_obj, "growth")
              if (chFirst <= 0.0d0 .or. chGrowth < 1.0d0) then
                call raiseError("mTupa: channel '" // trim(id) // "' needs firstSegment > 0 and growth >= 1")
                return
              end if
              brk = gradedBreaks(chLen, chFirst, chGrowth, chMax)
            end if

            chElem = newElementChannel(trim(id), trim(from_id), chLen, incDeg, azDeg, radius, brk)
            select type (c => chElem)
            type is (tChannel)
              if (json_has(elem_obj, "position")) then
                pos_arr  => json_child(elem_obj, "position")
                pos_item => json_item(pos_arr, 1); c%footPosition(1) = json_value_real(pos_item)
                pos_item => json_item(pos_arr, 2); c%footPosition(2) = json_value_real(pos_item)
                pos_item => json_item(pos_arr, 3); c%footPosition(3) = json_value_real(pos_item)
              end if
              c%speed      = spd
              c%resistance = res
              if (json_has(elem_obj, "inductance")) then
                c%hasInductance = .true.
                c%inductance    = json_real(elem_obj, "inductance")
              end if
              if (json_getbool(elem_obj, "calibrate")) then
                if (.not. allocated(c%speed%value)) then
                  call raiseError("mTupa: channel '" // trim(id) // "': calibrate needs a target speed")
                  return
                end if
                c%wantCalibration = .true.
              end if
            end select
            call study%structure%addElement(chElem)
          end block
        case ("mesh")
          id       = json_str(elem_obj, "id")
          pos_arr  => json_child(elem_obj, "position")
          pos_item => json_item(pos_arr, 1); x = json_value_real(pos_item)
          pos_item => json_item(pos_arr, 2); y = json_value_real(pos_item)
          pos_item => json_item(pos_arr, 3); z = json_value_real(pos_item)
          lengthX  = json_real(elem_obj, "lengthX")
          lengthY  = json_real(elem_obj, "lengthY")
          rowsX    = json_int(elem_obj, "rowsX")
          rowsY    = json_int(elem_obj, "rowsY")
          radius   = json_real(elem_obj, "radius")
          ! One count serves every bar, so the longest bar sets the target
          nseg     = readSegments(elem_obj, study%maxSegmentLength, &
                                  max(lengthX / real(max(rowsX, 1), kind=8), lengthY / real(max(rowsY, 1), kind=8)))
          mat_id   = json_str(elem_obj, "material")
          elem = newElementMesh(trim(id), [x, y, z], lengthX, lengthY, rowsX, rowsY, &
                                radius, nseg, trim(mat_id))
          call study%structure%addElement(elem)
        case default
          print *, "mTupa: unknown element type '", trim(elem_type), "' — skipped"
        end select
      end do
    end if

    ! Channels that asked for it are calibrated now, once their elements and
    ! the study's segment-length target are known (ADR 0025)
    call calibrateChannels(study)

    ! ------------------------------------------------------------------
    ! Optional sources / frequencies / outputs blocks (ADR 0013,
    ! ROADMAP Phase 5). Each is independently optional; the corresponding
    ! output argument is left unallocated when its block, or the caller's
    ! interest in it, is absent.
    ! ------------------------------------------------------------------

    if (present(sourceNodeIds) .and. present(sourceCurrents) .and. json_has(root, "sources")) then
      sources_arr => json_child(root, "sources")
      n = json_size(sources_arr)
      allocate(sourceNodeIds(n), sourceCurrents(n))
      if (present(sourceReturnNodeIds)) allocate(sourceReturnNodeIds(n))
      if (present(sourceIsVoltage)) then
        allocate(sourceIsVoltage(n))
        sourceIsVoltage = .false.
      end if
      do i = 1, n
        src_obj     => json_item(sources_arr, i)
        sourceNodeIds(i) = json_str(src_obj, "node")
        if (present(sourceReturnNodeIds)) sourceReturnNodeIds(i) = json_str(src_obj, "returnNode")
        ! A source carries either "current" (A) or "voltage" (V) — ADR
        ! 0013/0016. "voltage" wins if both are present (a malformed case);
        ! neither present defaults to a zero current injection.
        current_obj => json_child(src_obj, "voltage")
        if (associated(current_obj)) then
          sourceCurrents(i) = cmplx(json_real(current_obj, "re"), json_real(current_obj, "im"), kind=8)
          if (present(sourceIsVoltage)) sourceIsVoltage(i) = .true.
        else
          current_obj => json_child(src_obj, "current")
          if (associated(current_obj)) then
            sourceCurrents(i) = cmplx(json_real(current_obj, "re"), json_real(current_obj, "im"), kind=8)
          else
            sourceCurrents(i) = cmplx(0.0d0, 0.0d0, kind=8)
          end if
        end if
      end do
    end if

    if (present(freqHz) .and. json_has(root, "frequencies")) then
      freq_obj => json_child(root, "frequencies")
      freqHz = readFrequencyAxis(freq_obj)
    end if

    if (json_has(root, "outputs")) then
      outputs_obj => json_child(root, "outputs")
      if (present(outputNodeIds) .and. json_has(outputs_obj, "nodes")) then
        strArr => json_child(outputs_obj, "nodes")
        call readJsonStringArray(strArr, outputNodeIds)
      end if
      if (present(outputElectrodeIds) .and. json_has(outputs_obj, "electrodes")) then
        strArr => json_child(outputs_obj, "electrodes")
        call readJsonStringArray(strArr, outputElectrodeIds)
      end if
      if (present(outputQuantities) .and. json_has(outputs_obj, "quantities")) then
        strArr => json_child(outputs_obj, "quantities")
        call readJsonStringArray(strArr, outputQuantities)
      end if
    end if

    if (json_has(root, "observation")) call parseObservation(json_child(root, "observation"), study%observation)

    ! ------------------------------------------------------------------
    ! Optional signal block (ADR 0015): time-domain excitation, independent
    ! of sources/frequencies (a case may carry either, both, or neither).
    ! ------------------------------------------------------------------

    if ((present(signal) .or. present(signalSources)) .and. json_has(root, "signal")) then
      signal_obj => json_child(root, "signal")
      block
        type(tSignalSlot), allocatable :: slots(:)
        character(len=256), allocatable :: srcNodes(:)
        character(len=64), allocatable :: names(:)
        type(tJsonValue), pointer :: srcArr, srcObj, winObj
        type(tTransientOptions) :: opts
        integer :: nSrc, iSrc, iPrev
        character(len=256) :: str

        if (json_has(signal_obj, "signals")) then
          ! A list of independent signals, one response set each (ADR 0026)
          if (json_has(signal_obj, "sources") .or. json_has(signal_obj, "waveform")) then
            call raiseError("mTupa: signal.signals cannot be combined with signal.sources/waveform")
            return
          end if
          srcArr => json_child(signal_obj, "signals")
          nSrc = json_size(srcArr)
          if (nSrc < 1) then
            call raiseError("mTupa: signal.signals must hold at least one signal")
            return
          end if
          allocate(slots(nSrc), srcNodes(nSrc), names(nSrc))
          do iSrc = 1, nSrc
            srcObj => json_item(srcArr, iSrc)
            if (json_has(srcObj, "node")) then
              srcNodes(iSrc) = json_str(srcObj, "node")
            else if (json_has(signal_obj, "sourceNode")) then
              srcNodes(iSrc) = json_str(signal_obj, "sourceNode")
            else
              write(str, '(I0)') iSrc
              call raiseError("mTupa: signal.signals[" // trim(str) // "] has no node and signal.sourceNode is absent")
              return
            end if
            call parseSignalWaveform(srcObj, slots(iSrc)%sig)
            if (.not. allocated(slots(iSrc)%sig)) return
            ! Terminal defaults come from the block, entries override
            if (json_has(signal_obj, "returnNode") .or. json_has(signal_obj, "quantity")) then
              call parseSlotTerminals(signal_obj, slots(iSrc))
              if (.not. allocated(slots(iSrc)%sig)) return
            end if
            call parseSlotTerminals(srcObj, slots(iSrc))
            if (.not. allocated(slots(iSrc)%sig)) return
            if (json_has(srcObj, "name")) then
              str = json_str(srcObj, "name")
              if (len_trim(str) > len(names(iSrc))) then
                call raiseError("mTupa: signal.signals[].name '" // trim(str) // "' is longer than 64 characters")
                return
              end if
              names(iSrc) = trim(str)
            else
              write(names(iSrc), '("signal",I0)') iSrc
            end if
            if (len_trim(names(iSrc)) == 0 .or. scan(names(iSrc), ',"\' // char(10) // char(13)) > 0) then
              call raiseError("mTupa: signal.signals[].name '" // trim(names(iSrc)) // &
                              "' must be nonblank and free of commas, quotes and backslashes")
              return
            end if
            do iPrev = 1, iSrc - 1
              if (trim(names(iPrev)) == trim(names(iSrc))) then
                call raiseError("mTupa: duplicate signal.signals[].name '" // trim(names(iSrc)) // "'")
                return
              end if
            end do
          end do
        else if (json_has(signal_obj, "sources")) then
          ! Several simultaneous injections (ROADMAP Phase 9 item 4)
          if (json_has(signal_obj, "sourceNode") .or. json_has(signal_obj, "waveform")) then
            call raiseError("mTupa: signal.sources cannot be combined with signal.sourceNode/waveform")
            return
          end if
          srcArr => json_child(signal_obj, "sources")
          nSrc = json_size(srcArr)
          if (nSrc < 1) then
            call raiseError("mTupa: signal.sources must hold at least one source")
            return
          end if
          allocate(slots(nSrc), srcNodes(nSrc))
          do iSrc = 1, nSrc
            srcObj => json_item(srcArr, iSrc)
            srcNodes(iSrc) = json_str(srcObj, "node")
            call parseSignalWaveform(srcObj, slots(iSrc)%sig)
            if (.not. allocated(slots(iSrc)%sig)) return
            call parseSlotTerminals(srcObj, slots(iSrc))
            if (.not. allocated(slots(iSrc)%sig)) return
          end do
        else
          allocate(slots(1), srcNodes(1))
          srcNodes(1) = json_str(signal_obj, "sourceNode")
          call parseSignalWaveform(signal_obj, slots(1)%sig)
          if (.not. allocated(slots(1)%sig)) return
          call parseSlotTerminals(signal_obj, slots(1))
          if (.not. allocated(slots(1)%sig)) return
        end if

        ! Options (ADR 0021, ADR 0015 amendment 2026-09-30)
        if (json_has(signal_obj, "antialiasStart")) then
          opts%antialiasStart = json_real(signal_obj, "antialiasStart")
          if (opts%antialiasStart <= 0.0d0 .or. opts%antialiasStart > 1.0d0) then
            call raiseError("mTupa: signal.antialiasStart must be in (0, 1]")
            return
          end if
        end if
        if (json_has(signal_obj, "window")) then
          winObj => json_child(signal_obj, "window")
          ! Validate the full strings before storing them in the short
          ! option fields, so a long value cannot truncate into a valid one
          str = json_str(winObj, "type")
          select case (trim(str))
          case ("none", "hann")
            opts%window = trim(str)
          case default
            call raiseError("mTupa: unknown signal.window.type '" // trim(str) // "' (expected none or hann)")
            return
          end select
          if (json_has(winObj, "placement")) then
            str = json_str(winObj, "placement")
            select case (trim(str))
            case ("spectral", "time")
              opts%windowPlacement = trim(str)
            case default
              call raiseError("mTupa: unknown signal.window.placement '" // trim(str) // &
                              "' (expected spectral or time)")
              return
            end select
          end if
        end if
        if (json_has(signal_obj, "transform")) then
          str = json_str(signal_obj, "transform")
          select case (trim(str))
          case ("fft", "nlt")
            opts%transform = trim(str)
          case default
            call raiseError("mTupa: unknown signal.transform '" // trim(str) // "' (expected fft or nlt)")
            return
          end select
        end if
        if (json_has(signal_obj, "nltDamping")) then
          opts%nltDamping = json_real(signal_obj, "nltDamping")
          if (opts%nltDamping <= 0.0d0) then
            call raiseError("mTupa: signal.nltDamping must be > 0")
            return
          end if
          if (trim(opts%transform) /= "nlt") then
            call raiseError("mTupa: signal.nltDamping requires signal.transform = ""nlt""")
            return
          end if
        end if
        if (json_has(signal_obj, "transferFunction")) then
          str = json_str(signal_obj, "transferFunction")
          select case (trim(str))
          case ("full")
          case ("interpolated")
            opts%transferFunction = "interpolated"
            if (trim(opts%transform) == "nlt") then
              call raiseError("mTupa: signal.transform ""nlt"" cannot be combined with " // &
                              "signal.transferFunction ""interpolated""")
              return
            end if
            if (.not. json_has(root, "frequencies")) then
              call raiseError("mTupa: signal.transferFunction ""interpolated"" needs a ""frequencies"" block " // &
                              "(the scan grid)")
              return
            end if
            opts%scanFreqHz = readFrequencyAxis(json_child(root, "frequencies"))
          case default
            call raiseError("mTupa: unknown signal.transferFunction '" // trim(str) // &
                            "' (expected full or interpolated)")
            return
          end select
        end if

        if (present(signal)) allocate(signal, source=slots(1)%sig)
        if (present(signalSourceNode)) signalSourceNode = srcNodes(1)
        if (present(signalSourceNodeIds)) signalSourceNodeIds = srcNodes
        if (present(signalObserveNodeIds)) then
          strArr => json_child(signal_obj, "observeNodes")
          call readJsonStringArray(strArr, signalObserveNodeIds)
        end if
        if (present(signalObserveElectrodeIds) .and. json_has(signal_obj, "observeElectrodes")) then
          strArr => json_child(signal_obj, "observeElectrodes")
          call readJsonStringArray(strArr, signalObserveElectrodeIds)
        end if
        if (present(signalNyquistHz)) signalNyquistHz = json_real(signal_obj, "nyquistHz")
        if (present(signalFftPoints)) signalFftPoints = json_int(signal_obj, "fftPoints")
        if (present(signalFreqZeroHz)) then
          if (json_has(signal_obj, "freqZeroHz")) then
            signalFreqZeroHz = json_real(signal_obj, "freqZeroHz")
          else
            signalFreqZeroHz = 1.0d-6
          end if
        end if
        if (present(signalAntialiasStart)) signalAntialiasStart = opts%antialiasStart

        ! The loader rejects a scan axis it would have to extrapolate
        ! (ROADMAP Phase 9 item 1): it must span [freqZeroHz, nyquistHz].
        if (trim(opts%transferFunction) == "interpolated") then
          block
            real(8) :: fz, fn
            integer :: ns
            fz = 1.0d-6
            if (json_has(signal_obj, "freqZeroHz")) fz = json_real(signal_obj, "freqZeroHz")
            fn = json_real(signal_obj, "nyquistHz")
            ns = size(opts%scanFreqHz)
            if (opts%scanFreqHz(1) > fz * (1.0d0 + 1.0d-9) .or. opts%scanFreqHz(ns) < fn * (1.0d0 - 1.0d-9)) then
              write(str, '(ES10.3," .. ",ES10.3," Hz")') fz, fn
              call raiseError("mTupa: signal.transferFunction ""interpolated"": the frequencies axis must span " // &
                              "[freqZeroHz, nyquistHz] = " // trim(str) // " (no extrapolation)")
              return
            end if
          end block
        end if

        if (present(signalSources)) call move_alloc(slots, signalSources)
        if (present(signalNames) .and. allocated(names)) call move_alloc(names, signalNames)
        if (present(signalOptions)) signalOptions = opts
      end block
    end if
  end subroutine loadStudy

  subroutine readVec3(parent, key, v, ok)
    !! Read a JSON `[x, y, z]` array member `key` of `parent` into `v`;
    !! `ok` is false (and `v` zero) if it is absent or not three numbers.
    type(tJsonValue), intent(in), target :: parent
    character(len=*), intent(in) :: key
    real(8), intent(out) :: v(3)
    logical, intent(out) :: ok
    type(tJsonValue), pointer :: arr, item
    integer :: i

    v = 0.0d0
    ok = .false.
    if (.not. json_has(parent, key)) return
    arr => json_child(parent, key)
    if (json_size(arr) /= 3) return
    do i = 1, 3
      item => json_item(arr, i)
      v(i) = json_value_real(item)
    end do
    ok = .true.
  end subroutine readVec3

  subroutine parseObservation(obs_obj, obs)
    !! Parse the optional `"observation"` block (ADR 0027, theory.md §3.1):
    !! `points[]` (`id`, `position`), `grid` (`id`, `origin` [x, y], `z`,
    !! `lengthX`, `lengthY`, `nx`, `ny`, optional `step` {`length`,
    !! `directions`}), `touch[]` (`id`, `node`, `radius`, `points`, `z`) and
    !! `steps[]` (`id`, `from`, `to`).
    type(tJsonValue), intent(in), target :: obs_obj
    type(tObservation), intent(out) :: obs
    type(tJsonValue), pointer :: arr, item, grid_obj, step_obj, org
    integer :: i, n
    logical :: ok

    if (json_has(obs_obj, "points")) then
      arr => json_child(obs_obj, "points")
      n = json_size(arr)
      allocate(obs%points(n))
      do i = 1, n
        item => json_item(arr, i)
        obs%points(i)%id = json_str(item, "id")
        call readVec3(item, "position", obs%points(i)%p, ok)
        if (len_trim(obs%points(i)%id) == 0 .or. .not. ok) then
          call raiseError("mTupa: observation.points[] needs an id and a 3-component position")
          return
        end if
      end do
    end if

    if (json_has(obs_obj, "grid")) then
      grid_obj => json_child(obs_obj, "grid")
      obs%hasGrid = .true.
      if (json_has(grid_obj, "id")) obs%grid%id = json_str(grid_obj, "id")
      if (json_has(grid_obj, "origin")) then
        org => json_child(grid_obj, "origin")
        if (json_size(org) < 2) then
          call raiseError("mTupa: observation.grid.origin needs [x, y]")
          return
        end if
        item => json_item(org, 1); obs%grid%origin(1) = json_value_real(item)
        item => json_item(org, 2); obs%grid%origin(2) = json_value_real(item)
      end if
      obs%grid%z       = json_real(grid_obj, "z")
      obs%grid%lengthX = json_real(grid_obj, "lengthX")
      obs%grid%lengthY = json_real(grid_obj, "lengthY")
      obs%grid%nx      = json_int(grid_obj, "nx")
      obs%grid%ny      = json_int(grid_obj, "ny")
      if (obs%grid%nx < 1 .or. obs%grid%ny < 1) then
        call raiseError("mTupa: observation.grid needs nx >= 1 and ny >= 1")
        return
      end if
      if (json_has(grid_obj, "step")) then
        obs%grid%step = .true.
        step_obj => json_child(grid_obj, "step")
        if (json_has(step_obj, "length")) obs%grid%stepLength = json_real(step_obj, "length")
        if (json_has(step_obj, "directions")) obs%grid%stepDirections = json_int(step_obj, "directions")
        if (obs%grid%stepLength <= 0.0d0 .or. obs%grid%stepDirections < 1) then
          call raiseError("mTupa: observation.grid.step needs length > 0 and directions >= 1")
          return
        end if
      end if
    end if

    if (json_has(obs_obj, "touch")) then
      arr => json_child(obs_obj, "touch")
      n = json_size(arr)
      allocate(obs%touch(n))
      do i = 1, n
        item => json_item(arr, i)
        obs%touch(i)%node = json_str(item, "node")
        obs%touch(i)%id   = json_str(item, "id")
        if (len_trim(obs%touch(i)%id) == 0) obs%touch(i)%id = obs%touch(i)%node
        if (json_has(item, "radius")) obs%touch(i)%radius = json_real(item, "radius")
        if (json_has(item, "points")) obs%touch(i)%nPoints = json_int(item, "points")
        obs%touch(i)%z = json_real(item, "z")
        if (len_trim(obs%touch(i)%node) == 0 .or. obs%touch(i)%radius <= 0.0d0 .or. obs%touch(i)%nPoints < 3) then
          call raiseError("mTupa: observation.touch[] needs a node, radius > 0 and at least 3 points")
          return
        end if
      end do
    end if

    if (json_has(obs_obj, "steps")) then
      arr => json_child(obs_obj, "steps")
      n = json_size(arr)
      allocate(obs%steps(n))
      do i = 1, n
        item => json_item(arr, i)
        obs%steps(i)%id = json_str(item, "id")
        call readVec3(item, "from", obs%steps(i)%from, ok)
        if (ok) call readVec3(item, "to", obs%steps(i)%to, ok)
        if (len_trim(obs%steps(i)%id) == 0 .or. .not. ok) then
          call raiseError("mTupa: observation.steps[] needs an id and 3-component from/to")
          return
        end if
      end do
    end if
  end subroutine parseObservation

  function readFrequencyAxis(freq_obj) result(freqHz)
    !! Log-spaced axis of a "frequencies" block (`min`/`max`/
    !! `pointsPerDecade`, ADR 0013): nPoints = round(ppd·log10(max/min)) + 1.
    type(tJsonValue), pointer, intent(in) :: freq_obj
    real(8), allocatable :: freqHz(:)
    real(8) :: fMin, fMax, pointsPerDecade
    integer :: nPoints

    fMin = json_real(freq_obj, "min")
    fMax = json_real(freq_obj, "max")
    pointsPerDecade = json_real(freq_obj, "pointsPerDecade")
    nPoints = nint(pointsPerDecade * log10(fMax / fMin)) + 1
    freqHz = logFrequencyAxis(fMin, fMax, max(2, nPoints))
  end function readFrequencyAxis

  subroutine parseSlotTerminals(obj, slot)
    !! Read the two-node-source fields of a transient source (ADR 0025):
    !! optional "returnNode" and "quantity" ("current", the default, or
    !! "voltage": the waveform is then a source voltage in V across the node
    !! pair). Deallocates `slot%sig` on error.
    type(tJsonValue), pointer, intent(in) :: obj
    type(tSignalSlot), intent(inout) :: slot
    character(len=256) :: str

    if (json_has(obj, "returnNode")) slot%returnNode = json_str(obj, "returnNode")
    if (json_has(obj, "quantity")) then
      str = json_str(obj, "quantity")
      select case (trim(str))
      case ("current")
        slot%isVoltage = .false.
      case ("voltage")
        slot%isVoltage = .true.
      case default
        call raiseError("mTupa: unknown signal quantity '" // trim(str) // "' (expected current or voltage)")
        deallocate(slot%sig)
      end select
    end if
  end subroutine parseSlotTerminals

  subroutine parseSignalWaveform(obj, sig)
    !! Build one excitation waveform from a JSON object carrying
    !! "waveform" and its fields (ADR 0015 and amendments): the "signal"
    !! block itself, or one "signal.sources[]" entry (ROADMAP Phase 9
    !! item 4). `sig` stays unallocated on error.
    type(tJsonValue), pointer, intent(in) :: obj
    class(tSignal), allocatable, intent(out) :: sig
    character(len=256) :: waveformType, front
    real(8) :: imax, phaseDeg
    type(tJsonValue), pointer :: terms_arr, term_obj
    real(8), allocatable :: hI0(:), hN(:), hTau1(:), hTau2(:)
    integer :: nTerms, iTerm

    waveformType = json_str(obj, "waveform")
    imax = json_real(obj, "imax")
    select case (trim(waveformType))
    case ("heidler")
      if (json_has(obj, "terms")) then
        ! Standard parametrised Heidler (Heidler 1985 / IEC 62305-1,
        ! ADR 0015 amendment): one {i0, n, tau1, tau2} object per term.
        ! "imax" is optional here — absent means physical amplitudes
        ! (no peak rescale).
        terms_arr => json_child(obj, "terms")
        nTerms = json_size(terms_arr)
        allocate(hI0(nTerms), hN(nTerms), hTau1(nTerms), hTau2(nTerms))
        do iTerm = 1, nTerms
          term_obj => json_item(terms_arr, iTerm)
          hI0(iTerm)   = json_real(term_obj, "i0")
          hN(iTerm)    = json_real(term_obj, "n")
          hTau1(iTerm) = json_real(term_obj, "tau1")
          hTau2(iTerm) = json_real(term_obj, "tau2")
        end do
        if (json_has(obj, "imax")) then
          allocate(sig, source=newHeidlerSignalTerms(hI0, hN, hTau1, hTau2, imax=imax))
        else
          allocate(sig, source=newHeidlerSignalTerms(hI0, hN, hTau1, hTau2))
        end if
      else
        ! Legacy fixed 6-term set, peak-rescaled to the required imax.
        allocate(sig, source=newHeidlerSignal(imax))
      end if
    case ("doubleExp")
      front = json_str(obj, "front")
      allocate(sig, source=newDoubleExpSignal(imax, trim(front), jones=json_getbool(obj, "jones")))
    case ("portela")
      allocate(sig, source=newPortelaSignal(imax, json_real(obj, "alpha"), &
        json_real(obj, "tFront"), json_real(obj, "tTopEnd"), json_real(obj, "tTailEnd")))
    case ("sine")
      phaseDeg = 0.0d0
      if (json_has(obj, "phaseDeg")) phaseDeg = json_real(obj, "phaseDeg")
      allocate(sig, source=newSineSignal(imax, json_real(obj, "frequencyHz"), phaseDeg))
    case default
      call raiseError("mTupa: unknown signal.waveform '" // trim(waveformType) // &
                       "' (expected heidler, doubleExp, portela or sine)")
    end select
  end subroutine parseSignalWaveform

  subroutine readJsonStringArray(arr, out)
    !! Read a JSON array of strings into an allocatable character array
    !! (used for "outputs.nodes"/"electrodes"/"quantities", ADR 0013).
    type(tJsonValue), intent(in), target :: arr
    !! JSON_ARRAY of JSON_STRING values
    character(len=256), allocatable, intent(out) :: out(:)
    type(tJsonValue), pointer :: item
    integer :: i, n

    n = json_size(arr)
    allocate(out(n))
    do i = 1, n
      out(i) = ''
      item => json_item(arr, i)
      if (associated(item)) then
        if (json_value_type(item) == JSON_STRING) out(i) = json_value_str(item)
      end if
    end do
  end subroutine readJsonStringArray

  ! =====================================================================
  ! Upfront ID cross-reference validation
  ! =====================================================================

  subroutine validateStudyReferences(study, sourceNodeIds, signal, signalSourceNode, &
                                      signalObserveNodeIds, signalObserveElectrodeIds, &
                                      outputNodeIds, outputElectrodeIds, signalSourceNodeIds, &
                                      sourceReturnNodeIds, signalReturnNodeIds)
    !! Resolve every ID a case file references — `sources[].node`,
    !! `signal.sourceNode`/`observeNodes`/`observeElectrodes`,
    !! `outputs.nodes`/`electrodes` — against the assembled structure,
    !! right after (cheap) `assembleStructure` and before any
    !! geometry-factor or solve work runs. Without this, a bad ID is
    !! caught only deep inside `runSweep`/`transientResponse` (after the
    !! O(n^2) geometry-factor quadrature and a full frequency sweep have
    !! already run), or — for `outputs.nodes`/`outputs.electrodes` — not
    !! caught at all: `mResultsWriter`'s `wanted()` filter silently
    !! excludes an unmatched ID rather than erroring.
    !!
    !! Idempotent to call before `runSweep`/`transientResponse`:
    !! `assembleStructure` itself is now idempotent (`mStructure`), so the
    !! later, lazy assembly inside `tStudy%prepareStudy` stays a no-op.
    class(tStudy), intent(inout) :: study
    !! Study whose (not-yet-assembled) structure will be validated against
    character(len=*), intent(in), optional :: sourceNodeIds(:)
    !! "sources[].node" (ADR 0013)
    class(tSignal), allocatable, intent(in), optional :: signal
    !! The parsed "signal" block (ADR 0015); when absent/unallocated,
    !! `signalSourceNode`/`signalObserveNodeIds`/`signalObserveElectrodeIds`
    !! are not meaningful and are skipped regardless of whether they're
    !! present as arguments
    character(len=*), intent(in), optional :: signalSourceNode
    !! "signal.sourceNode"
    character(len=*), intent(in), optional :: signalObserveNodeIds(:)
    !! "signal.observeNodes"
    character(len=*), intent(in), optional :: signalObserveElectrodeIds(:)
    !! "signal.observeElectrodes"
    character(len=*), intent(in), optional :: outputNodeIds(:)
    !! "outputs.nodes"
    character(len=*), intent(in), optional :: outputElectrodeIds(:)
    !! "outputs.electrodes"
    character(len=*), intent(in), optional :: signalSourceNodeIds(:)
    !! "signal.sources[].node" (ROADMAP Phase 9 item 4), or the single
    !! "signal.sourceNode"
    character(len=*), intent(in), optional :: sourceReturnNodeIds(:)
    !! "sources[].returnNode" (ADR 0025); blank entries are skipped
    character(len=*), intent(in), optional :: signalReturnNodeIds(:)
    !! "signal.sources[].returnNode" (ADR 0025); blank entries are skipped
    integer :: i

    call study%structure%assembleStructure()

    if (present(sourceNodeIds)) then
      do i = 1, size(sourceNodeIds)
        call requireNodeReference(study, trim(sourceNodeIds(i)), "sources[].node")
      end do
    end if

    if (present(sourceReturnNodeIds)) then
      do i = 1, size(sourceReturnNodeIds)
        if (len_trim(sourceReturnNodeIds(i)) > 0) &
          call requireNodeReference(study, trim(sourceReturnNodeIds(i)), "sources[].returnNode")
      end do
    end if

    if (present(signalReturnNodeIds)) then
      do i = 1, size(signalReturnNodeIds)
        if (len_trim(signalReturnNodeIds(i)) > 0) &
          call requireNodeReference(study, trim(signalReturnNodeIds(i)), "signal.sources[].returnNode")
      end do
    end if

    if (present(signal)) then
      if (allocated(signal)) then
        if (present(signalSourceNode)) &
          call requireNodeReference(study, trim(signalSourceNode), "signal.sourceNode")
        if (present(signalSourceNodeIds)) then
          do i = 1, size(signalSourceNodeIds)
            call requireNodeReference(study, trim(signalSourceNodeIds(i)), "signal.sources[].node")
          end do
        end if
        if (present(signalObserveNodeIds)) then
          do i = 1, size(signalObserveNodeIds)
            call requireNodeReference(study, trim(signalObserveNodeIds(i)), "signal.observeNodes")
          end do
        end if
        if (present(signalObserveElectrodeIds)) then
          do i = 1, size(signalObserveElectrodeIds)
            call requireElectrodeReference(study, trim(signalObserveElectrodeIds(i)), &
                                            "signal.observeElectrodes")
          end do
        end if
      end if
    end if

    if (present(outputNodeIds)) then
      do i = 1, size(outputNodeIds)
        call requireNodeReference(study, trim(outputNodeIds(i)), "outputs.nodes")
      end do
    end if

    if (present(outputElectrodeIds)) then
      do i = 1, size(outputElectrodeIds)
        call requireElectrodeReference(study, trim(outputElectrodeIds(i)), "outputs.electrodes")
      end do
    end if

    if (allocated(study%observation%touch)) then
      do i = 1, size(study%observation%touch)
        call requireNodeReference(study, trim(study%observation%touch(i)%node), "observation.touch[].node")
      end do
    end if
  end subroutine validateStudyReferences

  subroutine requireNodeReference(study, nodeId, fieldName)
    !! Raise a clear, greppable error if `nodeId` (from JSON field
    !! `fieldName`) does not name a node in `study%structure`.
    class(tStudy), intent(in) :: study
    character(len=*), intent(in) :: nodeId, fieldName

    if (study%structure%findNodeIndex(nodeId) == 0) then
      call raiseError("mTupa: " // trim(fieldName) // " references unknown node '" // &
                       trim(nodeId) // "'")
    end if
  end subroutine requireNodeReference

  subroutine requireElectrodeReference(study, electrodeId, fieldName)
    !! Raise a clear, greppable error if `electrodeId` (from JSON field
    !! `fieldName`) does not name a discretised electrode segment in
    !! `study%structure` — the common mistake is naming the input element
    !! ID instead of the generated segment ID (`common/README.md`'s
    !! discretised-ID gotcha).
    class(tStudy), intent(in) :: study
    character(len=*), intent(in) :: electrodeId, fieldName

    if (study%structure%findElectrodeIndex(electrodeId) == 0) then
      call raiseError("mTupa: " // trim(fieldName) // " references unknown electrode '" // &
        trim(electrodeId) // "' (discretised segment IDs look like '<element id>_e<n>', " // &
        "not the input element/boundary-node ID — see common/README.md)")
    end if
  end subroutine requireElectrodeReference

  ! =====================================================================
  ! Convenience entry point
  ! =====================================================================

  subroutine runFromFile(filename)
    !! CLI entry point (`app/main.f90`): load a JSON case file, run it end
    !! to end, and report.
    !!
    !! Always discretises the structure (`assembleStructure`, directly for
    !! a structure-only case or via `runSweep`/`transientResponse` ->
    !! `prepareStudy` when either runs) before `study%report()`, so the
    !! printed element list shows real electrode segment IDs instead of
    !! "None" (report() before assembly cannot see them — the elements
    !! haven't been split into segments yet). `sources`/`frequencies`
    !! (ADR 0013) and `signal` (ADR 0015) are independent: either, both, or
    !! neither may be present. Each that is writes its own results
    !! (`<basename>_results.csv/.json` for the sweep,
    !! `<basename>_transient_results.csv/.json` for the transient run,
    !! `mResultsWriter`) to the current directory, honouring an `outputs`
    !! selection if present. The sweep's results are written before
    !! `transientResponse` runs, since `transientResponse` calls
    !! `study%runSweep` internally (its own unit-current, FFT-sample
    !! frequency axis) and would otherwise overwrite the harmonic sweep's
    !! stored results first. A structure-only case (like
    !! `buried_conductor_short.json`) stops after the summary — there is
    !! nothing to solve.
    character(len=*), intent(in) :: filename
    !! Path to the JSON study file
    type(tStudy) :: study
    !! Local study object (created, reported, then destroyed)
    character(len=256), allocatable :: sourceNodeIds(:), sourceReturnNodeIds(:)
    character(len=256), allocatable :: outputNodeIds(:), outputElectrodeIds(:), outputQuantities(:)
    complex(8), allocatable :: sourceCurrents(:)
    logical, allocatable :: sourceIsVoltage(:)
    real(8), allocatable :: freqHz(:)
    class(tSignal), allocatable :: signal
    character(len=256) :: signalSourceNode
    character(len=256), allocatable :: signalObserveNodeIds(:), signalObserveElectrodeIds(:)
    real(8) :: signalNyquistHz, signalFreqZeroHz, signalAntialiasStart
    integer :: signalFftPoints
    type(tSignalSlot), allocatable :: signalSources(:)
    character(len=256), allocatable :: signalSourceNodeIds(:)
    type(tTransientOptions) :: signalOptions
    character(len=256), allocatable :: signalReturnNodeIds(:)
    character(len=64), allocatable :: signalNames(:)
    integer :: i
    real(8), allocatable :: t(:), injectedCurrents(:,:), nodeResponses(:,:), i1Responses(:,:), i2Responses(:,:)
    real(8), allocatable :: nodeResp3(:,:,:), i1Resp3(:,:,:), i2Resp3(:,:,:)
    logical :: ranSweep, ranTransient
    character(len=512) :: base, csvFile, jsonFile
    integer(8) :: clockStart, clockEnd, clockRate

    call system_clock(count=clockStart, count_rate=clockRate)

    if (verbosityLevel() .eq. VERB_VERBOSE) then
      print *, ""
      print *, "Loading study ", trim(filename)
    end if
    call loadStudy(filename, study, sourceNodeIds=sourceNodeIds, &
                   sourceCurrents=sourceCurrents, sourceIsVoltage=sourceIsVoltage, freqHz=freqHz, &
                   outputNodeIds=outputNodeIds, outputElectrodeIds=outputElectrodeIds, &
                   outputQuantities=outputQuantities, &
                   signal=signal, signalSourceNode=signalSourceNode, &
                   signalObserveNodeIds=signalObserveNodeIds, &
                   signalObserveElectrodeIds=signalObserveElectrodeIds, &
                   signalNyquistHz=signalNyquistHz, signalFftPoints=signalFftPoints, &
                   signalFreqZeroHz=signalFreqZeroHz, signalAntialiasStart=signalAntialiasStart, &
                   signalSources=signalSources, signalSourceNodeIds=signalSourceNodeIds, &
                   signalOptions=signalOptions, sourceReturnNodeIds=sourceReturnNodeIds, &
                   signalNames=signalNames)

    if (allocated(signalSources)) then
      allocate(signalReturnNodeIds(size(signalSources)))
      do i = 1, size(signalSources)
        signalReturnNodeIds(i) = signalSources(i)%returnNode
      end do
    end if

    call validateStudyReferences(study, sourceNodeIds=sourceNodeIds, signal=signal, &
                                  signalObserveNodeIds=signalObserveNodeIds, &
                                  signalObserveElectrodeIds=signalObserveElectrodeIds, &
                                  outputNodeIds=outputNodeIds, outputElectrodeIds=outputElectrodeIds, &
                                  signalSourceNodeIds=signalSourceNodeIds, &
                                  sourceReturnNodeIds=sourceReturnNodeIds, signalReturnNodeIds=signalReturnNodeIds)

    ranSweep     = allocated(sourceNodeIds) .and. allocated(freqHz)
    ranTransient = allocated(signal)
    base = basenameNoExt(filename)

    if (ranSweep) then
      call study%runSweep(freqHz, sourceNodeIds, sourceCurrents, sourceIsVoltage=sourceIsVoltage, &
                          returnNodeIds=sourceReturnNodeIds)
      call study%report()

      csvFile  = trim(base) // "_results.csv"
      jsonFile = trim(base) // "_results.json"
      call writeResultsCsv(study, trim(csvFile), nodeIds=outputNodeIds, &
                            electrodeIds=outputElectrodeIds, quantities=outputQuantities)
      call writeResultsJson(study, trim(jsonFile), nodeIds=outputNodeIds, &
                             electrodeIds=outputElectrodeIds, quantities=outputQuantities)
      call verbose(VERB_NORMAL, "Wrote " // trim(csvFile) // " and " // trim(jsonFile))

      if (.not. study%observation%isEmpty()) then
        call computeObservations(study)
        csvFile  = trim(base) // "_potentials.csv"
        jsonFile = trim(base) // "_potentials.json"
        call writeObservationCsv(study, trim(csvFile))
        call writeObservationJson(study, trim(jsonFile))
        call verbose(VERB_NORMAL, "Wrote " // trim(csvFile) // " and " // trim(jsonFile))
      end if
    else if (.not. study%observation%isEmpty()) then
      call raiseError("tupa: the observation block needs a harmonic sweep (sources + frequencies); " // &
                      "transient observation is not supported yet (ADR 0027)")
      return
    end if

    if (ranTransient .and. allocated(signalNames)) then
      if (size(signalNames) < 2) deallocate(signalNames)   ! one signal: the ADR 0015 shape
    end if

    if (ranTransient .and. allocated(signalNames)) then
      ! A list of independent signals sharing one transfer function (ADR 0026)
      if (allocated(signalObserveElectrodeIds)) then
        call transientResponseSignals(study, signalSources, signalSourceNodeIds, signalObserveNodeIds, &
          signalNyquistHz, signalFftPoints, signalFreqZeroHz, t, injectedCurrents, nodeResp3, &
          observeElectrodeIds=signalObserveElectrodeIds, i1Responses=i1Resp3, i2Responses=i2Resp3, &
          options=signalOptions, independent=.true.)
      else
        call transientResponseSignals(study, signalSources, signalSourceNodeIds, signalObserveNodeIds, &
          signalNyquistHz, signalFftPoints, signalFreqZeroHz, t, injectedCurrents, nodeResp3, &
          options=signalOptions, independent=.true.)
      end if
      if (.not. ranSweep) call study%report()

      csvFile  = trim(base) // "_transient_results.csv"
      jsonFile = trim(base) // "_transient_results.json"
      call writeTransientSignalsCsv(signalNames, signalSourceNodeIds, t, injectedCurrents, &
        signalObserveNodeIds, nodeResp3, trim(csvFile), &
        observeElectrodeIds=signalObserveElectrodeIds, i1Responses=i1Resp3, i2Responses=i2Resp3)
      call writeTransientSignalsJson(study%title, signalNames, signalSourceNodeIds, t, injectedCurrents, &
        signalObserveNodeIds, nodeResp3, trim(jsonFile), &
        observeElectrodeIds=signalObserveElectrodeIds, i1Responses=i1Resp3, i2Responses=i2Resp3, &
        study=study)
      call verbose(VERB_NORMAL, "Wrote " // trim(csvFile) // " and " // trim(jsonFile))
    else if (ranTransient) then
      if (allocated(signalObserveElectrodeIds)) then
        call transientResponseSources(study, signalSources, signalSourceNodeIds, signalObserveNodeIds, &
          signalNyquistHz, signalFftPoints, signalFreqZeroHz, t, injectedCurrents, nodeResponses, &
          observeElectrodeIds=signalObserveElectrodeIds, i1Responses=i1Responses, i2Responses=i2Responses, &
          options=signalOptions)
      else
        call transientResponseSources(study, signalSources, signalSourceNodeIds, signalObserveNodeIds, &
          signalNyquistHz, signalFftPoints, signalFreqZeroHz, t, injectedCurrents, nodeResponses, &
          options=signalOptions)
      end if
      if (.not. ranSweep) call study%report()

      csvFile  = trim(base) // "_transient_results.csv"
      jsonFile = trim(base) // "_transient_results.json"
      call writeTransientResultsCsv(signalSourceNodeIds, t, injectedCurrents, &
        signalObserveNodeIds, nodeResponses, trim(csvFile), &
        observeElectrodeIds=signalObserveElectrodeIds, i1Responses=i1Responses, i2Responses=i2Responses)
      call writeTransientResultsJson(study%title, signalSourceNodeIds, t, injectedCurrents, &
        signalObserveNodeIds, nodeResponses, trim(jsonFile), &
        observeElectrodeIds=signalObserveElectrodeIds, i1Responses=i1Responses, i2Responses=i2Responses, &
        study=study)
      call verbose(VERB_NORMAL, "Wrote " // trim(csvFile) // " and " // trim(jsonFile))
    end if

    if (.not. (ranSweep .or. ranTransient)) then
      call study%structure%assembleStructure()
      call study%report()
      call verbose(VERB_NORMAL, "(structure-only case: no sources/frequencies/signal block -- nothing to solve)")
    end if

    call system_clock(count=clockEnd)
    call verbose(VERB_NORMAL, "Simulation duration: " // &
        formatDuration(real(clockEnd - clockStart, 8) / real(clockRate, 8)))
  end subroutine runFromFile

  function formatDuration(seconds) result(str)
    !! Render an elapsed wall-clock duration in human-readable form, e.g.
    !! "1 h 12 min 23.3234 s" (hours/minutes omitted when zero, so a sub-
    !! minute run just prints "23.3234 s"). Seconds always keep 4 decimal
    !! digits so short runs (quadrature-only, single-frequency cases) still
    !! show a meaningful duration instead of rounding to "0 s".
    real(8), intent(in) :: seconds
    character(len=:), allocatable :: str
    integer :: hours, minutes
    real(8) :: remainder, secs
    character(len=32) :: buf

    hours     = int(seconds / 3600.0d0)
    remainder = seconds - real(hours, 8) * 3600.0d0
    minutes   = int(remainder / 60.0d0)
    secs      = remainder - real(minutes, 8) * 60.0d0

    ! Guard against F0.4 rounding secs up to "60.0000" right at a minute
    ! boundary (e.g. secs = 59.99997): carry into minutes/hours instead.
    if (nint(secs * 10000.0d0) >= 600000) then
      secs = max(0.0d0, secs - 60.0d0)
      minutes = minutes + 1
      if (minutes >= 60) then
        minutes = minutes - 60
        hours = hours + 1
      end if
    end if

    str = ""
    if (hours > 0) then
      write(buf, '(I0,A)') hours, " h"
      str = str // trim(buf) // " "
    end if
    if (hours > 0 .or. minutes > 0) then
      write(buf, '(I0,A)') minutes, " min"
      str = str // trim(buf) // " "
    end if
    write(buf, '(F0.4,A)') secs, " s"
    if (buf(1:1) == '.') buf = '0' // adjustl(buf)
    str = str // trim(adjustl(buf))
  end function formatDuration

  function basenameNoExt(path) result(base)
    !! Last path component of `path` with its extension stripped, used to
    !! derive `<basename>_results.csv`/`.json` output filenames from the
    !! input case path (e.g. "../common/rod.json" -> "rod").
    character(len=*), intent(in) :: path
    character(len=256) :: base
    integer :: slashPos, dotPos, startPos, endPos

    slashPos = index(path, "/", back=.true.)
    startPos = slashPos + 1
    dotPos   = index(path(startPos:), ".", back=.true.)
    if (dotPos > 0) then
      endPos = startPos + dotPos - 2
    else
      endPos = len_trim(path)
    end if
    base = path(startPos:endPos)
  end function basenameNoExt

  subroutine runStudyFromFile(filename, study)
    !! Load a JSON case file and run its frequency sweep (ROADMAP Phase 5,
    !! ADR 0013): calls `loadStudy` for the `sources`/`frequencies` blocks,
    !! then `study%runSweep`. Both blocks must be present in the case file —
    !! a structure-only file (like `buried_conductor_short.json`/`buried_conductor_long.json`) has
    !! nothing to sweep and raises an error.
    character(len=*), intent(in) :: filename
    !! Path to the JSON study file
    type(tStudy), intent(out) :: study
    !! Output study object, with sweep results populated
    character(len=256), allocatable :: sourceNodeIds(:), sourceReturnNodeIds(:)
    complex(8), allocatable :: sourceCurrents(:)
    logical, allocatable :: sourceIsVoltage(:)
    real(8), allocatable :: freqHz(:)

    call loadStudy(filename, study, sourceNodeIds=sourceNodeIds, &
                   sourceCurrents=sourceCurrents, sourceIsVoltage=sourceIsVoltage, freqHz=freqHz, &
                   sourceReturnNodeIds=sourceReturnNodeIds)

    call validateStudyReferences(study, sourceNodeIds=sourceNodeIds, sourceReturnNodeIds=sourceReturnNodeIds)

    if (.not. (allocated(sourceNodeIds) .and. allocated(freqHz))) then
      call raiseError("runStudyFromFile: '" // trim(filename) // &
        "' has no 'sources'/'frequencies' block to sweep (ADR 0013)")
      return
    end if

    call study%runSweep(freqHz, sourceNodeIds, sourceCurrents, sourceIsVoltage=sourceIsVoltage, &
                        returnNodeIds=sourceReturnNodeIds)
  end subroutine runStudyFromFile

end module tupa
