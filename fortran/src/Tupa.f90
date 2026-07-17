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
  !!
  !! Currently only tLine elements are supported. Future versions will add tCatenary, tCircumference, tTower.
  use mStudy
  use mNode
  use mMaterial
  use mElementLine
  use mJsonParser
  use mSignal, only: tSignal, newHeidlerSignal, newDoubleExpSignal
  use mTransient, only: transientResponse
  use mResultsWriter, only: writeResultsCsv, writeResultsJson, &
                             writeTransientResultsCsv, writeTransientResultsJson
  use mError, only: raiseError
  use mVerbosity
  implicit none
  private

  public :: loadStudy, runFromFile, runStudyFromFile

contains

  ! =====================================================================
  ! JSON parsing and study loading
  ! =====================================================================

  subroutine loadStudy(filename, study, sourceNodeIds, sourceCurrents, freqHz, &
                        outputNodeIds, outputElectrodeIds, outputQuantities, &
                        signal, signalSourceNode, signalObserveNodeIds, signalObserveElectrodeIds, &
                        signalNyquistHz, signalFftPoints, signalFreqZeroHz)
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
    !!    corresponding optional output arguments.
    !!
    !! After this call, `study%structure` is fully populated and ready for assembly.
    !! Call `study%structure%assembleStructure()` to discretise elements into nodes
    !! and electrodes.
    character(len=*), intent(in)  :: filename
    !! Path to the JSON study file to parse
    type(tStudy),     intent(out) :: study
    !! Output study object (all fields populated)
    character(len=256), allocatable, intent(out), optional :: sourceNodeIds(:)
    !! Node IDs from the "sources" block (ADR 0013), one per current injection
    complex(8), allocatable, intent(out), optional :: sourceCurrents(:)
    !! Complex currents corresponding to `sourceNodeIds` (A)
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

    type(tJsonValue), target  :: root
    !! Root of the parsed JSON tree (must be TARGET for child pointers)
    type(tJsonValue), pointer :: soil_obj, nodes_arr, mats_arr, elems_arr
    !! Pointers to major JSON objects
    type(tJsonValue), pointer :: node_obj, mat_obj, elem_obj, pos_arr, pos_item
    !! Pointers to individual JSON objects and array items
    type(tJsonValue), pointer :: sources_arr, src_obj, current_obj
    type(tJsonValue), pointer :: freq_obj, outputs_obj, strArr
    !! Pointers for the sources/frequencies/outputs blocks (ADR 0013)
    type(tJsonValue), pointer :: signal_obj
    !! Pointer for the "signal" block (ADR 0015)
    class(tMaterial), allocatable :: mat
    !! Temporary material object for adding to structure
    class(tElement),  allocatable :: elem
    !! Temporary element object for adding to structure
    integer :: i, n, nseg
    !! Loop indices and segment count
    character(len=256) :: id, from_id, to_id, mat_id, elem_type
    !! String fields from JSON: identifiers and element type
    real(8) :: x, y, z, radius, sigma, epsr, mur_val
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

    nodes_arr => json_child(root, "nodes")
    n = json_size(nodes_arr)
    do i = 1, n
      node_obj => json_item(nodes_arr, i)
      id       = json_str(node_obj, "id")
      pos_arr  => json_child(node_obj, "position")
      pos_item => json_item(pos_arr, 1); x = pos_item%rval
      pos_item => json_item(pos_arr, 2); y = pos_item%rval
      pos_item => json_item(pos_arr, 3); z = pos_item%rval
      call study%structure%addNode(newNode(trim(id), [x, y, z]))
    end do

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
        nseg    = json_int(elem_obj, "segments")
        mat_id  = json_str(elem_obj, "material")
        elem = newElementLine(trim(id), trim(from_id), trim(to_id), &
                              radius, nseg, trim(mat_id))
        call study%structure%addElement(elem)
      case default
        print *, "mTupa: unknown element type '", trim(elem_type), "' — skipped"
      end select
    end do

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
      do i = 1, n
        src_obj     => json_item(sources_arr, i)
        sourceNodeIds(i) = json_str(src_obj, "node")
        current_obj => json_child(src_obj, "current")
        if (associated(current_obj)) then
          sourceCurrents(i) = cmplx(json_real(current_obj, "re"), json_real(current_obj, "im"), kind=8)
        else
          sourceCurrents(i) = cmplx(0.0d0, 0.0d0, kind=8)
        end if
      end do
    end if

    if (present(freqHz) .and. json_has(root, "frequencies")) then
      freq_obj => json_child(root, "frequencies")
      block
        real(8) :: fMin, fMax, pointsPerDecade
        integer :: nPoints
        fMin = json_real(freq_obj, "min")
        fMax = json_real(freq_obj, "max")
        pointsPerDecade = json_real(freq_obj, "pointsPerDecade")
        ! ADR 0013: nPoints = round(pointsPerDecade * log10(max/min)) + 1
        nPoints = nint(pointsPerDecade * log10(fMax / fMin)) + 1
        freqHz = logFrequencyAxis(fMin, fMax, max(2, nPoints))
      end block
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

    ! ------------------------------------------------------------------
    ! Optional signal block (ADR 0015): time-domain excitation, independent
    ! of sources/frequencies (a case may carry either, both, or neither).
    ! ------------------------------------------------------------------

    if (present(signal) .and. json_has(root, "signal")) then
      signal_obj => json_child(root, "signal")
      block
        character(len=256) :: waveformType, front
        real(8) :: imax

        waveformType = json_str(signal_obj, "waveform")
        imax = json_real(signal_obj, "imax")
        select case (trim(waveformType))
        case ("heidler")
          allocate(signal, source=newHeidlerSignal(imax))
        case ("doubleExp")
          front = json_str(signal_obj, "front")
          allocate(signal, source=newDoubleExpSignal(imax, trim(front), jones=json_getbool(signal_obj, "jones")))
        case default
          call raiseError("mTupa: unknown signal.waveform '" // trim(waveformType) // &
                           "' (expected heidler or doubleExp)")
          return
        end select

        if (present(signalSourceNode)) signalSourceNode = json_str(signal_obj, "sourceNode")
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
      end block
    end if
  end subroutine loadStudy

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
        if (item%vtype == JSON_STRING .and. allocated(item%sval)) out(i) = item%sval
      end if
    end do
  end subroutine readJsonStringArray

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
    character(len=256), allocatable :: sourceNodeIds(:)
    character(len=256), allocatable :: outputNodeIds(:), outputElectrodeIds(:), outputQuantities(:)
    complex(8), allocatable :: sourceCurrents(:)
    real(8), allocatable :: freqHz(:)
    class(tSignal), allocatable :: signal
    character(len=256) :: signalSourceNode
    character(len=256), allocatable :: signalObserveNodeIds(:), signalObserveElectrodeIds(:)
    real(8) :: signalNyquistHz, signalFreqZeroHz
    integer :: signalFftPoints
    real(8), allocatable :: t(:), injectedCurrent(:), nodeResponses(:,:), i1Responses(:,:), i2Responses(:,:)
    logical :: ranSweep, ranTransient
    character(len=512) :: base, csvFile, jsonFile
    integer(8) :: clockStart, clockEnd, clockRate

    call system_clock(count=clockStart, count_rate=clockRate)

    if (verbosityLevel() .eq. VERB_VERBOSE) then
      print *, ""
      print *, "Loading study ", trim(filename)
    end if
    call loadStudy(filename, study, sourceNodeIds=sourceNodeIds, &
                   sourceCurrents=sourceCurrents, freqHz=freqHz, &
                   outputNodeIds=outputNodeIds, outputElectrodeIds=outputElectrodeIds, &
                   outputQuantities=outputQuantities, &
                   signal=signal, signalSourceNode=signalSourceNode, &
                   signalObserveNodeIds=signalObserveNodeIds, &
                   signalObserveElectrodeIds=signalObserveElectrodeIds, &
                   signalNyquistHz=signalNyquistHz, signalFftPoints=signalFftPoints, &
                   signalFreqZeroHz=signalFreqZeroHz)

    ranSweep     = allocated(sourceNodeIds) .and. allocated(freqHz)
    ranTransient = allocated(signal)
    base = basenameNoExt(filename)

    if (ranSweep) then
      call study%runSweep(freqHz, sourceNodeIds, sourceCurrents)
      call study%report()

      csvFile  = trim(base) // "_results.csv"
      jsonFile = trim(base) // "_results.json"
      call writeResultsCsv(study, trim(csvFile), nodeIds=outputNodeIds, &
                            electrodeIds=outputElectrodeIds, quantities=outputQuantities)
      call writeResultsJson(study, trim(jsonFile), nodeIds=outputNodeIds, &
                             electrodeIds=outputElectrodeIds, quantities=outputQuantities)
      if (verbosityLevel() >= VERB_NORMAL) then
        print *, ""
        print *, "Wrote ", trim(csvFile), " and ", trim(jsonFile)
      end if
    end if

    if (ranTransient) then
      if (allocated(signalObserveElectrodeIds)) then
        call transientResponse(study, signal, trim(signalSourceNode), signalObserveNodeIds, &
          signalNyquistHz, signalFftPoints, signalFreqZeroHz, t, injectedCurrent, nodeResponses, &
          observeElectrodeIds=signalObserveElectrodeIds, i1Responses=i1Responses, i2Responses=i2Responses)
      else
        call transientResponse(study, signal, trim(signalSourceNode), signalObserveNodeIds, &
          signalNyquistHz, signalFftPoints, signalFreqZeroHz, t, injectedCurrent, nodeResponses)
      end if
      if (.not. ranSweep) call study%report()

      csvFile  = trim(base) // "_transient_results.csv"
      jsonFile = trim(base) // "_transient_results.json"
      call writeTransientResultsCsv(trim(signalSourceNode), t, injectedCurrent, &
        signalObserveNodeIds, nodeResponses, trim(csvFile), &
        observeElectrodeIds=signalObserveElectrodeIds, i1Responses=i1Responses, i2Responses=i2Responses)
      call writeTransientResultsJson(study%title, trim(signalSourceNode), t, injectedCurrent, &
        signalObserveNodeIds, nodeResponses, trim(jsonFile), &
        observeElectrodeIds=signalObserveElectrodeIds, i1Responses=i1Responses, i2Responses=i2Responses)
      if (verbosityLevel() >= VERB_NORMAL) then
        print *, ""
        print *, "Wrote ", trim(csvFile), " and ", trim(jsonFile)
      end if
    end if

    if (.not. (ranSweep .or. ranTransient)) then
      call study%structure%assembleStructure()
      call study%report()
      if (verbosityLevel() >= VERB_NORMAL) then
        print *, ""
        print *, "(structure-only case: no sources/frequencies/signal block -- nothing to solve)"
      end if
    end if

    call system_clock(count=clockEnd)
    if (verbosityLevel() >= VERB_NORMAL) then
      print *, ""
      print *, "Simulation duration: " // &
        formatDuration(real(clockEnd - clockStart, 8) / real(clockRate, 8))
    end if
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
    character(len=256), allocatable :: sourceNodeIds(:)
    complex(8), allocatable :: sourceCurrents(:)
    real(8), allocatable :: freqHz(:)

    call loadStudy(filename, study, sourceNodeIds=sourceNodeIds, &
                   sourceCurrents=sourceCurrents, freqHz=freqHz)

    if (.not. (allocated(sourceNodeIds) .and. allocated(freqHz))) then
      call raiseError("runStudyFromFile: '" // trim(filename) // &
        "' has no 'sources'/'frequencies' block to sweep (ADR 0013)")
      return
    end if

    call study%runSweep(freqHz, sourceNodeIds, sourceCurrents)
  end subroutine runStudyFromFile

end module tupa
