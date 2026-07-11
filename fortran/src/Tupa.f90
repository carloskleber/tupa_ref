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
  !! - `"soil"` (object) — soil properties: `permittivity`, `permeability`, `conductivity`
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
  use mResultsWriter, only: writeResultsCsv, writeResultsJson
  use mError, only: raiseError
  implicit none
  private

  public :: loadStudy, runFromFile, runStudyFromFile

contains

  ! =====================================================================
  ! JSON parsing and study loading
  ! =====================================================================

  subroutine loadStudy(filename, study, sourceNodeIds, sourceCurrents, freqHz, &
                        outputNodeIds, outputElectrodeIds, outputQuantities)
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

    type(tJsonValue), target  :: root
    !! Root of the parsed JSON tree (must be TARGET for child pointers)
    type(tJsonValue), pointer :: soil_obj, nodes_arr, mats_arr, elems_arr
    !! Pointers to major JSON objects
    type(tJsonValue), pointer :: node_obj, mat_obj, elem_obj, pos_arr, pos_item
    !! Pointers to individual JSON objects and array items
    type(tJsonValue), pointer :: sources_arr, src_obj, current_obj
    type(tJsonValue), pointer :: freq_obj, outputs_obj, strArr
    !! Pointers for the sources/frequencies/outputs blocks (ADR 0013)
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
    study%structure%soil = newMaterialLinear("soil", &
      json_real(soil_obj, "permittivity"), &
      json_real(soil_obj, "permeability"), &
      json_real(soil_obj, "conductivity"))

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
    !! a structure-only case or via `runSweep` -> `prepareStudy` when a
    !! sweep runs) before `study%report()`, so the printed element list
    !! shows real electrode segment IDs instead of "None" (report() before
    !! assembly cannot see them — the elements haven't been split into
    !! segments yet). If the case also carries `sources`/`frequencies`
    !! (ADR 0013), additionally runs the sweep and writes
    !! `<basename>_results.csv`/`.json` (`mResultsWriter`) to the current
    !! directory, honouring an `outputs` selection if present. A
    !! structure-only case (like `buried_conductor_short.json`) stops
    !! after the summary — there is nothing to sweep.
    character(len=*), intent(in) :: filename
    !! Path to the JSON study file
    type(tStudy) :: study
    !! Local study object (created, reported, then destroyed)
    character(len=256), allocatable :: sourceNodeIds(:)
    character(len=256), allocatable :: outputNodeIds(:), outputElectrodeIds(:), outputQuantities(:)
    complex(8), allocatable :: sourceCurrents(:)
    real(8), allocatable :: freqHz(:)
    character(len=512) :: base, csvFile, jsonFile

    call loadStudy(filename, study, sourceNodeIds=sourceNodeIds, &
                   sourceCurrents=sourceCurrents, freqHz=freqHz, &
                   outputNodeIds=outputNodeIds, outputElectrodeIds=outputElectrodeIds, &
                   outputQuantities=outputQuantities)

    if (allocated(sourceNodeIds) .and. allocated(freqHz)) then
      call study%runSweep(freqHz, sourceNodeIds, sourceCurrents)
      call study%report()

      base     = basenameNoExt(filename)
      csvFile  = trim(base) // "_results.csv"
      jsonFile = trim(base) // "_results.json"
      call writeResultsCsv(study, trim(csvFile), nodeIds=outputNodeIds, &
                            electrodeIds=outputElectrodeIds, quantities=outputQuantities)
      call writeResultsJson(study, trim(jsonFile), nodeIds=outputNodeIds, &
                             electrodeIds=outputElectrodeIds, quantities=outputQuantities)
      print *, ""
      print *, "Wrote ", trim(csvFile), " and ", trim(jsonFile)
    else
      call study%structure%assembleStructure()
      call study%report()
      print *, ""
      print *, "(structure-only case: no sources/frequencies block -- nothing to sweep)"
    end if
  end subroutine runFromFile

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
