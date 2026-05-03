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
  implicit none
  private

  public :: loadStudy, runFromFile

contains

  ! =====================================================================
  ! JSON parsing and study loading
  ! =====================================================================

  subroutine loadStudy(filename, study)
    !! Parse a JSON study file and populate all fields of a tStudy object.
    !!
    !! Performs the following steps:
    !! 1. Call `parseJsonFile()` to read and parse the JSON file into a tree
    !! 2. Extract study title from the "title" field
    !! 3. Parse "soil" object to define the soil medium
    !! 4. Parse "nodes" array to create boundary nodes
    !! 5. Parse "materials" array (if present) to define conductor materials
    !! 6. Parse "elements" array to create geometric elements (line segments, catenaries, etc.)
    !!
    !! After this call, `study%structure` is fully populated and ready for assembly.
    !! Call `study%structure%assembleStructure()` to discretise elements into nodes
    !! and electrodes.
    character(len=*), intent(in)  :: filename
    !! Path to the JSON study file to parse
    type(tStudy),     intent(out) :: study
    !! Output study object (all fields populated)

    type(tJsonValue), target  :: root
    !! Root of the parsed JSON tree (must be TARGET for child pointers)
    type(tJsonValue), pointer :: soil_obj, nodes_arr, mats_arr, elems_arr
    !! Pointers to major JSON objects
    type(tJsonValue), pointer :: node_obj, mat_obj, elem_obj, pos_arr, pos_item
    !! Pointers to individual JSON objects and array items
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
  end subroutine loadStudy

  ! =====================================================================
  ! Convenience entry point
  ! =====================================================================

  subroutine runFromFile(filename)
    !! Convenience entry point: load a JSON file, execute the simulation, and print results.
    !!
    !! This is a thin wrapper that:
    !! 1. Calls `loadStudy()` to parse the JSON file
    !! 2. Calls `study%run()` to execute the solver
    !! 3. Calls `study%report()` to print the summary
    !!
    !! Useful for scripting and testing; production code may prefer to call
    !! `loadStudy()` directly for more control.
    character(len=*), intent(in) :: filename
    !! Path to the JSON study file
    type(tStudy) :: study
    !! Local study object (created, executed, reported, then destroyed)

    call loadStudy(filename, study)
    call study%run()
    call study%report()
  end subroutine runFromFile

end module tupa
