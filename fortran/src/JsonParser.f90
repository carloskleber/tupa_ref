module mJsonParser
  !! Thin wrapper over json-fortran, presenting the same minimal accessor
  !! API the project used with the old hand-rolled parser (ADR 0006 —
  !! migration to json-fortran once the hand-rolled subset's limits (64
  !! items/container, no string escapes, no real error reporting) became a
  !! real problem). Callers (`fortran/src/Tupa.f90`) talk only to this
  !! accessor API, never to json-fortran's `json_core`/`json_value` types
  !! directly, per ADR 0006's "thin reader interface" requirement.
  !!
  !! **Supported JSON**: the full JSON grammar (objects, arrays, strings
  !! with escapes, numbers, booleans, null), no item-count cap, real
  !! error messages with line/column from json-fortran's parser.
  !!
  !! **Non-reentrant by design, same as the parser it replaces**: a single
  !! module-level `json_core` instance backs every `tJsonValue` in play.
  !! This project only ever has one case file loaded at a time (one
  !! `loadStudy` call per CLI run; sequential, not concurrent, in tests),
  !! so this is a deliberate simplicity trade, not an oversight.
  use json_module, only: json_core, json_value, &
                          jfNull => json_null, jfObject => json_object, &
                          jfArray => json_array, jfLogical => json_logical, &
                          jfInteger => json_integer, jfDouble => json_double, &
                          jfString => json_string
  use mError, only: raiseError
  implicit none
  private

  integer, parameter, public :: JSON_NULL   = 0
  !! Type tag: null value
  integer, parameter, public :: JSON_BOOL   = 1
  !! Type tag: boolean value (true/false)
  integer, parameter, public :: JSON_NUMBER = 2
  !! Type tag: number (integer or floating-point)
  integer, parameter, public :: JSON_STRING = 3
  !! Type tag: string value
  integer, parameter, public :: JSON_ARRAY  = 4
  !! Type tag: array (ordered list)
  integer, parameter, public :: JSON_OBJECT = 5
  !! Type tag: object (key-value pairs)

  integer, parameter :: MAX_KEY_LEN = 256
  !! Length of the fixed-length character functions (`json_str`) return in,
  !! kept from the old parser's API for source compatibility with callers

  type, public :: tJsonValue
    !! Handle to one node of the parsed JSON tree (wraps a json-fortran
    !! `json_value` pointer). Deliberately just a pointer wrapper: unlike
    !! the old hand-rolled parser's recursive derived type, this has no
    !! allocatable components, so intrinsic assignment/copy is safe.
    type(json_value), pointer :: p => null()
  end type tJsonValue

  type(json_core), save :: g_core
  !! Module-level parser/accessor instance backing every tJsonValue
  logical, save :: g_coreInitialised = .false.

  public :: parseJsonFile
  !! Parse a JSON file and populate a tJsonValue tree
  public :: json_child, json_item
  !! Return pointers to child values (by key / by 1-based array index)
  public :: json_size, json_has, json_str, json_real, json_int, json_getbool
  !! Scalar accessor functions, looked up by key on an object
  public :: json_value_type, json_value_real, json_value_str
  !! Scalar accessors on a tJsonValue itself (e.g. one already fetched via
  !! json_item on an array of non-object scalars) rather than by key

contains

  ! =====================================================================
  ! Public entry point
  ! =====================================================================

  subroutine parseJsonFile(filename, v)
    !! Parse a JSON file and return the root value as a tree handle.
    !!
    !! The resulting `v` is typically a JSON_OBJECT at the root. On a
    !! missing file or malformed JSON, routes json-fortran's own
    !! (line/column-aware) error message through `mError%raiseError`.
    character(len=*), intent(in)  :: filename
    !! Path to the JSON file to parse
    type(tJsonValue), intent(out) :: v
    !! Output: handle to the parsed JSON tree (root value)
    logical :: statusOk
    character(:), allocatable :: errMsg

    if (.not. g_coreInitialised) then
      call g_core%initialize()
      g_coreInitialised = .true.
    end if
    call g_core%clear_exceptions()

    call g_core%parse(file=filename, p=v%p)

    if (g_core%failed()) then
      call g_core%check_for_errors(statusOk, errMsg)
      call g_core%clear_exceptions()
      call raiseError("mJsonParser: " // errMsg)
      return
    end if
  end subroutine parseJsonFile

  ! =====================================================================
  ! Pointer-returning accessors
  ! =====================================================================

  function json_child(v, key) result(ptr)
    !! Retrieve a child value from an object by key, returning a pointer.
    !!
    !! Returns a null pointer if the key is not found or if `v` is not an
    !! object (json-fortran itself just reports "not found" in that case).
    type(tJsonValue), intent(in) :: v
    !! Object to search
    character(len=*), intent(in) :: key
    !! Key to look up
    type(tJsonValue), pointer :: ptr
    !! Output: pointer to a freshly wrapped handle for the child value
    !! (null if not found). Each call allocates a small wrapper shell;
    !! see the module docstring on the single-file, non-reentrant scope
    !! this is meant for — bounded and reclaimed at process exit, not
    !! worth a manual arena for case files this small.
    type(json_value), pointer :: child
    logical :: found

    nullify(ptr)
    if (.not. associated(v%p)) return
    call g_core%get_child(v%p, key, child, found)
    if (.not. found) return
    allocate(ptr)
    ptr%p => child
  end function json_child

  function json_item(v, i) result(ptr)
    !! Retrieve a child value from an array by 1-based index, returning a
    !! pointer. Returns a null pointer if `v` is not an array or `i` is
    !! out of bounds.
    type(tJsonValue), intent(in) :: v
    !! Array to index
    integer, intent(in) :: i
    !! 1-based index into the array
    type(tJsonValue), pointer :: ptr
    !! Output: pointer to the child value (null if out of bounds)
    type(json_value), pointer :: child
    integer :: vtype
    logical :: found

    nullify(ptr)
    if (.not. associated(v%p)) return
    call g_core%info(v%p, var_type=vtype)
    if (vtype /= jfArray) return
    if (i < 1 .or. i > g_core%count(v%p)) return
    call g_core%get_child(v%p, i, child, found)
    if (.not. found) return
    allocate(ptr)
    ptr%p => child
  end function json_item

  ! =====================================================================
  ! Scalar accessors, looked up by key on an object
  ! =====================================================================

  function json_has(v, key) result(found)
    !! Check whether an object contains a given key.
    type(tJsonValue), intent(in) :: v
    character(len=*), intent(in) :: key
    logical :: found
    type(json_value), pointer :: child

    found = .false.
    if (.not. associated(v%p)) return
    call g_core%get_child(v%p, key, child, found)
  end function json_has

  function json_size(v) result(n)
    !! Return the number of items in an array or object.
    type(tJsonValue), intent(in) :: v
    integer :: n
    n = 0
    if (associated(v%p)) n = g_core%count(v%p)
  end function json_size

  function json_str(v, key) result(s)
    !! Extract a string value from an object by key.
    !!
    !! Returns an empty string if the key is not found or if the value is
    !! not a string.
    type(tJsonValue), intent(in) :: v
    character(len=*),  intent(in) :: key
    character(len=MAX_KEY_LEN) :: s
    type(json_value), pointer :: child
    character(:), allocatable :: tmp
    integer :: vtype
    logical :: found

    s = ''
    if (.not. associated(v%p)) return
    call g_core%get_child(v%p, key, child, found)
    if (.not. found) return
    call g_core%info(child, var_type=vtype)
    if (vtype == jfString) then
      call g_core%get(child, tmp)
      s = tmp
    end if
  end function json_str

  function json_real(v, key) result(r)
    !! Extract a numeric value from an object by key as real(8).
    !!
    !! Returns 0.0 if the key is not found or if the value is not a
    !! number (integer- and floating-point-looking JSON literals both
    !! count, matching the old parser's "numbers are always real(8)").
    type(tJsonValue), intent(in) :: v
    character(len=*),  intent(in) :: key
    real(8) :: r
    type(json_value), pointer :: child
    integer :: vtype
    logical :: found

    r = 0.0d0
    if (.not. associated(v%p)) return
    call g_core%get_child(v%p, key, child, found)
    if (.not. found) return
    call g_core%info(child, var_type=vtype)
    if (vtype == jfInteger .or. vtype == jfDouble) call g_core%get(child, r)
  end function json_real

  function json_int(v, key) result(n)
    !! Extract a numeric value from an object by key as integer.
    !!
    !! Returns 0 if the key is not found or if the value is not a number.
    type(tJsonValue), intent(in) :: v
    character(len=*),  intent(in) :: key
    integer :: n
    n = int(json_real(v, key))
  end function json_int

  function json_getbool(v, key) result(b)
    !! Extract a boolean value from an object by key.
    !!
    !! Returns .false. if the key is not found or if the value is not a
    !! boolean.
    type(tJsonValue), intent(in) :: v
    character(len=*),  intent(in) :: key
    logical :: b
    type(json_value), pointer :: child
    integer :: vtype
    logical :: found

    b = .false.
    if (.not. associated(v%p)) return
    call g_core%get_child(v%p, key, child, found)
    if (.not. found) return
    call g_core%info(child, var_type=vtype)
    if (vtype == jfLogical) call g_core%get(child, b)
  end function json_getbool

  ! =====================================================================
  ! Scalar accessors on a tJsonValue itself (no key lookup) — for array
  ! elements that are themselves scalars, e.g. a position [x, y, z] triple
  ! fetched one item at a time via json_item.
  ! =====================================================================

  function json_value_type(v) result(t)
    !! The JSON_* type tag of `v` itself.
    type(tJsonValue), intent(in) :: v
    integer :: t
    integer :: vtype

    t = JSON_NULL
    if (.not. associated(v%p)) return
    call g_core%info(v%p, var_type=vtype)
    select case (vtype)
    case (jfNull);              t = JSON_NULL
    case (jfLogical);           t = JSON_BOOL
    case (jfInteger, jfDouble); t = JSON_NUMBER
    case (jfString);            t = JSON_STRING
    case (jfArray);             t = JSON_ARRAY
    case (jfObject);            t = JSON_OBJECT
    end select
  end function json_value_type

  function json_value_real(v) result(r)
    !! The numeric value of `v` itself as real(8); 0.0 if `v` isn't a
    !! number.
    type(tJsonValue), intent(in) :: v
    real(8) :: r
    integer :: vtype

    r = 0.0d0
    if (.not. associated(v%p)) return
    call g_core%info(v%p, var_type=vtype)
    if (vtype == jfInteger .or. vtype == jfDouble) call g_core%get(v%p, r)
  end function json_value_real

  function json_value_str(v) result(s)
    !! The string value of `v` itself; empty if `v` isn't a string.
    type(tJsonValue), intent(in) :: v
    character(:), allocatable :: s
    integer :: vtype

    s = ''
    if (.not. associated(v%p)) return
    call g_core%info(v%p, var_type=vtype)
    if (vtype == jfString) call g_core%get(v%p, s)
  end function json_value_str

end module mJsonParser
