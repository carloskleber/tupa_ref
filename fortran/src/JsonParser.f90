module mJsonParser
  !! Minimal recursive-descent JSON parser for study configuration files.
  !!
  !! **Design Philosophy:**
  !! This parser is hand-written to avoid external dependencies (no json-fortran).
  !! It uses a carefully controlled memory model to prevent double-free crashes with
  !! GFortran's handling of recursive derived types with allocatable components:
  !!
  !! - `tJsonValue` objects are NEVER copied via intrinsic assignment
  !! - `parse_object()` and `parse_array()` pre-allocate items() and write directly into them
  !! - `json_child()` and `json_item()` return pointers, never copies
  !! - Recursive tree ownership: parent owns child items; deallocating parent cascades
  !!
  !! **Supported JSON subset:**
  !! - Objects (key-value pairs)
  !! - Arrays (indexed items)
  !! - Strings (double-quoted; no escape sequences)
  !! - Numbers (integer and floating-point)
  !! - Booleans (true, false)
  !! - Null
  !!
  !! **Limitations:**
  !! - Does not support escape sequences in strings
  !! - Maximum 64 items per object or array (MAX_ITEMS)
  !! - Maximum 256 characters per object key (MAX_KEY_LEN)
  !! - Not reentrant (single global parse buffer)
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
  !! Maximum length of an object key
  integer, parameter :: MAX_ITEMS   = 64
  !! Maximum number of items in an object or array

  type, public :: tJsonValue
    !! A JSON value: scalar (null, bool, number, string) or container (array, object).
    !!
    !! Stores the type tag (vtype) and the value in one of the variant fields:
    !! - bval: boolean (for JSON_BOOL)
    !! - rval: number (for JSON_NUMBER)
    !! - sval: string (for JSON_STRING)
    !! - items: array of nested values (for JSON_ARRAY and JSON_OBJECT)
    !! - keys: parallel array of keys (for JSON_OBJECT only)
    integer :: vtype  = JSON_NULL
    !! Type tag from JSON_NULL, JSON_BOOL, JSON_NUMBER, JSON_STRING, JSON_ARRAY, JSON_OBJECT
    logical :: bval   = .false.
    !! Boolean value (only valid if vtype == JSON_BOOL)
    real(8) :: rval   = 0.0d0
    !! Numeric value (only valid if vtype == JSON_NUMBER)
    character(:), allocatable :: sval
    !! String value (only valid if vtype == JSON_STRING)
    type(tJsonValue), allocatable :: items(:)
    !! Array of child items (valid if vtype == JSON_ARRAY or JSON_OBJECT)
    character(len=MAX_KEY_LEN), allocatable :: keys(:)
    !! Array of keys parallel to items() (valid only if vtype == JSON_OBJECT)
    integer :: nitems = 0
    !! Count of items actually populated in the items() array
  end type tJsonValue

  character(:), allocatable :: g_buf
  !! Module-level: entire file contents as a single string
  integer :: g_pos, g_len
  !! Module-level: current parse position and buffer length

  public :: parseJsonFile
  !! Parse a JSON file and populate a tJsonValue tree
  public :: json_child, json_item
  !! Return pointers to child values (no copy)
  public :: json_size, json_has, json_str, json_real, json_int, json_getbool
  !! Scalar accessor functions

contains

  ! =====================================================================
  ! Public entry point — subroutine to avoid function-result copy
  ! =====================================================================

  subroutine parseJsonFile(filename, v)
    !! Parse a JSON file and return the root value as a tree structure.
    !!
    !! Reads the entire file into memory, then parses it using a recursive-descent
    !! algorithm. The resulting `v` is typically a JSON_OBJECT at the root.
    !!
    !! This is a subroutine (not a function) to avoid GFortran's double-free bug
    !! when returning recursive types with allocatable components.
    character(len=*), intent(in)  :: filename
    !! Path to the JSON file to parse
    type(tJsonValue), intent(out) :: v
    !! Output: the parsed JSON tree (root value)
    integer :: iunit, ios
    character(len=4096) :: line

    open(newunit=iunit, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) then
      call raiseError("mJsonParser: cannot open file '" // trim(filename) // "'")
      return
    end if

    g_buf = ''
    do
      read(iunit, '(A)', iostat=ios) line
      if (ios /= 0) exit
      g_buf = g_buf // trim(line) // ' '
    end do
    close(iunit)

    g_len = len(g_buf)
    g_pos = 1
    call parse_value(v)
  end subroutine parseJsonFile

  ! =====================================================================
  ! Core recursive parser — subroutines with intent(out), no copies
  ! =====================================================================

  recursive subroutine parse_value(v)
    !! Parse one JSON value (null, bool, number, string, array, or object).
    !!
    !! Dispatches to type-specific parsers (parse_object, parse_array, etc.).
    !! Written as a subroutine with intent(out) to write directly into the
    !! caller's tJsonValue, avoiding any intermediate copy.
    type(tJsonValue), intent(out) :: v
    !! Output: the parsed value
    character :: c

    call skip_ws()
    if (g_pos > g_len) return

    c = g_buf(g_pos:g_pos)
    select case (c)
    case ('{')
      call parse_object(v)
    case ('[')
      call parse_array(v)
    case ('"')
      v%vtype = JSON_STRING
      v%sval  = parse_string()
    case ('t')
      v%vtype = JSON_BOOL
      v%bval  = .true.
      g_pos   = g_pos + 4
    case ('f')
      v%vtype = JSON_BOOL
      v%bval  = .false.
      g_pos   = g_pos + 5
    case ('n')
      v%vtype = JSON_NULL
      g_pos   = g_pos + 4
    case default
      if (c == '-' .or. (c >= '0' .and. c <= '9')) then
        v%vtype = JSON_NUMBER
        v%rval  = parse_number()
      end if
    end select
  end subroutine parse_value

  recursive subroutine parse_object(v)
    !! Parse a JSON object (key-value pairs between `{` and `}`).
    !!
    !! Pre-allocates the items() array to MAX_ITEMS slots to allow parse_value()
    !! to write child values directly, avoiding tJsonValue copies. After parsing,
    !! trims keys() to the actual count.
    type(tJsonValue), intent(out) :: v
    !! Output: a JSON_OBJECT value
    character(len=MAX_KEY_LEN) :: tmp_keys(MAX_ITEMS)
    character :: c
    integer :: n
    character(:), allocatable :: key

    v%vtype = JSON_OBJECT
    n = 0
    g_pos = g_pos + 1
    allocate(v%items(MAX_ITEMS))

    do
      call skip_ws()
      if (g_pos > g_len) exit
      c = g_buf(g_pos:g_pos)
      select case (c)
      case ('}')
        g_pos = g_pos + 1
        exit
      case (',')
        g_pos = g_pos + 1
      case ('"')
        key = parse_string()
        call skip_ws()
        if (g_pos <= g_len) then
          if (g_buf(g_pos:g_pos) == ':') g_pos = g_pos + 1
        end if
        call skip_ws()
        n = n + 1
        if (n > MAX_ITEMS) then
          call raiseError("mJsonParser: object exceeds MAX_ITEMS (ADR 0006: switch to json-fortran)")
          return
        end if
        tmp_keys(n) = key
        call parse_value(v%items(n))
      end select
    end do

    v%nitems = n
    allocate(v%keys(n))
    if (n > 0) v%keys(1:n) = tmp_keys(1:n)
  end subroutine parse_object

  recursive subroutine parse_array(v)
    !! Parse a JSON array (values between `[` and `]`).
    !!
    !! Pre-allocates the items() array to MAX_ITEMS slots to allow parse_value()
    !! to write child values directly. After parsing, the actual count is stored
    !! in nitems; excess slots are left allocated but unused.
    type(tJsonValue), intent(out) :: v
    !! Output: a JSON_ARRAY value
    character :: c
    integer :: n

    v%vtype = JSON_ARRAY
    n = 0
    g_pos = g_pos + 1
    allocate(v%items(MAX_ITEMS))

    do
      call skip_ws()
      if (g_pos > g_len) exit
      c = g_buf(g_pos:g_pos)
      select case (c)
      case (']')
        g_pos = g_pos + 1
        exit
      case (',')
        g_pos = g_pos + 1
      case default
        n = n + 1
        if (n > MAX_ITEMS) then
          call raiseError("mJsonParser: array exceeds MAX_ITEMS (ADR 0006: switch to json-fortran)")
          return
        end if
        call parse_value(v%items(n))
      end select
    end do

    v%nitems = n
  end subroutine parse_array

  function parse_string() result(s)
    !! Extract the contents of a JSON string (between `"` delimiters).
    !!
    !! Handles escaped characters by skipping them. Currently does not unescape
    !! them (e.g., `\"` remains as two characters in the output).
    character(:), allocatable :: s
    !! Output: the string content (without quotes)
    integer :: start

    s = ''
    if (g_buf(g_pos:g_pos) /= '"') return
    g_pos = g_pos + 1
    start = g_pos
    do while (g_pos <= g_len)
      if (g_buf(g_pos:g_pos) == achar(92) .and. g_pos + 1 <= g_len) then
        g_pos = g_pos + 2
      else if (g_buf(g_pos:g_pos) == '"') then
        s = g_buf(start:g_pos-1)
        g_pos = g_pos + 1
        return
      else
        g_pos = g_pos + 1
      end if
    end do
  end function parse_string

  function parse_number() result(r)
    !! Extract and convert a JSON number (integer or floating-point).
    !!
    !! Reads a sequence of characters matching a number pattern (`-`, `+`, `0-9`, `.`, `e`, `E`)
    !! and converts to a real(8) via Fortran's internal READ.
    real(8) :: r
    !! Output: the numeric value
    integer :: start, ios

    start = g_pos
    do while (g_pos <= g_len)
      select case (g_buf(g_pos:g_pos))
      case ('-', '+', '0':'9', '.', 'e', 'E')
        g_pos = g_pos + 1
      case default
        exit
      end select
    end do
    read(g_buf(start:g_pos-1), *, iostat=ios) r
    if (ios /= 0) r = 0.0d0
  end function parse_number

  subroutine skip_ws()
    !! Skip whitespace characters (space, tab, newline, carriage return).
    do while (g_pos <= g_len)
      select case (g_buf(g_pos:g_pos))
      case (' ', achar(9), achar(10), achar(13))
        g_pos = g_pos + 1
      case default
        return
      end select
    end do
  end subroutine skip_ws

  ! =====================================================================
  ! Pointer-returning accessors — no tJsonValue copy, no double-free risk
  ! =====================================================================

  function json_child(v, key) result(ptr)
    !! Retrieve a child value from an object by key, returning a pointer.
    !!
    !! Returns a null pointer if the key is not found or if `v` is not an object.
    type(tJsonValue), intent(in), target :: v
    !! Object to search (must be JSON_OBJECT)
    character(len=*), intent(in) :: key
    !! Key to look up
    type(tJsonValue), pointer :: ptr
    !! Output: pointer to the child value (null if not found)
    integer :: idx

    ptr => null()
    idx = json_find_key(v, key)
    if (idx > 0) ptr => v%items(idx)
  end function json_child

  function json_item(v, i) result(ptr)
    !! Retrieve a child value from an array by 1-based index, returning a pointer.
    !!
    !! Returns a null pointer if the index is out of bounds or if `v` is not an array.
    type(tJsonValue), intent(in), target :: v
    !! Array to index (must be JSON_ARRAY)
    integer, intent(in) :: i
    !! 1-based index into the array
    type(tJsonValue), pointer :: ptr
    !! Output: pointer to the child value (null if out of bounds)

    ptr => null()
    if (v%vtype == JSON_ARRAY .and. allocated(v%items) &
        .and. i >= 1 .and. i <= v%nitems) then
      ptr => v%items(i)
    end if
  end function json_item

  ! =====================================================================
  ! Scalar accessors — safe as functions, return copies of scalar values
  ! =====================================================================

  pure function json_find_key(v, key) result(idx)
    !! Find the index of a key in an object, or return 0 if not found.
    type(tJsonValue), intent(in) :: v
    character(len=*),  intent(in) :: key
    integer :: idx, i

    idx = 0
    if (v%vtype /= JSON_OBJECT .or. .not. allocated(v%keys)) return
    do i = 1, v%nitems
      if (trim(v%keys(i)) == trim(key)) then
        idx = i
        return
      end if
    end do
  end function json_find_key

  pure function json_has(v, key) result(found)
    !! Check whether an object contains a given key.
    type(tJsonValue), intent(in) :: v
    character(len=*),  intent(in) :: key
    logical :: found
    found = json_find_key(v, key) > 0
  end function json_has

  pure function json_size(v) result(n)
    !! Return the number of items in an array or object.
    type(tJsonValue), intent(in) :: v
    integer :: n
    n = v%nitems
  end function json_size

  function json_str(v, key) result(s)
    !! Extract a string value from an object by key.
    !!
    !! Returns an empty string if the key is not found or if the value is not a string.
    type(tJsonValue), intent(in), target :: v
    character(len=*),  intent(in) :: key
    character(len=MAX_KEY_LEN) :: s
    type(tJsonValue), pointer :: child

    s = ''
    child => json_child(v, key)
    if (associated(child)) then
      if (child%vtype == JSON_STRING .and. allocated(child%sval)) &
        s = child%sval
    end if
  end function json_str

  function json_real(v, key) result(r)
    !! Extract a numeric value from an object by key as real(8).
    !!
    !! Returns 0.0 if the key is not found or if the value is not a number.
    type(tJsonValue), intent(in), target :: v
    character(len=*),  intent(in) :: key
    real(8) :: r
    type(tJsonValue), pointer :: child

    r = 0.0d0
    child => json_child(v, key)
    if (associated(child)) then
      if (child%vtype == JSON_NUMBER) r = child%rval
    end if
  end function json_real

  function json_int(v, key) result(n)
    !! Extract a numeric value from an object by key as integer.
    !!
    !! Returns 0 if the key is not found or if the value is not a number.
    type(tJsonValue), intent(in), target :: v
    character(len=*),  intent(in) :: key
    integer :: n
    type(tJsonValue), pointer :: child

    n = 0
    child => json_child(v, key)
    if (associated(child)) then
      if (child%vtype == JSON_NUMBER) n = int(child%rval)
    end if
  end function json_int

  function json_getbool(v, key) result(b)
    !! Extract a boolean value from an object by key.
    !!
    !! Returns .false. if the key is not found or if the value is not a boolean.
    type(tJsonValue), intent(in), target :: v
    character(len=*),  intent(in) :: key
    logical :: b
    type(tJsonValue), pointer :: child

    b = .false.
    child => json_child(v, key)
    if (associated(child)) then
      if (child%vtype == JSON_BOOL) b = child%bval
    end if
  end function json_getbool

end module mJsonParser
