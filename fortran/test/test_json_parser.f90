program test_json_parser
  !! Tests for mJsonParser's json-fortran migration (ADR 0006, 2026-08-01):
  !! exercises exactly what the old hand-rolled parser could NOT do —
  !! string escape sequences and arrays past the old 64-item cap — to prove
  !! the migration actually lifted those limits, per ADR 0006's own framing
  !! ("these cases double as parser conformance tests").
  use mJsonParser
  use check
  implicit none

  character(len=*), parameter :: fixtureFile = "test_json_parser_fixture.json"
  type(tJsonValue) :: root

  ! =====================================================================
  ! String escape sequences (unsupported by the old hand-rolled parser)
  ! =====================================================================

  call test_init("mJsonParser: string escape sequences")

  block
    integer :: iunit

    open(newunit=iunit, file=fixtureFile, status="replace", action="write")
    write(iunit, '(A)') '{'
    write(iunit, '(A)') '  "title": "a \"quoted\" word, a backslash \\ and a newline\n here",'
    write(iunit, '(A)') '  "tab": "col1\tcol2"'
    write(iunit, '(A)') '}'
    close(iunit)
  end block

  call parseJsonFile(fixtureFile, root)

  call test_ok('embedded escaped quotes decode to a literal " character', &
               index(trim(json_str(root, "title")), '"quoted"') > 0, &
               'expected the escaped \"quoted\" to decode to a literal quoted word')
  call test_ok("embedded backslash escape decodes to a literal backslash", &
               index(trim(json_str(root, "title")), achar(92)) > 0, &
               "expected the escaped backslash to decode to a single backslash character")
  call test_ok("embedded tab escape decodes to an actual tab character", &
               index(trim(json_str(root, "tab")), achar(9)) > 0, &
               "expected \t to decode to an actual tab character")

  ! =====================================================================
  ! Arrays past the old parser's 64-item cap
  ! =====================================================================

  call test_init("mJsonParser: arrays past the old 64-item cap")

  block
    integer :: iunit, i
    integer, parameter :: nItems = 200

    open(newunit=iunit, file=fixtureFile, status="replace", action="write")
    write(iunit, '(A)', advance="no") '{ "values": ['
    do i = 1, nItems
      if (i > 1) write(iunit, '(A)', advance="no") ", "
      write(iunit, '(I0)', advance="no") i * 10
    end do
    write(iunit, '(A)') '] }'
    close(iunit)
  end block

  call parseJsonFile(fixtureFile, root)

  block
    type(tJsonValue), pointer :: valuesArr, item
    integer, parameter :: nItems = 200

    valuesArr => json_child(root, "values")
    call test_ok("array size == 200 (well past the old 64-item cap)", &
                 json_size(valuesArr) == nItems, "expected 200 items")

    item => json_item(valuesArr, 1)
    call test_ok("first item == 10", nint(json_value_real(item)) == 10, "wrong first item")

    item => json_item(valuesArr, 150)
    call test_ok("item 150 == 1500 (past the old cap, mid-array)", &
                 nint(json_value_real(item)) == 1500, "wrong item at index 150")

    item => json_item(valuesArr, nItems)
    call test_ok("last item (200) == 2000", &
                 nint(json_value_real(item)) == 2000, "wrong last item")
  end block

  block
    logical :: exists
    integer :: iunit
    inquire(file=fixtureFile, exist=exists)
    if (exists) then
      open(newunit=iunit, file=fixtureFile, status="old")
      close(iunit, status="delete")
    end if
  end block

  call test_summary()

end program test_json_parser
