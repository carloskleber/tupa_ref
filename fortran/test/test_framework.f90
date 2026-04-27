module check
  implicit none
  private
  public :: test_ok, test_fail, test_init, test_summary, test_close

  integer :: npass = 0
  integer :: nfail = 0
  integer :: ntests = 0

contains

  subroutine test_init(name)
    character(len=*), intent(in) :: name
    print *, "========== ", trim(name), " =========="
    npass = 0
    nfail = 0
    ntests = 0
  end subroutine test_init

  subroutine test_ok(name, condition, msg)
    character(len=*), intent(in) :: name
    logical, intent(in) :: condition
    character(len=*), intent(in), optional :: msg

    ntests = ntests + 1
    if (condition) then
      npass = npass + 1
      print *, "  [PASS] ", trim(name)
    else
      nfail = nfail + 1
      print *, "  [FAIL] ", trim(name)
      if (present(msg)) print *, "         ", trim(msg)
    end if
  end subroutine test_ok

  subroutine test_fail(name, msg)
    character(len=*), intent(in) :: name
    character(len=*), intent(in), optional :: msg

    ntests = ntests + 1
    nfail = nfail + 1
    print *, "  [FAIL] ", trim(name)
    if (present(msg)) print *, "         ", trim(msg)
  end subroutine test_fail

  subroutine test_summary()
    print *, ""
    print *, "Results: ", ntests, " tests, ", npass, " passed, ", nfail, " failed"
    if (nfail == 0) then
      print *, "ALL TESTS PASSED!"
    else
      print *, "SOME TESTS FAILED!"
      stop 1
    end if
  end subroutine test_summary

  subroutine test_close()
    if (nfail > 0) stop 1
  end subroutine test_close

end module check