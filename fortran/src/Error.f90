module mError
  !! Minimal shared error-reporting entry point using fortran-error-handler (feh).
  !!
  !! Per project convention, solver code must never call `stop`/`error stop`
  !! directly; boundary-validation failures (e.g. an unresolvable ID from
  !! user input) are raised here instead. `raiseError` triggers a critical
  !! `ErrorInstance`, which feh reports and halts execution on.
  !!
  !! Library mode: a program that embeds the solver (a GUI, a language
  !! binding) cannot afford the halt. After `setLibraryMode(.true.)`,
  !! `raiseError` records the message and returns instead — nothing is
  !! printed and nothing halts. The raise sites in the input readers return
  !! right after raising, so those routines (`loadStudy`,
  !! `validateStudyReferences`, `assembleStructure`) unwind to their caller
  !! with the error recorded; the caller then asks `errorRaised()`.
  !!
  !! Protocol of a library caller: `clearError()`, switch the mode on, call
  !! the boundary routine, switch the mode off, read `errorCount()` /
  !! `errorMessage(i)`. After an error the study object is unusable and is
  !! discarded. Library mode is meant for those input boundaries only: the
  !! solve loops have not been audited for continuing after a raise.
  !! Recording is safe from OpenMP threads.
  use ErrorHandlerModule, only: ErrorHandler
  use ErrorInstanceModule, only: ErrorInstance
  implicit none
  private
  public :: raiseError
  public :: setLibraryMode, libraryModeActive, errorRaised, errorCount, errorMessage, clearError
  public :: MAX_STORED_ERRORS

  integer, parameter :: MAX_STORED_ERRORS = 32
  !! Messages kept per run; `errorCount` still counts the ones beyond it
  integer, parameter :: STORED_LEN = 1024

  type(ErrorHandler), save :: handler
  logical, save :: handlerInitialised = .false.

  logical, save :: libraryMode = .false.
  integer, save :: nRaised = 0
  character(len=STORED_LEN), save :: stored(MAX_STORED_ERRORS)

contains

  subroutine raiseError(message)
    !! Trigger a critical, on-the-fly error with the given message — or, in
    !! library mode, record it and return.
    character(len=*), intent(in) :: message

    if (libraryMode) then
      !$omp critical (tupaErrorLog)
      nRaised = nRaised + 1
      if (nRaised <= MAX_STORED_ERRORS) stored(nRaised) = message
      !$omp end critical (tupaErrorLog)
      return
    end if

    if (.not. handlerInitialised) then
      call handler%init()
      handlerInitialised = .true.
    end if
    call handler%trigger(error=ErrorInstance(code=1, message=message, isCritical=.true.))
  end subroutine raiseError

  subroutine setLibraryMode(on)
    !! Switch between halting (the default, the CLI) and recording errors.
    logical, intent(in) :: on

    libraryMode = on
  end subroutine setLibraryMode

  logical function libraryModeActive() result(active)
    active = libraryMode
  end function libraryModeActive

  logical function errorRaised() result(raised)
    !! True when an error has been recorded since the last `clearError`.
    !! Never true in the default mode, where a raise halts.
    raised = nRaised > 0
  end function errorRaised

  integer function errorCount() result(n)
    !! Errors recorded since the last `clearError`, including any beyond
    !! `MAX_STORED_ERRORS` whose text was dropped.
    n = nRaised
  end function errorCount

  function errorMessage(i) result(msg)
    !! Text of the `i`-th recorded error (1 = the first raised, usually the
    !! root cause); empty when `i` is outside the stored range.
    integer, intent(in) :: i
    character(len=:), allocatable :: msg

    if (i >= 1 .and. i <= min(nRaised, MAX_STORED_ERRORS)) then
      msg = trim(stored(i))
    else
      msg = ''
    end if
  end function errorMessage

  subroutine clearError()
    !! Forget every recorded error.
    nRaised = 0
  end subroutine clearError

end module mError
