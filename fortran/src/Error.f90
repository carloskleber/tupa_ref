module mError
  !! Minimal shared error-reporting entry point using fortran-error-handler (feh).
  !!
  !! Per project convention, solver code must never call `stop`/`error stop`
  !! directly; boundary-validation failures (e.g. an unresolvable ID from
  !! user input) are raised here instead. `raiseError` triggers a critical
  !! `ErrorInstance`, which feh reports and halts execution on.
  use ErrorHandlerModule, only: ErrorHandler
  use ErrorInstanceModule, only: ErrorInstance
  implicit none
  private
  public :: raiseError

  type(ErrorHandler), save :: handler
  logical, save :: handlerInitialised = .false.

contains

  subroutine raiseError(message)
    !! Trigger a critical, on-the-fly error with the given message.
    character(len=*), intent(in) :: message

    if (.not. handlerInitialised) then
      call handler%init()
      handlerInitialised = .true.
    end if
    call handler%trigger(error=ErrorInstance(code=1, message=message, isCritical=.true.))
  end subroutine raiseError

end module mError
