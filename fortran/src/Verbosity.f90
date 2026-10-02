module mVerbosity
  !! Global CLI verbosity level (ARCHITECTURE.md §5: print-based logging) and
  !! the message sink through which the library's console output flows.
  !! The level is set once from command-line flags in `app/main.f90`; read by
  !! any module that wants to gate routine/progress output. Errors and warnings
  !! are unaffected by this setting and always print (`mError`, "unknown
  !! element type" in `mTupa`).
  !!
  !! Message sink: library output goes through `emitMessage`. With no sink
  !! installed it is written to standard output exactly as the `print`
  !! statements it replaced did; a program that embeds the solver installs a
  !! sink with `setMessageSink` and receives every message with its severity
  !! instead. The sink may be called from OpenMP
  !! threads (the per-frequency progress line), so it must be thread-safe.
  implicit none
  private

  public :: setVerbosity, verbosityLevel, verbose
  public :: VERB_QUIET, VERB_NORMAL, VERB_VERBOSE
  public :: messageSink, setMessageSink, clearMessageSink, hasMessageSink, emitMessage
  public :: MSG_INFO, MSG_WARNING, MSG_ERROR

  integer, parameter :: VERB_QUIET   = 0
  !! -q/--quiet: suppress routine informational output (report, sweep summary)
  integer, parameter :: VERB_NORMAL  = 1
  !! Default: current behaviour (report + summary lines)
  integer, parameter :: VERB_VERBOSE = 2
  !! -v/--verbose: VERB_NORMAL plus extra progress detail

  integer, parameter :: MSG_INFO    = 0
  !! Routine or progress output (gated by the verbosity level at the call site)
  integer, parameter :: MSG_WARNING = 1
  !! Something suspicious the run goes on with (never gated)
  integer, parameter :: MSG_ERROR   = 2
  !! Not emitted by the library; for callers that forward recorded `mError`
  !! messages to their sink

  abstract interface
    subroutine messageSink(severity, text)
      !! Receiver of the library's console output. `text` may span several
      !! lines (the study report is one message, newline-separated).
      integer, intent(in) :: severity
      !! `MSG_INFO`, `MSG_WARNING` or `MSG_ERROR`
      character(len=*), intent(in) :: text
    end subroutine messageSink
  end interface

  integer, save :: currentLevel = VERB_NORMAL
  procedure(messageSink), pointer, save :: sink => null()

contains

  subroutine setVerbosity(level)
    integer, intent(in) :: level

    currentLevel = level
  end subroutine setVerbosity

  integer function verbosityLevel() result(level)
    level = currentLevel
  end function verbosityLevel

  subroutine setMessageSink(proc)
    !! Route every library message to `proc` instead of standard output.
    procedure(messageSink) :: proc

    sink => proc
  end subroutine setMessageSink

  subroutine clearMessageSink()
    !! Back to standard output.
    nullify(sink)
  end subroutine clearMessageSink

  logical function hasMessageSink() result(has)
    has = associated(sink)
  end function hasMessageSink

  subroutine emitMessage(severity, text)
    !! Deliver `text` to the installed sink, or to standard output when none
    !! is installed. Not gated by the verbosity level: callers gate.
    integer, intent(in) :: severity
    character(len=*), intent(in) :: text

    if (associated(sink)) then
      call sink(severity, text)
    else
      write(*, '(A)') text
    end if
  end subroutine emitMessage

  subroutine verbose(level, msg)
    !! Send `msg` if the current verbosity level is >= `level`.
    integer, intent(in) :: level
    character(len=*), intent(in) :: msg

    if (currentLevel < level) return
    if (associated(sink)) then
      call sink(MSG_INFO, trim(msg))
    else
      print *, trim(msg)
    end if
  end subroutine verbose

end module mVerbosity
