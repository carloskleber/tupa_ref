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
  public :: formatDuration, wallSeconds
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

  function wallSeconds() result(seconds)
    !! Wall-clock time (s) from an arbitrary origin; only differences are
    !! meaningful. Safe to call from OpenMP threads.
    real(8) :: seconds
    integer(8) :: count, rate

    call system_clock(count=count, count_rate=rate)
    seconds = real(count, 8) / real(rate, 8)
  end function wallSeconds

end module mVerbosity
