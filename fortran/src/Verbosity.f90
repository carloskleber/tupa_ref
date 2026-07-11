module mVerbosity
  !! Global CLI verbosity level (ARCHITECTURE.md §5: print-based logging).
  !! Set once from command-line flags in `app/main.f90`; read by any module
  !! that wants to gate routine/progress output. Errors and warnings are
  !! unaffected by this setting and always print (`mError`, "unknown element
  !! type" in `mTupa`).
  implicit none
  private

  public :: setVerbosity, verbosityLevel
  public :: VERB_QUIET, VERB_NORMAL, VERB_VERBOSE

  integer, parameter :: VERB_QUIET   = 0
  !! -q/--quiet: suppress routine informational output (report, sweep summary)
  integer, parameter :: VERB_NORMAL  = 1
  !! Default: current behaviour (report + summary lines)
  integer, parameter :: VERB_VERBOSE = 2
  !! -v/--verbose: VERB_NORMAL plus extra progress detail

  integer, save :: currentLevel = VERB_NORMAL

contains

  subroutine setVerbosity(level)
    integer, intent(in) :: level

    currentLevel = level
  end subroutine setVerbosity

  integer function verbosityLevel() result(level)
    level = currentLevel
  end function verbosityLevel

end module mVerbosity
