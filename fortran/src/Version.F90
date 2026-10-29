module mVersion
  !! Program name, version and git commit, for the startup banner and `--version`.
  !!
  !! `build.sh` writes the gitignored `version_generated.inc` (version from
  !! `fpm.toml`, commit from `git describe`); without it (plain `fpm build`) the
  !! values fall back to "unknown". Kept as an include rather than `-D` flags so a
  !! new commit recompiles only this file, not the whole dependency tree.
  implicit none
  private
  public :: PROGRAM_NAME, programVersion, programCommit, versionBanner

  character(len=*), parameter :: PROGRAM_NAME = "TUPÃ"

#if __has_include("version_generated.inc")
#include "version_generated.inc"
#else
  character(len=*), parameter :: VERSION_STRING = "unknown"
  character(len=*), parameter :: COMMIT_STRING = "unknown"
#endif

contains

  function programVersion() result(v)
    character(len=:), allocatable :: v
    v = VERSION_STRING
  end function programVersion

  function programCommit() result(c)
    character(len=:), allocatable :: c
    c = COMMIT_STRING
  end function programCommit

  function versionBanner() result(b)
    !! "TUPÃ 0.7.0 (commit ffae02b-dirty)"
    character(len=:), allocatable :: b
    b = PROGRAM_NAME // " " // VERSION_STRING // " (commit " // COMMIT_STRING // ")"
  end function versionBanner

end module mVersion
