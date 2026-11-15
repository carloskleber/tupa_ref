module mVersion
  !! Program name, version and git commit, for the startup banner and `--version`.
  !!
  !! `build.sh` rewrites `version_generated.inc` (version from `fpm.toml`, commit
  !! from `git describe`, build date). The committed default holds "unknown", so a plain
  !! `fpm build` (any compiler, ifx included; no `__has_include`) works as is. Kept as an include rather than `-D` flags so a
  !! new commit recompiles only this file, not the whole dependency tree.
  implicit none
  private
  public :: PROGRAM_NAME, programVersion, programCommit, programBuildDate, versionBanner

  character(len=*), parameter :: PROGRAM_NAME = "TUPA"
    !! Program name, without accents for ensure international compatibility.

#include "version_generated.inc"

contains

  function programVersion() result(v)
    character(len=:), allocatable :: v
    v = VERSION_STRING
  end function programVersion

  function programCommit() result(c)
    character(len=:), allocatable :: c
    c = COMMIT_STRING
  end function programCommit

  function programBuildDate() result(d)
    character(len=:), allocatable :: d
    d = BUILD_DATE_STRING
  end function programBuildDate

  function versionBanner() result(b)
    !! "TUPA 0.7.0 (commit ffae02b-dirty, built 2026-11-02)"
    character(len=:), allocatable :: b
    b = PROGRAM_NAME // " " // VERSION_STRING // " (commit " // COMMIT_STRING // &
        ", built " // BUILD_DATE_STRING // ")"
  end function versionBanner

end module mVersion
