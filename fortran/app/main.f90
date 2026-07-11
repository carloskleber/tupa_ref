program main
  !! TUPÃ electromagnetic field transient solver.
  !!
  !! **Entry point for command-line execution.**
  !!
  !! Loads a JSON study file and runs the complete electromagnetic analysis pipeline:
  !!
  !! **Usage:**
  !!   fpm run -- [-v|--verbose] [-q|--quiet] [--epsrel <value>] [--no-cache] <study.json>
  !!   ./tupa [-v|--verbose] [-q|--quiet] [--epsrel <value>] [--no-cache] <study.json>
  !!
  !! **Input:**
  !! - `<study.json>` — path to a JSON file describing the electromagnetic study
  !!   - See [JSON format details](../src/Tupa.f90) for schema
  !! - `-v`/`--verbose` — extra progress detail (`mVerbosity`'s `VERB_VERBOSE`)
  !! - `-q`/`--quiet` — suppress the routine report/summary output
  !!   (`mVerbosity`'s `VERB_QUIET`); errors and warnings still print
  !! - `--epsrel <value>` — relative-error factor for the adaptive geometry
  !!   quadrature (`mImpedance%geometryFactor2D`), default 1.0e-6
  !! - `--no-cache` — disable the geometry-factor quadrature memo table
  !!   (`mGeometryCache`); every congruent segment pair is re-integrated
  !!
  !! **Output:**
  !! - Printed summary of study geometry (nodes, materials, elements)
  !! - (Future) CSV and/or JSON files with frequency-domain solution
  !!
  !! **Example JSON:**
  !!   ```json
  !!   {
  !!     "title": "Buried conductor study",
  !!     "soil": {
  !!       "permittivity": 10.0,
  !!       "permeability": 1.0,
  !!       "conductivity": 0.01
  !!     },
  !!     "nodes": [
  !!       {"id": "node1", "position": [0, 0, 0]},
  !!       {"id": "node2", "position": [10, 0, -0.5]}
  !!     ],
  !!     "elements": [
  !!       {
  !!         "type": "line",
  !!         "id": "line1",
  !!         "from": "node1",
  !!         "to": "node2",
  !!         "radius": 0.005,
  !!         "segments": 5,
  !!         "material": "copper"
  !!       }
  !!     ]
  !!   }
  !!   ```
  use tupa, only: runFromFile
  use mError, only: raiseError
  use mVerbosity, only: setVerbosity, VERB_QUIET, VERB_VERBOSE
  use mImpedance, only: setQuadEpsRel
  use mGeometryCache, only: geomCacheSetEnabled
  implicit none

  character(len=512) :: filename, arg
  !! Path to the JSON study file, and a scratch buffer for each argument
  integer :: ios, iosVal, i, nargs
  !! Status flags for argument retrieval/parsing, loop index, argument count
  real(8) :: epsrel
  !! Parsed --epsrel value

  filename = ""
  nargs = command_argument_count()
  i = 1
  do while (i <= nargs)
    call get_command_argument(i, arg, status=ios)
    if (ios /= 0) then
      i = i + 1
      cycle
    end if
    select case (trim(arg))
    case ("-v", "--verbose")
      call setVerbosity(VERB_VERBOSE)
    case ("-q", "--quiet")
      call setVerbosity(VERB_QUIET)
    case ("--epsrel")
      i = i + 1
      if (i > nargs) call raiseError("--epsrel requires a value, e.g. --epsrel 1.0e-6")
      call get_command_argument(i, arg, status=ios)
      read(arg, *, iostat=iosVal) epsrel
      if (ios /= 0 .or. iosVal /= 0 .or. epsrel <= 0.0d0) &
        call raiseError("--epsrel: invalid value '" // trim(arg) // "' (must be a positive real)")
      call setQuadEpsRel(epsrel)
    case ("--no-cache")
      call geomCacheSetEnabled(.false.)
    case default
      filename = arg
    end select
    i = i + 1
  end do

  if (len_trim(filename) == 0) then
    print *, "Usage: tupa [-v|--verbose] [-q|--quiet] [--epsrel <value>] [--no-cache] <study.json>"
    call raiseError("missing study file argument")
  end if

  call runFromFile(trim(filename))
end program main
