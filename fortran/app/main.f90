program main
  !! TUPÃ electromagnetic field transient solver.
  !!
  !! **Entry point for command-line execution.**
  !!
  !! Loads a JSON study file and runs the complete electromagnetic analysis pipeline:
  !!
  !! **Usage:**
  !!   fpm run -- [-v|--verbose] [-q|--quiet] [--epsrel <value>] [--kernel single|double]
  !!              [--image-model frequency-dependent|ideal] [--threads <n>] [--no-cache] <study.json>
  !!   ./tupa [same options] <study.json>
  !!
  !! **Input:**
  !! - `<study.json>` — path to a JSON file describing the electromagnetic study
  !!   - See [JSON format details](../src/Tupa.f90) for schema
  !! - `-v`/`--verbose` — extra progress detail (`mVerbosity`'s `VERB_VERBOSE`)
  !! - `-q`/`--quiet` — suppress the routine report/summary output
  !!   (`mVerbosity`'s `VERB_QUIET`); errors and warnings still print
  !! - `--epsrel <value>` — relative-error factor for the adaptive geometry
  !!   quadrature (`mImpedance%geometryFactor1D`/`geometryFactor2D`), default 1.0e-6
  !! - `--kernel single|double` — geometry-factor quadrature: the mHEM
  !!   single integral (default, ROADMAP Phase 10 item 1) or the nested 2-D
  !!   oracle; a study's `numerics.kernel` overrides it
  !! - `--image-model frequency-dependent|ideal` — image reflection
  !!   coefficient: Γ(ω) (default, Phase 10 item 2) or the ideal ±1 limit; a
  !!   study's `numerics.imageModel` overrides it
  !! - `--threads <n>` — threads for the frequency loop (ROADMAP Phase 10
  !!   item 4); needs a `-fopenmp` build (`build.sh`), default `OMP_NUM_THREADS`
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
  !$ use omp_lib
  use tupa, only: runFromFile
  use mError, only: raiseError
  use mVerbosity, only: setVerbosity, VERB_QUIET, VERB_VERBOSE
  use mImpedance, only: setQuadEpsRel
  use mGeometry, only: setGeometryKernel, GEOM_KERNEL_SINGLE, GEOM_KERNEL_DOUBLE
  use mMesh, only: setDefaultImageModel, IMAGE_FREQ_DEPENDENT, IMAGE_IDEAL
  use mGeometryCache, only: geomCacheSetEnabled
  implicit none

  character(len=512) :: filename, arg
  !! Path to the JSON study file, and a scratch buffer for each argument
  integer :: ios, iosVal, i, nargs
  !! Status flags for argument retrieval/parsing, loop index, argument count
  real(8) :: epsrel
  !! Parsed --epsrel value
  integer :: nThreads
  !! Parsed --threads value
  logical :: threadsGiven = .false.
  !! Whether --threads was passed

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
    case ("--kernel")
      i = i + 1
      if (i > nargs) call raiseError("--kernel requires a value: single or double")
      call get_command_argument(i, arg, status=ios)
      select case (trim(arg))
      case ("single")
        call setGeometryKernel(GEOM_KERNEL_SINGLE)
      case ("double")
        call setGeometryKernel(GEOM_KERNEL_DOUBLE)
      case default
        call raiseError("--kernel: invalid value '" // trim(arg) // "' (expected single or double)")
      end select
    case ("--image-model")
      i = i + 1
      if (i > nargs) call raiseError("--image-model requires a value: frequency-dependent or ideal")
      call get_command_argument(i, arg, status=ios)
      select case (trim(arg))
      case ("frequency-dependent")
        call setDefaultImageModel(IMAGE_FREQ_DEPENDENT)
      case ("ideal")
        call setDefaultImageModel(IMAGE_IDEAL)
      case default
        call raiseError("--image-model: invalid value '" // trim(arg) // "' (expected frequency-dependent or ideal)")
      end select
    case ("--threads")
      i = i + 1
      if (i > nargs) call raiseError("--threads requires a value, e.g. --threads 4")
      call get_command_argument(i, arg, status=ios)
      read(arg, *, iostat=iosVal) nThreads
      if (ios /= 0 .or. iosVal /= 0 .or. nThreads < 1) &
        call raiseError("--threads: invalid value '" // trim(arg) // "' (must be a positive integer)")
      threadsGiven = .true.
    case ("--no-cache")
      call geomCacheSetEnabled(.false.)
    case default
      filename = arg
    end select
    i = i + 1
  end do

  if (len_trim(filename) == 0) then
    print *, "Usage: tupa [-v|--verbose] [-q|--quiet] [--epsrel <value>] [--kernel single|double] "// &
      "[--image-model frequency-dependent|ideal] [--threads <n>] [--no-cache] <study.json>"
    call raiseError("missing study file argument")
  end if

  if (threadsGiven) then
    !$ call omp_set_num_threads(nThreads)
    !$ threadsGiven = .false.
    if (threadsGiven) print *, "tupa: --threads ignored (built without OpenMP; the frequency loop runs serially)"
  end if

  call runFromFile(trim(filename))
end program main
