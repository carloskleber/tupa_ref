program test_library_mode
  !! Tests for the hooks that let another program embed the solver: the
  !! recoverable error mode of `mError` (an input error is recorded and the
  !! reader routines unwind instead of halting the process) and the message
  !! sink of `mVerbosity`. The failing paths of `loadStudy` and
  !! `validateStudyReferences` can only be exercised in-process thanks to the
  !! error mode.
  use tupa
  use mStudy
  use mError
  use mVerbosity
  use mSignal, only: tSignal
  use check
  implicit none

  character(len=*), parameter :: caseFile = "test_library_mode_case.json"
  character(len=*), parameter :: SOIL = '"soil":{"permittivity":10,"permeability":1,"conductivity":0.01}'
  character(len=*), parameter :: NODES = '"nodes":[{"id":"A","position":[0,0,-1]},{"id":"B","position":[10,0,-1]}]'
  character(len=*), parameter :: MATS = '"materials":[{"id":"cu","epsilonr":1,"mur":1,"sigma":5.8e7}]'
  character(len=*), parameter :: LINE = &
    '"elements":[{"type":"line","id":"L","from":"A","to":"B","radius":0.01,"segments":2,"material":"cu"}]'
  character(len=*), parameter :: SRC = '"sources":[{"node":"A","current":{"re":1,"im":0}}]'
  character(len=*), parameter :: FREQ = '"frequencies":{"min":100,"max":1000,"pointsPerDecade":2}'

  integer, save :: nSunk = 0, lastSeverity = -1
  character(len=256), save :: lastText = ''

  call test_init("mError: library mode is off by default and raising halts only there")
  call test_ok("library mode starts off", .not. libraryModeActive())
  call test_ok("no error recorded at start", .not. errorRaised() .and. errorCount() == 0)

  call test_init("mError: recording")
  call setLibraryMode(.true.)
  call raiseError("first problem")
  call raiseError("second problem")
  call test_ok("raise returns and is recorded", errorRaised() .and. errorCount() == 2)
  call test_ok("messages keep their order", errorMessage(1) == "first problem" .and. errorMessage(2) == "second problem")
  call test_ok("out-of-range index gives empty text", len(errorMessage(0)) == 0 .and. len(errorMessage(3)) == 0)
  call clearError()
  call test_ok("clearError forgets", .not. errorRaised() .and. errorCount() == 0)
  block
    integer :: i
    do i = 1, MAX_STORED_ERRORS + 5
      call raiseError("flood")
    end do
    call test_ok("count goes past the stored limit", errorCount() == MAX_STORED_ERRORS + 5)
    call test_ok("text beyond the limit is dropped", len(errorMessage(MAX_STORED_ERRORS + 1)) == 0)
  end block
  call clearError()
  call setLibraryMode(.false.)

  call test_init("loadStudy / validateStudyReferences: input errors are recoverable")

  call expectLoadError("a valid study loads without error", &
    '{"title":"ok",' // SOIL // ',' // NODES // ',' // MATS // ',' // LINE // ',' // SRC // ',' // FREQ // '}', &
    expectError=.false., needle="")
  call expectLoadError("a missing file", "", expectError=.true., needle="", missingFile=.true.)
  call expectLoadError("malformed JSON", '{"title": ', expectError=.true., needle="mJsonParser")
  call expectLoadError("no soil block", '{"title":"t",' // NODES // '}', expectError=.true., needle="no soil block")
  call expectLoadError("unknown soil type", '{"title":"t","soil":{"type":"magic"}}', expectError=.true., &
    needle="unknown soil.type")
  call expectLoadError("a node without a position", &
    '{"title":"t",' // SOIL // ',"nodes":[{"id":"A"}]}', expectError=.true., needle="needs a 3-component position")
  call expectLoadError("a mesh without a position", &
    '{"title":"t",' // SOIL // ',' // MATS // &
    ',"elements":[{"type":"mesh","id":"M","lengthX":4,"lengthY":4,"rowsX":2,"rowsY":2,"radius":0.01,' // &
    '"segments":1,"material":"cu"}]}', expectError=.true., needle="mesh 'M' needs a 3-component position")
  call expectLoadError("a line to an unknown node (raised while assembling)", &
    '{"title":"t",' // SOIL // ',' // NODES // ',' // MATS // &
    ',"elements":[{"type":"line","id":"L","from":"A","to":"Z","radius":0.01,"segments":2,"material":"cu"}]}', &
    expectError=.true., needle="end node 'Z' not found")
  call expectLoadError("a line with an unknown material", &
    '{"title":"t",' // SOIL // ',' // NODES // ',' // MATS // &
    ',"elements":[{"type":"line","id":"L","from":"A","to":"B","radius":0.01,"segments":2,"material":"zz"}]}', &
    expectError=.true., needle="material 'zz' not found")
  call expectLoadError("a mesh with one row (element validation)", &
    '{"title":"t",' // SOIL // ',' // MATS // &
    ',"elements":[{"type":"mesh","id":"M","position":[0,0,-1],"lengthX":4,"lengthY":4,"rowsX":1,"rowsY":2,' // &
    '"radius":0.01,"segments":1,"material":"cu"}]}', expectError=.true., needle="rowsX and rowsY")
  call expectLoadError("a source at an unknown node", &
    '{"title":"t",' // SOIL // ',' // NODES // ',' // MATS // ',' // LINE // &
    ',"sources":[{"node":"Q","current":{"re":1,"im":0}}],' // FREQ // '}', expectError=.true., &
    needle="sources[].node references unknown node 'Q'")
  call expectLoadError("a bad frequency axis", &
    '{"title":"t",' // SOIL // ',' // NODES // ',' // MATS // ',' // LINE // ',' // SRC // &
    ',"frequencies":{"min":0,"max":1000,"pointsPerDecade":2}}', expectError=.true., needle="frequencies needs")
  call expectLoadError("an unknown signal waveform", &
    '{"title":"t",' // SOIL // ',' // NODES // ',' // MATS // ',' // LINE // &
    ',"signal":{"waveform":"saw","sourceNode":"A","observeNodes":["B"],"nyquistHz":1e6,"fftPoints":64}}', &
    expectError=.true., needle="unknown signal.waveform")
  call expectLoadError("a waveform constructor that rejects its parameters", &
    '{"title":"t",' // SOIL // ',' // NODES // ',' // MATS // ',' // LINE // &
    ',"signal":{"waveform":"sine","imax":1,"frequencyHz":-5,"sourceNode":"A","observeNodes":["B"],' // &
    '"nyquistHz":1e6,"fftPoints":64}}', expectError=.true., needle="frequencyHz must be > 0")
  call expectLoadError("a signal block without observeNodes", &
    '{"title":"t",' // SOIL // ',' // NODES // ',' // MATS // ',' // LINE // &
    ',"signal":{"waveform":"sine","imax":1,"frequencyHz":50,"sourceNode":"A","nyquistHz":1e6,"fftPoints":64}}', &
    expectError=.true., needle="signal.observeNodes is required")
  call expectLoadError("every unresolved reference is reported, not only the first", &
    '{"title":"t",' // SOIL // ',' // NODES // ',' // MATS // ',' // LINE // &
    ',"sources":[{"node":"Q1","current":{"re":1,"im":0}},{"node":"Q2","current":{"re":1,"im":0}}],' // FREQ // '}', &
    expectError=.true., needle="Q2", minErrors=2)

  call expectLoadError("a valid study loads again after the failures (state was cleared)", &
    '{"title":"ok",' // SOIL // ',' // NODES // ',' // MATS // ',' // LINE // ',' // SRC // ',' // FREQ // '}', &
    expectError=.false., needle="")

  call test_ok("library mode is off again after the helper", .not. libraryModeActive())

  call test_init("mVerbosity: message sink")
  call test_ok("no sink at start", .not. hasMessageSink())
  call setMessageSink(collect)
  call test_ok("sink installed", hasMessageSink())
  call emitMessage(MSG_WARNING, "careful")
  call test_ok("emitMessage reaches the sink with its severity", &
               nSunk == 1 .and. lastSeverity == MSG_WARNING .and. trim(lastText) == "careful")
  call setVerbosity(VERB_QUIET)
  call verbose(VERB_NORMAL, "routine")
  call test_ok("verbose below the level does not reach the sink", nSunk == 1)
  call setVerbosity(VERB_NORMAL)
  call verbose(VERB_NORMAL, "routine")
  call test_ok("verbose at the level reaches the sink as info", &
               nSunk == 2 .and. lastSeverity == MSG_INFO .and. trim(lastText) == "routine")
  call writeCase('{"title":"w",' // SOIL // ',' // NODES // ',' // MATS // &
                 ',"elements":[{"type":"line","id":"L","from":"A","to":"B","radius":0.01,"segments":2,' // &
                 '"material":"cu"},{"type":"widget","id":"W"}]}')
  nSunk = 0
  block
    type(tStudy) :: study
    character(len=32) :: detail
    call loadStudy(caseFile, study)
    write(detail, '(I0,1X,I0)') nSunk, lastSeverity
    call test_ok("an unknown element type is a warning through the sink", &
                 nSunk == 1 .and. lastSeverity == MSG_WARNING .and. index(lastText, "widget") > 0, &
                 trim(detail) // " | " // trim(lastText))
  end block
  call clearMessageSink()
  call test_ok("sink cleared", .not. hasMessageSink())
  call removeCase()

  call test_summary()
  call test_close()

contains

  subroutine collect(severity, text)
    integer, intent(in) :: severity
    character(len=*), intent(in) :: text

    nSunk = nSunk + 1
    lastSeverity = severity
    lastText = text
  end subroutine collect

  subroutine writeCase(text)
    character(len=*), intent(in) :: text
    integer :: u

    open(newunit=u, file=caseFile, status="replace", action="write")
    write(u, '(A)') text
    close(u)
  end subroutine writeCase

  subroutine removeCase()
    integer :: u, ios

    open(newunit=u, file=caseFile, status="old", iostat=ios)
    if (ios == 0) close(u, status="delete")
  end subroutine removeCase

  subroutine expectLoadError(name, text, expectError, needle, missingFile, minErrors)
    !! Load `text` the way an embedding caller does — clear, library mode on, `loadStudy`,
    !! then `validateStudyReferences` when the load went through — and check
    !! whether an error was recorded and whether one of the messages holds
    !! `needle`.
    character(len=*), intent(in) :: name, text, needle
    logical, intent(in) :: expectError
    logical, intent(in), optional :: missingFile
    integer, intent(in), optional :: minErrors
    type(tStudy) :: study
    character(len=256), allocatable :: sourceNodeIds(:), outputNodeIds(:), outputElectrodeIds(:)
    character(len=256), allocatable :: signalObserveNodeIds(:), signalObserveElectrodeIds(:)
    character(len=256), allocatable :: signalSourceNodeIds(:), sourceReturnNodeIds(:)
    complex(8), allocatable :: sourceCurrents(:)
    real(8), allocatable :: freqHz(:)
    class(tSignal), allocatable :: signal
    logical :: raised, found
    integer :: i, need

    if (present(missingFile)) then
      if (missingFile) then
        call removeCase()
      else
        call writeCase(text)
      end if
    else
      call writeCase(text)
    end if

    call clearError()
    call setLibraryMode(.true.)
    call loadStudy(caseFile, study, sourceNodeIds=sourceNodeIds, sourceCurrents=sourceCurrents, freqHz=freqHz, &
                   outputNodeIds=outputNodeIds, outputElectrodeIds=outputElectrodeIds, &
                   signal=signal, signalObserveNodeIds=signalObserveNodeIds, &
                   signalObserveElectrodeIds=signalObserveElectrodeIds, &
                   signalSourceNodeIds=signalSourceNodeIds, sourceReturnNodeIds=sourceReturnNodeIds)
    if (.not. errorRaised()) then
      call validateStudyReferences(study, sourceNodeIds=sourceNodeIds, signal=signal, &
                                   signalObserveNodeIds=signalObserveNodeIds, &
                                   signalObserveElectrodeIds=signalObserveElectrodeIds, &
                                   outputNodeIds=outputNodeIds, outputElectrodeIds=outputElectrodeIds, &
                                   signalSourceNodeIds=signalSourceNodeIds, sourceReturnNodeIds=sourceReturnNodeIds)
    end if
    call setLibraryMode(.false.)
    raised = errorRaised()

    if (expectError) then
      found = len_trim(needle) == 0
      do i = 1, min(errorCount(), MAX_STORED_ERRORS)
        if (index(errorMessage(i), needle) > 0) found = .true.
      end do
      need = 1
      if (present(minErrors)) need = minErrors
      call test_ok(name, raised .and. found .and. errorCount() >= need, &
                   "expected an error mentioning '" // needle // "', got " // describe())
    else
      call test_ok(name, .not. raised, "unexpected error: " // describe())
    end if
    call clearError()
    call removeCase()
  end subroutine expectLoadError

  function describe() result(s)
    character(len=:), allocatable :: s

    if (errorCount() == 0) then
      s = "none"
    else
      s = errorMessage(1)
    end if
  end function describe

end program test_library_mode
