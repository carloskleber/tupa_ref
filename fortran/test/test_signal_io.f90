program test_signal_io
  !! Tests the JSON `signal` reader (ADR 0015): `loadStudy`'s optional
  !! signal-block arguments on a structure-only case (no block: stays
  !! unallocated) and on `common/portela1997_transient.json` (full block,
  !! including the `observeNodes`/`observeElectrodes` arrays and the
  !! `freqZeroHz` default), plus an end-to-end `transientResponse` run
  !! through the JSON-parsed signal, cross-checked against directly
  !! constructing the same waveform (`mSignal`, already unit-tested in
  !! test_signal.f90 — this test only exercises the JSON plumbing).
  use mCtes, only: dp
  use tupa
  use mStudy
  use mSignal
  use mTransient
  use check
  implicit none

  type(tStudy) :: study
  class(tSignal), allocatable :: signal
  character(len=256) :: signalSourceNode
  character(len=256), allocatable :: signalObserveNodeIds(:), signalObserveElectrodeIds(:)
  real(8) :: signalNyquistHz, signalFreqZeroHz
  integer :: signalFftPoints
  type(tDoubleExpSignal) :: reference
  real(dp), allocatable :: t(:), tRef(:), injectedCurrent(:), nodeResponses(:,:), i1Responses(:,:), i2Responses(:,:)

  ! ----------------------------------------------------------------
  ! Structure-only case: signal arguments must stay unallocated.
  ! ----------------------------------------------------------------
  call test_init("loadStudy: structure-only case leaves signal args unallocated")

  call loadStudy("../common/buried_conductor_short.json", study, signal=signal, &
                 signalSourceNode=signalSourceNode, signalObserveNodeIds=signalObserveNodeIds, &
                 signalObserveElectrodeIds=signalObserveElectrodeIds, signalNyquistHz=signalNyquistHz, &
                 signalFftPoints=signalFftPoints, signalFreqZeroHz=signalFreqZeroHz)

  call test_ok("signal not allocated (no 'signal' block)", &
               .not. allocated(signal), "buried_conductor_short.json has no signal block")
  call test_ok("signalObserveNodeIds not allocated", &
               .not. allocated(signalObserveNodeIds), "no signal block to read observeNodes from")
  call test_ok("signalObserveElectrodeIds not allocated", &
               .not. allocated(signalObserveElectrodeIds), "no signal block to read observeElectrodes from")

  ! ----------------------------------------------------------------
  ! Full case: signal block parsed correctly (ADR 0015), including the
  ! observeNodes/observeElectrodes arrays and the freqZeroHz default
  ! (portela1997_transient.json omits it).
  ! ----------------------------------------------------------------
  call test_init("loadStudy: signal block (ADR 0015)")

  call loadStudy("../common/portela1997_transient.json", study, signal=signal, &
                 signalSourceNode=signalSourceNode, signalObserveNodeIds=signalObserveNodeIds, &
                 signalObserveElectrodeIds=signalObserveElectrodeIds, signalNyquistHz=signalNyquistHz, &
                 signalFftPoints=signalFftPoints, signalFreqZeroHz=signalFreqZeroHz)

  call test_ok("signal allocated", allocated(signal), "signal block should have been parsed")
  call test_ok("signal is a tDoubleExpSignal ('doubleExp' waveform)", &
               same_type_as(signal, reference), "wrong concrete waveform type")
  call test_ok("sourceNode == Node_1", trim(signalSourceNode) == "Node_1", "wrong sourceNode")
  call test_ok("observeNodes has 2 entries", size(signalObserveNodeIds) == 2, "wrong observeNodes count")
  call test_ok("observeNodes[1] == Node_1", trim(signalObserveNodeIds(1)) == "Node_1", "wrong observeNodes[1]")
  call test_ok("observeNodes[2] == Node_2", trim(signalObserveNodeIds(2)) == "Node_2", "wrong observeNodes[2]")
  call test_ok("observeElectrodes has 1 entry", size(signalObserveElectrodeIds) == 1, &
               "wrong observeElectrodes count")
  call test_ok("observeElectrodes[1] == Line_1_e1", trim(signalObserveElectrodeIds(1)) == "Line_1_e1", &
               "wrong observeElectrodes[1]")
  call test_ok("nyquistHz == 1 MHz", abs(signalNyquistHz - 1.0d6) < 1.0d0, "wrong nyquistHz")
  call test_ok("fftPoints == 1024", signalFftPoints == 1024, "wrong fftPoints")
  call test_ok("freqZeroHz defaults to 1e-6 (omitted in JSON)", abs(signalFreqZeroHz - 1.0d-6) < 1.0d-12, &
               "freqZeroHz default not applied")

  ! ----------------------------------------------------------------
  ! The JSON-parsed signal must sample identically to constructing the
  ! same waveform directly (mSignal, cross-checked against imax=30000,
  ! front=f1_2_50, jones=false — example5.f90's parameters).
  ! ----------------------------------------------------------------
  call test_init("loadStudy: parsed signal waveform matches direct construction")

  reference = newDoubleExpSignal(30000.0d0, "f1_2_50")
  allocate(tRef(5))
  tRef = [0.0d0, 1.0d-6, 5.0d-6, 2.0d-5, 1.0d-4]
  call test_ok("waveform samples match a directly-constructed tDoubleExpSignal", &
               all(abs(signal%waveform(tRef) - reference%waveform(tRef)) < 1.0d-9 * 30000.0d0), &
               "JSON-parsed signal does not reproduce newDoubleExpSignal(30000, 'f1_2_50')")

  ! ----------------------------------------------------------------
  ! End-to-end: transientResponse runs on the JSON-parsed signal/config,
  ! observing both nodes and the requested electrode.
  ! ----------------------------------------------------------------
  call test_init("transientResponse: runs end to end from the JSON-parsed signal block")

  call transientResponse(study, signal, trim(signalSourceNode), signalObserveNodeIds, &
                          signalNyquistHz, signalFftPoints, signalFreqZeroHz, t, injectedCurrent, &
                          nodeResponses, observeElectrodeIds=signalObserveElectrodeIds, &
                          i1Responses=i1Responses, i2Responses=i2Responses)

  call test_ok("time axis has fftPoints points", size(t) == signalFftPoints, "")
  call test_ok("nodeResponses has one row per observeNodes entry", &
               size(nodeResponses, 1) == size(signalObserveNodeIds), "")
  call test_ok("i1Responses has one row per observeElectrodes entry", &
               size(i1Responses, 1) == size(signalObserveElectrodeIds), "")
  call test_ok("i2Responses has one row per observeElectrodes entry", &
               size(i2Responses, 1) == size(signalObserveElectrodeIds), "")
  call test_ok("peak injected current ~= imax (30 kA)", &
               abs(maxval(injectedCurrent) - 30000.0d0) < 0.1d0 * 30000.0d0, &
               "injected current peak far from imax")

  ! ----------------------------------------------------------------
  ! ADR 0016 (voltage sources) + ADR 0015 amendment (heidler terms):
  ! parse a scratch case carrying a voltage source, a current source and
  ! a parametrised Heidler signal, and cross-check the waveform against
  ! direct construction.
  ! ----------------------------------------------------------------
  call test_init("loadStudy: voltage source flag and parametrised heidler terms")

  block
    character(len=*), parameter :: tmpFile = "test_signal_io_tmp.json"
    type(tStudy) :: study2
    class(tSignal), allocatable :: signal2
    character(len=256), allocatable :: srcIds(:)
    complex(8), allocatable :: srcValues(:)
    logical, allocatable :: srcIsVoltage(:)
    real(8), allocatable :: freqHz(:)
    type(tHeidlerSignal) :: heidlerRef
    real(dp) :: tChk(4)
    integer :: unit

    open(newunit=unit, file=tmpFile, status="replace", action="write")
    write(unit, '(A)') '{'
    write(unit, '(A)') '  "title": "signal-io scratch case",'
    write(unit, '(A)') '  "soil": {"permittivity": 10.0, "permeability": 1.0, "conductivity": 0.01},'
    write(unit, '(A)') '  "nodes": ['
    write(unit, '(A)') '    {"id": "N1", "position": [0.0, 0.0, -0.5]},'
    write(unit, '(A)') '    {"id": "N2", "position": [10.0, 0.0, -0.5]}'
    write(unit, '(A)') '  ],'
    write(unit, '(A)') '  "materials": [{"id": "copper", "epsilonr": 1.0, "mur": 1.0, "sigma": 5.96e7}],'
    write(unit, '(A)') '  "elements": ['
    write(unit, '(A)') '    {"type": "line", "id": "L1", "from": "N1", "to": "N2",'
    write(unit, '(A)') '     "radius": 0.007, "segments": 2, "material": "copper"}'
    write(unit, '(A)') '  ],'
    write(unit, '(A)') '  "sources": ['
    write(unit, '(A)') '    {"node": "N1", "voltage": {"re": 10.0, "im": -2.0}},'
    write(unit, '(A)') '    {"node": "N2", "current": {"re": 1.0, "im": 0.0}}'
    write(unit, '(A)') '  ],'
    write(unit, '(A)') '  "frequencies": {"min": 100.0, "max": 1000.0, "pointsPerDecade": 3},'
    write(unit, '(A)') '  "signal": {'
    write(unit, '(A)') '    "waveform": "heidler",'
    write(unit, '(A)') '    "terms": [{"i0": 200000.0, "n": 10.0, "tau1": 19.0e-6, "tau2": 485.0e-6}],'
    write(unit, '(A)') '    "sourceNode": "N1",'
    write(unit, '(A)') '    "observeNodes": ["N1"],'
    write(unit, '(A)') '    "nyquistHz": 1.0e6,'
    write(unit, '(A)') '    "fftPoints": 256'
    write(unit, '(A)') '  }'
    write(unit, '(A)') '}'
    close(unit)

    call loadStudy(tmpFile, study2, sourceNodeIds=srcIds, sourceCurrents=srcValues, &
                   sourceIsVoltage=srcIsVoltage, freqHz=freqHz, signal=signal2)

    call test_ok("two sources parsed", size(srcIds) == 2, "wrong source count")
    call test_ok("sourceIsVoltage allocated with two entries", &
                 allocated(srcIsVoltage) .and. size(srcIsVoltage) == 2, "flags missing")
    call test_ok("N1 flagged as voltage source", srcIsVoltage(1), "voltage field not detected")
    call test_ok("N2 stays a current source", .not. srcIsVoltage(2), "current source misflagged")
    call test_ok("voltage value parsed as complex", &
                 abs(srcValues(1) - cmplx(10.0d0, -2.0d0, kind=8)) < 1.0d-12, "wrong voltage value")

    call test_ok("signal parsed as tHeidlerSignal", same_type_as(signal2, heidlerRef), &
                 "heidler-with-terms should still construct a tHeidlerSignal")
    heidlerRef = newHeidlerSignalTerms([200.0d3], [10.0d0], [19.0d-6], [485.0d-6])
    tChk = [0.0d0, 5.0d-6, 20.0d-6, 200.0d-6]
    call test_ok("parsed terms reproduce direct construction (no rescale)", &
                 all(abs(signal2%waveform(tChk) - heidlerRef%waveform(tChk)) < 1.0d-6 * 200.0d3), &
                 "JSON heidler terms do not reproduce newHeidlerSignalTerms")

    open(newunit=unit, file=tmpFile, status="old")
    close(unit, status="delete")
  end block

  call test_summary()

end program test_signal_io
