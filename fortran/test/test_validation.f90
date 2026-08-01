program test_validation
  !! Tests for the upfront ID cross-reference validation added to fix the
  !! bug where a typo'd `signal.observeElectrodes` entry (or any other
  !! sources/signal/outputs ID reference) was only caught deep inside
  !! `runSweep`/`transientResponse` — after the O(n^2) geometry-factor
  !! quadrature and a full frequency sweep had already run — instead of
  !! right after (cheap) structure assembly (`mTupa::validateStudyReferences`,
  !! `mStructure::findElectrodeIndex`, idempotent `assembleStructure`).
  use tupa
  use mStudy
  use mStructure
  use mElement
  use mElementMesh
  use mMaterial
  use mSignal, only: tSignal
  use check
  implicit none

  type(tStructure) :: structure
  class(tElement), allocatable :: elem
  class(tMaterial), allocatable :: mat

  ! =====================================================================
  ! assembleStructure is idempotent: calling it a second time must not
  ! double-register nodes/electrodes. This is what lets
  ! validateStudyReferences assemble early (before geometry/solve work)
  ! without disturbing the later, lazy assembly inside tStudy%prepareStudy.
  ! =====================================================================

  call test_init("tStructure%assembleStructure is idempotent")

  mat = newMaterialLinear("cond", 1.0d0, 1.0d0, 5.8d7)
  call structure%addMaterial(mat)
  elem = newElementMesh("M", [0.0d0, 0.0d0, -1.0d0], 4.0d0, 4.0d0, 2, 2, 0.01d0, 2, "cond")
  call structure%addElement(elem)

  call structure%assembleStructure()
  call test_ok("first assembleStructure call populates nodes", &
               structure%getNodeCount() > 0, "expected a nonzero node count after assembly")
  block
    integer :: nodesAfterFirst, electrodesAfterFirst
    nodesAfterFirst      = structure%getNodeCount()
    electrodesAfterFirst = structure%getElectrodeCount()

    call structure%assembleStructure()
    call structure%assembleStructure()

    call test_ok("node count unchanged after 2 more assembleStructure calls", &
                 structure%getNodeCount() == nodesAfterFirst, &
                 "assembly is not idempotent -- nodes were double-registered")
    call test_ok("electrode count unchanged after 2 more assembleStructure calls", &
                 structure%getElectrodeCount() == electrodesAfterFirst, &
                 "assembly is not idempotent -- electrodes were double-registered")
  end block

  ! =====================================================================
  ! findElectrodeIndex: the lookup validateStudyReferences relies on to
  ! reject signal.observeElectrodes/outputs.electrodes typos. Exercises
  ! exactly the real-world mistake (common/README.md's discretised-ID
  ! gotcha): naming a mesh bar's own compound ID ("M-0000-0001") instead
  ! of one of its discretised segment IDs ("M-0000-0001_e1"/"_e2").
  ! =====================================================================

  call test_init("tStructure%findElectrodeIndex resolves discretised segment IDs only")

  call test_ok("a real discretised segment ID resolves", &
               structure%findElectrodeIndex("M-0000-0001_e1") > 0, &
               "expected the first segment of bar M-0000-0001 to be found")
  call test_ok("the bar's own (undiscretised) compound ID does not resolve", &
               structure%findElectrodeIndex("M-0000-0001") == 0, &
               "'M-0000-0001' is the bar ID, not a segment ID -- must not match")
  call test_ok("a nonexistent ID does not resolve", &
               structure%findElectrodeIndex("does-not-exist") == 0, &
               "unrelated string must not spuriously match")

  ! =====================================================================
  ! validateStudyReferences: positive path. A real common/ case with a
  ! valid signal block must pass through without raising. (There is no
  ! in-process way to assert the *failing* path -- raiseError triggers a
  ! critical, halting error, same limitation already noted in
  ! test_mesh_element.f90 for tMeshElement's own raiseError guards. The
  ! failing path is verified manually instead: run
  !   fpm run Tupa -- test/fixture_signal_bad_electrode.json
  ! which fails immediately with "mTupa: signal.observeElectrodes
  ! references unknown electrode 'm-0000-0001' (...)" instead of hanging
  ! through a full geometry-factor calculation and frequency sweep.)
  ! =====================================================================

  call test_init("validateStudyReferences: valid signal block passes through (ADR 0015 case)")

  block
    type(tStudy) :: study
    class(tSignal), allocatable :: signal
    character(len=256) :: signalSourceNode
    character(len=256), allocatable :: signalObserveNodeIds(:), signalObserveElectrodeIds(:)

    call loadStudy("../common/portela1997_transient.json", study, signal=signal, &
                   signalSourceNode=signalSourceNode, signalObserveNodeIds=signalObserveNodeIds, &
                   signalObserveElectrodeIds=signalObserveElectrodeIds)

    call validateStudyReferences(study, signal=signal, signalSourceNode=signalSourceNode, &
                                  signalObserveNodeIds=signalObserveNodeIds, &
                                  signalObserveElectrodeIds=signalObserveElectrodeIds)

    ! Reaching this line at all is the assertion: a bad reference would
    ! have halted the process inside validateStudyReferences above.
    call test_ok("validateStudyReferences returned control (no reference was rejected)", &
                 .true.)
    call test_ok("assembleStructure ran as a side effect of validateStudyReferences", &
                 study%structure%assembled, "structure should be assembled by now")
    call test_ok("Line_1_e1 (signal.observeElectrodes) resolves in the assembled structure", &
                 study%structure%findElectrodeIndex("Line_1_e1") > 0, &
                 "portela1997_transient.json's own observeElectrodes entry should resolve")
  end block

  call test_summary()

end program test_validation
