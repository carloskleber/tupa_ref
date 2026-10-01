program test_parallel
  !! Frequency-level parallelism of `tStudy%runSweep` (ROADMAP Phase 10 item
  !! 4, §7 P6): the sweep is bit-identical for any thread count, current- and
  !! voltage-source paths alike. Without OpenMP (`-fopenmp`) the thread
  !! switches below are no-ops and the test degenerates to a repeatability
  !! check of the serial loop.
  !$ use omp_lib
  use mCtes
  use mStudy
  use mNode
  use mMaterial
  use mElementLine
  use mVerbosity, only: setVerbosity, VERB_QUIET, VERB_NORMAL
  use check
  implicit none

  integer :: nThreads

  call setVerbosity(VERB_QUIET)
  nThreads = 1
  !$ nThreads = max(2, omp_get_max_threads())

  call test_init("runSweep: results independent of the thread count")
  call compareSweeps(.false.)
  call compareSweeps(.true.)

  call setVerbosity(VERB_NORMAL)
  call test_summary()

contains

  subroutine compareSweeps(voltageSource)
    logical, intent(in) :: voltageSource
    type(tStudy) :: serial, threaded
    complex(8), allocatable :: zS(:), zT(:)
    real(8), allocatable :: freqHz(:)
    logical, allocatable :: isV(:)
    character(len=24) :: label
    integer :: i, k, nf
    logical :: same

    freqHz = logFrequencyAxis(1.0d2, 1.0d7, 41)
    nf = size(freqHz)
    label = merge("voltage source", "current source", voltageSource)

    !$ call omp_set_num_threads(1)
    call buildStudy(serial)
    if (voltageSource) then
      call serial%runSweep(freqHz, ["Node_1"], [cmplx(100.0d0, 0.0d0, kind=8)], [.true.])
    else
      call serial%runSweep(freqHz, ["Node_1"], [cmplx(1.0d0, 0.0d0, kind=8)])
    end if

    !$ call omp_set_num_threads(nThreads)
    call buildStudy(threaded)
    if (voltageSource) then
      call threaded%runSweep(freqHz, ["Node_1"], [cmplx(100.0d0, 0.0d0, kind=8)], [.true.])
    else
      call threaded%runSweep(freqHz, ["Node_1"], [cmplx(1.0d0, 0.0d0, kind=8)])
    end if

    same = .true.
    do k = 1, nf
      do i = 1, serial%structure%getNodeCount()
        if (serial%voltageResults%get(i, k) /= threaded%voltageResults%get(i, k)) same = .false.
      end do
      do i = 1, serial%structure%getElectrodeCount()
        if (serial%longCurrentResults%get(i, k) /= threaded%longCurrentResults%get(i, k)) same = .false.
        if (serial%transCurrentResults%get(i, k) /= threaded%transCurrentResults%get(i, k)) same = .false.
      end do
    end do
    call test_ok(trim(label) // ": voltages and currents bit-identical, 1 vs N threads", same, &
                 "threaded sweep differs from the serial one")

    zS = serial%inputImpedance("Node_1")
    zT = threaded%inputImpedance("Node_1")
    call test_ok(trim(label) // ": input impedance bit-identical", all(zS == zT), &
                 "input impedance differs between serial and threaded sweeps")
    call test_ok(trim(label) // ": study mesh left at the last frequency", &
                 all(serial%mesh%voltage == threaded%mesh%voltage), &
                 "post-sweep mesh state differs")
    !$ call omp_set_num_threads(omp_get_num_procs())
  end subroutine compareSweeps

  subroutine buildStudy(study)
    type(tStudy), intent(out) :: study
    class(tMaterial), allocatable :: mat
    class(tElement), allocatable :: elem

    study%title = "parallel sweep test"
    call study%structure%addNode(newNode("Node_1", [0.0d0, 0.0d0, -0.5d0]))
    call study%structure%addNode(newNode("Node_2", [10.0d0, 0.0d0, -0.5d0]))
    call study%structure%addNode(newNode("Node_3", [5.0d0, 5.0d0, -0.5d0]))
    mat = newMaterialLinear("copper", 1.0d0, 1.0d0, 5.96d7)
    call study%structure%addMaterial(mat)
    study%structure%soil = newMaterialLinear("soil", 10.0d0, 1.0d0, 0.01d0)
    elem = newElementLine("Line_1", "Node_1", "Node_2", 0.007d0, 8, "copper")
    call study%structure%addElement(elem)
    elem = newElementLine("Line_2", "Node_1", "Node_3", 0.007d0, 6, "copper")
    call study%structure%addElement(elem)
  end subroutine buildStudy

end program test_parallel
