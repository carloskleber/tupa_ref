program test_segmentation
  !! Per-study segment-length target (`numerics.maxSegmentLength`, ROADMAP
  !! Phase 10 item 3, theory.md §4.1): the loader raises every element's
  !! segment count to ceil(length / target), and a coarse-vs-fine
  !! convergence test on the Portela 1997 conductor shows the accuracy/cost
  !! trade-off the knob exposes.
  use tupa
  use mStudy
  use mNode
  use mVerbosity, only: setVerbosity, VERB_QUIET, VERB_NORMAL
  use check
  implicit none

  character(len=*), parameter :: caseFile = "test_segmentation_case.json"
  real(8), parameter :: targets(5) = [5.0d0, 2.5d0, 1.0d0, 0.5d0, 0.25d0]
  complex(8) :: zin(5, 2)
  real(8) :: err(4, 2), ref
  integer :: k, f, nseg(5)
  type(tStudy) :: study

  ! ----------------------------------------------------------------
  ! Loader: segment counts follow the target
  ! ----------------------------------------------------------------
  call test_init("numerics.maxSegmentLength sets the segment counts")

  call writeCase(caseFile, "2.5", "")
  call loadStudy(caseFile, study)
  call study%structure%assembleStructure()
  call test_ok("10 m line, target 2.5 m, no `segments`: 4 electrodes", &
               study%structure%getElectrodeCount() == 4, "expected ceil(10/2.5) = 4 segments")

  call writeCase(caseFile, "3.0", '"segments": 2,')
  call loadStudy(caseFile, study)
  call study%structure%assembleStructure()
  call test_ok("target 3 m raises an explicit `segments: 2` to ceil(10/3) = 4", &
               study%structure%getElectrodeCount() == 4, "the target must raise the explicit count")

  call writeCase(caseFile, "20.0", '"segments": 6,')
  call loadStudy(caseFile, study)
  call study%structure%assembleStructure()
  call test_ok("a target above the length never coarsens an explicit `segments: 6`", &
               study%structure%getElectrodeCount() == 6, "the target only ever refines")

  call writeCase(caseFile, "", '"segments": 3,')
  call loadStudy(caseFile, study)
  call study%structure%assembleStructure()
  call test_ok("no numerics block: the explicit count is untouched", &
               study%structure%getElectrodeCount() == 3, "default behaviour must be unchanged")

  ! ----------------------------------------------------------------
  ! Convergence: Zin vs segment-length target at 1 kHz and 1 MHz
  ! ----------------------------------------------------------------
  call test_init("Coarse-vs-fine convergence of Zin with the segment-length target")

  call setVerbosity(VERB_QUIET)
  do k = 1, 5
    block
      character(len=16) :: t
      type(tStudy) :: s
      complex(8), allocatable :: z(:)
      write(t, '(F0.3)') targets(k)
      call writeCase(caseFile, trim(t), "")
      call runStudyFromFile(caseFile, s)
      z = s%inputImpedance("Node_1")
      nseg(k) = s%structure%getElectrodeCount()
      zin(k, 1) = z(1)       ! 1 kHz
      zin(k, 2) = z(size(z)) ! 1 MHz
    end block
  end do
  call setVerbosity(VERB_NORMAL)

  call test_ok("segment counts 2, 4, 10, 20, 40", all(nseg == [2, 4, 10, 20, 40]), "unexpected discretisation")
  do f = 1, 2
    ref = abs(zin(5, f))
    do k = 1, 4
      err(k, f) = abs(abs(zin(k, f)) - ref) / ref
    end do
    call test_ok("error decreases as the target shrinks from 5 m to 2.5 m to 1 m", &
                 err(1, f) > err(2, f) .and. err(2, f) > err(3, f), &
                 "|Zin| did not converge towards the 0.25 m reference over the coarse range")
    call test_ok("targets of 1 m and below stay within 1 % of the 0.25 m reference", &
                 err(3, f) < 1.0d-2 .and. err(4, f) < 1.0d-2, &
                 "unexpectedly large discretisation error at segment lengths <= 1 m")
  end do
  call test_ok("at 1 MHz the 5 m segments are visibly coarse: > 5 %", &
               err(1, 2) > 5.0d-2, "expected a percent-level error from 5 m segments at 1 MHz")
  print '(A)', "   |Zin| relative error vs the 0.25 m reference (1 kHz | 1 MHz):"
  do k = 1, 4
    print '(A,F5.2,A,ES10.2,A,ES10.2,A,SP,ES10.2,A,ES10.2)', "     target ", targets(k), " m:", err(k, 1), " |", err(k, 2), &
      "   signed ", (abs(zin(k, 1)) - abs(zin(5, 1))) / abs(zin(5, 1)), " |", (abs(zin(k, 2)) - abs(zin(5, 2))) / abs(zin(5, 2))
  end do

  block
    integer :: u
    open(newunit=u, file=caseFile, status="old")
    close(u, status="delete")
  end block

  call test_summary()

contains

  subroutine writeCase(file, maxLen, segmentsField)
    !! Write the Portela 1997 conductor with an optional numerics target.
    character(len=*), intent(in) :: file, maxLen, segmentsField
    integer :: u

    open(newunit=u, file=file, status="replace", action="write")
    write(u, '(A)') '{ "title": "segmentation test",'
    write(u, '(A)') '  "soil": { "conductivity": 0.01, "permittivity": 10.0, "permeability": 1.0 },'
    if (len_trim(maxLen) > 0) write(u, '(A)') '  "numerics": { "maxSegmentLength": ' // trim(maxLen) // ' },'
    write(u, '(A)') '  "nodes": [ { "id": "Node_1", "position": [0.0, 0.0, -0.5] },'
    write(u, '(A)') '             { "id": "Node_2", "position": [10.0, 0.0, -0.5] } ],'
    write(u, '(A)') '  "materials": [ { "id": "copper", "epsilonr": 1.0, "mur": 1.0, "sigma": 5.96e7 } ],'
    write(u, '(A)') '  "elements": [ { "type": "line", "id": "Line_1", "from": "Node_1", "to": "Node_2",'
    write(u, '(A)') '                  "radius": 0.007, ' // trim(segmentsField) // ' "material": "copper" } ],'
    write(u, '(A)') '  "sources": [ { "node": "Node_1", "current": { "re": 1.0, "im": 0.0 } } ],'
    write(u, '(A)') '  "frequencies": { "min": 1000.0, "max": 1.0e6, "pointsPerDecade": 1 } }'
    close(u, status="keep")
  end subroutine writeCase

end program test_segmentation
