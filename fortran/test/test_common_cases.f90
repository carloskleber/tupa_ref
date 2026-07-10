program test_common_cases
  !! Tests the JSON `sources`/`frequencies`/`outputs` reader (ADR 0013,
  !! ROADMAP.md Phase 5): `loadStudy`'s optional arguments on a
  !! structure-only case (no sweep to read) and on a full case, the
  !! `outputs` write-time filter in mResultsWriter, and a numeric-tolerance
  !! regression comparison of `runStudyFromFile` against the checked-in
  !! common/ "_expected.csv" fixtures for the three new common/ cases.
  use tupa
  use mStudy
  use mResultsWriter
  use check
  implicit none

  character(len=256), allocatable :: sourceNodeIds(:), outputQuantities(:)
  complex(8), allocatable :: sourceCurrents(:)
  real(8), allocatable :: freqHz(:)
  type(tStudy) :: study

  ! ----------------------------------------------------------------
  ! Structure-only case (no sources/frequencies block): optional
  ! arguments must stay unallocated, matching pre-ADR-0013 behaviour.
  ! ----------------------------------------------------------------
  call test_init("loadStudy: structure-only case leaves sweep args unallocated")

  call loadStudy("../common/buried_conductor_short.json", study, sourceNodeIds=sourceNodeIds, &
                 sourceCurrents=sourceCurrents, freqHz=freqHz)

  call test_ok("sourceNodeIds not allocated (no 'sources' block)", &
               .not. allocated(sourceNodeIds), "buried_conductor_short.json has no sources block")
  call test_ok("freqHz not allocated (no 'frequencies' block)", &
               .not. allocated(freqHz), "buried_conductor_short.json has no frequencies block")
  call test_ok("structure still parsed (2 nodes)", &
               study%structure%getNodeCount() == 2, "structure-only parsing regressed")

  ! ----------------------------------------------------------------
  ! Full case: sources/frequencies/outputs blocks parsed correctly
  ! (ADR 0013).
  ! ----------------------------------------------------------------
  call test_init("loadStudy: sources/frequencies/outputs blocks (ADR 0013)")

  call loadStudy("../common/portela1997.json", study, sourceNodeIds=sourceNodeIds, &
                 sourceCurrents=sourceCurrents, freqHz=freqHz, &
                 outputQuantities=outputQuantities)

  call test_ok("one source parsed", size(sourceNodeIds) == 1, "wrong source count")
  call test_ok("source node id == Node_1", trim(sourceNodeIds(1)) == "Node_1", &
               "wrong source node id")
  call test_ok("source current == 1+0j", abs(sourceCurrents(1) - cmplx(1.0d0, 0.0d0, kind=8)) < 1.0d-12, &
               "wrong source current")
  call test_ok("frequency axis: 6 points (10 Hz - 1 MHz, 1/decade)", size(freqHz) == 6, &
               "pointsPerDecade -> nPoints conversion (ADR 0013) is wrong")
  call test_ok("frequency axis starts at 10 Hz", abs(freqHz(1) - 10.0d0) < 1.0d-6, &
               "wrong frequency axis start")
  call test_ok("frequency axis ends at 1 MHz", abs(freqHz(size(freqHz)) - 1.0d6) < 1.0d0, &
               "wrong frequency axis end")
  call test_ok("outputs.quantities has 4 entries", size(outputQuantities) == 4, &
               "wrong quantities count")

  ! ----------------------------------------------------------------
  ! runStudyFromFile end to end + outputs write-time filtering
  ! ----------------------------------------------------------------
  call test_init("runStudyFromFile + writeResultsCsv outputs filter (ADR 0013)")

  call runStudyFromFile("../common/portela1997.json", study)

  block
    character(len=*), parameter :: filtered = "test_common_filtered.csv"
    integer :: unit, ios, nLines
    logical :: onlyNode1Voltage
    character(len=256) :: line

    call writeResultsCsv(study, filtered, nodeIds=["Node_1"], quantities=["voltage"])

    open(newunit=unit, file=filtered, status="old", action="read")
    read(unit, '(A)') line ! header
    nLines = 0
    onlyNode1Voltage = .true.
    do
      read(unit, '(A)', iostat=ios) line
      if (ios /= 0) exit
      nLines = nLines + 1
      if (index(line, ",voltage,Node_1,") == 0) onlyNode1Voltage = .false.
    end do
    close(unit, status="delete")

    call test_ok("filtered CSV has exactly one row per frequency", &
                 nLines == size(study%sweepFreqHz), "row count does not match nodeIds/quantities filter")
    call test_ok("every filtered row is Node_1 voltage", onlyNode1Voltage, &
                 "writeResultsCsv did not honour the nodeIds/quantities filter")
  end block

  ! ----------------------------------------------------------------
  ! Regression comparison against checked-in common/ "_expected.csv"
  ! fixtures (golden files: this implementation's own output, not an
  ! independent physics oracle -- see ROADMAP.md Phase 5 / P3).
  ! ----------------------------------------------------------------
  call compareCase("../common/portela1997.json", "../common/portela1997_expected.csv", "Node_1")
  call compareCase("../common/rod.json", "../common/rod_expected.csv", "Node_1")
  call compareCase("../common/grid.json", "../common/grid_expected.csv", "Node_A")

  call test_summary()

contains

  subroutine compareCase(jsonFile, expectedCsv, sourceNode)
    !! Run `jsonFile` end to end, write a fresh CSV, and diff it numerically
    !! (relative tolerance) against the checked-in `expectedCsv` fixture.
    !! Also re-asserts passivity as a physical sanity check independent of
    !! the golden file.
    character(len=*), intent(in) :: jsonFile, expectedCsv, sourceNode
    character(len=*), parameter :: freshCsv = "test_common_fresh.csv"
    type(tStudy) :: caseStudy
    complex(8), allocatable :: zin(:)
    integer :: k

    call test_init("Regression: " // trim(jsonFile) // " vs " // trim(expectedCsv))

    call runStudyFromFile(jsonFile, caseStudy)
    call writeResultsCsv(caseStudy, freshCsv)

    call test_ok("fresh CSV matches expected fixture within tolerance", &
                 csvMatches(freshCsv, expectedCsv, 1.0d-6), &
                 "numeric drift between a fresh run and the checked-in fixture")

    zin = caseStudy%inputImpedance(sourceNode)
    do k = 1, size(zin)
      call test_ok("Re(Zin) >= 0 (passivity) at sweep point " // itoa(k), &
                   real(zin(k)) >= -1.0d-9 * max(1.0d0, abs(zin(k))), &
                   "input impedance must not have negative real part")
    end do

    block
      logical :: exists
      inquire(file=freshCsv, exist=exists)
      if (exists) then
        open(unit=99, file=freshCsv, status="old")
        close(99, status="delete")
      end if
    end block
  end subroutine compareCase

  logical function csvMatches(freshFile, expectedFile, reltol) result(ok)
    !! Compare two tidy-CSV files (mResultsWriter's `frequency_hz,quantity,
    !! id,re,im` shape) row by row: same row count, same quantity/id per
    !! row, and `re`/`im` equal within a relative tolerance.
    character(len=*), intent(in) :: freshFile, expectedFile
    real(8), intent(in) :: reltol
    integer :: uFresh, uExpected, iosF, iosE
    character(len=4096) :: lineF, lineE

    ok = .true.
    open(newunit=uFresh, file=freshFile, status="old", action="read")
    open(newunit=uExpected, file=expectedFile, status="old", action="read")

    read(uFresh, '(A)', iostat=iosF) lineF
    read(uExpected, '(A)', iostat=iosE) lineE
    if (trim(lineF) /= trim(lineE)) ok = .false.

    do
      read(uFresh, '(A)', iostat=iosF) lineF
      read(uExpected, '(A)', iostat=iosE) lineE
      if (iosF /= 0 .or. iosE /= 0) then
        if (iosF /= iosE) ok = .false. ! different row counts
        exit
      end if
      if (.not. rowMatches(lineF, lineE, reltol)) then
        ok = .false.
        exit
      end if
    end do

    close(uFresh)
    close(uExpected)
  end function csvMatches

  logical function rowMatches(rowFresh, rowExpected, reltol) result(ok)
    !! Compare one `frequency_hz,quantity,id,re,im` row: exact match on the
    !! text fields (frequency_hz, quantity, id), relative-tolerance match
    !! on re/im.
    character(len=*), intent(in) :: rowFresh, rowExpected
    real(8), intent(in) :: reltol
    character(len=64) :: fFreq, fQty, fId, fRe, fIm
    character(len=64) :: eFreq, eQty, eId, eRe, eIm
    real(8) :: vFreshRe, vFreshIm, vExpRe, vExpIm, scaleVal

    call splitCsvRow(rowFresh, fFreq, fQty, fId, fRe, fIm)
    call splitCsvRow(rowExpected, eFreq, eQty, eId, eRe, eIm)

    ok = (trim(fQty) == trim(eQty)) .and. (trim(fId) == trim(eId))
    if (.not. ok) return

    read(fRe, *) vFreshRe
    read(fIm, *) vFreshIm
    read(eRe, *) vExpRe
    read(eIm, *) vExpIm

    scaleVal = max(1.0d-12, abs(vExpRe), abs(vExpIm))
    ok = abs(vFreshRe - vExpRe) < reltol * scaleVal .and. &
         abs(vFreshIm - vExpIm) < reltol * scaleVal
  end function rowMatches

  subroutine splitCsvRow(row, freqStr, qty, id, reStr, imStr)
    !! Split a `frequency_hz,quantity,id,re,im` CSV row into its 5 fields.
    character(len=*), intent(in) :: row
    character(len=*), intent(out) :: freqStr, qty, id, reStr, imStr
    integer :: p1, p2, p3, p4

    p1 = index(row, ",")
    p2 = p1 + index(row(p1+1:), ",")
    p3 = p2 + index(row(p2+1:), ",")
    p4 = p3 + index(row(p3+1:), ",")

    freqStr = row(1:p1-1)
    qty     = row(p1+1:p2-1)
    id      = row(p2+1:p3-1)
    reStr   = row(p3+1:p4-1)
    imStr   = row(p4+1:)
  end subroutine splitCsvRow

  function itoa(i) result(s)
    integer, intent(in) :: i
    character(len=12) :: s
    write(s, '(I0)') i
  end function itoa

end program test_common_cases
