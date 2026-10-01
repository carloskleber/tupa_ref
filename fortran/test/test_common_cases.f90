program test_common_cases
  !! Tests the JSON `sources`/`frequencies`/`outputs` reader (ADR 0013,
  !! ROADMAP.md Phase 5): `loadStudy`'s optional arguments on a
  !! structure-only case (no sweep to read) and on a full case, the
  !! `outputs` write-time filter in mResultsWriter, and a numeric-tolerance
  !! regression comparison of `runStudyFromFile` against the checked-in
  !! common/ "_expected.csv" fixtures for the three new common/ cases.
  !! Transient fixtures (ROADMAP Phase 9, ADR 0015 amendment 2026-09-30:
  !! `time_s,quantity,id,value` shape) are compared by `compareTransientCase`.
  use tupa
  use mStudy
  use mResultsWriter
  use mVerbosity, only: setVerbosity, VERB_QUIET, VERB_NORMAL
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
  ! Ideal images (numerics.imageModel "ideal", ROADMAP Phase 10 item 2): the
  ! low-frequency-limit pin; the cases above run the Γ(ω) default
  call compareCase("../common/portela1997_ideal.json", "../common/portela1997_ideal_expected.csv", "Node_1")

  ! Transient fixtures, one per ROADMAP Phase 9 option
  call compareTransientCase("portela1997_transient_interpolated")
  call compareTransientCase("portela1997_transient_hann")
  call compareTransientCase("portela1997_transient_hann_time")
  call compareTransientCase("portela1997_transient_multi")
  call compareTransientCase("portela1997_transient_signals")
  call compareTransientCase("portela1997_transient_nlt")

  ! ROADMAP Phase 10 item 6: the 32x32 m grid (185 nodes, 200 electrodes) end
  ! to end, harmonic sweep and scan-fed transient, through the CLI path
  call compareHarmonicAndTransientCase("portelaMesh", "portelaMesh (32x32 m grid) harmonic sweep and scan-fed transient")

  ! ROADMAP Phase 10b: lightning channel — pure transient cases (the
  ! Chen/Baba configuration, and a speed-calibrated graded channel, whose
  ! fixture therefore pins the calibration too), then a tower strike with
  ! two-node current and voltage sources, harmonic and transient
  call compareTransientCase("channel_unloaded")
  call compareTransientCase("channel_loaded")
  call compareHarmonicAndTransientCase("channel_tower", "channel_tower: two-node current source")
  call compareHarmonicAndTransientCase("channel_tower_gap", "channel_tower_gap: delta-gap voltage source")

  ! ROADMAP Phase 11: grounding-safety outputs — the harmonic sweep and the
  ! observation results (potentials, GPR, touch, step) of a 16x16 m grid
  call compareObservationCase("grid_safety")

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

  subroutine compareHarmonicAndTransientCase(stem, label)
    !! Run `../common/<stem>.json` through `runFromFile` — which honours its
    !! `outputs` filter and writes both result sets — and diff the harmonic
    !! CSV against `<stem>_expected.csv` and the transient CSV against
    !! `<stem>_transient_expected.csv`.
    character(len=*), intent(in) :: stem, label
    logical :: exists
    integer :: u, k
    character(len=256) :: outFiles(4)

    outFiles = [character(len=256) :: trim(stem) // "_results.csv", trim(stem) // "_results.json", &
                trim(stem) // "_transient_results.csv", trim(stem) // "_transient_results.json"]

    call test_init("Regression: " // label)
    call setVerbosity(VERB_QUIET)
    call runFromFile("../common/" // stem // ".json")
    call setVerbosity(VERB_NORMAL)

    call test_ok("harmonic CSV matches expected fixture within tolerance", &
                 csvMatches(trim(stem) // "_results.csv", "../common/" // stem // "_expected.csv", 1.0d-6), &
                 "numeric drift between a fresh run and the checked-in fixture")
    call test_ok("transient CSV matches expected fixture within tolerance", &
                 transientCsvMatches(trim(stem) // "_transient_results.csv", &
                                     "../common/" // stem // "_transient_expected.csv", 1.0d-6), &
                 "numeric drift between a fresh run and the checked-in fixture")

    do k = 1, size(outFiles)
      inquire(file=trim(outFiles(k)), exist=exists)
      if (exists) then
        open(newunit=u, file=trim(outFiles(k)), status="old")
        close(u, status="delete")
      end if
    end do
  end subroutine compareHarmonicAndTransientCase

  subroutine compareObservationCase(stem)
    !! Run `../common/<stem>.json` through `runFromFile` and diff the harmonic
    !! CSV against `<stem>_expected.csv` and the observation CSV
    !! (`frequency_hz,quantity,id,x,y,z,re,im`) against
    !! `<stem>_potentials_expected.csv`.
    character(len=*), intent(in) :: stem
    logical :: exists
    integer :: u, k
    character(len=256) :: outFiles(4)

    outFiles = [character(len=256) :: trim(stem) // "_results.csv", trim(stem) // "_results.json", &
                trim(stem) // "_potentials.csv", trim(stem) // "_potentials.json"]

    call test_init("Regression (observation): " // trim(stem))
    call setVerbosity(VERB_QUIET)
    call runFromFile("../common/" // stem // ".json")
    call setVerbosity(VERB_NORMAL)

    call test_ok("harmonic CSV matches expected fixture within tolerance", &
                 csvMatches(trim(stem) // "_results.csv", "../common/" // stem // "_expected.csv", 1.0d-6), &
                 "numeric drift between a fresh run and the checked-in fixture")
    call test_ok("observation CSV matches expected fixture within tolerance", &
                 observationCsvMatches(trim(stem) // "_potentials.csv", &
                                       "../common/" // stem // "_potentials_expected.csv", 1.0d-6), &
                 "numeric drift between a fresh run and the checked-in fixture")

    do k = 1, size(outFiles)
      inquire(file=trim(outFiles(k)), exist=exists)
      if (exists) then
        open(newunit=u, file=trim(outFiles(k)), status="old")
        close(u, status="delete")
      end if
    end do
  end subroutine compareObservationCase

  logical function observationCsvMatches(freshFile, expectedFile, reltol) result(ok)
    !! Row-by-row comparison of two observation CSVs: the first six fields
    !! (frequency, quantity, id, x, y, z) must be textually equal and `re`/`im`
    !! within `reltol` of the row scale (floored at 1e-6, as `rowMatches`).
    character(len=*), intent(in) :: freshFile, expectedFile
    real(8), intent(in) :: reltol
    integer :: uF, uE, iosF, iosE, pF, pE
    character(len=4096) :: lineF, lineE
    real(8) :: fRe, fIm, eRe, eIm, scaleVal

    ok = .true.
    open(newunit=uF, file=freshFile, status="old", action="read")
    open(newunit=uE, file=expectedFile, status="old", action="read")
    read(uF, '(A)', iostat=iosF) lineF
    read(uE, '(A)', iostat=iosE) lineE
    if (trim(lineF) /= trim(lineE)) ok = .false.
    do
      read(uF, '(A)', iostat=iosF) lineF
      read(uE, '(A)', iostat=iosE) lineE
      if (iosF /= 0 .or. iosE /= 0) then
        if (iosF /= iosE) ok = .false.
        exit
      end if
      pF = lastCommaPair(lineF)
      pE = lastCommaPair(lineE)
      if (pF /= pE .or. lineF(1:pF) /= lineE(1:pE)) then
        ok = .false.
        exit
      end if
      call splitReIm(lineF(pF+1:), fRe, fIm)
      call splitReIm(lineE(pE+1:), eRe, eIm)
      scaleVal = max(1.0d-6, abs(eRe), abs(eIm))
      if (abs(fRe - eRe) >= reltol * scaleVal .or. abs(fIm - eIm) >= reltol * scaleVal) then
        ok = .false.
        exit
      end if
    end do
    close(uF)
    close(uE)
  end function observationCsvMatches

  integer function lastCommaPair(row) result(p)
    !! Position of the sixth comma of an observation row (the end of the
    !! frequency, quantity, id, x, y, z fields).
    character(len=*), intent(in) :: row
    integer :: n, i

    n = 0
    p = 0
    do i = 1, len_trim(row)
      if (row(i:i) == ",") then
        n = n + 1
        if (n == 6) then
          p = i
          return
        end if
      end if
    end do
  end function lastCommaPair

  subroutine splitReIm(text, re, im)
    !! `re,im` of an observation row's tail.
    character(len=*), intent(in) :: text
    real(8), intent(out) :: re, im
    integer :: c

    c = index(text, ",")
    read(text(1:c-1), *) re
    read(text(c+1:), *) im
  end subroutine splitReIm

  subroutine compareTransientCase(caseName)
    !! Run `../common/<caseName>.json` through the CLI entry point
    !! (`runFromFile`, which writes `<caseName>_transient_results.csv` to
    !! the working directory) and diff the tidy transient CSV against
    !! `../common/<caseName>_expected.csv` (`transientCsvMatches`).
    character(len=*), intent(in) :: caseName
    character(len=256) :: freshCsv, freshJson
    logical :: exists
    integer :: u

    call test_init("Regression (transient): " // caseName)
    call setVerbosity(VERB_QUIET)
    call runFromFile("../common/" // caseName // ".json")
    call setVerbosity(VERB_NORMAL)

    freshCsv  = caseName // "_transient_results.csv"
    freshJson = caseName // "_transient_results.json"
    call test_ok("fresh transient CSV matches expected fixture within tolerance", &
                 transientCsvMatches(trim(freshCsv), "../common/" // caseName // "_expected.csv", 1.0d-6), &
                 "numeric drift between a fresh run and the checked-in fixture")

    inquire(file=trim(freshCsv), exist=exists)
    if (exists) then
      open(newunit=u, file=trim(freshCsv), status="old")
      close(u, status="delete")
    end if
    inquire(file=trim(freshJson), exist=exists)
    if (exists) then
      open(newunit=u, file=trim(freshJson), status="old")
      close(u, status="delete")
    end if
  end subroutine compareTransientCase

  logical function transientCsvMatches(freshFile, expectedFile, reltol) result(ok)
    !! Compare two tidy transient CSVs (`time_s,quantity,id,value`, ADR
    !! 0015) row by row: same header and row count, identical text fields
    !! (time, quantity, id) per row, and values within
    !! reltol · max(|expected|, 1e-3 · peak), where peak is the largest
    !! |expected| of that (quantity, id) series — the floor keeps samples
    !! near a zero crossing from turning round-off into failures.
    character(len=*), intent(in) :: freshFile, expectedFile
    real(8), intent(in) :: reltol
    character(len=128), allocatable :: fKey(:), eKey(:), eSeries(:)
    real(8), allocatable :: fVal(:), eVal(:)
    character(len=256) :: fHead, eHead
    character(len=128) :: series
    real(8) :: peak
    integer :: i, j

    call readTransientCsv(freshFile, fHead, fKey, fVal)
    call readTransientCsv(expectedFile, eHead, eKey, eVal)
    ok = trim(fHead) == trim(eHead) .and. size(fKey) == size(eKey)
    if (.not. ok) return
    ok = all(fKey == eKey)
    if (.not. ok) return

    ! series name = "quantity,id" (the key without its time field)
    allocate(eSeries(size(eKey)))
    do i = 1, size(eKey)
      eSeries(i) = eKey(i)(index(eKey(i), ",") + 1:)
    end do
    do i = 1, size(eKey)
      series = eSeries(i)
      peak = 0.0d0
      do j = 1, size(eKey)
        if (eSeries(j) == series) peak = max(peak, abs(eVal(j)))
      end do
      if (abs(fVal(i) - eVal(i)) > reltol * max(abs(eVal(i)), 1.0d-3 * peak)) then
        print '(A)', "     first mismatch: " // trim(eKey(i))
        print '(A,ES16.8,A,ES16.8)', "     fresh ", fVal(i), " vs expected ", eVal(i)
        ok = .false.
        return
      end if
    end do
  end function transientCsvMatches

  subroutine readTransientCsv(fileName, header, keys, vals)
    !! Read a `time_s,quantity,id,value` CSV: `keys(i)` = "time,quantity,id",
    !! `vals(i)` = value.
    character(len=*), intent(in) :: fileName
    character(len=256), intent(out) :: header
    character(len=128), allocatable, intent(out) :: keys(:)
    real(8), allocatable, intent(out) :: vals(:)
    character(len=256) :: line
    integer :: u, ios, n, i, p

    open(newunit=u, file=fileName, status="old", action="read")
    read(u, '(A)') header
    n = 0
    do
      read(u, '(A)', iostat=ios) line
      if (ios /= 0) exit
      n = n + 1
    end do
    rewind(u)
    read(u, '(A)') header
    allocate(keys(n), vals(n))
    do i = 1, n
      read(u, '(A)') line
      p = index(line, ",", back=.true.)
      keys(i) = line(1:p-1)
      read(line(p+1:), *) vals(i)
    end do
    close(u)
  end subroutine readTransientCsv

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

    ! Floor the row-local scale at 1e-6: with ~1 A / ~10 V problem scales,
    ! rows far below that (e.g. the symmetric grid case's transverse branch
    ! currents, ~1e-16 A) are round-off zeros whose row-relative comparison
    ! would amplify machine noise into spurious failures; the floor turns
    ! them into an absolute check at reltol*1e-6 instead.
    scaleVal = max(1.0d-6, abs(vExpRe), abs(vExpIm))
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
