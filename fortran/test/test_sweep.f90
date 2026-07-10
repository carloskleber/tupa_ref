program test_sweep
  !! Tests for the frequency sweep, result storage, and CSV/JSON writers
  !! (ROADMAP.md Phase 3), using the Portela-1997-parameter buried conductor
  !! already validated in test_solve.f90.
  use mCtes
  use mStudy
  use mResultsWriter
  use mNode
  use mMaterial
  use mElementLine
  use check
  implicit none

  type(tStudy) :: study
  class(tMaterial), allocatable :: mat
  class(tElement), allocatable :: elem
  real(8), parameter :: length = 10.0d0, r0 = 0.007d0, depth = 0.5d0
  real(8), parameter :: sigmaSoil = 0.01d0, epsrSoil = 10.0d0
  real(8), allocatable :: freqHz(:), vmax(:)
  complex(8), allocatable :: zin(:), zinFromRun(:)
  character(len=*), parameter :: csvFile = "test_sweep_output.csv"
  character(len=*), parameter :: jsonFile = "test_sweep_output.json"
  integer :: k, idx, unit
  logical :: exists
  character(len=4096) :: line

  study%title = "Phase 3 sweep test - buried conductor (Portela 1997 parameters)"
  call study%structure%addNode(newNode("Node_1", [0.0d0, 0.0d0, -depth]))
  call study%structure%addNode(newNode("Node_2", [length, 0.0d0, -depth]))

  mat = newMaterialLinear("copper", 1.0d0, 1.0d0, 5.96d7)
  call study%structure%addMaterial(mat)
  study%structure%soil = newMaterialLinear("soil", epsrSoil, 1.0d0, sigmaSoil)

  elem = newElementLine("Line_1", "Node_1", "Node_2", r0, 10, "copper")
  call study%structure%addElement(elem)

  ! ----------------------------------------------------------------
  ! Log-spaced default frequency axis
  ! ----------------------------------------------------------------
  call test_init("logFrequencyAxis: log-spaced default axis")

  freqHz = logFrequencyAxis(1.0d2, 1.0d6, 5)

  call test_ok("5 points requested", size(freqHz) == 5, "wrong point count")
  call test_ok("first point == fMin", abs(freqHz(1) - 1.0d2) < 1.0d-6, "axis does not start at fMin")
  call test_ok("last point == fMax", abs(freqHz(5) - 1.0d6) < 1.0d0, "axis does not end at fMax")
  call test_ok("log-spaced: equal ratio between consecutive points", &
               abs(freqHz(2) / freqHz(1) - freqHz(3) / freqHz(2)) < 1.0d-9, &
               "consecutive ratios should be constant for a log-spaced axis")

  ! ----------------------------------------------------------------
  ! runSweep: results match what a manual run() loop would give
  ! ----------------------------------------------------------------
  call test_init("runSweep matches a manual per-frequency run() loop")

  call study%runSweep(freqHz, ["Node_1"], [cmplx(1.0d0, 0.0d0, kind=8)])

  idx = study%structure%findNodeIndex("Node_1")
  allocate(zinFromRun(size(freqHz)))
  do k = 1, size(freqHz)
    call study%run(2.0d0 * PI * freqHz(k), ["Node_1"], [cmplx(1.0d0, 0.0d0, kind=8)])
    zinFromRun(k) = study%mesh%voltage(idx)
  end do

  zin = study%inputImpedance("Node_1")

  call test_ok("inputImpedance size matches sweep", size(zin) == size(freqHz), "wrong result count")
  do k = 1, size(freqHz)
    call test_ok("Zin matches manual run() at point " // trim(freqStr(freqHz(k))), &
                 abs(zin(k) - zinFromRun(k)) < 1.0d-9 * max(1.0d0, abs(zinFromRun(k))), &
                 "runSweep result diverges from an equivalent manual run() call")
  end do

  ! ----------------------------------------------------------------
  ! maxVoltageMagnitude: at the injection node, output must be >= |V| there
  ! ----------------------------------------------------------------
  call test_init("maxVoltageMagnitude bounds the injection-node voltage")

  vmax = study%maxVoltageMagnitude()
  call test_ok("size matches sweep", size(vmax) == size(freqHz), "wrong result count")
  do k = 1, size(freqHz)
    call test_ok("vmax >= |Zin| (1 A injected) at " // trim(freqStr(freqHz(k))), &
                 vmax(k) >= abs(zin(k)) - 1.0d-9 * max(1.0d0, abs(zin(k))), &
                 "max voltage magnitude must be at least the injection node's own voltage")
  end do

  ! ----------------------------------------------------------------
  ! CSV writer: file exists, has a header, and one row per (freq, entity, quantity)
  ! ----------------------------------------------------------------
  call test_init("writeResultsCsv produces a well-formed file")

  call writeResultsCsv(study, csvFile)

  inquire(file=csvFile, exist=exists)
  call test_ok("CSV file was created", exists, "writeResultsCsv did not create the output file")

  if (exists) then
    open(newunit=unit, file=csvFile, status="old", action="read")
    read(unit, '(A)') line
    call test_ok("CSV header matches expected columns", &
                 trim(line) == "frequency_hz,quantity,id,re,im", &
                 "unexpected CSV header: " // trim(line))

    block
      integer :: nLines, ios, expected
      nLines = 0
      do
        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        nLines = nLines + 1
      end do
      ! 1 voltage row/node + 2 current rows/electrode (i1,i2), per frequency.
      ! tLine discretisation adds internal nodes, so don't hardcode counts.
      expected = (study%voltageResults%entityCount() + 2 * study%longCurrentResults%entityCount()) * size(freqHz)
      call test_ok("CSV row count matches (nodes + 2*electrodes) * frequencies", &
                   nLines == expected, &
                   "unexpected CSV row count")
    end block
    close(unit, status="delete")
  end if

  ! ----------------------------------------------------------------
  ! JSON writer: file exists and round-trips the frequency count / title
  ! ----------------------------------------------------------------
  call test_init("writeResultsJson produces a well-formed file (ADR 0012 v0 shape)")

  call writeResultsJson(study, jsonFile)

  inquire(file=jsonFile, exist=exists)
  call test_ok("JSON file was created", exists, "writeResultsJson did not create the output file")

  if (exists) then
    block
      character(len=65536) :: whole
      integer :: ios, n
      logical :: hasTitle, hasFrequencies, hasNodes, hasElectrodes, hasDerived

      open(newunit=unit, file=jsonFile, status="old", action="read")
      whole = ""
      do
        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        whole = trim(whole) // trim(line)
      end do
      close(unit, status="delete")

      hasTitle       = index(whole, '"title"') > 0
      hasFrequencies = index(whole, '"frequencies"') > 0
      hasNodes       = index(whole, '"nodes"') > 0
      hasElectrodes  = index(whole, '"electrodes"') > 0
      hasDerived     = index(whole, '"inputImpedance"') > 0

      call test_ok("JSON has title", hasTitle, "missing title key")
      call test_ok("JSON has frequencies", hasFrequencies, "missing frequencies key")
      call test_ok("JSON has nodes", hasNodes, "missing nodes key")
      call test_ok("JSON has electrodes", hasElectrodes, "missing electrodes key")
      call test_ok("JSON has derived.inputImpedance", hasDerived, "missing derived.inputImpedance key")

      n = 0
      block
        integer :: pos
        pos = 1
        do
          idx = index(whole(pos:), '"re"')
          if (idx == 0) exit
          n = n + 1
          pos = pos + idx + 3
        end do
      end block
      call test_ok("JSON has at least one complex value written", n > 0, "no {re,im} pairs found")
    end block
  end if

  call test_summary()

contains

  function freqStr(f) result(s)
    real(8), intent(in) :: f
    character(len=32) :: s
    write(s, '(F0.1," Hz")') f
  end function freqStr

end program test_sweep
