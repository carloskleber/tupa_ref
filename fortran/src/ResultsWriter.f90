module mResultsWriter
  !! CSV (primary) and JSON results writers for a solved `tStudy` sweep
  !! (ROADMAP.md Phase 3 item 3). The JSON shape is frozen by
  !! [ADR 0012](../../docs/adr/0012-results-json-schema.md); the CSV shape
  !! is this writer's own choice (CONVENTIONS.md only fixes CSV as primary,
  !! not its columns) — tidy/long form (one row per frequency x quantity x
  !! entity) so column count doesn't depend on node/electrode count.
  use mStudy
  use mError, only: raiseError
  implicit none
  private

  public :: writeResultsCsv, writeResultsJson

contains

  function fmtReal(x) result(s)
    !! Format a real(8) value for CSV/JSON output.
    real(8), intent(in) :: x
    character(len=24) :: s

    write(s, '(ES16.8)') x
    s = adjustl(s)
  end function fmtReal

  logical function hasSweepResults(study) result(ok)
    !! True once `runSweep` has populated the study's result sets.
    type(tStudy), intent(in) :: study

    ok = study%voltageResults%frequencyCount() > 0
  end function hasSweepResults

  subroutine writeResultsCsv(study, filename)
    !! Write every sweep result in tidy (long) form: one row per
    !! (frequency, quantity, entity), columns
    !! `frequency_hz,quantity,id,re,im`. `quantity` is one of
    !! `voltage`/`i1`/`i2` (i1 = longitudinal, i2 = transverse current,
    !! theory.md §6 naming).
    type(tStudy), intent(in) :: study
    character(len=*), intent(in) :: filename
    integer :: unit, nf, nno, nseg, i, k
    complex(8) :: v

    if (.not. hasSweepResults(study)) then
      call raiseError("writeResultsCsv: study has no sweep results (call runSweep first)")
      return
    end if

    nf   = study%voltageResults%frequencyCount()
    nno  = study%voltageResults%entityCount()
    nseg = study%longCurrentResults%entityCount()

    open(newunit=unit, file=filename, status="replace", action="write")
    write(unit, '(A)') "frequency_hz,quantity,id,re,im"

    do k = 1, nf
      do i = 1, nno
        v = study%voltageResults%get(i, k)
        write(unit, '(A)') trim(fmtReal(study%sweepFreqHz(k))) // ",voltage," // &
          trim(study%voltageResults%entityId(i)) // "," // &
          trim(fmtReal(real(v))) // "," // trim(fmtReal(aimag(v)))
      end do
      do i = 1, nseg
        v = study%longCurrentResults%get(i, k)
        write(unit, '(A)') trim(fmtReal(study%sweepFreqHz(k))) // ",i1," // &
          trim(study%longCurrentResults%entityId(i)) // "," // &
          trim(fmtReal(real(v))) // "," // trim(fmtReal(aimag(v)))

        v = study%transCurrentResults%get(i, k)
        write(unit, '(A)') trim(fmtReal(study%sweepFreqHz(k))) // ",i2," // &
          trim(study%transCurrentResults%entityId(i)) // "," // &
          trim(fmtReal(real(v))) // "," // trim(fmtReal(aimag(v)))
      end do
    end do

    close(unit)
  end subroutine writeResultsCsv

  function fmtComplexJson(v) result(s)
    !! `{"re": .., "im": ..}` object matching the input schema's complex
    !! convention (common/README.md), reused by ADR 0012 for outputs.
    complex(8), intent(in) :: v
    character(len=:), allocatable :: s

    s = '{"re": ' // trim(fmtReal(real(v))) // ', "im": ' // trim(fmtReal(aimag(v))) // '}'
  end function fmtComplexJson

  subroutine writeResultsJson(study, filename)
    !! Write the study's last sweep as ADR 0012 v0 JSON:
    !! `{title, frequencies, nodes[{id,voltage}], electrodes[{id,i1,i2}],
    !! derived{inputImpedance}}`. `derived.inputImpedance` uses the sweep's
    !! first source node (ADR 0012 does not define a multi-port derived
    !! quantity); omitted if the sweep injected no source.
    type(tStudy), intent(in) :: study
    character(len=*), intent(in) :: filename
    integer :: unit, nf, nno, nseg, i, k
    complex(8), allocatable :: zin(:)

    if (.not. hasSweepResults(study)) then
      call raiseError("writeResultsJson: study has no sweep results (call runSweep first)")
      return
    end if

    nf   = study%voltageResults%frequencyCount()
    nno  = study%voltageResults%entityCount()
    nseg = study%longCurrentResults%entityCount()

    open(newunit=unit, file=filename, status="replace", action="write")
    write(unit, '(A)') "{"
    write(unit, '(A)') '  "title": "' // trim(study%title) // '",'

    write(unit, '(A)', advance="no") '  "frequencies": ['
    do k = 1, nf
      write(unit, '(A)', advance="no") trim(fmtReal(study%sweepFreqHz(k)))
      if (k < nf) write(unit, '(A)', advance="no") ", "
    end do
    write(unit, '(A)') "],"

    write(unit, '(A)') '  "nodes": ['
    do i = 1, nno
      write(unit, '(A)', advance="no") '    { "id": "' // trim(study%voltageResults%entityId(i)) // '", "voltage": ['
      do k = 1, nf
        write(unit, '(A)', advance="no") fmtComplexJson(study%voltageResults%get(i, k))
        if (k < nf) write(unit, '(A)', advance="no") ", "
      end do
      if (i < nno) then
        write(unit, '(A)') "] },"
      else
        write(unit, '(A)') "] }"
      end if
    end do
    write(unit, '(A)') "  ],"

    write(unit, '(A)') '  "electrodes": ['
    do i = 1, nseg
      write(unit, '(A)', advance="no") '    { "id": "' // trim(study%longCurrentResults%entityId(i)) // '", "i1": ['
      do k = 1, nf
        write(unit, '(A)', advance="no") fmtComplexJson(study%longCurrentResults%get(i, k))
        if (k < nf) write(unit, '(A)', advance="no") ", "
      end do
      write(unit, '(A)', advance="no") '], "i2": ['
      do k = 1, nf
        write(unit, '(A)', advance="no") fmtComplexJson(study%transCurrentResults%get(i, k))
        if (k < nf) write(unit, '(A)', advance="no") ", "
      end do
      if (i < nseg) then
        write(unit, '(A)') "] },"
      else
        write(unit, '(A)') "] }"
      end if
    end do
    write(unit, '(A)') "  ],"

    write(unit, '(A)') '  "derived": {'
    if (allocated(study%sweepSourceIds)) then
      zin = study%inputImpedance(trim(study%sweepSourceIds(1)))
      write(unit, '(A)', advance="no") '    "inputImpedance": ['
      do k = 1, nf
        write(unit, '(A)', advance="no") fmtComplexJson(zin(k))
        if (k < nf) write(unit, '(A)', advance="no") ", "
      end do
      write(unit, '(A)') "]"
    end if
    write(unit, '(A)') "  }"
    write(unit, '(A)') "}"

    close(unit)
  end subroutine writeResultsJson

end module mResultsWriter
