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

  logical function wanted(name, list) result(ok)
    !! True if `name` should be included: always, when `list` is not
    !! present (ADR 0013 — omitting a selector means "everything"), or when
    !! `name` appears in `list` (used for both entity IDs and quantity names).
    character(len=*), intent(in) :: name
    character(len=*), intent(in), optional :: list(:)
    integer :: i

    if (.not. present(list)) then
      ok = .true.
      return
    end if
    ok = .false.
    do i = 1, size(list)
      if (trim(list(i)) == trim(name)) then
        ok = .true.
        return
      end if
    end do
  end function wanted

  subroutine writeResultsCsv(study, filename, nodeIds, electrodeIds, quantities)
    !! Write every sweep result in tidy (long) form: one row per
    !! (frequency, quantity, entity), columns
    !! `frequency_hz,quantity,id,re,im`. `quantity` is one of
    !! `voltage`/`i1`/`i2` (i1 = longitudinal, i2 = transverse current,
    !! theory.md §6 naming). `nodeIds`/`electrodeIds`/`quantities` are the
    !! ADR 0013 `outputs` selection — omitted (the default) means every
    !! node/electrode/quantity, matching pre-ADR-0013 behaviour exactly.
    type(tStudy), intent(in) :: study
    character(len=*), intent(in) :: filename
    character(len=*), intent(in), optional :: nodeIds(:)
    !! Node IDs to include (all, if omitted)
    character(len=*), intent(in), optional :: electrodeIds(:)
    !! Electrode IDs to include (all, if omitted)
    character(len=*), intent(in), optional :: quantities(:)
    !! Quantity names to include: "voltage"/"i1"/"i2" (all, if omitted)
    integer :: unit, nf, nno, nseg, i, k
    complex(8) :: v
    character(len=256) :: id

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
        id = trim(study%voltageResults%entityId(i))
        if (.not. (wanted(trim(id), nodeIds) .and. wanted("voltage", quantities))) cycle
        v = study%voltageResults%get(i, k)
        write(unit, '(A)') trim(fmtReal(study%sweepFreqHz(k))) // ",voltage," // &
          trim(id) // "," // &
          trim(fmtReal(real(v))) // "," // trim(fmtReal(aimag(v)))
      end do
      do i = 1, nseg
        id = trim(study%longCurrentResults%entityId(i))
        if (.not. wanted(trim(id), electrodeIds)) cycle

        if (wanted("i1", quantities)) then
          v = study%longCurrentResults%get(i, k)
          write(unit, '(A)') trim(fmtReal(study%sweepFreqHz(k))) // ",i1," // &
            trim(id) // "," // &
            trim(fmtReal(real(v))) // "," // trim(fmtReal(aimag(v)))
        end if

        if (wanted("i2", quantities)) then
          v = study%transCurrentResults%get(i, k)
          write(unit, '(A)') trim(fmtReal(study%sweepFreqHz(k))) // ",i2," // &
            trim(id) // "," // &
            trim(fmtReal(real(v))) // "," // trim(fmtReal(aimag(v)))
        end if
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

  subroutine writeResultsJson(study, filename, nodeIds, electrodeIds, quantities)
    !! Write the study's last sweep as ADR 0012 v0 JSON:
    !! `{title, frequencies, nodes[{id,voltage}], electrodes[{id,i1,i2}],
    !! derived{inputImpedance}}`. `derived.inputImpedance` uses the sweep's
    !! first source node (ADR 0012 does not define a multi-port derived
    !! quantity); omitted if the sweep injected no source. `nodeIds`/
    !! `electrodeIds`/`quantities` are the ADR 0013 `outputs` selection —
    !! omitted (the default) means every node/electrode/quantity, matching
    !! pre-ADR-0013 behaviour exactly.
    type(tStudy), intent(in) :: study
    character(len=*), intent(in) :: filename
    character(len=*), intent(in), optional :: nodeIds(:)
    !! Node IDs to include (all, if omitted)
    character(len=*), intent(in), optional :: electrodeIds(:)
    !! Electrode IDs to include (all, if omitted)
    character(len=*), intent(in), optional :: quantities(:)
    !! Quantity names to include: "voltage"/"i1"/"i2"/"inputImpedance" (all, if omitted)
    integer :: unit, nf, nno, nseg, i, k
    logical :: first, wantI1, wantI2
    complex(8), allocatable :: zin(:)
    character(len=256) :: id

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
    first = .true.
    do i = 1, nno
      id = trim(study%voltageResults%entityId(i))
      if (.not. (wanted(trim(id), nodeIds) .and. wanted("voltage", quantities))) cycle
      if (.not. first) write(unit, '(A)') ","
      first = .false.
      write(unit, '(A)', advance="no") '    { "id": "' // trim(id) // '", "voltage": ['
      do k = 1, nf
        write(unit, '(A)', advance="no") fmtComplexJson(study%voltageResults%get(i, k))
        if (k < nf) write(unit, '(A)', advance="no") ", "
      end do
      write(unit, '(A)', advance="no") "] }"
    end do
    if (.not. first) write(unit, '(A)') ""
    write(unit, '(A)') "  ],"

    write(unit, '(A)') '  "electrodes": ['
    first = .true.
    do i = 1, nseg
      id = trim(study%longCurrentResults%entityId(i))
      wantI1 = wanted(trim(id), electrodeIds) .and. wanted("i1", quantities)
      wantI2 = wanted(trim(id), electrodeIds) .and. wanted("i2", quantities)
      if (.not. (wantI1 .or. wantI2)) cycle
      if (.not. first) write(unit, '(A)') ","
      first = .false.
      write(unit, '(A)', advance="no") '    { "id": "' // trim(id) // '"'
      if (wantI1) then
        write(unit, '(A)', advance="no") ', "i1": ['
        do k = 1, nf
          write(unit, '(A)', advance="no") fmtComplexJson(study%longCurrentResults%get(i, k))
          if (k < nf) write(unit, '(A)', advance="no") ", "
        end do
        write(unit, '(A)', advance="no") "]"
      end if
      if (wantI2) then
        write(unit, '(A)', advance="no") ', "i2": ['
        do k = 1, nf
          write(unit, '(A)', advance="no") fmtComplexJson(study%transCurrentResults%get(i, k))
          if (k < nf) write(unit, '(A)', advance="no") ", "
        end do
        write(unit, '(A)', advance="no") "]"
      end if
      write(unit, '(A)', advance="no") " }"
    end do
    if (.not. first) write(unit, '(A)') ""
    write(unit, '(A)') "  ],"

    write(unit, '(A)') '  "derived": {'
    if (allocated(study%sweepSourceIds) .and. wanted("inputImpedance", quantities)) then
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
