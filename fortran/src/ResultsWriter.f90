module mResultsWriter
  !! CSV (primary) and JSON results writers for a solved `tStudy` sweep
  !! (ROADMAP.md Phase 3 item 3), plus the transient (time-domain) results
  !! writer (ADR 0015). The JSON shapes are frozen by
  !! [ADR 0012](../../docs/adr/0012-results-json-schema.md) (frequency
  !! domain) and [ADR 0015](../../docs/adr/0015-time-domain-signal-schema.md)
  !! (transient); the CSV shape is this writer's own choice (CONVENTIONS.md
  !! only fixes CSV as primary, not its columns) — tidy/long form (one row
  !! per axis-point x quantity x entity) so column count doesn't depend on
  !! node/electrode count.
  use mStudy
  use mElement, only: tElement
  use mElementChannel, only: tChannel
  use mError, only: raiseError
  implicit none
  private

  public :: writeResultsCsv, writeResultsJson
  public :: writeTransientResultsCsv, writeTransientResultsJson

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

  subroutine writeChannelsJson(unit, study)
    !! Write the optional `"channels"` block (ADR 0025): one entry per
    !! lightning-channel element with the loading the solver used — the
    !! per-segment series inductance and resistance, and the calibration
    !! outcome — so a run records its calibrated value. Writes nothing when
    !! the study has no channel, leaving other files unchanged.
    integer, intent(in) :: unit
    type(tStudy), intent(in) :: study
    class(tElement), pointer :: el
    integer :: i, k, n
    logical :: first

    first = .true.
    do i = 1, study%structure%getElementCount()
      el => study%structure%getElement(i)
      select type (el)
      type is (tChannel)
        if (.not. allocated(el%electrodes)) cycle
        if (first) write(unit, '(A)') '  "channels": ['
        if (.not. first) write(unit, '(A)') ","
        first = .false.
        n = size(el%electrodes)
        write(unit, '(A)', advance="no") '    { "id": "' // trim(el%id) // '", "calibrated": '
        if (el%calibrated) then
          write(unit, '(A)', advance="no") 'true, "scale": ' // trim(fmtReal(el%loadScale)) // &
                                            ', "measuredSpeed": ' // trim(fmtReal(el%calibratedSpeed))
        else
          write(unit, '(A)', advance="no") 'false'
        end if
        write(unit, '(A)', advance="no") ', "inductance": ['
        do k = 1, n
          write(unit, '(A)', advance="no") trim(fmtReal(el%electrodes(k)%loadInductance))
          if (k < n) write(unit, '(A)', advance="no") ", "
        end do
        write(unit, '(A)', advance="no") '], "resistance": ['
        do k = 1, n
          write(unit, '(A)', advance="no") trim(fmtReal(el%electrodes(k)%loadResistance))
          if (k < n) write(unit, '(A)', advance="no") ", "
        end do
        write(unit, '(A)', advance="no") "] }"
      end select
    end do
    if (.not. first) then
      write(unit, '(A)') ""
      write(unit, '(A)') "  ],"
    end if
  end subroutine writeChannelsJson

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
    call writeChannelsJson(unit, study)

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

  ! =====================================================================
  ! Transient (time-domain) results, ADR 0015
  ! =====================================================================

  subroutine writeTransientResultsCsv(sourceNodeIds, t, injectedCurrents, observeNodeIds, &
                                       nodeResponses, filename, observeElectrodeIds, i1Responses, i2Responses)
    !! Tidy (long) CSV for a `mTransient%transientResponseSources` run: one
    !! row per (time, quantity, entity), columns `time_s,quantity,id,value`.
    !! `quantity` is `injectedCurrent` (id = a source node; with several
    !! sources, one row per distinct node in first-appearance order, holding
    !! the net current injected there — sources sharing a node are summed,
    !! so (time, quantity, id) stays a unique key), `voltage` (id = an
    !! `observeNodeIds` entry), or `i1`/`i2` (id = an `observeElectrodeIds`
    !! entry, only written if present). No `title` column, matching
    !! `writeResultsCsv`'s convention of not carrying the study title into
    !! the tidy CSV.
    character(len=*), intent(in) :: sourceNodeIds(:)
    real(8), intent(in) :: t(:)
    real(8), intent(in) :: injectedCurrents(:,:)
    !! Shape (size(sourceNodeIds), size(t))
    character(len=*), intent(in) :: observeNodeIds(:)
    real(8), intent(in) :: nodeResponses(:,:)
    !! Shape (size(observeNodeIds), size(t))
    character(len=*), intent(in) :: filename
    character(len=*), intent(in), optional :: observeElectrodeIds(:)
    real(8), intent(in), optional :: i1Responses(:,:), i2Responses(:,:)
    !! Shape (size(observeElectrodeIds), size(t)) each, if present
    integer :: unit, nt, k, i, j
    character(len=256) :: id
    integer, allocatable :: firstOf(:)
    real(8) :: netCurrent

    ! firstOf(i) = index of the first source sharing sourceNodeIds(i)'s node
    allocate(firstOf(size(sourceNodeIds)))
    do i = 1, size(sourceNodeIds)
      firstOf(i) = i
      do j = 1, i - 1
        if (trim(sourceNodeIds(j)) == trim(sourceNodeIds(i))) then
          firstOf(i) = j
          exit
        end if
      end do
    end do

    nt = size(t)
    open(newunit=unit, file=filename, status="replace", action="write")
    write(unit, '(A)') "time_s,quantity,id,value"

    do k = 1, nt
      do i = 1, size(sourceNodeIds)
        if (firstOf(i) /= i) cycle
        netCurrent = injectedCurrents(i, k)
        do j = i + 1, size(sourceNodeIds)
          if (firstOf(j) == i) netCurrent = netCurrent + injectedCurrents(j, k)
        end do
        write(unit, '(A)') trim(fmtReal(t(k))) // ",injectedCurrent," // trim(sourceNodeIds(i)) // "," // &
          trim(fmtReal(netCurrent))
      end do
      do i = 1, size(observeNodeIds)
        id = trim(observeNodeIds(i))
        write(unit, '(A)') trim(fmtReal(t(k))) // ",voltage," // trim(id) // "," // &
          trim(fmtReal(nodeResponses(i, k)))
      end do
      if (present(observeElectrodeIds)) then
        do i = 1, size(observeElectrodeIds)
          id = trim(observeElectrodeIds(i))
          if (present(i1Responses)) write(unit, '(A)') trim(fmtReal(t(k))) // ",i1," // trim(id) // "," // &
            trim(fmtReal(i1Responses(i, k)))
          if (present(i2Responses)) write(unit, '(A)') trim(fmtReal(t(k))) // ",i2," // trim(id) // "," // &
            trim(fmtReal(i2Responses(i, k)))
        end do
      end if
    end do

    close(unit)
  end subroutine writeTransientResultsCsv

  subroutine writeTransientResultsJson(title, sourceNodeIds, t, injectedCurrents, observeNodeIds, &
                                        nodeResponses, filename, observeElectrodeIds, i1Responses, i2Responses, &
                                        study)
    !! Write a `mTransient%transientResponseSources` run as ADR 0015
    !! transient results JSON: `{title, sourceNode, time, injectedCurrent,
    !! [sources[{node,current}],] nodes[{id,voltage}], electrodes[{id,i1,i2}]}`.
    !! `sourceNode`/`injectedCurrent` describe the first source (the only
    !! one in a single-source run); `sources` lists every source and is
    !! written only when there is more than one (ADR 0015 amendment
    !! 2026-09-30), so single-source files are unchanged.
    character(len=*), intent(in) :: title
    character(len=*), intent(in) :: sourceNodeIds(:)
    real(8), intent(in) :: t(:)
    real(8), intent(in) :: injectedCurrents(:,:)
    !! Shape (size(sourceNodeIds), size(t))
    character(len=*), intent(in) :: observeNodeIds(:)
    real(8), intent(in) :: nodeResponses(:,:)
    !! Shape (size(observeNodeIds), size(t))
    character(len=*), intent(in) :: filename
    character(len=*), intent(in), optional :: observeElectrodeIds(:)
    real(8), intent(in), optional :: i1Responses(:,:), i2Responses(:,:)
    !! Shape (size(observeElectrodeIds), size(t)) each, if present
    type(tStudy), intent(in), optional :: study
    !! Source of the optional `"channels"` block (ADR 0025)
    integer :: unit, nt, i, k

    nt = size(t)
    open(newunit=unit, file=filename, status="replace", action="write")
    write(unit, '(A)') "{"
    write(unit, '(A)') '  "title": "' // trim(title) // '",'
    if (present(study)) call writeChannelsJson(unit, study)
    write(unit, '(A)') '  "sourceNode": "' // trim(sourceNodeIds(1)) // '",'

    write(unit, '(A)', advance="no") '  "time": ['
    do k = 1, nt
      write(unit, '(A)', advance="no") trim(fmtReal(t(k)))
      if (k < nt) write(unit, '(A)', advance="no") ", "
    end do
    write(unit, '(A)') "],"

    write(unit, '(A)', advance="no") '  "injectedCurrent": ['
    do k = 1, nt
      write(unit, '(A)', advance="no") trim(fmtReal(injectedCurrents(1, k)))
      if (k < nt) write(unit, '(A)', advance="no") ", "
    end do
    write(unit, '(A)') "],"

    if (size(sourceNodeIds) > 1) then
      write(unit, '(A)') '  "sources": ['
      do i = 1, size(sourceNodeIds)
        write(unit, '(A)', advance="no") '    { "node": "' // trim(sourceNodeIds(i)) // '", "current": ['
        do k = 1, nt
          write(unit, '(A)', advance="no") trim(fmtReal(injectedCurrents(i, k)))
          if (k < nt) write(unit, '(A)', advance="no") ", "
        end do
        write(unit, '(A)', advance="no") "] }"
        if (i < size(sourceNodeIds)) write(unit, '(A)') ","
      end do
      write(unit, '(A)') ""
      write(unit, '(A)') "  ],"
    end if

    write(unit, '(A)') '  "nodes": ['
    do i = 1, size(observeNodeIds)
      write(unit, '(A)', advance="no") '    { "id": "' // trim(observeNodeIds(i)) // '", "voltage": ['
      do k = 1, nt
        write(unit, '(A)', advance="no") trim(fmtReal(nodeResponses(i, k)))
        if (k < nt) write(unit, '(A)', advance="no") ", "
      end do
      write(unit, '(A)', advance="no") "] }"
      if (i < size(observeNodeIds)) write(unit, '(A)') ","
    end do
    write(unit, '(A)') ""
    if (present(observeElectrodeIds)) then
      write(unit, '(A)') "  ],"
      write(unit, '(A)') '  "electrodes": ['
      do i = 1, size(observeElectrodeIds)
        write(unit, '(A)', advance="no") '    { "id": "' // trim(observeElectrodeIds(i)) // '"'
        if (present(i1Responses)) then
          write(unit, '(A)', advance="no") ', "i1": ['
          do k = 1, nt
            write(unit, '(A)', advance="no") trim(fmtReal(i1Responses(i, k)))
            if (k < nt) write(unit, '(A)', advance="no") ", "
          end do
          write(unit, '(A)', advance="no") "]"
        end if
        if (present(i2Responses)) then
          write(unit, '(A)', advance="no") ', "i2": ['
          do k = 1, nt
            write(unit, '(A)', advance="no") trim(fmtReal(i2Responses(i, k)))
            if (k < nt) write(unit, '(A)', advance="no") ", "
          end do
          write(unit, '(A)', advance="no") "]"
        end if
        write(unit, '(A)', advance="no") " }"
        if (i < size(observeElectrodeIds)) write(unit, '(A)') ","
      end do
      write(unit, '(A)') ""
      write(unit, '(A)') "  ]"
    else
      write(unit, '(A)') "  ],"
      write(unit, '(A)') '  "electrodes": []'
    end if
    write(unit, '(A)') "}"

    close(unit)
  end subroutine writeTransientResultsJson

end module mResultsWriter
