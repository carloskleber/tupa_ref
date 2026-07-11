module mElementLine
  !! Straight-line conductor element with equally spaced electrode segments.
  !!
  !! A `tLine` element connects two boundary nodes by a chain of `nElectrodes`
  !! equally-spaced cylindrical segments (electrodes). All segments have the same
  !! radius and material. After assembly, the internal nodes are created and
  !! registered with the parent structure.
  use mElement
  use mStructure
  use mNode
  use mMaterial
  use mElectrode
  use mError, only: raiseError
  use mCtes, only: newl
  implicit none
  private

  public :: newElementLine

  type, extends(tElement), public :: tLine
    !! Straight conductor spanning two nodes with evenly-spaced internal segments.
    character(len=256) :: idNodeStart
    !! User-assigned ID of the start boundary node
    character(len=256) :: idNodeEnd
    !! User-assigned ID of the end boundary node
    character(len=256) :: idMaterial
    !! User-assigned ID of the material
    type(tNode), pointer :: nodeStart => null()
    !! Pointer to the start node (resolved during assembly)
    type(tNode), pointer :: nodeEnd => null()
    !! Pointer to the end node (resolved during assembly)
  contains
    procedure :: assemble => assembleLine
    procedure :: report   => reportLine
  end type tLine

contains

  function newElementLine(id, idNodeStart, idNodeEnd, radius, nElectrodes, idMaterial) result(this)
    !! Construct a `tLine` element with the given endpoints and electrode count.
    !!
    !! Node and material pointers are not yet resolved; they are set by the
    !! `assembleLine` subroutine after the element is added to a tStructure.
    class(tElement), allocatable :: this
    !! Result allocated as a polymorphic tElement (caller may move-alloc to tLine)
    type(tLine), allocatable :: line
    character(len=*), intent(in) :: id
    !! Element identifier
    character(len=*), intent(in) :: idNodeStart
    !! User ID of the start node
    character(len=*), intent(in) :: idNodeEnd
    !! User ID of the end node
    real(8), intent(in) :: radius
    !! Cylindrical radius of all electrode segments (m)
    integer(4), intent(in) :: nElectrodes
    !! Number of segments (= number of internal nodes + 1)
    character(len=*), intent(in) :: idMaterial
    !! User ID of the material to assign to all segments

    allocate(line)
    line%radius        = radius
    line%nElectrodes   = nElectrodes
    line%nNodes        = nElectrodes + 1
    line%id            = id
    line%idNodeStart   = idNodeStart
    line%idNodeEnd     = idNodeEnd
    line%idMaterial    = idMaterial
    call move_alloc(line, this)
  end function newElementLine

  subroutine assembleLine(this, structure)
    !! Discretise the line into `nElectrodes` equally-spaced cylindrical segments.
    !!
    !! Resolves the boundary node and material IDs against the parent structure,
    !! creates `nElectrodes-1` internal nodes equally spaced between them, and
    !! creates `nElectrodes` electrode segments connecting consecutive nodes.
    !! New nodes and electrodes are registered both locally (`this%nodes`,
    !! `this%electrodes`, for reporting) and globally (`structure%nodes`,
    !! `structure%electrodes`, for the mesh solver).
    class(tLine), intent(inout), target :: this
    class(*), intent(inout) :: structure
    integer(4) :: idxStart, idxEnd
    class(tMaterial), pointer :: mat
    real(8) :: pStart(3), pEnd(3), inc(3)
    integer(4), allocatable :: nodeIdx(:)
    type(tElectrode) :: electrode
    type(tNode) :: internalNode
    character(len=256) :: buf
    integer(4) :: k

    select type (structure)
    type is (tStructure)
      idxStart = structure%findNodeIndex(trim(this%idNodeStart))
      if (idxStart == 0) then
        call raiseError("tLine '" // trim(this%id) // "': start node '" // &
          trim(this%idNodeStart) // "' not found")
        return
      end if

      idxEnd = structure%findNodeIndex(trim(this%idNodeEnd))
      if (idxEnd == 0) then
        call raiseError("tLine '" // trim(this%id) // "': end node '" // &
          trim(this%idNodeEnd) // "' not found")
        return
      end if

      mat => structure%findMaterial(trim(this%idMaterial))
      if (.not. associated(mat)) then
        call raiseError("tLine '" // trim(this%id) // "': material '" // &
          trim(this%idMaterial) // "' not found")
        return
      end if
      allocate(this%material, source=mat)

      pStart = structure%nodes(idxStart)%p
      pEnd   = structure%nodes(idxEnd)%p
      inc    = (pEnd - pStart) / real(this%nElectrodes, kind=8)

      ! Build the chain of node indices spanning the line: boundary, internal..., boundary
      allocate(nodeIdx(this%nElectrodes + 1))
      nodeIdx(1) = idxStart
      nodeIdx(this%nElectrodes + 1) = idxEnd

      if (this%nElectrodes > 1) then
        allocate(this%nodes(this%nElectrodes - 1))
        do k = 1, this%nElectrodes - 1
          write(buf, '(A,"_n",I0)') trim(this%id), k
          internalNode = newNode(trim(buf), pStart + real(k, kind=8) * inc)
          call structure%addNode(internalNode)
          nodeIdx(k + 1) = structure%getNodeCount()
          this%nodes(k) = internalNode
        end do
      end if

      allocate(this%electrodes(this%nElectrodes))
      do k = 1, this%nElectrodes
        write(buf, '(A,"_e",I0)') trim(this%id), k
        electrode = newElectrode(trim(buf), nodeIdx(k), nodeIdx(k + 1))
        electrode%radius = this%radius
        electrode%material => this%material
        call structure%addElectrode(electrode)
        this%electrodes(k) = electrode
      end do
    end select
  end subroutine assembleLine

  subroutine reportLine(this, str)
    !! Build a human-readable summary of the line element and append to `str`.
    class(tLine), intent(in) :: this
    character(:), allocatable, intent(inout) :: str
    !! Accumulator string — text is appended
    character(len=128) :: buf
    integer :: i

    ! Header: element ID, material, node count, radius
    str = str // "Element ID: " // trim(this%id) // &
      ", Material: " // trim(this%idMaterial)

    write(buf, '(I0)') this%nNodes
    str = str // ", Nodes: " // trim(buf)

    write(buf, '(F0.3)') this%radius
    str = str // ", Radius: " // trim(buf) // " m" // newl

    ! Electrode details
    str = str // new_line('a') // "  Electrodes:"
    if (allocated(this%electrodes)) then
      do i = 1, this%nElectrodes
        write(buf, '(I0)') i
        str = str // newl // "    Electrode " // trim(buf) // &
          ": ID = " // trim(this%electrodes(i)%id)
      end do
      str = str // newl
    else
      str = str // " None" // newl
    end if
  end subroutine reportLine

end module mElementLine
