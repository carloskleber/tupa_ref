module mElementCatenary
  !! Sagging conductor between two nodes (overhead shield wire or phase
  !! conductor span), discretised into straight segments like `tLine`
  !! (ROADMAP.md Phase 13 item 1, ADR 0023).
  !!
  !! The profile is the legacy Matlab `Catenaria.m` one: a parabola hanging
  !! below the chord, with the given `sag` at midspan and uniform spacing
  !! along the chord (theory.md §4.4). For end nodes at the same height it
  !! is the legacy geometry exactly.
  use mElement
  use mElementLine
  use mStructure
  use mError, only: raiseError
  use mCtes, only: newl
  implicit none
  private

  public :: newElementCatenary

  type, extends(tLine), public :: tCatenary
    !! Parabolic span: a `tLine` whose chain nodes are lowered by the sag profile.
    real(8) :: sag
    !! Downward displacement at midspan (m); negative bows upward
  contains
    procedure :: assemble     => assembleCatenary
    procedure :: report       => reportCatenary
    procedure :: nodePosition => nodePositionCatenary
  end type tCatenary

contains

  function newElementCatenary(id, idNodeStart, idNodeEnd, sag, radius, nElectrodes, idMaterial) result(this)
    !! Construct a `tCatenary` element; references are resolved at assembly.
    class(tElement), allocatable :: this
    type(tCatenary), allocatable :: cat
    character(len=*), intent(in) :: id
    !! Element identifier
    character(len=*), intent(in) :: idNodeStart
    !! User ID of the start node
    character(len=*), intent(in) :: idNodeEnd
    !! User ID of the end node
    real(8), intent(in) :: sag
    !! Midspan sag below the chord (m)
    real(8), intent(in) :: radius
    !! Cylindrical radius of all electrode segments (m)
    integer(4), intent(in) :: nElectrodes
    !! Number of straight segments
    character(len=*), intent(in) :: idMaterial
    !! User ID of the conductor material

    allocate(cat)
    cat%radius        = radius
    cat%nElectrodes   = nElectrodes
    cat%nNodes        = nElectrodes + 1
    cat%id            = id
    cat%idNodeStart   = idNodeStart
    cat%idNodeEnd     = idNodeEnd
    cat%idMaterial    = idMaterial
    cat%sag           = sag
    call move_alloc(cat, this)
  end function newElementCatenary

  function nodePositionCatenary(this, pStart, pEnd, k) result(p)
    !! Chord point at s = k/n lowered by 4·sag·s·(1 - s) (theory.md §4.4).
    class(tCatenary), intent(in) :: this
    real(8), intent(in) :: pStart(3)
    !! Start boundary-node position (m)
    real(8), intent(in) :: pEnd(3)
    !! End boundary-node position (m)
    integer(4), intent(in) :: k
    !! Chain node index, 0 (start) to nElectrodes (end)
    real(8) :: p(3)
    real(8) :: s

    s = real(k, kind=8) / real(this%nElectrodes, kind=8)
    p = this%tLine%nodePosition(pStart, pEnd, k)
    p(3) = p(3) - 4.0d0 * this%sag * s * (1.0d0 - s)
  end function nodePositionCatenary

  subroutine assembleCatenary(this, structure)
    !! Reject a span that leaves its end nodes' half-space, then discretise
    !! it as a `tLine` with the sag profile.
    class(tCatenary), intent(inout), target :: this
    class(*), intent(inout) :: structure
    integer(4) :: idxStart, idxEnd, k
    real(8) :: pStart(3), pEnd(3), p(3)

    select type (structure)
    type is (tStructure)
      idxStart = structure%findNodeIndex(trim(this%idNodeStart))
      idxEnd   = structure%findNodeIndex(trim(this%idNodeEnd))
      if (idxStart /= 0 .and. idxEnd /= 0 .and. this%nElectrodes > 1) then
        pStart = structure%nodes(idxStart)%p
        pEnd   = structure%nodes(idxEnd)%p
        do k = 1, this%nElectrodes - 1
          p = this%nodePosition(pStart, pEnd, k)
          if ((min(pStart(3), pEnd(3)) >= 0.0d0 .and. p(3) < 0.0d0) .or. &
              (max(pStart(3), pEnd(3)) <= 0.0d0 .and. p(3) > 0.0d0)) then
            call raiseError("tCatenary '" // trim(this%id) // &
              "': the sag profile crosses the air-soil interface (z = 0)")
            return
          end if
        end do
      end if
    end select

    ! Not `this%tLine%assemble`: the parent component's dynamic type is tLine,
    ! which would bind the straight `nodePosition`.
    call assembleLine(this, structure)
  end subroutine assembleCatenary

  subroutine reportCatenary(this, str)
    !! Line report plus the sag.
    class(tCatenary), intent(in) :: this
    character(:), allocatable, intent(inout) :: str
    !! Accumulator string — text is appended
    character(len=64) :: buf

    write(buf, '(F0.3)') this%sag
    str = str // "Catenary, sag " // trim(buf) // " m" // newl
    call this%tLine%report(str)
  end subroutine reportCatenary

end module mElementCatenary
