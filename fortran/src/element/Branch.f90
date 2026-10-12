module mElementBranch
  !! Lumped series R-L-C branch element (`"type": "branch"`, theory.md §6,
  !! ADR 0028): a two-terminal circuit element between two existing nodes,
  !! or between one node and remote earth, with no geometry and no
  !! electromagnetic coupling to any segment.
  !!
  !! It plants no nodes and no electrodes: `assemble` only resolves the node
  !! IDs and registers a `tBranch` with the structure, and the study layer
  !! stamps its admittance into the Kirchhoff rows of the nodal system at
  !! every frequency (`mMesh::stampBranch`). The legacy Matlab "Impedancia"
  !! mechanism (an extra branch unknown with a zeroed `Z_t` row) is
  !! equivalent; the admittance stamp needs no extra unknown.
  use mElement
  use mStructure
  use mBranch
  use mError, only: raiseError
  use mCtes, only: newl
  implicit none
  private

  public :: newElementBranch

  type, extends(tElement), public :: tBranchElement
    !! Series branch `Z(s) = R + sL + 1/(sC)` from `idNodeA` to `idNodeB`.
    character(len=256) :: idNodeA = ""
    !! User-assigned ID of the first node
    character(len=256) :: idNodeB = ""
    !! User-assigned ID of the second node; blank = remote earth
    real(8) :: resistance = 0.0d0
    !! R (Ω)
    real(8) :: inductance = 0.0d0
    !! L (H)
    real(8) :: capacitance = 0.0d0
    !! C (F), 0 = none
  contains
    procedure :: assemble => assembleBranch
    procedure :: report   => reportBranch
  end type tBranchElement

contains

  function newElementBranch(id, idNodeA, idNodeB, resistance, inductance, capacitance) result(this)
    !! Construct a `tBranchElement`. Validates the values here, at load time,
    !! so no error is ever raised from inside a threaded sweep: R, L, C must
    !! be non-negative, and at least one must be present (a branch with no
    !! impedance is a short circuit, which the stamp cannot represent).
    class(tElement), allocatable :: this
    character(len=*), intent(in) :: id, idNodeA, idNodeB
    real(8), intent(in) :: resistance, inductance, capacitance
    type(tBranchElement), allocatable :: br

    allocate(br)
    br%id          = id
    br%idNodeA     = idNodeA
    br%idNodeB     = idNodeB
    br%resistance  = resistance
    br%inductance  = inductance
    br%capacitance = capacitance
    br%radius      = 0.0d0
    br%nNodes      = 0
    br%nElectrodes = 0
    if (resistance < 0.0d0 .or. inductance < 0.0d0 .or. capacitance < 0.0d0) then
      call raiseError("tBranch '" // trim(id) // "': resistance, inductance and capacitance must be non-negative")
    else if (resistance == 0.0d0 .and. inductance == 0.0d0 .and. capacitance == 0.0d0) then
      call raiseError("tBranch '" // trim(id) // "': needs a nonzero resistance, inductance or capacitance " // &
                      "(a short circuit cannot be stamped; use a small resistance)")
    end if
    call move_alloc(br, this)
  end function newElementBranch

  subroutine assembleBranch(this, structure)
    !! Resolve the node IDs against the parent structure and register the
    !! branch (node index 0 = remote earth).
    class(tBranchElement), intent(inout), target :: this
    class(*), intent(inout) :: structure
    type(tBranch) :: branch
    integer(4) :: idxA, idxB

    select type (structure)
    type is (tStructure)
      idxA = structure%findNodeIndex(trim(this%idNodeA))
      if (idxA == 0) then
        call raiseError("tBranch '" // trim(this%id) // "': node '" // trim(this%idNodeA) // "' not found")
        return
      end if
      idxB = 0
      if (len_trim(this%idNodeB) > 0) then
        idxB = structure%findNodeIndex(trim(this%idNodeB))
        if (idxB == 0) then
          call raiseError("tBranch '" // trim(this%id) // "': node '" // trim(this%idNodeB) // "' not found")
          return
        end if
        if (idxB == idxA) then
          call raiseError("tBranch '" // trim(this%id) // "': both terminals are the same node")
          return
        end if
      end if
      branch%id          = this%id
      branch%nodeA       = idxA
      branch%nodeB       = idxB
      branch%resistance  = this%resistance
      branch%inductance  = this%inductance
      branch%capacitance = this%capacitance
      call structure%addBranch(branch)
    end select
  end subroutine assembleBranch

  subroutine reportBranch(this, str)
    !! Human-readable summary of the branch, appended to `str`.
    class(tBranchElement), intent(in) :: this
    character(:), allocatable, intent(inout) :: str
    character(len=128) :: buf

    str = str // "Element ID: " // trim(this%id) // ", Branch: " // trim(this%idNodeA) // " -> "
    if (len_trim(this%idNodeB) > 0) then
      str = str // trim(this%idNodeB)
    else
      str = str // "remote earth"
    end if
    write(buf, '("R = ",ES10.3," ohm, L = ",ES10.3," H, C = ",ES10.3," F")') &
      this%resistance, this%inductance, this%capacitance
    str = str // ", " // trim(buf) // newl
  end subroutine reportBranch

end module mElementBranch
