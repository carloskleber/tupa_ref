module mElementMesh
  !! Rectangular grounding-mesh grid ("grid/mesh generator element",
  !! ROADMAP.md §7): a composite element that plants its own axis-aligned
  !! grid of "main" nodes and wires them with `tLine` bars along both axes
  !! (`mElementLine`) — one call per bar to the existing bar-assembly logic,
  !! not a re-implementation of it.
  !!
  !! Named `tMeshElement` (JSON `"type": "mesh"`) rather than reusing
  !! `tMesh` to avoid colliding with the unrelated linear-system type of
  !! that name in `Mesh.f90`.
  !!
  !! Design note (legacy comparison, background only —
  !! `CLAUDE.local.md`): the Matlab `Malha.m` "malha" command connects
  !! nodes via an index formula that is only correct for a *square* grid
  !! (every shipped `.est` case using it happens to be square, masking the
  !! bug); the C++ `elementos::Malha` is self-consistent and documents a
  !! node/segment naming mnemonic, reused here:
  !!
  !!     main node: "<mesh id>-<row:02d><col:02d>"
  !!     bar id:    "<mesh id>-<row1:02d><col1:02d>-<row2:02d><col2:02d>"
  !!
  !! (0-based row/col indices.) Main nodes are externally referenceable —
  !! by a `sources[].node` current injection, or by another element's
  !! `from`/`to` — which is why they are created directly (like a line's
  !! own internal nodes) rather than requiring the user to pre-declare them
  !! in the top-level `nodes` array.
  use mElement
  use mStructure
  use mNode
  use mMaterial
  use mElementLine
  use mError, only: raiseError
  implicit none
  private

  public :: newElementMesh

  integer(4), parameter :: MAX_MESH_ROWS = 100
  !! rowsX/rowsY upper bound: row/col indices are 0-based and 2-digit
  !! zero-padded in the ID mnemonic, so at most 100 each (indices 0..99)

  type, extends(tElement), public :: tMeshElement
    !! Rectangular grid of crossed `tLine` bars spanning `lengthX` x
    !! `lengthY` from `position` (one corner), at constant depth/height
    !! `position(3)`.
    real(8) :: position(3)
    !! Corner of the grid [x, y, z] (m); z > 0 places it in air, z < 0 in
    !! soil (theory.md §2) — z = 0 is rejected at assembly time
    real(8) :: lengthX, lengthY
    !! Extent of the grid along X and Y (m)
    integer(4) :: rowsX, rowsY
    !! Number of bars parallel to the X axis (rowsX, spaced along Y) and
    !! to the Y axis (rowsY, spaced along X); >= 2 each
    integer(4) :: segments
    !! Electrode segments per bar (passed through to each `tLine`)
    character(len=256) :: idMaterial
    !! User-assigned ID of the material shared by every bar
  contains
    procedure :: assemble => assembleMesh
    procedure :: report   => reportMesh
  end type tMeshElement

contains

  function newElementMesh(id, position, lengthX, lengthY, rowsX, rowsY, &
                           radius, segments, idMaterial) result(this)
    !! Construct a `tMeshElement`. Geometry/material are validated and
    !! expanded later, in `assembleMesh`.
    class(tElement), allocatable :: this
    type(tMeshElement), allocatable :: mesh
    character(len=*), intent(in) :: id
    !! Element identifier — also the node/bar ID mnemonic's prefix
    real(8), intent(in) :: position(3)
    !! Grid corner [x, y, z] (m)
    real(8), intent(in) :: lengthX, lengthY
    !! Grid extent along X and Y (m)
    integer(4), intent(in) :: rowsX, rowsY
    !! Bars parallel to X (rowsX) and to Y (rowsY)
    real(8), intent(in) :: radius
    !! Cylindrical radius of every bar segment (m)
    integer(4), intent(in) :: segments
    !! Electrode segments per bar
    character(len=*), intent(in) :: idMaterial
    !! User ID of the material shared by every bar

    allocate(mesh)
    mesh%id          = id
    mesh%position    = position
    mesh%lengthX     = lengthX
    mesh%lengthY     = lengthY
    mesh%rowsX       = rowsX
    mesh%rowsY       = rowsY
    mesh%radius      = radius
    mesh%segments    = segments
    mesh%idMaterial  = idMaterial
    call move_alloc(mesh, this)
  end function newElementMesh

  ! =====================================================================
  ! ID mnemonic
  ! =====================================================================

  pure function rowColTag(row, col) result(tag)
    !! 4-character "RRCC" tag for a (row, col) main node, 0-based, 2-digit
    !! zero-padded each (see module header mnemonic).
    integer(4), intent(in) :: row, col
    character(len=4) :: tag
    character(len=2) :: r, c

    write(r, '(I2.2)') row
    write(c, '(I2.2)') col
    tag = r // c
  end function rowColTag

  pure function meshNodeId(meshId, row, col) result(nodeId)
    !! Full ID of the main node at (row, col): "<meshId>-<RRCC>".
    character(len=*), intent(in) :: meshId
    integer(4), intent(in) :: row, col
    character(len=256) :: nodeId

    nodeId = trim(meshId) // "-" // rowColTag(row, col)
  end function meshNodeId

  ! =====================================================================
  ! Assembly
  ! =====================================================================

  subroutine assembleMesh(this, structure)
    !! Validate parameters, create the `rowsX * rowsY` main nodes, then
    !! delegate every bar between adjacent main nodes to a `tLine`
    !! (`mElementLine%assembleLine`) for its own internal-node/electrode
    !! chain — see module header for the composite-element rationale.
    class(tMeshElement), intent(inout), target :: this
    class(*), intent(inout) :: structure
    class(tMaterial), pointer :: mat
    integer(4) :: row, col, nMainNodes, nBars, nElectrodesTotal, nNodesTotal
    integer(4) :: idxNode, idxInternalNode, idxElectrode

    select type (structure)
    type is (tStructure)
      if (this%rowsX < 2 .or. this%rowsY < 2) then
        call raiseError("tMeshElement '" // trim(this%id) // "': rowsX and rowsY must each be >= 2")
        return
      end if
      if (this%rowsX > MAX_MESH_ROWS .or. this%rowsY > MAX_MESH_ROWS) then
        call raiseError("tMeshElement '" // trim(this%id) // "': rowsX/rowsY must each be <= 100 " // &
                         "(2-digit node ID mnemonic)")
        return
      end if
      if (this%segments < 1) then
        call raiseError("tMeshElement '" // trim(this%id) // "': segments must be >= 1")
        return
      end if
      if (this%lengthX <= 0.0d0 .or. this%lengthY <= 0.0d0) then
        call raiseError("tMeshElement '" // trim(this%id) // "': lengthX and lengthY must be > 0")
        return
      end if
      if (this%position(3) == 0.0d0) then
        call raiseError("tMeshElement '" // trim(this%id) // "': position z = 0 (exactly on the " // &
                         "air-soil interface) is not supported — a segment straddling the interface " // &
                         "is not well-defined by the image-method formulation (theory.md §2, §5)")
        return
      end if

      mat => structure%findMaterial(trim(this%idMaterial))
      if (.not. associated(mat)) then
        call raiseError("tMeshElement '" // trim(this%id) // "': material '" // &
                         trim(this%idMaterial) // "' not found")
        return
      end if
      allocate(this%material, source=mat)

      nMainNodes       = this%rowsX * this%rowsY
      nBars            = this%rowsX * (this%rowsY - 1) + this%rowsY * (this%rowsX - 1)
      nElectrodesTotal = nBars * this%segments
      nNodesTotal      = nMainNodes + nBars * (this%segments - 1)

      allocate(this%nodes(nNodesTotal))
      allocate(this%electrodes(nElectrodesTotal))
      this%nNodes      = nNodesTotal
      this%nElectrodes = nElectrodesTotal

      ! Main nodes: rowsX * rowsY grid points. rowsX bars parallel to X
      ! (each lengthX long) are spaced along Y by lengthY/(rowsX-1); rowsY
      ! bars parallel to Y are spaced along X by lengthX/(rowsY-1).
      idxNode = 0
      do row = 0, this%rowsX - 1
        do col = 0, this%rowsY - 1
          idxNode = idxNode + 1
          this%nodes(idxNode) = newNode(meshNodeId(this%id, row, col), this%position + &
            [col * this%lengthX / real(this%rowsY - 1, kind=8), &
             row * this%lengthY / real(this%rowsX - 1, kind=8), 0.0d0])
          call structure%addNode(this%nodes(idxNode))
        end do
      end do

      idxInternalNode = nMainNodes
      idxElectrode    = 0

      ! Bars parallel to the X axis: fixed row, consecutive columns.
      do row = 0, this%rowsX - 1
        do col = 0, this%rowsY - 2
          call assembleBar(this, structure, row, col, row, col + 1, idxInternalNode, idxElectrode)
        end do
      end do
      ! Bars parallel to the Y axis: fixed column, consecutive rows.
      do col = 0, this%rowsY - 1
        do row = 0, this%rowsX - 2
          call assembleBar(this, structure, row, col, row + 1, col, idxInternalNode, idxElectrode)
        end do
      end do
    end select
  end subroutine assembleMesh

  subroutine assembleBar(this, structure, row1, col1, row2, col2, idxInternalNode, idxElectrode)
    !! Build one bar between two adjacent main nodes by delegating to a
    !! `tLine`, then fold its internal nodes/electrodes into `this%nodes`/
    !! `this%electrodes` at the given running indices (already sized
    !! exactly by `assembleMesh`).
    class(tMeshElement), intent(inout) :: this
    class(*), intent(inout) :: structure
    integer(4), intent(in) :: row1, col1, row2, col2
    integer(4), intent(inout) :: idxInternalNode, idxElectrode
    class(tElement), allocatable, target :: bar
    character(len=256) :: barId
    integer(4) :: nInternal

    barId = trim(this%id) // "-" // rowColTag(row1, col1) // "-" // rowColTag(row2, col2)
    bar = newElementLine(trim(barId), meshNodeId(this%id, row1, col1), meshNodeId(this%id, row2, col2), &
                          this%radius, this%segments, trim(this%idMaterial))
    call bar%assemble(structure)

    if (allocated(bar%nodes)) then
      nInternal = size(bar%nodes)
      this%nodes(idxInternalNode + 1 : idxInternalNode + nInternal) = bar%nodes
      idxInternalNode = idxInternalNode + nInternal
    end if

    this%electrodes(idxElectrode + 1 : idxElectrode + this%segments) = bar%electrodes
    idxElectrode = idxElectrode + this%segments
  end subroutine assembleBar

  ! =====================================================================
  ! Report
  ! =====================================================================

  subroutine reportMesh(this, str)
    !! Compact summary (unlike `reportLine`, a real grid can carry
    !! hundreds of electrodes, too many to usefully list one by one).
    class(tMeshElement), intent(in) :: this
    character(:), allocatable, intent(inout) :: str
    character(len=128) :: buf

    str = str // "Element ID: " // trim(this%id) // ", Material: " // trim(this%idMaterial)

    write(buf, '(I0,"x",I0," rows, ",F0.3,"m x ",F0.3,"m, radius ",F0.4,"m")') &
      this%rowsX, this%rowsY, this%lengthX, this%lengthY, this%radius
    str = str // ", " // trim(buf) // new_line('a')

    write(buf, '("  Nodes: ",I0," (",I0,"x",I0," main), Electrodes: ",I0)') &
      this%nNodes, this%rowsX, this%rowsY, this%nElectrodes
    str = str // trim(buf) // new_line('a')
  end subroutine reportMesh

end module mElementMesh
