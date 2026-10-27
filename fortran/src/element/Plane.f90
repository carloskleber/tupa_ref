module mElementPlane
  !! Planar wire-grid surface ("plane" element, docs/marx_generator.md):
  !! a rectangle of any orientation approximated by crossed thin-wire bars,
  !! the usual wire-grid model of a conducting wall, ceiling or screen
  !! (Richmond 1966; Ludwig 1987).
  !!
  !! The rectangle spans `origin`, `origin + u` and `origin + v` (`u`, `v`
  !! the edge vectors), cut into `cellsU x cellsV` cells; one bar of one
  !! segment runs along every cell edge. Nodes and bars that already exist in
  !! the structure (a shared cube edge, a neighbouring plane) are reused, so
  !! adjacent planes join into one connected enclosure. Main nodes are named
  !! `<id>-<i:02d>_<j:02d>` (0-based) unless a node already sits there.
  !!
  !! The bar radius is an input: for a surface of cell size d the
  !! equal-area rule gives a = d / (2 pi) per bar pair (Ludwig 1987), and
  !! the grid must stay under lambda/10 at the highest frequency.
  use mElement
  use mStructure
  use mNode
  use mMaterial
  use mElectrode
  use mError, only: raiseError
  implicit none
  private

  public :: newElementPlane

  real(8), parameter :: NODE_TOL = 1.0d-6
  !! Positions closer than this (m) are the same node

  type, extends(tElement), public :: tPlaneElement
    real(8) :: origin(3), edgeU(3), edgeV(3)
    !! Corner and edge vectors of the rectangle (m)
    integer(4) :: cellsU, cellsV
    !! Cells along `edgeU` and `edgeV` (>= 1)
    character(len=256) :: idMaterial
    !! Material of every bar
  contains
    procedure :: assemble => assemblePlane
    procedure :: report   => reportPlane
  end type tPlaneElement

contains

  function newElementPlane(id, origin, edgeU, edgeV, cellsU, cellsV, radius, idMaterial) result(this)
    class(tElement), allocatable :: this
    type(tPlaneElement), allocatable :: plane
    character(len=*), intent(in) :: id
    real(8), intent(in) :: origin(3), edgeU(3), edgeV(3), radius
    integer(4), intent(in) :: cellsU, cellsV
    character(len=*), intent(in) :: idMaterial

    allocate(plane)
    plane%id         = id
    plane%origin     = origin
    plane%edgeU      = edgeU
    plane%edgeV      = edgeV
    plane%cellsU     = cellsU
    plane%cellsV     = cellsV
    plane%radius     = radius
    plane%idMaterial = idMaterial
    call move_alloc(plane, this)
  end function newElementPlane

  integer(4) function nodeAt(structure, p) result(idx)
    !! Index of the structure node within NODE_TOL of `p`; 0 if none.
    type(tStructure), intent(in) :: structure
    real(8), intent(in) :: p(3)
    integer(4) :: i

    idx = 0
    do i = 1, structure%nodeCount
      if (maxval(abs(structure%nodes(i)%p - p)) < NODE_TOL) then
        idx = i
        return
      end if
    end do
  end function nodeAt

  logical function barExists(structure, a, b) result(found)
    type(tStructure), intent(in) :: structure
    integer(4), intent(in) :: a, b
    integer(4) :: i

    found = .false.
    do i = 1, structure%electrodeCount
      if ((structure%electrodes(i)%nodeIndices(1) == a .and. structure%electrodes(i)%nodeIndices(2) == b) .or. &
          (structure%electrodes(i)%nodeIndices(1) == b .and. structure%electrodes(i)%nodeIndices(2) == a)) then
        found = .true.
        return
      end if
    end do
  end function barExists

  subroutine assemblePlane(this, structure)
    class(tPlaneElement), intent(inout), target :: this
    class(*), intent(inout) :: structure

    select type (structure)
    type is (tStructure)
      call buildPlane(this, structure)
    end select
  end subroutine assemblePlane

  subroutine buildPlane(this, structure)
    !! Create the grid nodes (reusing coincident ones), then one electrode
    !! per cell edge that the structure does not hold yet.
    class(tPlaneElement), intent(inout), target :: this
    type(tStructure), intent(inout) :: structure
    class(tMaterial), pointer :: mat
    type(tElectrode) :: electrode
    type(tNode), allocatable :: newNodes(:)
    type(tElectrode), allocatable :: newElectrodes(:)
    integer(4), allocatable :: grid(:,:)
    real(8) :: p(3), q(3)
    character(len=256) :: buf
    integer(4) :: i, j, nNew, nBar

    block
      if (this%cellsU < 1 .or. this%cellsV < 1) then
        call raiseError("tPlaneElement '" // trim(this%id) // "': cellsU and cellsV must be >= 1")
        return
      end if
      if (this%radius <= 0.0d0) then
        call raiseError("tPlaneElement '" // trim(this%id) // "': radius must be > 0")
        return
      end if
      if (sqrt(sum(this%edgeU**2)) == 0.0d0 .or. sqrt(sum(this%edgeV**2)) == 0.0d0) then
        call raiseError("tPlaneElement '" // trim(this%id) // "': edge vectors must be nonzero")
        return
      end if
      mat => structure%findMaterial(trim(this%idMaterial))
      if (.not. associated(mat)) then
        call raiseError("tPlaneElement '" // trim(this%id) // "': material '" // trim(this%idMaterial) // "' not found")
        return
      end if
      allocate(this%material, source=mat)

      allocate(grid(0:this%cellsU, 0:this%cellsV))
      allocate(newNodes((this%cellsU + 1) * (this%cellsV + 1)))
      nNew = 0
      do j = 0, this%cellsV
        do i = 0, this%cellsU
          p = this%origin + this%edgeU * (real(i, 8) / this%cellsU) + this%edgeV * (real(j, 8) / this%cellsV)
          grid(i, j) = nodeAt(structure, p)
          if (grid(i, j) == 0) then
            write(buf, '(A,"-",I2.2,"_",I2.2)') trim(this%id), i, j
            nNew = nNew + 1
            newNodes(nNew) = newNode(trim(buf), p)
            call structure%addNode(newNodes(nNew))
            grid(i, j) = structure%getNodeCount()
          end if
        end do
      end do

      allocate(newElectrodes(this%cellsU * (this%cellsV + 1) + (this%cellsU + 1) * this%cellsV))
      nBar = 0
      do j = 0, this%cellsV
        do i = 0, this%cellsU
          if (i < this%cellsU) call addBar(grid(i, j), grid(i + 1, j))
          if (j < this%cellsV) call addBar(grid(i, j), grid(i, j + 1))
        end do
      end do
      this%nodes = newNodes(1:nNew)
      this%electrodes = newElectrodes(1:nBar)
      this%nNodes = nNew
      this%nElectrodes = nBar
    end block

  contains

    subroutine addBar(a, b)
      integer(4), intent(in) :: a, b
      if (barExists(structure, a, b)) return
      q = 0.5d0 * (structure%nodes(a)%p + structure%nodes(b)%p)
      if (abs(q(3)) < NODE_TOL) then
        call raiseError("tPlaneElement '" // trim(this%id) // "': a bar lies on the air-soil interface z = 0, " // &
                        "which the image method cannot represent; lift the plane (the ground is already the floor)")
        return
      end if
      write(buf, '(A,"_b",I0)') trim(this%id), nBar + 1
      electrode = newElectrode(trim(buf), a, b)
      electrode%radius = this%radius
      electrode%material => this%material
      call structure%addElectrode(electrode)
      nBar = nBar + 1
      newElectrodes(nBar) = electrode
    end subroutine addBar

  end subroutine buildPlane

  subroutine reportPlane(this, str)
    class(tPlaneElement), intent(in) :: this
    character(:), allocatable, intent(inout) :: str
    character(len=160) :: buf

    str = str // "Element ID: " // trim(this%id) // ", Material: " // trim(this%idMaterial)
    write(buf, '(", plane ",I0,"x",I0," cells, radius ",F0.4," m, new nodes ",I0,", bars ",I0)') &
      this%cellsU, this%cellsV, this%radius, this%nNodes, this%nElectrodes
    str = str // trim(buf) // new_line('a')
  end subroutine reportPlane

end module mElementPlane
