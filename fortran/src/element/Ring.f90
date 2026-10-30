module mElementRing
  !! Circular conductor ("ring" element, ROADMAP Phase 12 item 1; legacy
  !! `Anel.m`): a closed loop of `chords` straight one-segment bars inscribed
  !! in a circle of any orientation. Used for grounding rings and for the
  !! toroidal corona shields of impulse generators and dividers.
  !!
  !! The circle is given by its `centre`, the `normal` direction of its plane
  !! and its radius. The first vertex sits at `centre + R * y'`, where y' is
  !! the global y axis tilted with the plane (the legacy convention, so the
  !! ring joins the same node as in the Matlab input); the others follow at
  !! equal angles about the normal. The first vertex is the node named after
  !! the element (`<id>`), the others `<id>-<k:02d>`. A vertex that coincides
  !! with a node already in the structure reuses it. All vertices must lie in
  !! one medium (air or soil).
  use mElement
  use mStructure
  use mNode
  use mMaterial
  use mElectrode
  use mError, only: raiseError
  implicit none
  private

  public :: newElementRing

  real(8), parameter :: NODE_TOL = 1.0d-6
  !! Positions closer than this (m) are the same node
  real(8), parameter :: TWO_PI = 6.283185307179586d0

  type, extends(tElement), public :: tRingElement
    real(8) :: centre(3), normal(3)
    !! Centre of the circle (m) and direction of its normal
    real(8) :: ringRadius
    !! Radius of the circle (m); `radius` is that of the conductor
    integer(4) :: chords
    !! Number of bars (>= 3)
    character(len=256) :: idMaterial
    !! Material of every bar
  contains
    procedure :: assemble => assembleRing
    procedure :: report   => reportRing
  end type tRingElement

contains

  function newElementRing(id, centre, normal, ringRadius, conductorRadius, chords, idMaterial) result(this)
    class(tElement), allocatable :: this
    type(tRingElement), allocatable :: ring
    character(len=*), intent(in) :: id
    real(8), intent(in) :: centre(3), normal(3), ringRadius, conductorRadius
    integer(4), intent(in) :: chords
    character(len=*), intent(in) :: idMaterial

    allocate(ring)
    ring%id         = id
    ring%centre     = centre
    ring%normal     = normal
    ring%ringRadius = ringRadius
    ring%radius     = conductorRadius
    ring%chords     = chords
    ring%idMaterial = idMaterial
    call move_alloc(ring, this)
  end function newElementRing

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

  function vertex(this, k) result(p)
    !! k-th vertex (0-based): the point (R sin a, R cos a, 0) of the circle in
    !! the xy plane, rotated by the angle between z and n about cross(z, n)
    !! (Rodrigues), then moved to the centre.
    type(tRingElement), intent(in) :: this
    integer(4), intent(in) :: k
    real(8) :: p(3), n(3), axis(3), a, c, s, v(3), sinTilt, cosTilt

    n = this%normal / sqrt(sum(this%normal**2))
    a = TWO_PI * real(k, 8) / real(this%chords, 8)
    v = [this%ringRadius * sin(a), this%ringRadius * cos(a), 0.0d0]
    axis = [-n(2), n(1), 0.0d0]                        ! cross(z, n): turns z onto n
    sinTilt = sqrt(sum(axis**2))
    if (sinTilt < 1.0d-12) then
      p = this%centre + v
      return
    end if
    axis = axis / sinTilt
    cosTilt = n(3)
    c = cosTilt
    s = sinTilt
    p = this%centre + v * c + cross(axis, v) * s + axis * dot_product(axis, v) * (1.0d0 - c)
  end function vertex

  function cross(a, b) result(c)
    real(8), intent(in) :: a(3), b(3)
    real(8) :: c(3)
    c = [a(2) * b(3) - a(3) * b(2), a(3) * b(1) - a(1) * b(3), a(1) * b(2) - a(2) * b(1)]
  end function cross

  subroutine assembleRing(this, structure)
    class(tRingElement), intent(inout), target :: this
    class(*), intent(inout) :: structure

    select type (structure)
    type is (tStructure)
      call buildRing(this, structure)
    end select
  end subroutine assembleRing

  subroutine buildRing(this, structure)
    !! Create the vertices (reusing coincident nodes), then one electrode per
    !! chord.
    class(tRingElement), intent(inout), target :: this
    type(tStructure), intent(inout) :: structure
    class(tMaterial), pointer :: mat
    type(tElectrode) :: electrode
    type(tNode), allocatable :: newNodes(:)
    type(tElectrode), allocatable :: newElectrodes(:)
    integer(4), allocatable :: idx(:)
    real(8) :: p(3), zmin, zmax
    character(len=256) :: buf
    integer(4) :: k, nNew

    if (this%chords < 3) then
      call raiseError("tRingElement '" // trim(this%id) // "': a ring needs at least 3 chords")
      return
    end if
    if (this%radius <= 0.0d0 .or. this%ringRadius <= 0.0d0) then
      call raiseError("tRingElement '" // trim(this%id) // "': radius and ringRadius must be > 0")
      return
    end if
    if (sqrt(sum(this%normal**2)) == 0.0d0) then
      call raiseError("tRingElement '" // trim(this%id) // "': normal must be nonzero")
      return
    end if
    mat => structure%findMaterial(trim(this%idMaterial))
    if (.not. associated(mat)) then
      call raiseError("tRingElement '" // trim(this%id) // "': material '" // trim(this%idMaterial) // "' not found")
      return
    end if
    allocate(this%material, source=mat)

    zmin = huge(1.0d0)
    zmax = -huge(1.0d0)
    do k = 0, this%chords - 1
      p = vertex(this, k)
      zmin = min(zmin, p(3))
      zmax = max(zmax, p(3))
    end do
    if (zmin < -NODE_TOL .and. zmax > NODE_TOL) then
      call raiseError("tRingElement '" // trim(this%id) // "': vertices in both media (above and below z = 0)")
      return
    end if
    if (abs(zmin) < NODE_TOL .and. abs(zmax) < NODE_TOL) then
      call raiseError("tRingElement '" // trim(this%id) // "': the ring lies on the air-soil interface z = 0, " // &
                      "which the image method cannot represent")
      return
    end if

    allocate(idx(0:this%chords - 1), newNodes(this%chords))
    nNew = 0
    do k = 0, this%chords - 1
      p = vertex(this, k)
      idx(k) = nodeAt(structure, p)
      if (idx(k) == 0) then
        if (k == 0) then
          buf = this%id
        else
          write(buf, '(A,"-",I2.2)') trim(this%id), k
        end if
        nNew = nNew + 1
        newNodes(nNew) = newNode(trim(buf), p)
        call structure%addNode(newNodes(nNew))
        idx(k) = structure%getNodeCount()
      end if
    end do

    allocate(newElectrodes(this%chords))
    do k = 0, this%chords - 1
      write(buf, '(A,"_b",I0)') trim(this%id), k + 1
      electrode = newElectrode(trim(buf), idx(k), idx(mod(k + 1, this%chords)))
      electrode%radius = this%radius
      electrode%material => this%material
      call structure%addElectrode(electrode)
      newElectrodes(k + 1) = electrode
    end do
    this%nodes = newNodes(1:nNew)
    this%electrodes = newElectrodes
    this%nNodes = nNew
    this%nElectrodes = this%chords
  end subroutine buildRing

  subroutine reportRing(this, str)
    class(tRingElement), intent(in) :: this
    character(:), allocatable, intent(inout) :: str
    character(len=160) :: buf

    str = str // "Element ID: " // trim(this%id) // ", Material: " // trim(this%idMaterial)
    write(buf, '(", ring R ",F0.4," m, ",I0," chords, conductor radius ",F0.4," m, new nodes ",I0)') &
      this%ringRadius, this%chords, this%radius, this%nNodes
    str = str // trim(buf) // new_line('a')
  end subroutine reportRing

end module mElementRing
