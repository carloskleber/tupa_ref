module mObservation
  !! Observation-point specification and result containers for the
  !! grounding-safety outputs (ROADMAP Phase 11, theory.md §3.1, ADR 0027).
  !!
  !! A case's optional `"observation"` block asks for potentials ψ at points
  !! off the conductors, a rectangular surface grid of them, touch-voltage
  !! sites around designated nodes and step pairs. `tObservation` is the
  !! parsed request (it lives in `tStudy`, filled by `mTupa::loadStudy`);
  !! `tObservationResults` holds what `mPotentials::computeObservations`
  !! evaluates from a solved sweep. The module depends on nothing but
  !! `mResult` so that `mStudy` can own both.
  use mResult
  implicit none
  private

  public :: tObservation, tObservationPoint, tGridSpec, tTouchSpec, tStepSpec, tObservationResults

  type :: tObservationPoint
    !! One explicit observation point.
    character(256) :: id = ''
    !! User-assigned ID
    real(8) :: p(3) = 0.0d0
    !! Position (m); z <= 0 is soil, z > 0 air (theory.md §2)
  end type tObservationPoint

  type :: tGridSpec
    !! Rectangular grid of observation points in one horizontal plane.
    character(256) :: id = 'grid'
    !! Grid ID; its sites are named `<id>_<ix>_<iy>` (1-based)
    real(8) :: origin(2) = 0.0d0
    !! (x, y) of the first grid point (m)
    real(8) :: z = 0.0d0
    !! Plane height (m); 0 = the soil surface
    real(8) :: lengthX = 0.0d0, lengthY = 0.0d0
    !! Extent along x and y (m)
    integer :: nx = 2, ny = 2
    !! Points along x and y (a count of 1 puts the row at the origin)
    logical :: step = .false.
    !! Also compute the step-voltage map
    real(8) :: stepLength = 1.0d0
    !! Step length (m) of the map; IEEE Std 80's 1 m stride
    integer :: stepDirections = 8
    !! Azimuths tried per grid point for the step map
  end type tGridSpec

  type :: tTouchSpec
    !! Touch-voltage site: the legacy geometric definition (theory.md §3.1).
    character(256) :: id = ''
    !! User-assigned ID
    character(256) :: node = ''
    !! Designated node whose potential u is the reference
    real(8) :: radius = 1.0d0
    !! Circle radius (m), centred on the node's (x, y)
    integer :: nPoints = 36
    !! Points on the circle
    real(8) :: z = 0.0d0
    !! Height of the circle's plane (m); 0 = the soil surface
  end type tTouchSpec

  type :: tStepSpec
    !! Step pair: Δψ = ψ(to) − ψ(from).
    character(256) :: id = ''
    !! User-assigned ID
    real(8) :: from(3) = 0.0d0, to(3) = 0.0d0
    !! The two points (m)
  end type tStepSpec

  type :: tObservation
    !! The parsed `"observation"` block.
    type(tObservationPoint), allocatable :: points(:)
    !! `observation.points`
    logical :: hasGrid = .false.
    !! A `grid` block is present
    type(tGridSpec) :: grid
    !! `observation.grid`
    type(tTouchSpec), allocatable :: touch(:)
    !! `observation.touch`
    type(tStepSpec), allocatable :: steps(:)
    !! `observation.steps`
  contains
    procedure :: isEmpty => observationIsEmpty
    !! No observation was requested
    procedure :: siteCount => observationSiteCount
    !! Number of potential sites: the explicit points, then the grid
    procedure :: site => observationSite
    !! ID and position of potential site `i`
  end type tObservation

  type :: tObservationResults
    !! Outputs of `computeObservations`, one set per frequency of the sweep.
    type(tPotentials) :: potentials
    !! ψ at every site (points, then the grid in x-major order)
    type(tPotentials) :: gpr
    !! Potential u of each touch site's node: the ground potential rise
    type(tMagnitudes) :: touch
    !! Touch voltage max_k |ψ_k − u| per touch site
    type(tPotentials) :: steps
    !! Δψ of each step pair
    type(tMagnitudes) :: stepMap
    !! Step voltage at every grid point (only if `grid%step`)
    real(8), allocatable :: sitePos(:,:)
    !! Position (3, nSites) of every `potentials` site
    real(8), allocatable :: freqHz(:)
    !! Frequency axis (Hz) the sets were evaluated at
  end type tObservationResults

contains

  logical function observationIsEmpty(this) result(empty)
    class(tObservation), intent(in) :: this

    empty = .false.
    if (this%hasGrid) return
    if (allocated(this%points)) then
      if (size(this%points) > 0) return
    end if
    if (allocated(this%touch)) then
      if (size(this%touch) > 0) return
    end if
    if (allocated(this%steps)) then
      if (size(this%steps) > 0) return
    end if
    empty = .true.
  end function observationIsEmpty

  integer function observationSiteCount(this) result(n)
    class(tObservation), intent(in) :: this

    n = 0
    if (allocated(this%points)) n = size(this%points)
    if (this%hasGrid) n = n + this%grid%nx * this%grid%ny
  end function observationSiteCount

  subroutine observationSite(this, i, id, p)
    !! ID and position of potential site `i` (1 .. `siteCount`).
    class(tObservation), intent(in) :: this
    integer, intent(in) :: i
    character(256), intent(out) :: id
    real(8), intent(out) :: p(3)
    integer :: nPts, j, ix, iy
    character(len=16) :: sx, sy

    nPts = 0
    if (allocated(this%points)) nPts = size(this%points)
    if (i <= nPts) then
      id = this%points(i)%id
      p = this%points(i)%p
      return
    end if
    j  = i - nPts - 1
    ix = j / this%grid%ny + 1
    iy = mod(j, this%grid%ny) + 1
    write(sx, '(I0)') ix
    write(sy, '(I0)') iy
    id = trim(this%grid%id) // "_" // trim(sx) // "_" // trim(sy)
    p(1) = this%grid%origin(1)
    if (this%grid%nx > 1) p(1) = p(1) + (ix - 1) * this%grid%lengthX / (this%grid%nx - 1)
    p(2) = this%grid%origin(2)
    if (this%grid%ny > 1) p(2) = p(2) + (iy - 1) * this%grid%lengthY / (this%grid%ny - 1)
    p(3) = this%grid%z
  end subroutine observationSite

end module mObservation
