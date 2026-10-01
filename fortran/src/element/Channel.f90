module mElementChannel
  !! Lightning return-stroke channel in air: a chain of straight segments
  !! rising from a strike point, loaded with a distributed series impedance
  !! z_ch = R' + jωL' that slows the current wave to a prescribed
  !! return-stroke speed (ROADMAP.md Phase 10b item 1, theory.md §4.5,
  !! ADR 0025).
  !!
  !! The channel is an ordinary wire in the model: every segment is a
  !! `tElectrode`, so all of §4.1–§5 applies unchanged. The only addition is
  !! the loading in the internal-impedance slot of the self impedance
  !! (`tElectrode%loaded`). The wire itself is a perfect conductor — the
  !! channel's own resistance is `R'`.
  !!
  !! Nodes: the base node `<id>-base` coincides with the strike node but is a
  !! separate node (the channel source is a two-node source between them,
  !! ADR 0025), intermediate nodes are `<id>_n<k>`, the top node is
  !! `<id>-top`. A free-standing channel (no strike node, e.g. above ideal
  !! ground) has only its base node, at `footPosition`; the source then acts
  !! against remote earth. The strike node must be attached to something
  !! (the struck object): an isolated node makes the nodal system singular. Segments are `<id>_e<k>`, numbered upward from the base.
  use mElement
  use mStructure
  use mNode
  use mElectrode
  use mError, only: raiseError
  use mCtes, only: newl, MU0, EPSILON0
  implicit none
  private

  public :: newElementChannel, gradedBreaks, channelLoadingEstimate, evalPiecewise
  public :: channelUnitVector, channelHeightFirstSegment

  real(8), parameter :: PI_ = 3.14159265358979323846d0
  real(8), parameter :: LIGHT_SPEED = 1.0d0 / sqrt(MU0 * EPSILON0)

  type, public :: tPiecewise
    !! Piecewise-constant profile along the channel: `value(i)` applies up to
    !! and including `upTo(i)` (the previous break excluded); the last value
    !! continues beyond the last break.
    real(8), allocatable :: upTo(:)
    real(8), allocatable :: value(:)
  end type tPiecewise

  type, extends(tElement), public :: tChannel
    !! Loaded vertical or inclined wire above a strike node.
    character(len=256) :: idStrike
    !! User ID of the strike node (the channel's foot position); blank = a
    !! free-standing channel whose foot is `footPosition`
    real(8) :: footPosition(3) = 0.0d0
    !! Foot of a free-standing channel (m), used when `idStrike` is blank
    real(8) :: length
    !! Channel length along its axis (m)
    real(8) :: incidenceDeg = 0.0d0
    !! Angle of the axis from the vertical (degrees)
    real(8) :: azimuthDeg = 0.0d0
    !! Azimuth of the axis in the xy plane, from +x (degrees)
    real(8), allocatable :: breaks(:)
    !! Distances s_0 = 0, ..., s_n = length of the chain nodes along the axis (m)
    type(tPiecewise) :: speed
    !! Target return-stroke speed v(s) (m/s); unallocated = unloaded unless `inductance` is set
    logical :: hasInductance = .false.
    !! True when an explicit uniform L' was given
    real(8) :: inductance = 0.0d0
    !! Explicit uniform series inductance per unit length (H/m)
    type(tPiecewise) :: resistance
    !! Series resistance per unit length R'(s) (Ω/m); unallocated = 0
    real(8) :: loadScale = 1.0d0
    !! Factor on the closed-form L'(z) of a speed-loaded channel — 1 until
    !! calibrated (`calibrateChannelLoading`)
    logical :: wantCalibration = .false.
    !! Set by the loader (`"calibrate": true`); cleared once calibrated
    logical :: calibrated = .false.
    !! True when `loadScale` came from a calibration
    real(8) :: calibratedSpeed = 0.0d0
    !! Speed measured on the channel alone with the final loading (m/s), calibrated only
  contains
    procedure :: assemble => assembleChannel
    procedure :: report   => reportChannel
    procedure :: segmentLoading => segmentLoadingChannel
  end type tChannel

contains

  function newElementChannel(id, idStrike, length, incidenceDeg, azimuthDeg, radius, breaks) result(this)
    !! Construct a `tChannel` with its segment break distances. Loading
    !! (`speed`, `inductance`, `resistance`) is set on the returned object.
    class(tElement), allocatable :: this
    type(tChannel), allocatable :: ch
    character(len=*), intent(in) :: id
    character(len=*), intent(in) :: idStrike
    real(8), intent(in) :: length, incidenceDeg, azimuthDeg, radius
    real(8), intent(in) :: breaks(:)
    !! Distances along the axis of the chain nodes, 0 to `length` (size n + 1)

    allocate(ch)
    ch%id           = id
    ch%idStrike     = idStrike
    ch%length       = length
    ch%incidenceDeg = incidenceDeg
    ch%azimuthDeg   = azimuthDeg
    ch%radius       = radius
    ch%breaks       = breaks
    ch%nElectrodes  = size(breaks) - 1
    ch%nNodes       = size(breaks)
    call move_alloc(ch, this)
  end function newElementChannel

  function gradedBreaks(length, first, growth, maxSegment) result(breaks)
    !! Break distances of a channel graded from its foot: segment k has
    !! length min(first · growth^(k-1), maxSegment) until the length is
    !! covered, then every segment is scaled by the same factor so the chain
    !! ends exactly at `length`. The scaling keeps every ratio between
    !! adjacent segments, so it never exceeds `growth` (theory.md §4.5:
    !! bounded adjacent-length ratio); `maxSegment` and `first` can only
    !! shrink by it. `growth = 1` gives a uniform chain of
    !! ceil(length / maxSegment) segments.
    real(8), intent(in) :: length
    !! Channel length (m)
    real(8), intent(in) :: first
    !! Length of the segment at the foot (m)
    real(8), intent(in) :: growth
    !! Ratio of successive segment lengths (≥ 1)
    real(8), intent(in) :: maxSegment
    !! Longest segment (m)
    real(8), allocatable :: breaks(:)
    real(8), allocatable :: seg(:)
    real(8) :: l, total, scale
    integer :: n, j

    allocate(seg(0))
    total = 0.0d0
    l = min(first, maxSegment)
    n = 0
    do while (total < length * (1.0d0 - 1.0d-12))
      seg = [seg, l]
      total = total + l
      n = n + 1
      l = min(l * growth, maxSegment)
      if (n > 1000000) exit
    end do
    scale = length / total
    allocate(breaks(n + 1))
    breaks(1) = 0.0d0
    do j = 1, n
      breaks(j + 1) = breaks(j) + seg(j) * scale
    end do
    breaks(n + 1) = length
  end function gradedBreaks

  real(8) function evalPiecewise(p, s, dflt) result(v)
    !! Value of the profile at distance `s`; `dflt` where it is unallocated.
    type(tPiecewise), intent(in) :: p
    real(8), intent(in) :: s, dflt
    integer :: i

    if (.not. allocated(p%value)) then
      v = dflt
      return
    end if
    v = p%value(size(p%value))
    do i = 1, size(p%upTo)
      if (s <= p%upTo(i)) then
        v = p%value(i)
        return
      end if
    end do
  end function evalPiecewise

  real(8) function channelLoadingEstimate(z, radius, speed) result(lPrime)
    !! Closed-form series inductance per unit length (H/m) that slows a
    !! wire of the given radius at height `z` to `speed`:
    !! L' = L0 (c²/v² - 1), L0 = μ0/(2π) ln(2z/r0) (theory.md §4.5). A
    !! starting value for a calibration, not the final loading.
    real(8), intent(in) :: z, radius, speed
    real(8) :: l0

    l0 = MU0 / (2.0d0 * PI_) * log(2.0d0 * z / radius)
    lPrime = l0 * (LIGHT_SPEED**2 / speed**2 - 1.0d0)
  end function channelLoadingEstimate

  function channelUnitVector(incidenceDeg, azimuthDeg) result(u)
    !! Unit vector of the channel axis (pointing up for zero incidence).
    real(8), intent(in) :: incidenceDeg, azimuthDeg
    real(8) :: u(3)
    real(8) :: th, ph

    th = incidenceDeg * PI_ / 180.0d0
    ph = azimuthDeg * PI_ / 180.0d0
    u = [sin(th) * cos(ph), sin(th) * sin(ph), cos(th)]
  end function channelUnitVector

  real(8) function channelHeightFirstSegment(this) result(h)
    !! Height of the first segment's midpoint above the strike node (m).
    class(tChannel), intent(in) :: this
    h = 0.5d0 * (this%breaks(1) + this%breaks(2)) * cos(this%incidenceDeg * PI_ / 180.0d0)
  end function channelHeightFirstSegment

  subroutine segmentLoadingChannel(this, k, zStrike, rPrime, lPrime, ierr)
    !! Series loading R', L' of segment `k` (1-based from the base), with
    !! the strike node at height `zStrike`. `ierr` /= 0 when the segment is
    !! too low for the closed-form estimate (2z <= r0) or the target speed
    !! exceeds c.
    class(tChannel), intent(in) :: this
    integer(4), intent(in) :: k
    real(8), intent(in) :: zStrike
    real(8), intent(out) :: rPrime, lPrime
    integer, intent(out) :: ierr
    real(8) :: sMid, z, v

    ierr = 0
    sMid = 0.5d0 * (this%breaks(k) + this%breaks(k + 1))
    rPrime = evalPiecewise(this%resistance, sMid, 0.0d0)
    if (this%hasInductance) then
      lPrime = this%inductance
    else if (allocated(this%speed%value)) then
      v = evalPiecewise(this%speed, sMid, LIGHT_SPEED)
      if (v <= 0.0d0 .or. v > LIGHT_SPEED * (1.0d0 + 1.0d-12)) then
        ierr = 2
        lPrime = 0.0d0
        return
      end if
      z = zStrike + sMid * cos(this%incidenceDeg * PI_ / 180.0d0)
      if (2.0d0 * z <= this%radius) then
        ierr = 1
        lPrime = 0.0d0
        return
      end if
      lPrime = this%loadScale * channelLoadingEstimate(z, this%radius, min(v, LIGHT_SPEED))
    else
      lPrime = 0.0d0
    end if
  end subroutine segmentLoadingChannel

  subroutine assembleChannel(this, structure)
    !! Plant the base, intermediate and top nodes and the loaded segments.
    class(tChannel), intent(inout), target :: this
    class(*), intent(inout) :: structure
    integer(4) :: idxStrike, k, n, baseIdx, ierr
    integer(4), allocatable :: nodeIdx(:)
    real(8) :: pStrike(3), u(3), rP, lP
    type(tElectrode) :: electrode
    type(tNode) :: node
    character(len=256) :: buf

    select type (structure)
    type is (tStructure)
      if (len_trim(this%idStrike) == 0) then
        pStrike = this%footPosition
      else
        idxStrike = structure%findNodeIndex(trim(this%idStrike))
        if (idxStrike == 0) then
          call raiseError("tChannel '" // trim(this%id) // "': strike node '" // trim(this%idStrike) // "' not found")
          return
        end if
        pStrike = structure%nodes(idxStrike)%p
      end if
      u = channelUnitVector(this%incidenceDeg, this%azimuthDeg)
      n = this%nElectrodes

      if (u(3) <= 1.0d-12) then
        call raiseError("tChannel '" // trim(this%id) // "': the channel must rise (incidence < 90 degrees)")
        return
      end if
      if (pStrike(3) < 0.0d0) then
        call raiseError("tChannel '" // trim(this%id) // "': the strike node must be in air (z >= 0)")
        return
      end if

      allocate(nodeIdx(n + 1))
      allocate(this%nodes(n + 1))
      do k = 0, n
        if (k == 0) then
          write(buf, '(A,"-base")') trim(this%id)
        else if (k == n) then
          write(buf, '(A,"-top")') trim(this%id)
        else
          write(buf, '(A,"_n",I0)') trim(this%id), k
        end if
        node = newNode(trim(buf), pStrike + this%breaks(k + 1) * u)
        call structure%addNode(node)
        nodeIdx(k + 1) = structure%getNodeCount()
        this%nodes(k + 1) = node
      end do
      baseIdx = nodeIdx(1)

      allocate(this%electrodes(n))
      do k = 1, n
        call this%segmentLoading(k, pStrike(3), rP, lP, ierr)
        if (ierr == 1) then
          call raiseError("tChannel '" // trim(this%id) // "': a segment is too close to the ground for the " // &
                          "speed-loading formula (2 z <= radius)")
          return
        else if (ierr == 2) then
          call raiseError("tChannel '" // trim(this%id) // "': the target speed must be in (0, c]")
          return
        end if
        write(buf, '(A,"_e",I0)') trim(this%id), k
        electrode = newElectrode(trim(buf), nodeIdx(k), nodeIdx(k + 1))
        electrode%radius         = this%radius
        electrode%loaded         = .true.
        electrode%loadResistance = rP
        electrode%loadInductance = lP
        call structure%addElectrode(electrode)
        this%electrodes(k) = electrode
      end do
    end select
  end subroutine assembleChannel

  subroutine reportChannel(this, str)
    !! Channel summary: geometry, loading and calibration status.
    class(tChannel), intent(in) :: this
    character(:), allocatable, intent(inout) :: str
    character(len=64) :: buf

    str = str // "Element ID: " // trim(this%id) // ", Lightning channel from node " // trim(this%idStrike)
    write(buf, '(F0.3)') this%length
    str = str // ", length " // trim(buf) // " m"
    write(buf, '(I0)') this%nElectrodes
    str = str // ", segments " // trim(buf)
    write(buf, '(F0.4)') this%radius
    str = str // ", radius " // trim(buf) // " m" // newl
    if (this%hasInductance) then
      write(buf, '(ES11.4)') this%inductance
      str = str // "  Series loading L' = " // trim(adjustl(buf)) // " H/m" // newl
    else if (allocated(this%speed%value)) then
      write(buf, '(ES11.4)') this%speed%value(1)
      str = str // "  Speed-loaded (target " // trim(adjustl(buf)) // " m/s)"
      if (this%calibrated) then
        write(buf, '(F0.4)') this%loadScale
        str = str // ", calibrated scale " // trim(buf)
      end if
      str = str // newl
    end if
  end subroutine reportChannel

end module mElementChannel
