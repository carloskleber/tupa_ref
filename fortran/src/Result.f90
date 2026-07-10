module mResult
  !! Container for frequency-domain solution results: node voltages and electrode currents.
  !!
  !! This module defines abstract and concrete result types that store the outputs of
  !! the frequency-domain solver for each angular frequency (ROADMAP.md Phase 3). Each
  !! result type owns a copy of the entity IDs (node or electrode) and the frequency
  !! axis it was solved at, plus its own complex-valued data array — no pointers into
  !! `tStructure`/`tMesh`, so a result set stays valid even if the study is mutated
  !! afterwards.
  !!
  !! Three concrete types are provided:
  !! - `tVoltages`: complex node voltages V_i(ω) — shape (nNodes, nFrequencies)
  !! - `tLongCurrents`: longitudinal electrode currents I_long(ω) — shape (nElectrodes, nFrequencies)
  !! - `tTransCurrents`: transverse (earth-leakage) electrode currents I_trans(ω) — shape (nElectrodes, nFrequencies)
  !!
  !! `tStudy%runSweep` (Study.f90) fills these; `mResultsWriter` reads them back out.
  implicit none
  private

  public :: tResult, tVoltages, tLongCurrents, tTransCurrents

  type, abstract :: tResult
    !! Abstract base type for frequency-domain results.
    !!
    !! Stores the result set's own identifier, a copy of the entity IDs (node or
    !! electrode, depending on the concrete type) and the frequency axis, plus
    !! deferred allocation of the concrete data array. Concrete types override
    !! `alloc()` to allocate their specific data storage.
    private
    character(256) :: id
    !! User-assigned identifier for the result set
    character(256), allocatable :: ids(:)
    !! Copy of the entity IDs indexed the same way as the data array's first dimension
    real(8), allocatable :: omega(:)
    !! Copy of the angular frequencies ω (rad/s) the solution was computed at
  contains
    procedure(alloc_interface), deferred :: alloc
    !! Allocate internal data storage from entity IDs and a frequency axis
    procedure, non_overridable :: storeAxes => resultStoreAxes
    !! Shared helper: copy `ids`/`omega` into the base type (called by `alloc` overrides)
    procedure :: frequencyCount => resultFrequencyCount
    !! Number of frequencies in the axis
    procedure :: frequency => resultFrequency
    !! Angular frequency ω (rad/s) at sweep index `i`
    procedure :: entityCount => resultEntityCount
    !! Number of entities (nodes or electrodes) in this result set
    procedure :: entityId => resultEntityId
    !! User-assigned ID of entity at index `i`
  end type tResult

  abstract interface
    subroutine alloc_interface(this, ids, omega)
      !! Deferred interface for allocating result data storage.
      !!
      !! Each concrete type calls `storeAxes` then allocates its own data array
      !! with dimensions (size(ids), size(omega)).
      import :: tResult
      class(tResult), intent(inout) :: this
      character(len=*), intent(in) :: ids(:)
      !! IDs of the entities this result is indexed by (nodes or electrodes)
      real(8), intent(in) :: omega(:)
      !! Angular frequencies ω (rad/s) of the sweep
    end subroutine alloc_interface
  end interface

  type, extends(tResult) :: tVoltages
    !! Result type for complex node voltages V(ω).
    !!
    !! Stores the solution voltage at each node as a function of angular frequency.
    !! Shape: (nNodes, nFrequencies). Data is complex-valued in the frequency domain.
    private
    complex(8), allocatable :: voltages(:,:)
    !! Complex voltage array: voltages(iNode, iFreq) is V_i(ω_iFreq)
  contains
    procedure :: alloc => allocVoltages
    !! Allocate voltages array with dimensions (nNodes, nFrequencies)
    procedure :: get => getVoltage
    !! Read V(iNode, iFreq)
    procedure :: set => setVoltage
    !! Write V(iNode, iFreq)
  end type tVoltages

  interface tVoltages
    !! Constructor interface for tVoltages results.
    module procedure newResultVoltages
  end interface

  type, extends(tResult) :: tLongCurrents
    !! Result type for complex longitudinal electrode currents I_long(ω).
    !!
    !! Stores the longitudinal (wire-to-wire) current flowing along each electrode
    !! as a function of angular frequency. Shape: (nElectrodes, nFrequencies).
    !! These currents represent current flow along the axis of the conductor.
    private
    complex(8), allocatable :: currents(:,:)
    !! Complex longitudinal current array: currents(iElectrode, iFreq) is I_long_i(ω_iFreq)
  contains
    procedure :: alloc => allocLongCurrents
    !! Allocate currents array with dimensions (nElectrodes, nFrequencies)
    procedure :: get => getLongCurrent
    !! Read I_long(iElectrode, iFreq)
    procedure :: set => setLongCurrent
    !! Write I_long(iElectrode, iFreq)
  end type tLongCurrents

  interface tLongCurrents
    !! Constructor interface for tLongCurrents results.
    module procedure newResultLongCurrents
  end interface

  type, extends(tResult) :: tTransCurrents
    !! Result type for complex transverse (earth-leakage) electrode currents I_trans(ω).
    !!
    !! Stores the transverse (perpendicular to conductor axis) current from each electrode
    !! to the soil as a function of angular frequency. Shape: (nElectrodes, nFrequencies).
    !! These currents represent the electromagnetic coupling to the earth medium.
    private
    complex(8), allocatable :: currents(:,:)
    !! Complex transverse current array: currents(iElectrode, iFreq) is I_trans_i(ω_iFreq)
  contains
    procedure :: alloc => allocTransCurrents
    !! Allocate currents array with dimensions (nElectrodes, nFrequencies)
    procedure :: get => getTransCurrent
    !! Read I_trans(iElectrode, iFreq)
    procedure :: set => setTransCurrent
    !! Write I_trans(iElectrode, iFreq)
  end type tTransCurrents

  interface tTransCurrents
    !! Constructor interface for tTransCurrents results.
    module procedure newResultTransCurrents
  end interface

contains

  ! =====================================================================
  ! Constructors for result types
  ! =====================================================================

  function newResultVoltages(id) result(this)
    !! Construct a tVoltages result object with the given identifier.
    character(len=*), intent(in) :: id
    !! User-assigned identifier for this result set
    type(tVoltages) :: this

    this%id = id
  end function newResultVoltages

  function newResultLongCurrents(id) result(this)
    !! Construct a tLongCurrents result object with the given identifier.
    character(len=*), intent(in) :: id
    !! User-assigned identifier for this result set
    type(tLongCurrents) :: this

    this%id = id
  end function newResultLongCurrents

  function newResultTransCurrents(id) result(this)
    !! Construct a tTransCurrents result object with the given identifier.
    character(len=*), intent(in) :: id
    !! User-assigned identifier for this result set
    type(tTransCurrents) :: this

    this%id = id
  end function newResultTransCurrents

  ! =====================================================================
  ! Base-type accessors
  ! =====================================================================

  subroutine resultStoreAxes(this, ids, omega)
    !! Copy `ids`/`omega` into the base type. Called by each concrete
    !! type's `alloc` before it allocates its own data array.
    class(tResult), intent(inout) :: this
    character(len=*), intent(in) :: ids(:)
    real(8), intent(in) :: omega(:)
    integer :: i

    allocate(this%ids(size(ids)))
    do i = 1, size(ids)
      this%ids(i) = ids(i)
    end do
    this%omega = omega
  end subroutine resultStoreAxes

  integer(4) function resultFrequencyCount(this) result(n)
    !! Number of frequencies in the axis (0 if not yet allocated).
    class(tResult), intent(in) :: this

    if (allocated(this%omega)) then
      n = size(this%omega)
    else
      n = 0
    end if
  end function resultFrequencyCount

  real(8) function resultFrequency(this, i) result(w)
    !! Angular frequency ω (rad/s) at sweep index `i`.
    class(tResult), intent(in) :: this
    integer(4), intent(in) :: i

    w = this%omega(i)
  end function resultFrequency

  integer(4) function resultEntityCount(this) result(n)
    !! Number of entities (nodes or electrodes) in this result set.
    class(tResult), intent(in) :: this

    if (allocated(this%ids)) then
      n = size(this%ids)
    else
      n = 0
    end if
  end function resultEntityCount

  function resultEntityId(this, i) result(id)
    !! User-assigned ID of entity at index `i`.
    class(tResult), intent(in) :: this
    integer(4), intent(in) :: i
    character(256) :: id

    id = this%ids(i)
  end function resultEntityId

  ! =====================================================================
  ! Allocation and data accessors
  ! =====================================================================

  subroutine allocVoltages(this, ids, omega)
    !! Allocate the voltages array with dimensions (nNodes, nFrequencies).
    class(tVoltages), intent(inout) :: this
    character(len=*), intent(in) :: ids(:)
    !! Node IDs — determines first dimension
    real(8), intent(in) :: omega(:)
    !! Angular frequencies — determines second dimension

    call this%storeAxes(ids, omega)
    allocate(this%voltages(size(ids), size(omega)))
  end subroutine allocVoltages

  complex(8) function getVoltage(this, iNode, iFreq) result(v)
    !! Read V(iNode, iFreq).
    class(tVoltages), intent(in) :: this
    integer(4), intent(in) :: iNode, iFreq

    v = this%voltages(iNode, iFreq)
  end function getVoltage

  subroutine setVoltage(this, iNode, iFreq, v)
    !! Write V(iNode, iFreq).
    class(tVoltages), intent(inout) :: this
    integer(4), intent(in) :: iNode, iFreq
    complex(8), intent(in) :: v

    this%voltages(iNode, iFreq) = v
  end subroutine setVoltage

  subroutine allocLongCurrents(this, ids, omega)
    !! Allocate the longitudinal currents array with dimensions (nElectrodes, nFrequencies).
    class(tLongCurrents), intent(inout) :: this
    character(len=*), intent(in) :: ids(:)
    !! Electrode IDs — determines first dimension
    real(8), intent(in) :: omega(:)
    !! Angular frequencies — determines second dimension

    call this%storeAxes(ids, omega)
    allocate(this%currents(size(ids), size(omega)))
  end subroutine allocLongCurrents

  complex(8) function getLongCurrent(this, iElectrode, iFreq) result(i1)
    !! Read I_long(iElectrode, iFreq).
    class(tLongCurrents), intent(in) :: this
    integer(4), intent(in) :: iElectrode, iFreq

    i1 = this%currents(iElectrode, iFreq)
  end function getLongCurrent

  subroutine setLongCurrent(this, iElectrode, iFreq, i1)
    !! Write I_long(iElectrode, iFreq).
    class(tLongCurrents), intent(inout) :: this
    integer(4), intent(in) :: iElectrode, iFreq
    complex(8), intent(in) :: i1

    this%currents(iElectrode, iFreq) = i1
  end subroutine setLongCurrent

  subroutine allocTransCurrents(this, ids, omega)
    !! Allocate the transverse currents array with dimensions (nElectrodes, nFrequencies).
    class(tTransCurrents), intent(inout) :: this
    character(len=*), intent(in) :: ids(:)
    !! Electrode IDs — determines first dimension
    real(8), intent(in) :: omega(:)
    !! Angular frequencies — determines second dimension

    call this%storeAxes(ids, omega)
    allocate(this%currents(size(ids), size(omega)))
  end subroutine allocTransCurrents

  complex(8) function getTransCurrent(this, iElectrode, iFreq) result(i2)
    !! Read I_trans(iElectrode, iFreq).
    class(tTransCurrents), intent(in) :: this
    integer(4), intent(in) :: iElectrode, iFreq

    i2 = this%currents(iElectrode, iFreq)
  end function getTransCurrent

  subroutine setTransCurrent(this, iElectrode, iFreq, i2)
    !! Write I_trans(iElectrode, iFreq).
    class(tTransCurrents), intent(inout) :: this
    integer(4), intent(in) :: iElectrode, iFreq
    complex(8), intent(in) :: i2

    this%currents(iElectrode, iFreq) = i2
  end subroutine setTransCurrent

end module mResult
