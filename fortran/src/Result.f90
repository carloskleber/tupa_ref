module mResult
  !! Container for frequency-domain solution results: node voltages and electrode currents.
  !!
  !! This module defines abstract and concrete result types that store the outputs of
  !! the frequency-domain solver for each angular frequency. Each result type owns:
  !! - Complex-valued data (voltages or currents)
  !! - Pointers to the geometric entities (nodes, elements, electrodes)
  !! - Pointer to the frequency axis (array of ω values)
  !!
  !! Three concrete types are provided:
  !! - `tVoltages`: complex node voltages V_i(ω) — shape (nNodes, nFrequencies)
  !! - `tLongCurrents`: longitudinal electrode currents I_long(ω) — shape (nElectrodes, nFrequencies)
  !! - `tTransCurrents`: transverse (earth-leakage) electrode currents I_trans(ω) — shape (nElectrodes, nFrequencies)
  !!
  !! After solver execution, results are obtained by calling `getOutputs()` from mMesh.
  use mNode
  use mElement
  use mElectrode
  implicit none

  type, abstract :: tResult
    !! Abstract base type for frequency-domain results.
    !!
    !! Stores references to the geometric entities and frequency axis, plus
    !! deferred allocation of concrete data arrays. Concrete types override
    !! `alloc()` to allocate their specific data storage.
    private
    character(256) :: id
    !! User-assigned identifier for the result set
    type(tNode), pointer :: nodes(:)
    !! Pointer to array of nodes (for voltage results)
    class(tElement), pointer :: elements(:)
    !! Pointer to array of elements (for possible element-based results)
    type(tElectrode), pointer :: electrodes(:)
    !! Pointer to array of electrodes (for current results)
    real(8), pointer :: omega(:)
    !! Pointer to array of angular frequencies ω at which solution was computed
  contains
    procedure(alloc_interface), deferred :: alloc
    !! Allocate internal data arrays according to entity counts and frequency axis
  end type tResult

  abstract interface
    subroutine alloc_interface(this, nodes, elements, electrodes, omega)
      !! Deferred interface for allocating result data arrays.
      !!
      !! Each concrete type implements this to allocate its own data storage
      !! (voltages or currents) with dimensions derived from the passed geometry
      !! and frequency axis.
      import :: tResult, tNode, tElement, tElectrode
      class(tResult), intent(inout) :: this
      type(tNode), pointer, intent(inout) :: nodes(:)
      !! Array of nodes in the mesh
      class(tElement), pointer, intent(inout) :: elements(:)
      !! Array of elements in the mesh
      type(tElectrode), pointer, intent(inout) :: electrodes(:)
      !! Array of electrodes in the mesh
      real(8), pointer, intent(inout) :: omega(:)
      !! Array of angular frequencies
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
  ! Allocation procedures
  ! =====================================================================

  subroutine allocVoltages(this, nodes, elements, electrodes, omega)
    !! Allocate the voltages array with dimensions (nNodes, nFrequencies).
    !!
    !! Called after the mesh and frequency axis are known. Stores references
    !! to the geometry and frequency array, then allocates internal storage
    !! for complex voltages at each node and frequency.
    class(tVoltages), intent(inout) :: this
    type(tNode), pointer, intent(inout) :: nodes(:)
    !! Array of nodes in the mesh — determines first dimension
    class(tElement), pointer, intent(inout) :: elements(:)
    !! Array of elements (unused for voltage results)
    type(tElectrode), pointer, intent(inout) :: electrodes(:)
    !! Array of electrodes (unused for voltage results)
    real(8), pointer, intent(inout) :: omega(:)
    !! Array of angular frequencies — determines second dimension

    this%nodes => nodes
    this%elements => elements
    this%electrodes => electrodes
    this%omega => omega
    allocate(this%voltages(size(nodes), size(omega)))
  end subroutine allocVoltages

  subroutine allocLongCurrents(this, nodes, elements, electrodes, omega)
    !! Allocate the longitudinal currents array with dimensions (nElectrodes, nFrequencies).
    !!
    !! Called after the mesh and frequency axis are known. Stores references
    !! to the geometry and frequency array, then allocates internal storage
    !! for complex longitudinal currents at each electrode and frequency.
    class(tLongCurrents), intent(inout) :: this
    type(tNode), pointer, intent(inout) :: nodes(:)
    !! Array of nodes (unused for longitudinal current results)
    class(tElement), pointer, intent(inout) :: elements(:)
    !! Array of elements (unused for longitudinal current results)
    type(tElectrode), pointer, intent(inout) :: electrodes(:)
    !! Array of electrodes in the mesh — determines first dimension
    real(8), pointer, intent(inout) :: omega(:)
    !! Array of angular frequencies — determines second dimension

    this%nodes => nodes
    this%elements => elements
    this%electrodes => electrodes
    this%omega => omega
    allocate(this%currents(size(electrodes), size(omega)))
  end subroutine allocLongCurrents

  subroutine allocTransCurrents(this, nodes, elements, electrodes, omega)
    !! Allocate the transverse currents array with dimensions (nElectrodes, nFrequencies).
    !!
    !! Called after the mesh and frequency axis are known. Stores references
    !! to the geometry and frequency array, then allocates internal storage
    !! for complex transverse (earth-leakage) currents at each electrode and frequency.
    class(tTransCurrents), intent(inout) :: this
    type(tNode), pointer, intent(inout) :: nodes(:)
    !! Array of nodes (unused for transverse current results)
    class(tElement), pointer, intent(inout) :: elements(:)
    !! Array of elements (unused for transverse current results)
    type(tElectrode), pointer, intent(inout) :: electrodes(:)
    !! Array of electrodes in the mesh — determines first dimension
    real(8), pointer, intent(inout) :: omega(:)
    !! Array of angular frequencies — determines second dimension

    this%nodes => nodes
    this%elements => elements
    this%electrodes => electrodes
    this%omega => omega
    allocate(this%currents(size(electrodes), size(omega)))
  end subroutine allocTransCurrents

end module mResult
