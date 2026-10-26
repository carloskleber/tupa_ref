module mElectrode
  !! Line segment (electrode) connecting two tNode instances.
  !!
  !! In the HEM formulation each physical conductor is discretised into a set of
  !! cylindrical electrodes. Every electrode carries a longitudinal current `il`
  !! (along its axis) and a transverse current `it` (leaking radially to earth).
  !! Both currents are complex-valued frequency-domain quantities solved by
  !! `Mesh.injectSignal`.
  use mNode
  use mMaterial, only: tMaterial
  implicit none
  private

  public :: newElectrode, lumpedImpedance

  type, public :: tElectrode
    !! Cylindrical conductor segment between two nodes.
    character(256) :: id
    !! Identifier string — only required for key (named) electrodes
    integer(4) :: nodeIndices(2)
    !! 1-based indices into the parent tStructure%nodes array
    type(tNode), pointer :: node1 => null()
    !! Pointer to the start node (set by Structure%assemble)
    type(tNode), pointer :: node2 => null()
    !! Pointer to the end node (set by Structure%assemble)
    real(8) :: radius = 0.0d0
    !! Cylindrical radius of the electrode (m)
    class(tMaterial), pointer :: material => null()
    !! Conductor material (points into the owning element's material);
    !! null for a series-loaded segment
    logical :: loaded = .false.
    !! True for a segment whose internal-impedance slot carries the
    !! lightning-channel loading z_ch = R' + jωL' (theory.md §4.5) instead of
    !! the skin-effect impedance of a conductor material (ROADMAP Phase 10b)
    real(8) :: loadResistance = 0.0d0
    !! Series loading resistance per unit length R' (Ω/m), `loaded` only
    real(8) :: loadInductance = 0.0d0
    !! Series loading inductance per unit length L' (H/m), `loaded` only
    real(8) :: lumpResistance = 0.0d0
    !! Lumped series resistance R embedded in this segment (Ω); with
    !! `lumpInductance`/`lumpCapacitance` the total series impedance
    !! R + sL + 1/(sC) is added to the segment's internal impedance (the
    !! NEC "LD card"), so the segment keeps its geometry and coupling
    real(8) :: lumpInductance = 0.0d0
    !! Lumped series inductance L embedded in this segment (H)
    real(8) :: lumpCapacitance = 0.0d0
    !! Lumped series capacitance C embedded in this segment (F); 0 = none
    complex(8) :: il
    !! Longitudinal current through the electrode (A)
    complex(8) :: it
    !! Transverse (leakage) current of the electrode (A)
  end type tElectrode

contains

  complex(8) function lumpedImpedance(this, s) result(z)
    !! Embedded lumped series impedance `R + sL + 1/(sC)` of the segment at
    !! the complex frequency `s` (`jω`, or `c + jω` for the NLT); zero when
    !! the segment carries none. A capacitor needs `s /= 0`, which neither
    !! the harmonic sweep nor the NLT bins ever use.
    type(tElectrode), intent(in) :: this
    complex(8), intent(in) :: s

    z = cmplx(this%lumpResistance, 0.0d0, kind=8) + s * this%lumpInductance
    if (this%lumpCapacitance > 0.0d0) z = z + 1.0d0 / (s * this%lumpCapacitance)
  end function lumpedImpedance

  function newElectrode(id, nodeIndex1, nodeIndex2) result(this)
    !! Construct a tElectrode connecting two nodes by their array indices.
    !!
    !! Node pointers are not set here; they must be resolved by the enclosing
    !! tStructure after all nodes have been added.
    character(len=*), intent(in) :: id
    !! Identifier string for this electrode
    integer(4), intent(in) :: nodeIndex1
    !! 1-based index of the start node in tStructure%nodes
    integer(4), intent(in) :: nodeIndex2
    !! 1-based index of the end node in tStructure%nodes
    type(tElectrode) :: this

    this%id             = id
    this%nodeIndices(1) = nodeIndex1
    this%nodeIndices(2) = nodeIndex2
    this%il             = cmplx(0.0d0, 0.0d0, kind=8)
    this%it             = cmplx(0.0d0, 0.0d0, kind=8)
  end function newElectrode

  function center(this) result(point)
    !! Return the geometric midpoint of the electrode segment (m).
    !!
    !! Returns the origin [0,0,0] if either node pointer is not yet associated.
    class(tElectrode), intent(in) :: this
    real(8) :: point(3)

    if (.not. associated(this%node1) .or. .not. associated(this%node2)) then
      point = [0.0d0, 0.0d0, 0.0d0]
      return
    end if

    point = 0.5d0 * (this%node1%p + this%node2%p)
  end function center

end module mElectrode
