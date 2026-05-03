module mStudy
  !! Top-level orchestration object for a complete electromagnetic study.
  !!
  !! `tStudy` contains all data needed to define and execute one complete
  !! simulation: geometry (structure), mesh, materials, loads, and results.
  !! It serves as the container passed between I/O (JSON parsing) and the
  !! frequency-domain solver.
  !!
  !! Typical workflow:
  !! 1. JSON parsing creates and populates a tStudy instance
  !! 2. Study calls `structure%assembleStructure()` to discretise elements
  !! 3. Study calls `mesh%calcTopologia()` and `mesh%calcFreq2(ω)` to solve
  !! 4. Study stores results from `mesh%getSaidas()`
  !! 5. I/O writes results to CSV or JSON
  use mMesh
  use mStructure
  use mElement
  use mMaterial
  use mResult
  use mCtes, only: newl
  implicit none

  type :: tStudy
    !! Container for a complete electromagnetic field study.
    !!
    !! Manages the geometric structure, mesh, and collection of frequency-domain
    !! results. All inputs (nodes, elements, materials, loads) are stored in
    !! the `structure` component; all computed outputs are stored in the
    !! `results` array.
    character(len=256) :: title
    !! User-assigned name for the study
    type(tStructure) :: structure
    !! Geometric model: nodes, elements, materials, and soil/air media
    type(tMesh) :: mesh
    !! Frequency-domain mesh and solver: topology matrices and impedance system
    class(tElement), pointer :: element => null()
    !! Temporary pointer for iteration during element management
    class(tMaterial), pointer :: mat => null()
    !! Temporary pointer for iteration during material management
    class(tResult), allocatable :: results(:)
    !! Array of frequency-domain results: voltages, longitudinal currents, transverse currents
  contains
    procedure :: report
    !! Print a human-readable summary of the study contents
    procedure :: run
    !! Execute the full simulation pipeline (discretisation, solving, extraction)
  end type tStudy

contains

  ! =====================================================================
  ! Study execution and reporting
  ! =====================================================================

  subroutine run(this)
    !! Execute the complete simulation pipeline for this study.
    !!
    !! **Phase 1 (current)**: Placeholder that prints a message.
    !!
    !! **Phase 2 (future)**: Will implement the full sequence:
    !! 1. Call `this%structure%assembleStructure()` to discretise all elements
    !! 2. Call `this%mesh%calcTopologia()` to build topology matrices (A, B, C, D)
    !! 3. For each frequency ω in the frequency axis:
    !!    - Call `this%mesh%calcParam(ω)` to compute medium constants
    !!    - Call `this%mesh%calcFreq2(ω)` to assemble Zeq and solve Zeq·x = b
    !!    - Call `this%mesh%getSaidas(ω)` to extract voltages and currents
    !! 4. Store results in `this%results` for output
    class(tStudy), intent(inout) :: this

    print *, "Study '", trim(this%title), "' loaded (solver not yet wired)."
  end subroutine run

  subroutine report(this)
    !! Print a formatted text report of the study geometry and properties.
    !!
    !! Outputs:
    !! - Study title
    !! - Node count, material count, element count
    !! - Detailed list of all nodes with coordinates
    !! - Detailed list of all materials with properties
    !! - Detailed list of all elements with their parameters
    class(tStudy), intent(in) :: this
    character(:), allocatable :: str
    character(len=256) :: line
    integer :: i
    class(tElement), pointer :: element => null()
    class(tMaterial), pointer :: mat => null()

    str = "=========================================" // newl // &
          "Example Study Initialization" // newl // &
          "=========================================" // newl
    str = str // "Study Title: " // trim(this%title) // newl
    write(line,'("Number of Nodes: ",I0)') this%structure%getNodeCount()
    str = str // trim(line) // newl
    write(line,'("Number of Materials: ",I0)') this%structure%getMaterialCount()
    str = str // trim(line) // newl
    write(line,'("Number of Elements: ",I0)') this%structure%getElementCount()
    str = str // trim(line) // newl
    str = str // "Nodes:" // newl
    do i = 1, this%structure%getNodeCount()
      write(line,'("  ",A," at (",F0.2,", ",F0.2,", ",F0.2,")")') &
        trim(this%structure%nodes(i)%id), &
        this%structure%nodes(i)%p(1), this%structure%nodes(i)%p(2), &
        this%structure%nodes(i)%p(3)
      str = str // trim(line) // newl
    end do
    str = str // "Materials:" // newl
    do i = 1, this%structure%getMaterialCount()
      mat => this%structure%getMaterial(i)
      call mat%report(str)
    end do
    str = str // "Elements:" // newl
    do i = 1, this%structure%getElementCount()
      element => this%structure%getElement(i)
      call element%report(str)
    end do
    str = str // "=========================================" // newl
    print *, str
  end subroutine report

end module mStudy
