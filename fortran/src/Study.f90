module mStudy
  use mMesh
  use mStructure
  use mElement
  use mResult
  implicit none

  type :: tStudy
    !! Main object, containing all data for a simulation study
    character(len=256) :: title
    type(tStructure) :: structure
    type(tMesh) :: mesh
    class(tElement), pointer :: element => null()
    class(tResult), allocatable :: results(:)
    !! Array of results to be extracted and postprocessing]
  contains
    procedure :: report
  end type tStudy
contains
  subroutine report(this)
    !! Print a summary report of the study
    implicit none
    class(tStudy), intent(in) :: this
    integer :: i
    class(tElement), pointer :: element => null()

    print *, "========================================="
    print *, "Example Study Initialization"
    print *, "========================================="
    print *, "Study Title: ", trim(this%title)
    print *, ""
    print *, "Number of Nodes: ", this%structure%getNodeCount()
    print *, "Number of Materials: ", this%structure%getMaterialCount()
    print *, "Number of Elements: ", this%structure%getElementCount()
    print *, ""
    print *, "Nodes:"
    do i = 1, this%structure%getNodeCount()
      print *, "  ", trim(this%structure%nodes(i)%id), &
        " at (", this%structure%nodes(i)%p(1), ", ", &
        this%structure%nodes(i)%p(2), ", ", &
        this%structure%nodes(i)%p(3), ")"
    end do
    print *, ""
    print *, "Elements:"
    do i = 1, this%structure%getElementCount()
      element => this%structure%getElement(i)
      print *, "  ", trim(element%id), " with material ", &
        trim(element%material%id), " and ", element%nNodes, " nodes."
    end do
    print *, "========================================="
  end subroutine report
end module
