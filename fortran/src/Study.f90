module mStudy
  use mMesh
  use mStructure
  implicit none

  type :: tStudy
    !! Main object, containing all data for a simulation study
    character(len=256) :: title
    type(tStructure) :: structure
    type(tMesh) :: mesh
    class(tElement), pointer :: element => null()
    type(tResult), allocatable :: results(:)
    !! Array of results to be extracted and postprocessing
  end type tStudy
contains
  subroutine report(study)
    !! Print a summary report of the study
    implicit none
    type(tStudy), intent(in) :: study
    print *, "========================================="
    print *, "Example Study Initialization"
    print *, "========================================="
    print *, "Study Title: ", trim(study%title)
    print *, ""
    print *, "Number of Nodes: ", study%structure%getNodeCount()
    print *, "Number of Materials: ", study%structure%getMaterialCount()
    print *, "Number of Elements: ", study%structure%getElementCount()
    print *, ""
    print *, "Nodes:"
    do i = 1, study%structure%getNodeCount()
      print *, "  ", trim(study%structure%nodes(i)%id), &
        " at (", study%structure%nodes(i)%p(1), ", ", &
        study%structure%nodes(i)%p(2), ", ", &
        study%structure%nodes(i)%p(3), ")"
    end do
    print *, ""
    print *, "Elements:"
    do i = 1, study%structure%getElementCount()
      element = study%structure%getElement(i)
      print *, "  ", trim(element%id), &
        " - Length: ", element%length, " m"
    end do
    print *, "========================================="
  end subroutine report
end module
