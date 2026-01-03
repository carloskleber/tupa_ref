module mStudy
  use mMesh
  use mStructure
  implicit none

  type :: tStudy
    !! Main object, containing all data for a simulation study
    character(len=256) :: title
    type(tStructure) :: structure
    type(tMesh) :: mesh
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
        " at (", study%structure%nodes(i)%x, ", ", &
        study%structure%nodes(i)%y, ", ", &
        study%structure%nodes(i)%z, ")"
    end do
    print *, ""
    print *, "Elements:"
    do i = 1, study%structure%getElementCount()
      print *, "  ", trim(study%structure%elements(i)%id), &
        " - Length: ", study%structure%elements(i)%length, " m"
    end do
    print *, "========================================="
  end subroutine report
end module
