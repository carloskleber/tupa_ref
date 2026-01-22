module mStudy
  use mMesh
  use mStructure
  use mElement
  use mMaterial
  use mResult
  use mCtes, only: newl
  implicit none

  type :: tStudy
    !! Main object, containing all data for a simulation study
    character(len=256) :: title
    type(tStructure) :: structure
    type(tMesh) :: mesh
    class(tElement), pointer :: element => null()
    class(tMaterial), pointer :: mat => null()
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
    end do
    str = str // trim(line) // newl
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
end module
