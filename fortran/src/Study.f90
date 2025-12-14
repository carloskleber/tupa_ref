module mStudy
  use mMesh
  use mStructure
  implicit none

  type :: tStudy
    character(len=256) :: title
    type(tStructure) :: structure
    type(tMesh) :: mesh
  end type tStudy
contains

end module
