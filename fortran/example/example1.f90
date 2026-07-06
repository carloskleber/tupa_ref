program example1
  use mStudy
  use mNode
  use mMaterial
  use mElementLine
  use mCtes
  use stdlib_version
  implicit none

  type(tStudy) :: study
  type(tNode) :: node1, node2, node3
  type(tLinear) :: copper, soil
  type(tLine) :: line1, line2
  class(tElement), allocatable :: elem
  !! Temporary element
  class(tMaterial), allocatable :: mat
  integer :: i

  ! debug - stdlib version
  print *, stdlib_version_string

  print *, color_green, "Starting Example 1: buried bare conductor", color_reset
  ! Initialize study
  study%title = "Example 1 - buried bare conductor"

  call study%structure%addNode(newNode("Node_1", [0.d0, 0.d0, -0.5d0]))
  call study%structure%addNode(newNode("Node_2", [2.d0, 0.d0, -0.5d0]))

  mat = newMaterialLinear("copper", 1.d0, 1.d0, 5.96d7)
  call study%structure%addMaterial(mat)
  study%structure%soil = newMaterialLinear("", 1.d0, 1.d0, 1d-2)

  ! Create tLine element
  elem = newElementLine("Line_1", "Node_1", "Node_2", 0.01d0, 2, "copper")
  call study%structure%addElement(elem)

  ! Run the study: 100 kHz, 1 A injected at Node_1
  call study%run(2.0d0 * PI * 1.0d5, ["Node_1"], [cmplx(1.0d0, 0.0d0, kind=8)])

  ! Print summary
  call study%report()
  print *, color_green, "Example 1 completed.", color_reset
end program example1
