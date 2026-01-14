program example1
  use mStudy
  use mNode
  use mMaterial
  use mElementLine
  implicit none

  type(tStudy) :: study
  type(tNode) :: node1, node2, node3
  type(tLinear) :: copper, soil
  type(tLine) :: line1, line2
  integer :: i
  class(tElement), allocatable :: elem
  !! Temporary element

  ! Initialize study
  study%title = "Example 1 - buried bare conductor"

  ! Create 3 nodes
  call study%structure%addNode(newNode("Node_1", [0.d0, 0.d0, -0.5d0]))
  call study%structure%addNode(newNode("Node_2", [10.d0, 0.d0, -0.5d0]))
  call study%structure%addNode(newNode("Node_3", [20.d0, 0.d0, -0.5d0]))

  ! Add materials to structure
  call study%structure%addMaterial(newMaterialLinear("copper", 1.d0, 1.d0, 5.96d7))
  study%structure%soil = newMaterialLinear("", 1.d0, 1.d0, 1d-2)

  ! Create 2 tLine elements
  elem = newElementLine("Line_1", "Node_1", "Node_2", 0.01d0, 10, "copper")
  call study%structure%addElement(elem)
  elem = newElementLine("Line_2", "Node_2", "Node_3", 0.01d0, 10, "copper")
  call study%structure%addElement(elem)

  ! Print summary
  call study%report()

end program example1
