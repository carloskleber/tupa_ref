program example1
  use mStudy
  use mNode
  use mMaterial
  use mElement
  implicit none

  type(tStudy) :: study
  type(tNode) :: node1, node2, node3
  type(tLinear) :: material1, material2
  type(tLine) :: line1, line2
  integer :: i

  ! Initialize study
  study%title = "Example Transmission Line Study"

  ! Create 3 nodes using constructor
  node1 = createNode("Node_1", 0.0d0, 0.0d0, 0.0d0)
  node2 = createNode("Node_2", 10.0d0, 0.0d0, 0.0d0)
  node3 = createNode("Node_3", 20.0d0, 0.0d0, 0.0d0)

  ! Add nodes to structure
  call study%structure%addNode(node1)
  call study%structure%addNode(node2)
  call study%structure%addNode(node3)

  ! Create 2 materials (air and soil)
  ! Material 1: Air
  material1%epsilon = 8.854d-12
  material1%mu = 4.0d-7 * atan(1.0d0) * 4.0d0
  material1%sigma = 0.0d0
  material1%propagationConstant = cmplx(0.0d0, 0.0d0, kind=8)

  ! Material 2: Soil
  material2%epsilon = 80.0d0 * 8.854d-12
  material2%mu = 4.0d-7 * atan(1.0d0) * 4.0d0
  material2%sigma = 0.05d0
  material2%propagationConstant = cmplx(0.0d0, 0.0d0, kind=8)

  ! Add materials to structure
  call study%structure%addMaterial(material1)
  call study%structure%addMaterial(material2)

  ! Create 2 tLine elements
  line1%id = "Line_1"
  line1%radius = 0.01d0
  line1%nSegments = 10
  line1%nNodes = 2

  line2%id = "Line_2"
  line2%radius = 0.01d0
  line2%nSegments = 10
  line2%nNodes = 2

  ! Add elements to structure
  call study%structure%addElement(line1)
  call study%structure%addElement(line2)

  ! Print summary
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

end program example1
