program test_assemble
  !! Tests for tLine%assemble and the tStructure node/electrode wiring it
  !! relies on (ROADMAP.md Phase 1, item 1).
  use mStructure
  use mElement
  use mElementLine
  use mMaterial
  use mNode
  use check
  implicit none

  type(tStructure) :: structure
  type(tNode) :: n1, n2
  class(tElement), allocatable :: elem
  class(tMaterial), allocatable :: mat
  integer :: k
  real(8), parameter :: TOL = 1.0d-12

  call test_init("tLine%assemble: node and electrode counts")

  n1 = newNode("N1", [0.0d0, 0.0d0, 0.0d0])
  n2 = newNode("N2", [10.0d0, 0.0d0, 0.0d0])
  call structure%addNode(n1)
  call structure%addNode(n2)

  mat = newMaterialLinear("cond", 1.0d0, 1.0d0, 5.8d7)
  call structure%addMaterial(mat)

  elem = newElementLine("L1", "N1", "N2", 0.007d0, 10, "cond")
  call structure%addElement(elem)

  call structure%assembleStructure()

  call test_ok("node count == 2 boundary + 9 internal", &
               structure%getNodeCount() == 11, "expected 11 nodes total")
  call test_ok("electrode count == 10", &
               structure%getElectrodeCount() == 10, "expected 10 electrodes")

  call test_init("tLine%assemble: internal node positions")

  block
    logical :: allOk
    allOk = .true.
    do k = 1, 9
      if (any(abs(structure%nodes(2+k)%p - [real(k, kind=8), 0.0d0, 0.0d0]) > TOL)) allOk = .false.
    end do
    call test_ok("internal nodes equally spaced along the line", allOk, &
                 "internal node positions do not match linear interpolation")
  end block

  call test_init("tLine%assemble: electrode connectivity chain")

  block
    logical :: allOk
    allOk = .true.
    if (any(structure%electrodes(1)%nodeIndices /= [1, 3])) allOk = .false.
    do k = 2, 9
      if (any(structure%electrodes(k)%nodeIndices /= [k+1, k+2])) allOk = .false.
    end do
    if (any(structure%electrodes(10)%nodeIndices /= [11, 2])) allOk = .false.
    call test_ok("electrodes chain boundary -> internal... -> boundary", allOk, &
                 "electrode node-index chain does not match the expected sequence")
  end block

  call test_init("tLine%assemble: radius and material resolution")

  block
    logical :: allOk
    allOk = .true.
    do k = 1, 10
      if (abs(structure%electrodes(k)%radius - 0.007d0) > TOL) allOk = .false.
      if (.not. associated(structure%electrodes(k)%material)) allOk = .false.
    end do
    call test_ok("all electrodes carry the line's radius", allOk, &
                 "electrode radius was not propagated from the tLine element")
    call test_ok("material ID resolved correctly", &
                 trim(structure%electrodes(1)%material%id) == "cond", &
                 "electrode material does not point to the resolved 'cond' material")
  end block

  call test_summary()

end program test_assemble
