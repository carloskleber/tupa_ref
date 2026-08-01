program test_mesh_element
  !! Tests for tMeshElement%assemble (mElementMesh) and the tStructure
  !! FIFO element-ordering fix it depends on (plan: docs/adr/0020).
  use mStructure
  use mElement
  use mElementMesh
  use mElementLine
  use mMaterial
  use mNode
  use check
  implicit none

  type(tStructure) :: structure
  class(tElement), allocatable :: elem
  class(tMaterial), allocatable :: mat
  real(8), parameter :: TOL = 1.0d-12
  integer(4) :: k

  ! =====================================================================
  ! Square grid: 3x3 main nodes, 2 segments/bar
  ! =====================================================================

  call test_init("tMeshElement%assemble: square grid (3x3 rows, 2 segments/bar) counts")

  mat = newMaterialLinear("cond", 1.0d0, 1.0d0, 5.8d7)
  call structure%addMaterial(mat)

  elem = newElementMesh("M", [0.0d0, 0.0d0, -1.0d0], 4.0d0, 4.0d0, 3, 3, 0.01d0, 2, "cond")
  call structure%addElement(elem)
  call structure%assembleStructure()

  ! 9 main nodes + 12 bars * (2-1) internal nodes = 21; 12 bars * 2 segments = 24 electrodes
  call test_ok("node count == 21 (9 main + 12 internal)", &
               structure%getNodeCount() == 21, "expected 21 nodes total")
  call test_ok("electrode count == 24 (12 bars * 2 segments)", &
               structure%getElectrodeCount() == 24, "expected 24 electrodes")

  call test_init("tMeshElement%assemble: square grid main node ID mnemonic and positions")

  block
    integer(4) :: idx
    idx = structure%findNodeIndex("M-0000")
    call test_ok("main node M-0000 exists at (0,0,-1)", &
                 idx > 0 .and. all(abs(structure%nodes(idx)%p - [0.0d0, 0.0d0, -1.0d0]) < TOL), &
                 "corner node M-0000 missing or mispositioned")

    idx = structure%findNodeIndex("M-0202")
    call test_ok("main node M-0202 exists at (4,4,-1)", &
                 idx > 0 .and. all(abs(structure%nodes(idx)%p - [4.0d0, 4.0d0, -1.0d0]) < TOL), &
                 "opposite corner node M-0202 missing or mispositioned")

    idx = structure%findNodeIndex("M-0102")
    call test_ok("main node M-0102 (row=1, col=2) exists at (4,2,-1)", &
                 idx > 0 .and. all(abs(structure%nodes(idx)%p - [4.0d0, 2.0d0, -1.0d0]) < TOL), &
                 "edge node M-0102 missing or mispositioned")
  end block

  ! =====================================================================
  ! Non-square grid: 3 rows x 4 cols, 1 segment/bar (no internal nodes) —
  ! the concrete regression test for the legacy Matlab stride bug
  ! (Malha.m's node-index formula is only correct when rowsX == rowsY).
  ! =====================================================================

  call test_init("tMeshElement%assemble: non-square grid (3 rows x 4 cols) full connectivity")

  block
    type(tStructure) :: s2
    integer(4) :: degree(12), idx, n1, n2, expected(12)
    character(len=8) :: nodeIds(12)
    integer(4) :: row, col, p

    mat = newMaterialLinear("cond", 1.0d0, 1.0d0, 5.8d7)
    call s2%addMaterial(mat)
    elem = newElementMesh("G", [0.0d0, 0.0d0, -2.0d0], 6.0d0, 4.0d0, 3, 4, 0.01d0, 1, "cond")
    call s2%addElement(elem)
    call s2%assembleStructure()

    call test_ok("node count == 12 (3x4 main, no internal nodes)", &
                 s2%getNodeCount() == 12, "expected 12 nodes total")
    call test_ok("electrode count == 17 (3*3 X-bars + 4*2 Y-bars)", &
                 s2%getElectrodeCount() == 17, "expected 17 electrodes")

    ! Build the id -> expected-degree table for all 12 main nodes
    ! (corners: 2, edges: 3, interior: 4) and verify by direct adjacency
    ! count from the assembled electrodes — this only comes out right if
    ! every bar connects the geometrically correct pair of main nodes,
    ! which is exactly what the legacy square-only index formula gets
    ! wrong for a non-square grid.
    p = 0
    do row = 0, 2
      do col = 0, 3
        p = p + 1
        write(nodeIds(p), '("G-",I2.2,I2.2)') row, col
        if ((row == 0 .or. row == 2) .and. (col == 0 .or. col == 3)) then
          expected(p) = 2
        else if (row == 1 .and. col > 0 .and. col < 3) then
          expected(p) = 4
        else
          expected(p) = 3
        end if
      end do
    end do

    degree = 0
    do k = 1, s2%getElectrodeCount()
      n1 = s2%electrodes(k)%nodeIndices(1)
      n2 = s2%electrodes(k)%nodeIndices(2)
      do p = 1, 12
        idx = s2%findNodeIndex(trim(nodeIds(p)))
        if (idx == n1 .or. idx == n2) degree(p) = degree(p) + 1
      end do
    end do

    call test_ok("adjacency degree matches a correct 3x4 grid (2/3/4 corner/edge/interior)", &
                 all(degree == expected), "wrong topology — a stride-formula bug would miswire this")
  end block

  ! Note: rowsX < 2 and position z == 0 both call raiseError (fatal —
  ! fortran-error-handler halts the process on a critical ErrorInstance),
  ! same as tLine's own "node/material not found" guards in Line.f90 —
  ! neither has a dedicated test in this codebase either, for the same
  ! reason: there is no in-process way to assert on a call that halts the
  ! test binary itself.

  ! =====================================================================
  ! FIFO ordering: a "line" declared after a "mesh" must be able to
  ! connect to one of the mesh's main nodes (regression test for the
  ! Structure.f90 addElementToStructure prepend -> append fix).
  ! =====================================================================

  call test_init("tStructure: FIFO element order lets a later line reference an earlier mesh's node")

  block
    type(tStructure) :: s3
    class(tElement), allocatable :: meshElem, lineElem

    mat = newMaterialLinear("cond", 1.0d0, 1.0d0, 5.8d7)
    call s3%addMaterial(mat)
    call s3%addNode(newNode("Anchor", [10.0d0, 10.0d0, 5.0d0]))

    meshElem = newElementMesh("M2", [0.0d0, 0.0d0, -1.0d0], 2.0d0, 2.0d0, 2, 2, 0.01d0, 1, "cond")
    call s3%addElement(meshElem)
    lineElem = newElementLine("Down", "Anchor", "M2-0000", 0.01d0, 1, "cond")
    call s3%addElement(lineElem)

    call s3%assembleStructure()

    call test_ok("mesh (declared first) then line referencing M2-0000 (declared second) both assembled", &
                 s3%findNodeIndex("M2-0000") > 0 .and. s3%getElectrodeCount() == 5, &
                 "line failed to resolve a node created by an earlier-declared mesh element " // &
                 "(FIFO ordering regression)")
  end block

  call test_summary()

end program test_mesh_element
