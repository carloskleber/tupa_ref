program test_plane
  !! Wire-grid plane element (docs/marx_generator.md): node/bar counts, and
  !! reuse of the nodes and bars two planes share along an edge.
  use mCtes, only: dp
  use mStructure
  use mMaterial
  use mElementPlane
  use mElement
  use mVerbosity, only: setVerbosity, VERB_QUIET
  use check
  implicit none

  type(tStructure) :: st
  class(tMaterial), allocatable :: mat
  class(tElement), allocatable :: elem
  integer(4) :: i, nDup, j

  call setVerbosity(VERB_QUIET)
  call test_init("Plane element: two planes sharing an edge")
  mat = newMaterialLinear("copper", 1.0d0, 1.0d0, 5.96d7)
  call st%addMaterial(mat)
  st%soil = newMaterialLinear("soil", 10.0d0, 1.0d0, 0.01d0)
  elem = newElementPlane("Top", [0.0d0, 0.0d0, 1.0d0], [4.0d0, 0.0d0, 0.0d0], [0.0d0, 4.0d0, 0.0d0], 2, 2, 0.05d0, "copper")
  call st%addElement(elem)
  elem = newElementPlane("Wall", [0.0d0, 0.0d0, 1.0d0], [0.0d0, 4.0d0, 0.0d0], [0.0d0, 0.0d0, 4.0d0], 2, 2, 0.05d0, "copper")
  call st%addElement(elem)
  call st%assembleStructure()
  call test_ok("15 nodes (9 + 9 - 3 shared)", st%getNodeCount() == 15, "nodes")
  call test_ok("22 bars (12 + 12 - 2 shared)", st%getElectrodeCount() == 22, "bars")
  nDup = 0
  do i = 1, st%electrodeCount
    do j = i + 1, st%electrodeCount
      if (all(st%electrodes(i)%nodeIndices == st%electrodes(j)%nodeIndices) .or. &
          (st%electrodes(i)%nodeIndices(1) == st%electrodes(j)%nodeIndices(2) .and. &
           st%electrodes(i)%nodeIndices(2) == st%electrodes(j)%nodeIndices(1))) nDup = nDup + 1
    end do
  end do
  call test_ok("no duplicate bars", nDup == 0, "dup")
  call test_ok("bars carry the radius", all(abs(st%electrodes(1:st%electrodeCount)%radius - 0.05d0) < 1.0d-15), "radius")
  call test_summary()
end program test_plane
