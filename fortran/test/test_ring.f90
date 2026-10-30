program test_ring
  !! Ring element: vertex count, closed loop, radius of every vertex from the
  !! centre, tilted ring in its plane, reuse of a coincident node.
  use mCtes, only: dp
  use mStructure
  use mMaterial
  use mElementRing
  use mElement
  use mNode
  use mVerbosity, only: setVerbosity, VERB_QUIET
  use check
  implicit none

  type(tStructure) :: st
  class(tMaterial), allocatable :: mat
  class(tElement), allocatable :: elem
  integer(4) :: i, first
  real(8) :: rmax, rmin, dn, c(3), n(3)

  call setVerbosity(VERB_QUIET)
  call test_init("Ring element: closed polygon of chords")
  mat = newMaterialLinear("alu", 1.0d0, 1.0d0, 3.54d7)
  call st%addMaterial(mat)
  st%soil = newMaterialLinear("soil", 10.0d0, 1.0d0, 0.01d0)

  ! Horizontal ring: 10 chords, first vertex at centre + R y
  c = [-40.69d0, -47.97d0, 24.0d0]
  elem = newElementRing("torgi", c, [0.0d0, 0.0d0, 1.0d0], 3.75d0, 1.0d0, 10, "alu")
  call st%addElement(elem)
  call st%assembleStructure()
  call test_ok("10 nodes", st%getNodeCount() == 10, "nodes")
  call test_ok("10 bars (closed loop)", st%getElectrodeCount() == 10, "bars")
  first = st%electrodes(1)%nodeIndices(1)
  call test_ok("last bar closes on the first node", st%electrodes(10)%nodeIndices(2) == first, "closed")
  call test_ok("first vertex at centre + R y", &
               maxval(abs(st%nodes(first)%p - [c(1), c(2) + 3.75d0, c(3)])) < 1.0d-12, "vertex 0")
  rmax = 0.0d0; rmin = huge(1.0d0)
  do i = 1, st%nodeCount
    rmax = max(rmax, sqrt(sum((st%nodes(i)%p - c)**2)))
    rmin = min(rmin, sqrt(sum((st%nodes(i)%p - c)**2)))
  end do
  call test_ok("all vertices on the circle", abs(rmax - 3.75d0) < 1.0d-12 .and. abs(rmin - 3.75d0) < 1.0d-12, "radius")
  call test_ok("bars carry the conductor radius", &
               all(abs(st%electrodes(1:st%electrodeCount)%radius - 1.0d0) < 1.0d-15), "cond radius")

  ! Tilted ring: every vertex stays in the plane normal to n, on the circle
  call new_structure_tilted()

contains

  subroutine new_structure_tilted()
    type(tStructure) :: s2
    class(tElement), allocatable :: e2
    real(8) :: p(3)
    integer(4) :: k

    c = [5.0d0, 0.0d0, 10.0d0]
    n = [1.0d0, 1.0d0, 1.0d0] / sqrt(3.0d0)
    mat = newMaterialLinear("alu", 1.0d0, 1.0d0, 3.54d7)
    call s2%addMaterial(mat)
    s2%soil = newMaterialLinear("soil", 10.0d0, 1.0d0, 0.01d0)
    e2 = newElementRing("tilt", c, [1.0d0, 1.0d0, 1.0d0], 2.0d0, 0.05d0, 8, "alu")
    call s2%addElement(e2)
    call s2%assembleStructure()
    dn = 0.0d0; rmax = 0.0d0
    do k = 1, s2%nodeCount
      p = s2%nodes(k)%p - c
      dn = max(dn, abs(dot_product(p, n)))
      rmax = max(rmax, abs(sqrt(sum(p**2)) - 2.0d0))
    end do
    call test_ok("tilted ring is planar", dn < 1.0d-12, "planar")
    call test_ok("tilted ring is circular", rmax < 1.0d-12, "circular")
    call test_ok("tilted ring has 8 bars", s2%getElectrodeCount() == 8, "bars")
    call test_summary()
  end subroutine new_structure_tilted

end program test_ring
