module mElectrode
  implicit none
  private

  type :: tElectrode
    !! 
    character(len=256) :: id
    !! Electrode identifier
    integer(4) nNodes
    integer(4), allocatable :: nodeIndices(:)
    !! Indices of nodes that make up the electrode
  end type tElectrode
contains
end module mElectrode
