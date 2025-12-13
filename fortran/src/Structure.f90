module mStructure
    use stdlib_lists
    use stdlib_strings
    use mNode
    implicit none
    private
    public :: tStructure

    type :: tStructure
        type(string_type) :: name
        !! Structure identifier
        type(list_t) :: nodes
        !! Dynamic list of nodes
    contains
        procedure :: addNode => addNodeToStructure
        procedure :: getNodeCount => getNodeCountStructure
    end type tStructure

contains

    subroutine addNodeToStructure(this, node)
        class(tStructure), intent(inout) :: this
        type(tNode), intent(in) :: node
        
        call this%nodes%push_back(node)
    end subroutine addNodeToStructure

    function getNodeCountStructure(this) result(count)
        class(tStructure), intent(in) :: this
        integer :: count
        
        count = this%nodes%size()
    end function getNodeCountStructure

end module mStructure