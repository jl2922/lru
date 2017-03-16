module doubly_linked_list_module

	use det_module
	
	implicit none
	
	private
	
	type doubly_linked_list_node_type
		type(doubly_linked_list_node_type), pointer :: prev => null()
		type(doubly_linked_list_node_type), pointer :: next => null()
		type(det_type), pointer :: item => null()
	end type doubly_linked_list_node_type
	
	type doubly_linked_list_type
		type(doubly_linked_list_node_type), pointer :: head => null()
		type(doubly_linked_list_node_type), pointer :: tail => null()
		integer, public :: n = 0
		contains
			generic, public :: push_front => push_front_item, push_front_node
			procedure, public :: pop_back
			procedure, public :: front
			procedure, public :: back
			procedure, public :: remove
			procedure, public :: size
			procedure :: push_front_item
			procedure :: push_front_node
	end type doubly_linked_list_type
	
	contains
	
	function new_node(item) result(node)
		type(det_type), pointer, intent(inout) :: item
		type(doubly_linked_list_node_type), pointer :: node
		
		allocate(node)
		node%item => item
	end function new_node
	
	function new_doubly_linked_list() result(list)
		type(doubly_linked_list_type), pointer :: list
		
		allocate(this%head)
		allocate(this%tail)
	end function new_doubly_linked_list
	
	function delete(list)
		type(doubly_linked_list_type), pointer, intent(inout) :: list
		
		if (.not. associated(list)) return
		call delete(list%head)
		call delete(list%tail)
		deallocate(list)
		nullify(list)
	end function delete
	
	function delete(node)
		type(doubly_linked_list_node_type), pointer, intent(inout) :: node
		
		if (.not. associated(node)) return
		call delete(node%item)
		deallocate(node)
		nullify(node)
	end function delete
	
	subroutine push_front_item(this, item)
		class(doubly_linked_list_type), intent(inout) :: this
		type(det_type), pointer, intent(in) :: item
		type(doubly_linked_list_node_type), pointer :: node

		node => new_node(item)
		call this%push_front(node)
	end subroutine push_front_item
	
	subroutine push_front_node(this, node)
		class(doubly_linked_list_type), intent(inout) :: this
		type(doubly_linked_list_node_type), pointer, intent(in) :: node

		node%next => this%head%next
		node%prev => this%head
		this%head%next => node
		node%next%prev => node
		this%n = this%n + 1
	end subroutine push_front_node

	subroutine pop_back(this)
		class(doubly_linked_list_type), intent(inout) :: this
		type(doubly_linked_list_node_type), pointer :: node
		
		if (this%n <= 0) return
		node => this%tail%prev
		call remove(node)
	end subroutine pop_back

	subroutine remove(this, node)
		class(doubly_linked_list_type), intent(inout) :: this
		type(doubly_linked_list_node_type), pointer, intent(in) :: node
		
		node%prev%next => node%next
		node%next%prev => node%prev
		this%n = this%n - 1
	end subroutine remove
	
	function front(this)
		class(doubly_linked_list_type), intent(inout) :: this
		type(doubly_linked_list_node_type), pointer :: front
		
		front => this%head%next
	end function front
	
	function back(this)
		class(doubly_linked_list_type), intent(inout) :: this
		type(doubly_linked_list_node_type), pointer :: back
		
		back => this%tail%prev
	end function back
	
	function size(this)
		class(doubly_linked_list_type), intent(inout) :: this
		integer :: size
		
		size = this%n
	end function size

end module doubly_linked_list_module