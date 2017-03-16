module lru_cache_module
	
	use doubly_linked_list_module
	use hash_map_module

	implicit none
	
	private
	
	type lru_cache_type
		type(doubly_linked_list_type), pointer :: list => null()
		type(hash_map_type), pointer :: map => null()
		integer :: capacity = 0
		contains
			procedure, public :: has
			procedure, public :: cache
	end type lru_cache_type
	
	contains
	
	function has(this, key)
		class(lru_cache_type), intent(inout) :: this
		
		has = this%map%has(key)
	end function has
	
	subroutine cache(this, key)
		class(lru_cache_type), intent(inout) :: this
		type(det_type), pointer, intent(in) :: key
		type(doubly_linked_list_node_type), pointer :: node
		
		if (this%capacity <= 0) then
			call backtrace
			stop 'Invalid LRU capacity.'
		endif
		
		if (this%has(key)) then
			node => this%map%get(key)
			call this%list%remove(node)
			call this%list%push_front(node)
		else
			if (this%list%n >= this%capacity) then
				node => this%list%back()
				call this%map%remove(node%item)
				call this%list%pop_back()
				call delete(node)
			endif
			call this%list%push_front(key)
			node => this%list%front()
			call this%map%set(key, node)
		endif
	end subroutine cache
	
end module lru_cache_module