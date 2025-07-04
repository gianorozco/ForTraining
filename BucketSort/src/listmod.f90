module listmod
  type node
     !integer :: value
     real :: value
     type(node),pointer :: next
  end type node

  type list
     type(node), pointer :: head
  end type list

contains
  subroutine attach(the_list, new_value) !attach node at the end of a list, without sorting
    implicit none
    type(list),intent(inout) :: the_list
    !integer, intent(in) :: new_value
    real, intent(in) :: new_value

    if (.not.associated(the_list%head)) then
       allocate(the_list%head)
       the_list%head%value = new_value
    else
       call node_attach(the_list%head, new_value)
    end if
  end subroutine attach

  recursive subroutine node_attach(the_node, new_value)
    implicit none
    type(node), intent(inout) :: the_node
    !integer, intent(in) :: new_value
    real, intent(in) :: new_value
    if (.not. associated(the_node%next)) then
       allocate(the_node%next)
       the_node%next%value = new_value
    else
       call node_attach(the_node%next, new_value)
    end if
  end subroutine node_attach

  subroutine attach2(the_list, new_value) !attach without calling recursive functions
    implicit none
    type(list), intent(inout) :: the_list
    integer, intent(in) :: new_value
    type(node), pointer :: current

    if (.not. associated(the_list%head)) then
       allocate(the_list%head)
       the_list%head%value = new_value
    else
       current => the_list%head
       do while (.true.)
          if (.not. associated(current%next)) then
             allocate(current%next)
             current%next%value = new_value
             exit
          else
             current => current%next
          end if
       end do
    end if
  end subroutine attach2

  recursive subroutine insert(the_list, in_value) !attach but sorting the list
    type(list), intent(inout) :: the_list
    integer, intent(in) :: in_value
    type(node), pointer :: current, point_temp
    integer :: temp

    if (.not. associated(the_list%head)) then
       allocate(the_list%head)
       the_list%head%value = in_value
    else
       current => the_list%head
       do while (.true.)
          if (in_value < current%value) then
             allocate(point_temp)
             temp = current%value
             point_temp%value = temp
             point_temp%next => current%next
             current%value = in_value
             current%next => point_temp
             exit
          else
             if (.not. associated(current%next)) then
                allocate(current%next)
                current%next%value = in_value
                exit
             else
                current => current%next
             end if
          end if
       end do
    end if
  end subroutine insert

  subroutine print(the_list)
    implicit none
    type(list), intent(inout) :: the_list
    type(node), pointer :: current

    print *, "["
    current => the_list%head
    do while (.true.)
       print *, current%value
       if (.not. associated(current%next)) then
          exit
       end if
       current => current%next
    end do
    print *, "]"
  end subroutine print

  function list_size(the_list) result(count)
    type(list), intent(inout) :: the_list
    type(node), pointer :: current
    integer :: count

    count = 0
    if (.not. associated(the_list%head)) then
       return
    end if
    current => the_list%head
    do while (.true.)
       count = count + 1
       if (.not. associated(current%next)) then
          exit
       end if
       current => current%next
    end do
  end function list_size

  function create_array(the_list, size) result(List_Array) !create an array given a list, requires list size.
    type(list), intent(inout) :: the_list
    type(node), pointer :: current
    integer, intent(inout) :: size
    real, dimension(size) :: List_Array
    integer :: i

    if (size == 0) then
       return
    end if
    i = 1
    current => the_list%head
    do while (.true.)
       List_Array(i) = current%value
       if(.not. associated(current%next)) then
          exit
       end if
       current => current%next
       i = i + 1
    end do
  end function create_array
end module listmod
