module QuickSort_Mod
  implicit none
  public :: quicksort
  public :: partition
contains
  subroutine swap(a, b)
    implicit none
    real, intent(inout) :: a, b
    real :: temp

    temp = a
    a = b
    b = temp
  end subroutine swap
  recursive subroutine quicksort(A, p, r, comp_info)
    integer, intent(inout) :: p
    integer, intent(inout) :: r
    integer :: q, q_input
    real, intent(inout) :: A(:)
    integer, intent(inout) :: comp_info

    if (p < r) then
       q = partition(A,p,r, comp_info)
       q_input = q - 1
       call quicksort(A,p,q_input, comp_info)
       q_input = q + 1
       call quicksort(A,q_input, r, comp_info)
    end if
  end subroutine quicksort

  function partition(A, p, r, comp_info) result(res)
    real, intent(inout) :: A(:)
    integer, intent(inout) :: p
    integer, intent(in) :: r
    real :: x
    integer :: i, j
    integer :: res
    integer, intent(inout) :: comp_info

    x = A(r)
    i = p - 1
    do j=p, r-1
       if (A(j) <= x) then
          i = i + 1
          if (i /= j) then
            call swap(A(i), A(j))
         end if
      end if
      comp_info = comp_info + 1
    end do
    if ((i+1) /= r ) then
       call swap (A(i+1), A(r))
    end if
    res = i + 1
   end function partition
end module QuickSort_Mod
