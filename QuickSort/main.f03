program main
  use perm_mod
  implicit none
  type :: sort_results
     integer :: C
     integer :: S
  end type sort_results
  integer, dimension(20) :: rand_perm
  integer :: size, i, sumC, sumS, start
  real :: average
  type(sort_results) :: perm_info

  perm_info%C = 0
  perm_info%S = 0
  size = 20
  start = 1

  do i = 1, 500
     rand_perm = random_permutation(size)
     call quicksort(rand_perm, start, size, perm_info)
  end do
  print *, perm_info%C, perm_info%S
  print *, "Average of C is:"
  average = real(perm_info%C) / real(500)
  print *, average
  print *, "Average of S is:"
  average = real(perm_info%S) / real(500)
  print *, average
contains
  recursive subroutine quicksort(A, p, r, perm_info)
    integer, intent(inout) :: p
    integer, intent(inout) :: r
    integer :: q, q_input
    integer, intent(inout) :: A(:)
    type(sort_results), intent(inout) :: perm_info

    if (p < r) then
       q = partition(A,p,r, perm_info)
       q_input = q - 1
       call quicksort(A,p,q_input, perm_info)
       q_input = q + 1
       call quicksort(A,q_input, r, perm_info)
    end if
  end subroutine quicksort

  function partition(A, p, r, perm_info) result(res)
    integer, intent(inout) :: A(:)
    integer, intent(inout) :: p
    integer, intent(in) :: r
    integer :: x, i, j
    integer :: res
    type(sort_results), intent(inout) :: perm_info


    x = A(r)
    i = p - 1
    do j=p, r-1
       if (A(j) <= x) then
          i = i + 1
          if (i /= j) then
            call swap(A(i), A(j))
            perm_info%S = perm_info%S + 1
         end if
      end if
      perm_info%C = perm_info%C + 1
    end do
    if ((i+1) /= r ) then
       call swap (A(i+1), A(r))
       perm_info%S = perm_info%S + 1
    end if
    res = i + 1
   end function partition
end program
