program hires
  use perm_mod
  implicit none
  type :: t_pair
     integer :: best
     integer :: updates
  end type t_pair
  integer, dimension (8) :: A
  integer, dimension(20) :: rand_perm
  integer, allocatable :: matrix(:, :)
  integer :: size, perm_count
  integer :: i, j, n
  type(t_pair) :: hiring_result
  integer :: best, updates_sum
  real :: expectation
  integer :: print_flag

  A = [1,2,3,4,5,6,7,8]
  size = 8
  perm_count = size
  n = 8
  do while (n > 1)
     perm_count = perm_count * (n-1)
     n = n - 1
  end do

  print *, perm_count
  allocate(matrix(perm_count,size))
  do i = 1, perm_count
     do j = 1, size
        matrix(i, j) = j
     end do
  end do

  call gen_permutations(matrix, A)

  updates_sum = 0
  print_flag = 0
  do i = 1, perm_count
     hiring_result = hiring(matrix(i,:), size, print_flag)
     updates_sum = updates_sum + hiring_result%updates
  end do

  expectation = real(updates_sum) / real(perm_count)
  print *, expectation

  print *, "Random portion of Program"
  size = 20
  updates_sum = 0
  print_flag = 1
  do i = 1, 500
    rand_perm = random_permutation(size)
    hiring_result = hiring(rand_perm, size, print_flag)
    updates_sum = updates_sum + hiring_result%updates
  end do

  expectation = real(updates_sum) / real(500)
  print *, expectation
contains
  function hiring(a, size, print_flag) result(output)
    implicit none
    integer, intent(in) :: a(:)
    integer, intent(in) :: size
    integer, intent(in) :: print_flag
    type(t_pair) :: output
    integer :: i

    output%best =0
    output%updates =0
    do i=1, size
       if (a(i) > output%best) then
          output%best = a(i)
          output%updates = output%updates + 1
       end if
    end do
  end function hiring

end program



