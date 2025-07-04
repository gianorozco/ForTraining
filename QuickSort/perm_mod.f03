module perm_mod
  use, intrinsic :: iso_fortran_env, only: real64
contains
   subroutine swap(a, b)
    implicit none
    integer, intent(inout) :: a, b
    integer :: temp

    temp = a
    a = b
    b = temp
  end subroutine swap

  recursive subroutine permute(A, idx, size, matrix, res_size)
    implicit none
    integer, intent(inout) :: A(:)
    integer, intent(in) :: idx, size
    integer, intent(inout) :: matrix(:,:)
    integer, intent(inout) :: res_size
    integer :: i

    if (idx == size) then
       res_size = res_size + 1
       matrix(res_size, :) = A
    else
       do i = idx, size
          !swap(&arr[idx], &arr[i]);
          call swap(A(idx), A(i))
          call permute(A, idx+1, size, matrix, res_size)
          !swap(&arr[idx], &arr[i]);
          call swap(A(idx), A(i))
       end do
    end if
  end subroutine permute

  subroutine gen_permutations(matrix, A)
    implicit none
    integer, intent(out) :: matrix(:,:)
    integer, intent(inout) :: A(:)
    integer :: res_size

    res_size = 0
    call permute(A, 1, size(A), matrix, res_size)
  end subroutine gen_permutations

  function random_permutation(size) result(A)
    implicit none
    integer, intent(in) :: size
    integer, dimension(size) :: A
    integer :: i, j, flag, rng_int
    real(real64) :: rng

    A(:) = 0
    i = 1
    do while (i <= size)
      flag = 0
      call random_number(rng)
      rng_int = int((rng* size) + 1)
      !print *, rng_int
      do j =1, (size + 1)
        if (A(j) == rng_int) then
          flag = 1
          exit
        end if
      end do
      if (flag == 0) then
        A(i) = rng_int
        !print *, A(i)
        i = i + 1
      end if

    end do
  end function random_permutation
end module perm_mod










