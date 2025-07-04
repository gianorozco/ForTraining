program main
  use, intrinsic :: iso_fortran_env, only: real64
  use BucketSort_Mod
  implicit none
  real(real64) :: rng
  real, dimension(100) :: numbers
  integer :: i, buckets, comp_count

  comp_count = 0
  buckets = 100
  numbers(:) = 0
  do i = 1, 100
     numbers(i) = rng
     call random_number(rng)
  end do
  call bucketsort(numbers, buckets, comp_count)
end program main
