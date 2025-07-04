module BucketSort_Mod
  use QuickSort_Mod
  use listmod
  implicit none
contains
  subroutine bucketsort(A, buckets, comp_count)
    real, intent(inout) :: A(:)
    integer, intent(inout) :: buckets
    integer, intent(inout) :: comp_count
    type(list), allocatable :: B(:)
    real, allocatable :: temp(:)
    real, dimension(buckets) :: avg
    integer :: i, bi, size, start

    allocate(B(buckets))
    do i =1, buckets
       nullify(B(i)%head)
    end do

    do i=1, buckets
       bi = buckets * A(i)
       call attach(B(bi+1), A(i))
    end do

    do i=1, buckets
       size = list_size(B(i))
       print *, "Bucket ", i, " : size is ", size
       allocate(temp(size))
       avg(i) = real(size)/buckets
       temp = create_array(B(i), size)
       print *, "Before sorting"
       print *, temp
       start = 1
       call quicksort(temp, start, size, comp_count)
       print *, "After sorting"
       print *, temp
       deallocate(temp)
    end do
    print *, "Comparison Count"
    print *, comp_count
  end subroutine bucketsort
end module BucketSort_Mod
