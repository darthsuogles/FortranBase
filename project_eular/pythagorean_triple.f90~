! Project Eular
! Problem 5
!    Find the smallest number divisible by 1...20
program divisibility

  implicit none

  integer, parameter:: double = selected_real_kind(15,307)
  integer, parameter:: long = selected_int_kind(12)
  integer:: primes(8), max_multiple(8), curr_multiple(8), i, j, tmp, n = 8
  real(double):: thresh = tiny(1.0_double)

  primes = (/ 2, 3, 5, 7, 11, 13, 17, 19 /)
  max_multiple = (/ 1, 1, 1, 1, 1, 1, 1, 1/)
  do i=1,20
     curr_multiple = (/ 0, 0, 0, 0, 0, 0, 0, 0 /)
     tmp = i
     do j=1,n
        do while ( mod(tmp, primes(j)) == 0 ) 
           tmp = tmp / primes(j)
           curr_multiple(j) = curr_multiple(j) + 1
        end do
     end do
     do j=1,n
        max_multiple(j) = max(max_multiple(j), curr_multiple(j))
     end do
  end do

  tmp = 1
  do j=1,n
     tmp = tmp * (primes(j) ** max_multiple(j))
  end do

  print *, 'smallest number divisible by 1 ... 20 = ', tmp

end program divisibility
