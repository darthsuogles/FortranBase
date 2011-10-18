! Project Eular
! Problem 9
!    Find the only Pythagorean triple whose sum is 1000
program pythagorean_triple

  implicit none

  integer, parameter:: double = selected_real_kind(15,307)
  integer, parameter:: long = selected_int_kind(12)
  integer:: a, b, c, n = 1000
  real(double):: thresh = tiny(1.0_double)

  do a=1,n-2
     do b=a+1,n-1
        c = n - a - b
        if ( c**2 == a**2 + b**2 ) then
           print *, a, b, c
           return
        end if
     end do
  end do

end program pythagorean_triple
