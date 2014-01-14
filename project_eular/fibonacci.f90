! Project Eular
! Problem 2
!    Find the sum of even fibonacci numbers not exceeding four million
program fibonacci

  implicit none

  integer, parameter:: double = selected_real_kind(15,307)
  integer, parameter:: long = selected_int_kind(12)
  integer(double):: a0 = 1, a1 = 1, limit = 4000000, i, tmp
  real(double):: sum = 0, thresh = tiny(1.0_double)

  do while ( a1 <= limit )
     tmp = a1
     a1 = a0 + a1
     a0 = tmp
     if (mod(a1, 2) == 0) then
        sum = sum + a1
     end if     
  end do

  print *, 'sum = ', sum

end program fibonacci
