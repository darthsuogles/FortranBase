! Project Eular
! Problem 4
!    Find the largest palindrome for the product of two 3-digit number
program palindrome

  implicit none

  integer, parameter:: double = selected_real_kind(15,307)
  integer, parameter:: long = selected_int_kind(12)
  integer(double):: p0 = 1, p1 = 1, limit = 4000000, tmp, prod, iprod = 0
  real(double):: curr_max_palindrome = 0, thresh = tiny(1.0_double)

  do p0 = 1,999
     do p1 = 1,999
        tmp = p0 * p1
        prod = tmp
        iprod = 0
        do while (tmp > 0)
           iprod = iprod * 10 + mod(tmp, 10)
           tmp = tmp / 10
        end do
        if ( prod == iprod ) then
           if ( prod > curr_max_palindrome ) then
              print *, 'palindrome: ', prod
              curr_max_palindrome = prod
           end if
        end if
     end do
  end do

  print *, 'max palindrome = ', curr_max_palindrome

end program palindrome
