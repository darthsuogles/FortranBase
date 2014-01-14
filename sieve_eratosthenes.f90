! Sieve of Eratosthenes 
! 
! The algorithm finds prime numbers up to a certain integer

program sieve_erato

#define N 1000000

implicit none
logical:: checker(N)
integer:: n = N, k, i, k2

checker = .true.
do k=2,n    
   k2 = k*k
   if (k2 > n) then
      exit
   end if

   if (checker(k)) then
      do i=k2, n, k
         checker(i) = .false.      
      end do
   end if
end do

! do k=2,n
!    if (checker(k)) then
!       write(*,'(i0, 1x)', advance='no') k
!    end if
! end do
! write(*, '(i0, 1x)')

end program sieve_erato
