! Eular Sieve 
! 
! An improved version of the Sieve of Eratothenes
! The algorithm finds prime numbers up to a certain integer

program sieve_eular

#define N 100

implicit none
logical:: checker(N)
integer:: n = N, k, i, k2

checker = .true.
do k=2,n      

   if ( .not. checker(k) ) cycle   

   k2 = k*k
   if (k2 > n) then
      exit
   end if

   do i=k2, n*k, k2
      if ( i<n ) checker(i) = .false.      
      checker(i/k) = .false.
   end do

end do

do k=2,n
   if (checker(k)) then
      write(*,'(i0, 1x)', advance='no') k
   end if
end do
write(*, '(1x)')

end program sieve_eular
