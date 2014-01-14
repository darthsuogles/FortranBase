!//////////////////////////////////////////
!/// this file provides an interactive 
!/// interface to call the blas functions
!//////////////////////////////////////////

program philinab

use mLinAlg ! the linear algebra wrapper
use mGutenberg ! printing methods
implicit none

logical:: flag = .true.
character(len=255):: cmd

do while (flag)

   read(*,*) cmd
   write(*,*) cmd
   if ( trim(cmd) == 'exit' ) then
      flag = .false.
   end if

end do


end program philinab
