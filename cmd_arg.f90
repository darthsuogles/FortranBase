program cmd_arg_test

implicit none
integer:: count, i, j, k
character(len=255):: Cmd
character(len=25):: argnum

call get_command(cmd)
write (*,*) trim(cmd)
count = command_argument_count()
write (*,*) 'num args:', count

do i=1, count
   call get_command_argument(i, argnum)
   write(*, '(a,i0,a)') "argument #", i, "is: "//trim(argnum)
end do

end program cmd_arg_test


