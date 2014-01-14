program io_test

implicit none
integer:: i=98, a, b
character:: str(43) = " 10 100 "

write(*,'(a10, i0)') "i0", i
write(*,'(a10, i5)') "i5", i
write(*,'(a10, i5.3)' ) "i5.3", i
write(*,'(a10, i1)') "i1", i

!read(str, *) a, b

end program io_test
