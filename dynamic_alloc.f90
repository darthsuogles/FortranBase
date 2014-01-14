program dynamic_alloc

implicit none

integer, allocatable, dimension(:,:):: A
integer:: info, n = 3, m = 4

if ( .not. allocated(A) ) then
   allocate( A(m,n), stat=info )
   print *,info
end if
   deallocate( A, stat=info )
print *,info

end program dynamic_alloc
