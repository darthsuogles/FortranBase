module vector_type

implicit none
private

integer, parameter, public:: kpr = selected_real_kind(15,307)
type, public:: vector_t
   integer:: n=100
   logical:: inited = .false.
   real(kpr), dimension(:), allocatable:: a
end type vector_t

! subroutine declarations
public normalize, abs, print_vec,&
     vec_alloc, vec_dealloc,&
     vec_init_file, vec_out_file

contains
  ! normalize the vector
  subroutine normalize(vec)
    type(vector_t), intent(inout):: vec
    integer:: i
    real(kpr):: res = 0.0_kpr
    do i=1, vec%n
       res = res + vec%a(i)**2
    end do
    res = sqrt(res)
    do i=1, vec%n
       vec%a(i) = vec%a(i)/res
    end do
  end subroutine normalize

  ! each element get its absolute value
  subroutine abs(vec)
    integer:: i
    type(vector_t), intent(inout):: vec
    do i=1, vec%n
       if ( vec%a(i) < 0.0_kpr ) then
          vec%a(i) = -vec%a(i)
       end if
    end do
  end subroutine abs

  ! print the vector with format
  subroutine print_vec(vec)
    integer:: i
    type(vector_t), intent(in):: vec
    do i=1,vec%n
       write(*,*) vec%a(i)
    end do
  end subroutine print_vec
  
  ! allocate the vector
  subroutine vec_alloc(vec, n)
    integer, intent(in):: n
    type(vector_t), intent(inout):: vec
    if ( .not. allocated(vec%a) ) then
       allocate(vec%a(n))
       vec%n = n
       vec%inited = .true.
    end if
  end subroutine vec_alloc

  ! deallocate the vector
  subroutine vec_dealloc(vec)
    type(vector_t), intent(inout):: vec
    if ( allocated(vec%a) ) then
       deallocate(vec%a)
       vec%n = 0
       vec%inited = .false.
    end if
  end subroutine vec_dealloc

  ! initialize the vector from a file
  subroutine vec_init_file(vec, file_name)
    type(vector_t), intent(inout):: vec
    character(len=255), intent(in):: file_name
    logical:: is_open, is_exist
    integer:: unit,i

    if ( .not. vec%inited ) then
       write(0, *) 'vector must be initialized first'
       return
    end if

    inquire(file=file_name, exist = is_exist, opened=is_open, number=unit)
    print *,'file exist?:', is_exist, 'file opened?:', is_open, unit
    if ( .not. is_exist ) then
       write(0, *) 'the file:', file_name, 'does not exist'
    end if
    if ( .not. is_open ) then
       open(101, file=file_name, status="unknown", action="read")
       unit = 101
    end if
    !do i=1,vec%n
       read(unit,*) vec%a
    !end do
       close(unit)
  end subroutine vec_init_file

  ! output the content of the vector to a file
  subroutine vec_out_file(vec, file_name)
    type(vector_t), intent(in):: vec
    character(len=255), intent(in):: file_name
    integer:: i, unit
    logical:: is_open, is_exist

    if ( .not. vec%inited ) then
       write(0, *) 'vector must be initialized first'
       return
    end if

    inquire(file=file_name, exist=is_exist, opened=is_open, number=unit)
    print *,'file exist?:', is_exist, 'file opened?:', is_open, unit
    if ( .not. is_exist ) then
       write(0,*) 'the file:', file_name, 'does not exist'
    end if
    if ( .not. is_open ) then
       open(101, file=file_name, status="unknown", action="write")
       unit = 101
    end if
    
!    do i=1, vec%n
       write(unit, '(es16.8 e3)') vec%a
 !   end do
    close(unit)

  end subroutine vec_out_file

 end module vector_type

program vector_test

use vector_type
implicit none

integer:: i, j, k, count
character(len=255):: vec_input, vec_output, cmd
character(len=25):: argv
type(vector_t):: vec

call get_command(cmd)
write (*, *) trim(cmd)
count = command_argument_count()
if ( count /= 2 ) then
   print *,'usage: a.out [vec_input file] [vec_output file]'
end if


do i=1, count
   call get_command_argument(i, argv)
   select case(i)
      case(1)
         vec_input = trim(argv)
      case(2)
         vec_output = trim(argv)
   end select
end do



call vec_alloc(vec, 4)
print *, vec%inited
call vec_init_file(vec, vec_input)
call print_vec(vec)

print *,'normalized'
call normalize(vec)
call print_vec(vec)

print *,'absolute'
call abs(vec)
call print_vec(vec)

call vec_out_file(vec, vec_output)
call vec_dealloc(vec)

end program vector_test
