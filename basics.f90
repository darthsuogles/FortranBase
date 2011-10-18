
integer function prod(a, b)
  prod = a*b
end function prod

subroutine getprod(res, a, b)
  res = a*b
end subroutine getprod

program basics

  !  use person_type_module
  implicit none

  ! declarations
  ! 15 digits precision, exponent in +/- 307
  integer, parameter:: DP = selected_real_kind(15,307)
  ! in the range (e-9, e9)
  integer, parameter:: long = selected_int_kind(9)

  integer:: sel = 15, idx, i, j, k
  real(kind=DP):: lo = 90, hi = 112
  complex(kind=DP):: z = (1.0, 0.0)
  logical:: flag = .true.
  ! type(perons_type):: person

  !person%name = "James Bond"
  !person%isMarried = .false.

  ! executale statements
  if (lo > hi) then
     print *, 'lo is greater than hi'
  else
     print *, 'hi is greater than lo'
  end if

  print *, 'complex', z
  print *, 'logical', flag


  select case (sel)
  case(:12)
     write(*,*) 'no entrance, sorry'
  case(13:17)
     write(*,*) 'okay, com''on in boy'
  case(18:)
     write(*,*) 'gee, you are too old'
  case default
     write(*,*) 'this is weired'
  end select

  do while (lo < hi)
     print *,'ge'
     hi = hi - lo
  end do

  print *, lo+hi

  !lo = prod(hi, lo)
  print *, hi
  call getprod(hi, hi, lo)
  print *, hi; print *, lo

  if ( flag .eqv. .true. ) then
     print *,':)'
  end if
  if ( flag .eqv. .true. ) then
     print *, ':(' 
  end if

  print *, 'print a very small number', tiny(1.0_DP)
  print *, (idx**2, idx=1, 10, 2)

  do idx=1,0
     print *,'error'
  end do

  main:do i=1,10
     inner:do j=1,10
        if (j>7) then
           cycle
        else if (j>9) then
           exit main
        end if
     end do inner
  end do main

end program basics
