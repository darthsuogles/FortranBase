program intrinsic_test

implicit none

integer, parameter:: double = selected_real_kind(14,307)
integer:: a=1129, b=23, i, j, k
real(kind=double):: x=213.2_double, y=212.121_double
integer, dimension(100):: vec
character(len=100):: str='   dsaduiwuald   '

! remove trailing blanks of a string
print *,trim(str)
print *,trim('dsa dsa da  dsa        ')//trim(str)
print *,'length of the string:', len(str)
print *,'length of the trimmed string:', len(trim(str))
! adjust the blanks
print *, 'length of the modified trimmed string', len(trim(adjustl(str)))

do i=1,100
   call random_number(y)
   vec(i) = y
   print *,y
end do
print *, maxval(vec)


! special numbers
print *, 'huge:', huge(1.0_double)
print *, 'tiny:', tiny(1.0_double)
print *, 'epsilon:', epsilon(1.0_double)

! module
print *, 'mod:', mod(a, b)

! trignometrical functions
print *, 'sin:', sin(x), 'cos:', cos(x)

! type conversion
x = real(2133213)
print *, x



end program intrinsic_test




