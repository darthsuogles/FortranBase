program complex_test

implicit none

integer, parameter:: double = selected_real_kind(15,307)
real(double):: rand_x, rand_y
complex(double):: z

call random_number(rand_x)
call random_number(rand_y)
z = cmplx(rand_x, rand_y)
print *,z

end program complex_test

