! solve the equation 
! a x^2 + b x + c = 0
program solve_equation

implicit none

integer, parameter:: double=selected_real_kind(15,307)
real(double):: a, b, c, res2
real(double):: thresh = epsilon(1.0_double), zero=tiny(1.0_double)

print *, 'input a, b, c'
read(*,*) a, b, c


if ( abs(a) < zero ) then

   if ( abs(b) < zero ) then
      print *, 'invalid'
   else
      print *, 'x=', -c/b
   end if

else

   res2 = b**2 - 4*a*c      
   if ( res2 < 0.0_double ) then
      print *, 'x1=',  (-cmplx(b,zero) + cmplx(0.0_double, sqrt(-res2)))/(2*cmplx(a,zero))
      print *, 'x2=',  (-cmplx(b,zero) - cmplx(0.0_double, sqrt(-res2)))/(2*cmplx(a,zero))

   else
      print *, 'x1=', (-b + sqrt(res2))/(2*a)
      print *, 'x2=', (-b - sqrt(res2))/(2*a)
   end if

end if

end program solve_equation
