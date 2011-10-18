! Some experiment with a power series
program power_series

implicit none

integer, parameter:: double = selected_real_kind(15,307)
integer:: a = 2, count=0, n = 110
real(double):: thresh = tiny(1.0_double)

!do n = a, 2

   do while (count < 10)
      a = fraction(scale(2.0_double, a)/n)*n
      count = count + 1
      print *, a
   end do

   ! print *, n, 'factorized as:', &
   !      a-int(b), a+int(b), (a-int(b))*(a+int(b))

!end do

end program power_series
