! The Fermat Factorization Method
program fermat_factorization

  implicit none

  integer, parameter:: double = selected_real_kind(15,307)
  integer, parameter:: long = selected_int_kind(12)
  integer(double):: a, b2
  real(double):: b, thresh = tiny(1.0_double), n = 600851475143.0_double

  print *, 'Input number: '
  read(*,*)n
  
  a = ceiling(sqrt(n))
  b2 = a*a - n
  b = sqrt(real(b2, double))

  do while (abs(b-int(b)) > thresh .or. (a-int(b))*(a+int(b)) /= n)
     a = a + 1
     b2 = a*a - n
     b = sqrt(real(b2, double))
  end do

  print *, n, 'factorized as:', &
       a-int(b), ' * ', a+int(b), '=', (a-int(b))*(a+int(b))


end program fermat_factorization
