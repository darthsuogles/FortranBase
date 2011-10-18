program matrix_basics

integer, parameter::DP=selected_real_kind(14)
real(DP), dimension(100,100):: A, B, C

A = 1
B = 2

!C = matmul(A, B)
C = 3*A

A = A-B
A = abs(A)
print *, dot_product(maxval(A,2),maxval(A,2))
print *, sum(A)



end program matrix_basics
