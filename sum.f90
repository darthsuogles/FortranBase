! compute the summation of 

! it contains functions to be called later
module algebra

implicit none
private

type, public:: Vector
   character(len=100)::data
end type Vector
integer, public, save:: state
public:: summation, summation2, pp

contains
  function summation2(N)
    integer:: summation2
    integer, intent(in):: N
    integer::i

    summation2 = 0
    do i=0,N
       summation2 = summation2 + i**2
    end do
  end function summation2

  function summation(N)
    integer:: summation
    integer, intent(in):: N
    integer::i

    summation = 0
    do i=1,N
       summation = summation + i
    end do
  end function summation
  
  subroutine pp(a, b, c)
    integer, intent(in):: a, b
    integer, intent(out):: c

    c = a + b
    print *,a, b, c
    print *,state
  end subroutine pp

end module algebra

! the test program
program sum

use algebra

implicit none
integer, parameter:: double = selected_real_kind(15,307)
integer, parameter:: double_complex = cmplx(double, double)
integer:: N=100, i, j, k, res
integer, dimension(100):: arr 
type(Vector):: vec
integer:: A(4,4) = -1

print *,maxval(abs(A))
print *,any(A<10), all(A<0), size(A(:,1)), shape(A)

i = 10
j = 9
k = 8
!state = 10
call pp(i, j, j)

vec%data = 'dsadsa'
print *,vec%data

! array initialization
do i=1, N
   arr(i) = i
end do

! compute the summation of square
do i=1, N
   res = res + arr(i)**2
end do
print *, i, res
res = 0

i = 1
do while (i < N+1) 
   res = res + arr(i)**2
   i = i + 1
end do
print *, i, res
res = 0

i = 1
do 
   res = res + arr(i)**2
   i = i + 1
   if ( i == N+1 ) exit
end do
print *, i, res
res = 0

print *,'compute the even terms'
i = 1
do i=2,N,2
   res = res + i**2
end do
print *, i, res
res = 0

i = 1
do i=1,N
   if ( mod(i,2) == 1 ) cycle
   res = res + i**2
end do
print *, i, res
res = 0

i = 0
do while (i < N)
   i = i + 1
   if ( mod(i,2) == 1 ) cycle
   res = res + i**2
end do
print *,i,res
res = 0

i = 1
do 
   if ( mod(i,2)==0 ) res = res + i**2
   i = i + 1
   if ( i > N ) exit
end do
print *,i,res
res = 0

print *, summation2(N), summation(N)

! contains
!   function summation(N)
!     integer:: summation
!     integer, intent(in):: N
!     integer::i

!     summation = 0
!     do i=0,N,2
!        summation = summation + i**2
!     end do
!   end function summation

end program sum
