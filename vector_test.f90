program vector_test

use vector
implicit none

integer:: i, j, k
type(vector_t):: vec

vec%a = cos(1.23_kpr)
call normalize(vec)
print *,vec%a

call abs(vec)
print *,vec%a

end program vector_test
