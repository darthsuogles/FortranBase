program LinAlg_test

use mLinAlg ! contains linear algebra routines
use mGutenberg ! contains printing methods

implicit none

type(realMatrixType)::  A, B
type(complexMatrixType):: Z
integer:: m = 4, n = 3
character(len=255):: label1 = "real matrix", label2 = "complex matrix", real_file = "real.mat"

!call AllocMatrix(rmat, m, n, 0)
call ReadMatrix(A, real_file)
!call ReadMatrix(cmat, real_file)
call WriteMatrix(A, label="original matrix")

A = A + A
call WriteMatrix(A, label="addition")

B =  1.23_kpr * A * 3.12_kpr * A + 2.0_kpr * A
call WriteMatrix(B, label="multiplication")

call DestroyMatrix(A)
call DestroyMatrix(B)

end program LinAlg_test
