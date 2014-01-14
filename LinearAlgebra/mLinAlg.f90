module mLinAlg

  use iso_c_binding
  use mConstants

  implicit none
  !private

  !integer, parameter, public:: kpr = selected_real_kind(15,307)

  !/// definition of the real matrix type
  type, public:: realMatrixType
     integer:: iRows, iCols
     real(kpr), dimension(:,:), allocatable:: data
  end type realMatrixType

  !/// definition of the complex matrix type
  type, public:: complexMatrixType
     integer:: iRows, iCols
     complex(kpr), dimension(:,:), allocatable:: data
  end type complexMatrixType

  !/// the matrix memory allocation interface
  interface AllocMatrix
     module procedure &
          AllocRealMatrix, &
          AllocComplexMatrix
  end interface

  !/// the matrix memory deallocation interface
  interface DestroyMatrix
     module procedure &
          DestroyRealMatrix, &
          DestroyComplexMatrix
  end interface

  !/// the matrix multiplication interface
  interface operator(*)
     module procedure &
          MultRealMatrix1L, &
          MultComplexMatrix1L, &
          MultRealMatrix1R, &
          MultComplexMatrix1R, &
          MultRealMatrix3, &
          MultComplexMatrix3
  end interface

  !/// the matrix addition interface
  interface operator(+)
     module procedure &
          AddRealMatrix, &
          AddComplexMatrix
  end interface

  public AllocRealMatrix, AllocComplexMatrix, &
       DestroyRealMatrix, DestroyComplexMatrix, &
       AddRealMatrix, AddComplexMatrix, &
       MultRealMatrix1L, MultComplexMatrix1L, &
       MultRealMatrix1R, MultComplexMatrix1R, &
       MultRealMatrix3, MultComplexMatrix3

contains

  !///////////////////////////////////////////////////
  !// Constructor 
  !///////////////////////////////////////////////////

  subroutine AllocRealMatrix(matrix, iRows, iCols, err_unit)
    type(realMatrixType), intent(inout):: matrix
    integer, intent(in):: iRows, iCols
    integer, intent(in), optional:: err_unit
    integer:: unit, info
    if (present(err_unit)) then
       unit = err_unit
    else
       unit = 0
    end if

#ifdef Memory
    if ( .not. allocated(matrix%data) ) then
#endif
       allocate(matrix%data(iRows, iCols), stat=info)
       matrix%iRows = iRows
       matrix%iCols = iCols
#ifdef Memory
    else
       write(unit, *) 'error: the matrix is already allocated'
       return
    end if
#endif  

    if ( info /= 0 ) then
       write(unit, *) 'error: the matrix is not allocated'
       return
    end if

  end subroutine AllocRealMatrix


  subroutine AllocComplexMatrix(matrix, iRows, iCols, err_unit)
    type(complexMatrixType), intent(inout):: matrix
    integer, intent(in):: iRows, iCols
    integer, intent(in), optional:: err_unit
    integer:: unit, info

    if ( present(err_unit) ) then
       unit = err_unit
    else
       unit = 0
    end if

#ifdef Memory
    if ( .not. allocated(matrix%data) ) then
#endif
       allocate(matrix%data(iRows, iCols), stat = info)
       matrix%iRows = iRows
       matrix%iCols = iCols
#ifdef Memory
    else
       write(unit, *) 'warning: the matrix is already allocated'
       return
    end if
#endif  

    if ( info /= 0 ) then
       write(unit, *) 'error: the matrix is not allocated'
       return
    end if

  end subroutine AllocComplexMatrix



  !///////////////////////////////////////////////////
  !// Destructor 
  !///////////////////////////////////////////////////

  subroutine DestroyRealMatrix(matrix, err_unit)
    type(realMatrixType), intent(inout):: matrix
    integer, intent(in), optional:: err_unit
    integer:: unit, info

    if (present(err_unit)) then
       unit = err_unit
    else
       unit = 0
    end if

#ifdef Memory
    if ( allocated(matrix%data) ) then
#endif
       deallocate(matrix%data, stat = info)
       matrix%iRows = 0
       matrix%iCols = 0
#ifdef Memory
    else
       write(unit, *) 'error: the matrix is not allocated'
       return
    end if
#endif

    if ( info /= 0 ) then
       write(unit, *) 'error: matrix deallocation failed'
    end if

  end subroutine DestroyRealMatrix

  !/// destructor of complex matrix
  subroutine DestroyComplexMatrix(matrix, err_unit)
    type(complexMatrixType), intent(inout):: matrix
    integer, intent(in), optional:: err_unit
    integer:: unit, info

    if ( present(err_unit) ) then
       unit = err_unit
    else
       unit = 0
    end if

#ifdef Memory
    if ( allocated(matrix%data) ) then
#endif
       deallocate(matrix%data, stat = info)
       matrix%iRows = 0
       matrix%iCols = 0
#ifdef Memory
    else
       write(unit, *) 'warning: the matrix is not allocated'
       return
    end if
#endif

    if ( info /= 0 ) then
       write(err_unit, *) 'error: matrix deallocation failed'
    end if

  end subroutine DestroyComplexMatrix




  !///////////////////////////////////////////////////
  !// Matrix Addition 
  !///////////////////////////////////////////////////

  !/// perform addition with two real matrices
  type(realMatrixType) function AddRealMatrix(matrix1, matrix2) result(res)
    type(realMatrixType), intent(in):: matrix1, matrix2
    

    if ( matrix1%iRows == matrix2%iRows .and. &
         matrix1%iCols == matrix2%iCols ) then
       call AllocMatrix(res, matrix1%iRows, matrix1%iCols)
       res%data = matrix1%data + matrix2%data
       res%iRows = matrix1%iRows
       res%iCols = matrix1%iCols
    end if

  end function AddRealMatrix

  !/// perform addiion with two complex matrices
  type(complexMatrixType) function AddComplexMatrix(matrix1, matrix2) result(res)
    type(complexMatrixType), intent(in):: matrix1, matrix2

    if ( matrix1%iRows == matrix2%iRows .and. &
         matrix1%iCols == matrix2%iCols ) then
       call AllocMatrix(res, matrix1%iRows, matrix1%iCols)
       res%data = matrix1%data + matrix2%data
       res%iRows = matrix1%iRows
       res%iCols = matrix1%iCols
    end if

  end function AddComplexMatrix



  !///////////////////////////////////////////////////
  !// Matrix Multiplication
  !///////////////////////////////////////////////////


  !/// perform matrix scalar multiplication in real
  type(realMatrixType) function MultRealMatrix1L(matrix, alpha) result(res)
    type(realMatrixType), intent(in):: matrix
    real(kpr), intent(in):: alpha
    integer:: i,j

    call AllocMatrix(res, matrix%iRows, matrix%iCols)
    do j=1,matrix%iRows
       do i=1, matrix%iCols
          res%data(i,j) = alpha * matrix%data(i,j)
       end do
    end do

  end function MultRealMatrix1L


  !/// perform matrix scalar multiplication in complex
  type(complexMatrixType) function MultComplexMatrix1L(matrix, alpha) result(res)
    type(complexMatrixType), intent(in):: matrix
    real(kpr), intent(in):: alpha
    integer:: i,j

    call AllocMatrix(res, matrix%iRows, matrix%iCols)
    do j=1,matrix%iRows
       do i=1, matrix%iCols
          res%data(i,j) = alpha * matrix%data(i,j)
       end do
    end do

  end function MultComplexMatrix1L

    !/// perform matrix scalar multiplication in real
  type(realMatrixType) function MultRealMatrix1R(alpha, matrix) result(res)
    type(realMatrixType), intent(in):: matrix
    real(kpr), intent(in):: alpha
    integer:: i,j

    call AllocMatrix(res, matrix%iRows, matrix%iCols)
    do j=1,matrix%iRows
       do i=1, matrix%iCols
          res%data(i,j) = alpha * matrix%data(i,j)
       end do
    end do

  end function MultRealMatrix1R


  !/// perform matrix scalar multiplication in complex
  type(complexMatrixType) function MultComplexMatrix1R(alpha, matrix) result(res)
    type(complexMatrixType), intent(in):: matrix
    real(kpr), intent(in):: alpha
    integer:: i,j

    call AllocMatrix(res, matrix%iRows, matrix%iCols)
    do j=1,matrix%iRows
       do i=1, matrix%iCols
          res%data(i,j) = alpha * matrix%data(i,j)
       end do
    end do

  end function MultComplexMatrix1R


  !/// perform multiplication of two real matrices
  type(realMatrixType) function MultRealMatrix3(matrix1, matrix2) result(res)
    type(realMatrixType), intent(in):: matrix1, matrix2

    if ( matrix1%iCols == matrix2%iRows ) then
       call AllocMatrix(res, matrix1%iRows, matrix2%iCols)
       res%data = matmul(matrix1%data, matrix2%data)
       res%iRows = matrix1%iRows
       res%iCols = matrix2%iCols
    else
       print *,'error: matrix shape mismatch'
    end if

  end function MultRealMatrix3


  !/// perform multiplication of two complex matrices
  type(complexMatrixType) function MultComplexMatrix3(matrix1, matrix2) result(res)
    type(complexMatrixType), intent(in):: matrix1, matrix2

    if ( matrix1%iCols == matrix2%iRows ) then
       call AllocMatrix(res, matrix1%iRows, matrix2%iCols)
       res%data = matmul(matrix1%data, matrix2%data)
       res%iRows = matrix1%iRows
       res%iCols = matrix1%iCols
    else
       print *,'error: matrix shape mismatch'
    end if

  end function MultComplexMatrix3



end module mLinAlg


