module mGutenberg

  use mLinAlg
  implicit none
  !private

  interface ReadMatrix
     module procedure ReadRealMatrix, ReadComplexMatrix
  end interface

  interface WriteMatrix
     module procedure WriteRealMatrix, WriteComplexMatrix
  end interface

  public ReadRealMatrix, ReadComplexMatrix, &
       WriteRealMatrix, WriteComplexMatrix

contains
  !///////////////////////////////////////////////////
  !// helper functions
  !///////////////////////////////////////////////////

  subroutine ReadRealMatrix(matrix, file_name)
    type(realMatrixType), intent(out):: matrix
    character(len=*), intent(in):: file_name
    integer:: line_idx, unit, m, n, info
    logical:: is_open, is_exist

    inquire(file=file_name, exist=is_exist, opened=is_open, number=unit)
    if ( .not. is_exist ) then
       print *,'error: the file', file_name, 'does not exist'
       return
    end if
    if ( .not. is_open ) then
       open(101, file=file_name, status="unknown", action="read")
       unit = 101
    end if

    ! get the size of the matrix
    read(unit, *, iostat=info) m, n
    if ( info /= 0 ) then
       print *, 'error: read failed'
       return
    end if
    call AllocMatrix(matrix, m, n)

    ! read row by row 
    do line_idx=1, m
       read(unit, *, iostat=info) matrix%data(line_idx,:)
       if ( info /= 0 ) then
          print *, 'error: corrupted reading, matrix initialization failed'
          return
       end if
    end do
    close(unit)

  end subroutine ReadRealMatrix

  !/// initialize a matrix from a file
  subroutine ReadComplexMatrix(matrix, file_name)
    type(complexMatrixType), intent(out):: matrix
    character(len=*), intent(in):: file_name
    integer:: line_idx, unit, m, n, info
    logical:: is_open, is_exist

    inquire(file=file_name, exist=is_exist, opened=is_open, number=unit)
    if ( .not. is_exist ) then
       print *,'error: the file', file_name, 'does not exist'
       return
    end if
    if ( .not. is_open ) then
       open(101, file=file_name, status="unknown", action="read")
       unit = 101
    end if

    ! get the size of the 
    read(unit, *, iostat=info) m, n
    if ( info /= 0 ) then
       print *, 'error: read failed'
       return
    end if
    call AllocMatrix(matrix, m, n)

    do line_idx=1, m
       read(unit, *, iostat=info) matrix%data(line_idx,:)
       if ( info /= 0 ) then
          print *, 'error: corrupted reading, matrix initialization failed'
          !DestroyMatrix(matrix)
          return
       end if
    end do
    close(unit)

  end subroutine ReadComplexMatrix


  !/// output the matrix to a file
  subroutine WriteRealMatrix(matrix, unit, label)
    type(realMatrixType), intent(in):: matrix
    character(len=*):: label
    integer, optional:: unit
    integer:: i, j, out_unit, err_unit=0, is_exist, is_open, info

    if ( present(unit) ) then
       out_unit = unit
    else
       out_unit = 6
    end if

    write(out_unit, '(a, i0, 1x, i0)') label//":", matrix%iRows, matrix%iCols
    do i=1, matrix%iRows
       do j=1, matrix%iCols
          write(out_unit, '(f16.8, 1x)', advance="no", iostat=info) matrix%data(i,j)
          if ( info /= 0 ) then
             write(err_unit, *) 'error: matrix output error'
          end if
       end do
       write(out_unit, *) ''
    end do

  end subroutine WriteRealMatrix


  !/// output the matrix to a file
  subroutine WriteComplexMatrix(matrix, unit, label)
    type(complexMatrixType), intent(in):: matrix
    character(len=*):: label
    integer, optional:: unit
    integer:: i, j, out_unit, err_unit=0, is_exist, is_open, info

    if ( present(unit) ) then
       out_unit = unit
    else
       out_unit = 6
    end if

    write(out_unit, '(a, i0, 1x, i0)') label//":", matrix%iRows, matrix%iCols
    do i=1, matrix%iRows
       do j=1, matrix%iCols
          write(out_unit, '(2(f16.8, 1x), 1x)', advance="no", iostat=info) matrix%data(i,j)
          if ( info /= 0 ) then
             write(err_unit, *) 'error: matrix output error'
          end if
       end do
       write(out_unit, *) ''
    end do

  end subroutine WriteComplexMatrix

end module mGutenberg
