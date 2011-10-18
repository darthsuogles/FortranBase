program random

  implicit none

  integer, dimension(1):: old, seed
  integer :: i, k=1
  real, dimension(3:3) :: harvest
  seed(1) = 12345

  call random_seed
  call random_seed(size=k)
  write(*,*) ' Number of integers for starting valiue = ', k

  call random_seed(get=old(1:k))
  WRITE(*,*) ' Old starting value = ', OLD

  CALL RANDOM_NUMBER(HARVEST)
  WRITE(*,*) ' Random numbers : ', HARVEST

  CALL RANDOM_SEED(GET=OLD(1:K))
  WRITE(*,*) ' Present starting value = ', OLD

  CALL RANDOM_SEED(PUT=SEED(1:K))
  CALL RANDOM_SEED(GET=OLD(1:K))
  WRITE(*,*) ' New starting value = ', OLD

  CALL RANDOM_NUMBER(HARVEST)
  WRITE(*,*) ' Random numbers : ', HARVEST

  DO I = 1, 3
     CALL RANDOM_SEED(GET=OLD(1:K))
     WRITE(*,*) ' Present starting value = ', OLD
     CALL RANDOM_NUMBER(HARVEST)
     WRITE(*,*) ' Random numbers : ', HARVEST
     CALL RANDOM_NUMBER(HARVEST)
     WRITE(*,*) ' Random numbers : ', HARVEST
  END DO


end program random
