module rand_mod
  ! module contains three funcitons
  ! ran1 returns a uniform random number between 0-1
  ! spread returns random number between min - max
  ! normal returns a normal distribution

  use numz
  implicit none

contains

  !/// return a pseudo random number
  real(b8) function rand1()
    !   real(b8), intent(out):: rand1
    real(b8):: harvest

    call random_number(harvest) ! call the intrinsic random number routine
    rand1 = harvest
  end function rand1


  !/// return a random number in the range
  real(b8) function spread(min, max) ! return random number between min - max
    !    real(b8), intent(out):: spread
    real(b8), intent(in):: min, max

    spread = (max-min)*rand1() + min
  end function spread

  !/// return a normal distribution
  real(b8) function normal(mean, sigma)
    real(b8), intent(in):: mean, sigma
    integer:: flag
    real(b8):: tmp, fac, gsave, rsq, r1, r2
    save flag, gsave

    data flag /0/
    if ( flag == 0 ) then
       rsq = 2.0_b8
       do while ( rsq > 1.0_b8 .or. rsq == 0.0_b8 )
          r1 = 2.0_b8 * rand1() - 1.0_b8
          r2 = 2.0_b8 * rand1() - 1.0_b8
          rsq = r1*r1 + r2*r2
       end do
       fac = sqrt(-2.0_b8 * log(rsq)/rsq)
       gsave = r1*fac
       tmp = r2*fac
       flag = 1
    else
       tmp = gsave
       flag = 0
    end if
    normal = tmp*sigma + mean
  end function normal

end module rand_mod
