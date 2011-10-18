program mc_pi

integer, parameter::double = selected_real_kind(15,307)
integer:: dim = 2
real(kind=double):: radius, rand_p(2), inside_count=0, outside_out=0, pi_ratio

do i=1,100000
   do k = 1,dim
      call random_number(rand_p(k))
   end do
   if (rand_p(1)*rand_p(1) + rand_p(2)*rand_p(2) < 1) then
      inside_count = inside_count + 1
   else
      outside_count = outside_count + 1
   end if
end do

pi_ratio = inside_count / outside_count

print *,pi_ratio

end program mc_pi
