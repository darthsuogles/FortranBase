! user defined type
module person_type_module

implicit none
private 
integer, parameter, public:: kpr=selected_real_type(15,307)

type, public:: person_type
   character(len=100):: name
   logical:: isMarried = .false.
   integer:: age
   real(kind=kpr):: weight
end type person_type   

end module person_type_module
