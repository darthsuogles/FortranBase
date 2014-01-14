! the main program of the genetic algorithm
program darwin
  use numz
  implicit none

  write(*,"(a, 1x, i0, f16.8)") "pi has precision", precision(pi), pi
  call set_size()
  write(*,"(a, 1x, i0)") "gene_size=", gene_size

contains

subroutine set_size()
  gene_size = 10
end subroutine

end program darwin
