all: linAlg_test

FC = gfortran
FPP = -cpp

linAlg_test: linAlg_test.f90 mLinAlg.o mGutenberg.o mConstants.o
	$(FC) linAlg_test.f90 -o linAlg_test mLinAlg.o mGutenberg.o 


mLinAlg.o: mLinAlg.f90 mConstants.f90
	$(FC) -c mConstants.f90
	$(FC) $(FPP) -c -DMemory mLinAlg.f90


mGutenberg.o: mGutenberg.f90 mLinAlg.o
	$(FC) -c mGutenberg.f90

clean:
	rm linAlg_test
	rm -f *.o *.mod *.s
