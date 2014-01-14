all: darwin

FC = gfortran

darwin: darwin.f90 numz.o rand.o
	$(FC) -o darwin darwin.f90 numz.o rand.o

rand.o: rand_mod.f90
	$(FC) -c -o rand.o rand_mod.f90

numz.o: numz.f90
	$(FC) -c numz.f90

clean:
	rm darwin
	rm *.o *.mod
