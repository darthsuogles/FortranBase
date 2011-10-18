all: darwin

darwin: darwin.f90 numz.o rand.o
	ifort -o darwin darwin.f90 numz.o rand.o

rand.o: rand_mod.f90
	ifort -c -o rand.o rand_mod.f90

numz.o: numz.f90
	ifort -c numz.f90

clean:
	rm darwin
	rm *.o *.mod