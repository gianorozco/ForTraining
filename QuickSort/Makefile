FC = gfortran

all: qs

qs: perm_mod.f03 main.f03
	$(FC) -o qs perm_mod.f03 main.f03

clean:
	rm -f qs
	rm -f perm_mod.mod
