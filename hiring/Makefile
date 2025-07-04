FC = gfortran

all: hires

hires: perm_mod.f03 hires.f03
	$(FC) -o hires perm_mod.f03 hires.f03

clean:
	rm -f hires
	rm -f perm_mod.mod
