

.PHONY: all clean

# Init submodules if needed and make all subdirectories.
all:    ucaml/Makefile
	cd ext/extlib; make
	cd ucaml; make
	cd src; make

# If ucaml content does not exist, init and update submodules
ucaml/Makefile:
	git submodule init
	git submodule update

# Clean all submodules and the main Modelyze source
clean:
	cd ext/extlib; make
	cd ucaml; make clean
	cd src; make clean
