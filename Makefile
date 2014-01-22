


all:	ucaml/Makefile
	cd ucaml; make

# If ucaml content does not exist, init and update submodules
ucaml/Makefile:
	git submodule init
	git submodule update

clean:
	cd ucaml; make clean
