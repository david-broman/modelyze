
DIRS = src,ext/ucamlib/src,ext/extlib

# ,ext/sundials

.PHONY: all clean

# Init submodules if needed and make native version. 
# The resulting executable can be found under /bin and /library (symlinks)
all:    ext/ucamlib/Makefile native


# Compile native version
native: comp_c_files
	ocamlbuild -Is $(DIRS) moz.native 
	@mv moz.native bin/moz
	@(cd library; rm -f moz; ln -s ../_build/src/moz.native moz)

# Compile byte code version
byte: 	comp_c_files
	ocamlbuild -Is $(DIRS) moz.byte	
	@mv moz.byte bin/moz
	@(cd library; rm -f moz; ln -s ../_build/src/moz.byte moz)

comp_c_files:
#	ocamlbuild ext/sundials/ida_stubs.o


# If ucamlib content does not exist, init and update submodules
ext/ucamlib/Makefile:
	git submodule init
	git submodule update
	cd ext/ucamlib; git checkout master

# Update git sub modules
update:
	cd ext/ucamlib; git checkout master; git pull


# Clean all submodules and the main Modelyze source
clean:
	@ocamlbuild -clean

