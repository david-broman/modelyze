

.PHONY: all clean

# Init submodules if needed and make all subdirectories.
all:    ext/ucaml/Makefile native


native:
	ocamlbuild -Is src,ext/ucaml,ext/extlib moz.native	

byte:
	ocamlbuild -Is src,ext/ucaml,ext/extlib moz.byte	

# If ucaml content does not exist, init and update submodules
ucaml/Makefile:
	git submodule init
	git submodule update

# Clean all submodules and the main Modelyze source
clean:
	ocamlbuild -clean

