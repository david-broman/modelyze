############################################################################
# Modelyze toolchain
# Copyright (C) 2010-2014 David Broman
#
# Modelyze toolchain is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Modelyze toolchain is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Modelyze toolchain.  If not, see <http://www.gnu.org/licenses/>.
#############################################################################


# This is the path to standard C libraries. Currently, what is needed is that
# SUNDIALS libraries are installed at this path.
C_LIBS = /usr/local/lib

# Directories where ocamlbuild can find source code.
DIRS = src,ext/ucamlib/src,ext/extlib,ext/sundials

# These are the files and libraries of C code that should be linked.
C_FILES = ext/sundials/ida_stubs.o,$(C_LIBS)/libsundials_ida.a,$(C_LIBS)/libsundials_nvecserial.a


.PHONY: all clean

# Init submodules if needed and make native version. 
# The resulting executable can be found under /bin and /library (symlinks)
all:    ext/ucamlib/Makefile native


# Compile native version
native: comp_c_files
	@ocamlbuild -Is $(DIRS) moz.native -lflags $(C_FILES) 
	@mv moz.native bin/moz
	@(cd library; rm -f moz; ln -s ../_build/src/moz.native moz)

# Compile byte code version (is not currently working)
byte: 	comp_c_files
	@ocamlbuild -Is $(DIRS) moz.byte -lflag -custom,$(C_FILES)
	@mv moz.byte bin/moz
	@(cd library; rm -f moz; ln -s ../_build/src/moz.byte moz)

# C-files 
comp_c_files:
	@ocamlbuild ext/sundials/ida_stubs.o -cflags '-ccopt -Qunused-arguments'

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

