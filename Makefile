############################################################################
# Modelyze toolchain
# Copyright (C) 2010-2015 David Broman
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

# Directories where ocamlbuild can find source code.
# DIRS = src,ext/ucamlib/src,ext/extlib,ext/sundials
DIRS = src,ext/ucamlib/src,ext/extlib

.PHONY: all clean

# Init submodules if needed and make native version.
# The resulting executable can be found under /bin and /library (symlinks)
all:    native


# Compile native version
native: bytesfix #comp_c_files
	@ocamlbuild -use-ocamlfind -pkg 'sundialsml' -Is $(DIRS) moz.native
	@rm -f bytes.ml
	@rm -f moz.native
	@rm -rf bin; mkdir bin; cd bin; cp -f ../_build/src/moz.native moz

# Compile debuggable byte version
# debug: bytesfix #comp_c_files
       # @ocamlbuild -use-ocamlfind -pkg 'sundialsml' -Is $(DIRS)  moz.d.byte

# Handling subtree for ext/ucamlib
UCAMLIB_GIT = https://github.com/david-broman/ucamlib.git
UCAMLIB_MSG = 'Updated ucamlib'
add_ucamlib:
	git subtree add --prefix ext/ucamlib $(UCAMLIB_GIT) master --squash -m 'Added ucamlib'
pull_ucamlib:
	git subtree pull --prefix ext/ucamlib $(UCAMLIB_GIT) master --squash -m $(UCAMLIB_MSG)
push_ucamlib:
	git subtree push --prefix ext/ucamlib $(UCAMLIB_GIT) master --squash


test:   all
	@cd test; ./regression

# Clean all submodules and the main Modelyze source
clean:
	@ocamlbuild -clean
	@rm -f bytes.ml
	@rm -rf bin


# Solves a problem because of the introduction of module Bytes in the standard
# library in OCaml version 4.02. In the fix, we create a bytes.ml file locally
# if the current OCaml version does not inclue module Bytes.

BYTES = "let length s = String.length s\n"\
        "let create l = String.create l\n"\
        "let get s n = String.get s n\n"\
        "let set s n c = String.set s n c\n"

bytesfix:
	@echo "let main = Bytes.create 1" > t.ml
	@ocaml t.ml >/dev/null 2>&1 || echo $(BYTES) > bytes.ml
	@rm -f t.ml
