# Copyright (c) 2010-2014, David Broman
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without 
# modification, are permitted provided that the following conditions are met:
#
#    * Redistributions of source code must retain the above copyright 
#      notice, this list of conditions and the following disclaimer.
#    * Redistributions in binary form must reproduce the above copyright notice, 
#      this list of conditions and the following disclaimer in the 
#      documentation and/or other materials provided with the distribution.
#    * Neither the name of the Linköping University nor the names of its 
#      contributors may be used to endorse or promote products derived from 
#      this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE 
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON 
# ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#  



DIRS = src,test

.PHONY: all test clean

all:	bytesfix libs apidoc
	ocamlbuild -Is $(DIRS) ucamlib.cma
	ocamlbuild -Is $(DIRS) ucamlib.cmxa
	@rm -f bytes.ml
	@cp _build/src/ucamlib.cma libs/.
	@cp _build/src/ucamlib.cmxa libs/.
	@echo "--------------------------------------"	
	@echo "Finished building ucamlib."	
	@echo "The new libraries are availble in 'libs/'."
	@echo "The generated API documenation is available in 'doc/api'."

libs:	
	mkdir libs

test:	bytesfix
	ocamlbuild -Is $(DIRS) maintest.byte --
	@rm -f bytes.ml
	@rm -f maintest.byte

apidoc:
	ocamlbuild -Is $(DIRS) doc/ucamlib.docdir/index.html
	@rm -f doc/api
	@mv ucamlib.docdir api; mv api doc/.

clean:
	ocamlbuild -clean
	@rm -rf libs
	@rm -rf doc/api
	@rm -f bytes.ml
	@echo ""
	@echo "Finished cleaning project."


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





