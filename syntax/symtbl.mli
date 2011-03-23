(*
Modeling Kernel Language (MKL) toolchain
Copyright (C) 2010 David Broman

MKL toolchain is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

MKL toolchain is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with MKL toolchain.  If not, see <http://www.gnu.org/licenses/>.
*)


(** Symbol table 
  
    Static module used for fast lookup of parsed symbols.
    Used by both Mamel and Modelica implementation. Since ocamllex
    and ocamlyacc are static, this module is also static.
*)




val add : Ustring.t -> int
(** Add an identifier [s] to the symbol table. Returns an 
    integer id. If the identifier already exists, the
    current integer id is returned.  *)

val get : int -> Ustring.t
(** Get the ustring representation for a symbol with integer id [i] 
   Raises [Not_found] if the id value is not found *)

val clear : unit -> unit
(** Clear all contents of the symbol table *)

