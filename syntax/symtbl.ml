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

open Ustring.Op
module USHashtbl = Hashtbl.Make(Ustring)



let symtab1  = USHashtbl.create 1024 
let (symtab2 : (int,ustring) Hashtbl.t) = Hashtbl.create 1024 
let idcount = ref 0

(** Add an identifier [s] to the symbol table. Returns an 
    integer id. If the identifier already exists, the
    current integer id is returned.  *)
let add s = 
  try USHashtbl.find symtab1 s
  with
      Not_found -> 
	USHashtbl.add symtab1 s !idcount;
	Hashtbl.add symtab2 !idcount s;
	incr idcount;
	!idcount - 1

(** Get the ustring representation for a symbol with integer id [i] 
   Raises [Not_found] if the id value is not found *)

let get i = Hashtbl.find symtab2 i


let clear () =
  USHashtbl.clear symtab1;
  Hashtbl.clear symtab2;
  idcount := 0


