(*
Modelyze toolchain
Copyright (C) 2010-2012 David Broman

Modelyze toolchain is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Modelyze toolchain is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Modelyze toolchain.  If not, see <http://www.gnu.org/licenses/>.
*)


exception Mkl_type_error of Message.message

val typeofterm :   Ast.tm -> Ast.ty 

val typecheck : Ast.tm -> Ast.tm

val consistent : Ast.ty -> Ast.ty -> bool
