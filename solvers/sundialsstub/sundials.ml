(*
    Modeling Kernel Language (Modelyze) evaluator
    Copyright (C) 2010-2012 David Broman

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

module Ida =
struct

  type st = nativeint
   
  exception Eqs_mismatch
  exception Init_error

  type residual = float -> float array -> float array -> float array 
  type rootfinder = float -> float array -> float array -> float array

  type params = float * int * float * float * string * int * string

  let raise_failure() = 
    raise (Failure "Simulation is not supported in native compiled mode")

  let norootcb t yy yp = [||] 

  let make ?(start_time=0.0) ?(reltol=1.0e-5) ?(abstol=1.0e-5) ?(roots=0) 
           ?(rootfun=norootcb) init_y init_y' init_id res = raise_failure()

  let reinit s = raise_failure()
  let close s = raise_failure()
  let equations s = raise_failure()
  let reltol s = raise_failure()
  let abstol s = raise_failure()
  let y s = raise_failure()
  let yp s = raise_failure()
  let step s f = raise_failure()
  let time s = raise_failure()
  let roots s = raise_failure()
  let set s a1 a2 = raise_failure()

end

