(*
    Modeling Kernel Language (MKL) evaluator
    Copyright (C) 2010-2011 David Broman

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

open Printf

let cb_count = ref 0
  
module Ida =
struct
  type st = nativeint
   
  exception Eqs_mismatch
  exception Init_error

  type residual = float -> float array -> float array -> float array 
  type rootfinder = float -> float array -> float array -> float array

  (* Tuple (start_time,no_of_equations,reltol,abstol,res_cb_idstr,root_cb_idstr) *)
  type params = float * int * float * float * string * int * string
 
  (* p1: params, p2: initial value for y, p3: initial values for y',
     p4: id for initial condition calculation *)
  external make_ext : 
    params -> float array -> float array -> float array -> nativeint = "ida_make_ext_c"
 
  let norootcb t yy yp = [||] 
 
  let make ?(start_time=0.0) ?(reltol=1.0e-5) ?(abstol=1.0e-5) ?(roots=0) 
           ?(rootfun=norootcb) init_y init_y' init_id res =
    let size = Array.length init_y in 
    if (Array.length init_y') != size then raise Eqs_mismatch
    else
      (* Create unique ID and register residual callback ID *) 
      let cb_res_id = "IDA-RES-CB-" ^ (string_of_int !cb_count) in
      incr cb_count;
      Callback.register cb_res_id res;
      (* If root finder is used, register callback *)
      let cb_root_id = "IDA-ROOT-CB-" ^ (string_of_int !cb_count) in
      incr cb_count;
      Callback.register cb_root_id rootfun;    
      (* Call the C-function *)
      let st = make_ext (start_time,size,reltol,abstol,cb_res_id,roots,cb_root_id) 
	init_y init_y' init_id in
      if st = Nativeint.zero then raise Init_error else st

  external reinit : st -> unit = "ida_reinit_c" 

  external close : st -> unit = "ida_close_c" 

  external equations : st -> int = "ida_equations_c" 

  external reltol : st -> float = "ida_reltol_c" 

  external abstol : st -> float = "ida_abstol_c" 

  external y : st -> float array = "ida_y_c"

  external yp : st -> float array = "ida_yp_c"

  external step : st -> float -> float = "ida_step_c"

  external time : st -> float = "ida_time_c" 
 
  external roots : st -> int array = "ida_roots_c"

  external set : st -> float array -> float array -> unit = "ida_set_c"

end


