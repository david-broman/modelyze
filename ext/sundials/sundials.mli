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


module Ida :  
sig
  (** Wrapper module for Sundial's DAE solver IDA *)

  exception Eqs_mismatch
  (** Raised when the number of equations supplied (e.g. array sizes) 
      are wrong *)

  exception Init_error
  (** Raised when there is an error in the init procedure. *)  

  type st
  (** The simulation instance type *) 

  type residual = float -> float array -> float array -> float array 
  (** Type of the residual callback function. Parameter 1 is the current
      simulation time. Parameter 2 is the current value of the dependent 
      variable vector y(t). Parameter 3 is the current value of y'(t). 
      The function should return the residual vector F(t,y,y'). Note that
      all three vectors must have same size as the number of equations. *)

  type rootfinder = float -> float array -> float array -> float array
  (** Type of the root finder callback function. Parameter is the current
      simulation time. Parameter 2 is the current value of the dependent 
      variable vector y(t). Parameter 3 is the current value of y'(t). 
      The function should return an array with the root differences sought.
      Note that the size of this array should be the same as the one
      defined when function [rootinit] was called *) 

  val make : ?start_time:float -> ?reltol:float -> ?abstol:float -> 
             ?roots:int -> ?rootfun:rootfinder ->
             float array -> float array -> float array -> residual -> st
  (** [make ~start_time:time ~reltol:rt ~abstol:at ~roots:nr ~rootfun:rootf y yp res] 
     creates a new simulation instance with start time [time], 
     relative error tolerance [rt], absolute error tolerance [at], 
     number of roots to find [nr], the root finder function [rootf], initial 
     values [y], inital derivative values [yp], and the residual function [res]. 
     Both the realtive and absolute error tolerances are default set to 1.0e-5 and
     the default start time is 0.  By default, there are no root finders. If a root 
     finder should be used, both then number of roots must be defined [roots] and 
     the root function [rootfun]. See types [residual] and [rootfinder] for more 
     information about the callback function. The function can throw exceptions 
     [Eqs_mismatch] and [Init_error]. *)

  val reinit: st -> unit 
  (** Reinitialize the solver again with the same time and value as currently
      exists in the simulation instance *)

  val close: st -> unit 
  (** Close a simulation instance. This function must be called
      after using the system to free up memory. *)

  val equations : st -> int 
  (** Returns the number of equations. *)

  val reltol : st -> float 
  (** Returns the current relative error tolerance. *)

  val abstol : st -> float 
  (** Returns the current absolute error tolerance. *)

  val y : st -> float array 
  (** Returns an array of variables. *)

  val yp : st -> float array 
  (** Returns an array of derivative variables. *)

  val step : st -> float -> float 
  (** [step st t] tries to simulate [t] time forward and returns 
      the time after the step. If something goes wrong, 
      the step time zero will be returned. *) 

  val time : st -> float 
  (** Returns the current simulation time *) 

  val roots : st -> int array 
  (** Returns an array representing the different roots. A zero
      element means no root found, and a non-zero element that the root
      was found. An empty array is returned if no roots at all were detected. 
      This function should always be called after the [step] function.*)

  val set : st -> float array -> float array -> unit
  (** Set the y vector (parameter 2) and yp vector (parameter 3). *)

end





