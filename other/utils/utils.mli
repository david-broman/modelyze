(* 
Copyright (c) 2010, David Broman
All rights reserved.

Redistribution and use in source and binary forms, with or without 
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright 
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice, 
      this list of conditions and the following disclaimer in the 
      documentation and/or other materials provided with the distribution.
    * Neither the name of the Linköping University nor the names of its 
      contributors may be used to endorse or promote products derived from 
      this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE 
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON 
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** Different utility functions that are missing in the standard library *)

val last : 'a list -> 'a
(** Returns the last element in a list. Raises [Invalid_argument "Utils.last"]
    if the list is empty. *)

val findindex : 'a -> 'a list -> int
(** Function [findindex x l] returns the index for the first occurance of [x] in list [l]. Raises [Not_found] if [x] does not exist in [l]. *)

val find_associndex : 'a -> ('a * 'b) list -> ('b * int)
(** Expression [find_associndex x l] returns a tuple with value and index for
    the first occurance of [x] in the association list [l]. 
    Raises [Not_found] if [x] is not a key in [l].*)

val ( |> ) : 'a -> ('a -> 'b) -> 'b
(** Pipe-forward operator *)

val ( <| ) :  ('a -> 'b) -> 'a -> 'b
(** Pipe-backward operator *)

val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** Forward composition operator *)

val map_option : ('a -> 'b) -> 'a option -> 'b option

val map2sc : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
(* Map2 short-ciruit *)

val filtermap : ('a -> 'b option) -> 'a list -> 'b list

val foldmap : ('a -> 'b -> 'a * 'c) -> 'a -> 'b list -> 'a * 'c list

val option_split : ('a option) list -> ('a list) option

val string_of_intlist : int list -> string
(** Converts a list of integers to a string, where the least 8 significant bits
    are used of each integer. *)

val intlist_of_string : string -> int list
(** Converts a string into a list of integers *)

val write_bin_file : string -> string -> unit
(** Call [write_bin_file n d] creates a binary file named [n] and stores 
    string data [d] in the file. Raises [Sys_error] if error creating or
    writing to file. *)

val genlist : (int -> 'a) -> int -> 'a list
(** Call [genlist f n] Generates a list with [n] elements, where expression [f i] 
    is the value of each element and [i] is the index in the list starting at 0. *)

val xor : bool -> bool -> bool

module Int :
sig
  type t = int
  val compare : t -> t -> int
end
(** Integer module with functions [compare] and type [t], which makes it easy to be
    passed as argument to functors [Set.Make] and [Map.Make]
*)

    
