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




let rec last xs =
  match xs with 
    | [] -> raise (Invalid_argument "Utils.last")
    | [x] -> x
    | x::xs -> last xs

let findindex x l = 
  let rec findidx l c =
    match l with
      | [] -> raise Not_found
      | y::ys -> if x = y then c else findidx ys (c+1)
  in findidx l 0

let find_associndex x l = 
  let rec findidx l c =
    match l with
      | [] -> raise Not_found
      | (y,v)::ys -> if x = y then (v,c) else findidx ys (c+1)
  in findidx l 0

let (|>) x f = f x 

let (<|) f x = f x 

let (>>) f g x = g(f x)

let map_option f op = 
  match op with
    | Some t -> Some (f t)
    | None -> None

let rec map2sc f l1 l2 =
  match l1,l2 with
    | [],_ -> []
    | _,[] -> []
    | (x::xs),(y::ys) -> (f x y)::(map2sc f xs ys)

let rec filtermap f ls =
  match ls with
    | x::xs -> (match f x with 
		  | Some y -> y::(filtermap f xs) 
		  | None -> filtermap f xs)
    | [] -> []

let foldmap f k ls =
  let rec work f k ls a =
    match ls with
      | x::xs -> 
        let (k',x') = f k x in
          work f k' xs (x'::a)
      | [] -> (k,List.rev a)
  in work f k ls []


let rec option_split lst =
  match lst with
    | (Some x)::xs -> 
	(match option_split xs with
	  | Some xs' -> Some (x::xs')
	  | None -> None)
    | (None)::xs -> None
    | [] -> Some []


let string_of_intlist il = 
  let s = String.create (List.length il) in
  il |> List.fold_left (fun i x -> (s.[i] <- char_of_int x); i+1) 0 |> ignore;
  s

let intlist_of_string s =
  let rec work n a = if n >= 0
    then work (n-1) ((int_of_char (s.[n]))::a) else a in
  work (String.length s) []

let write_bin_file filename str =
  let ch = open_out_bin filename in
  output_string ch str;
  close_out ch  

let genlist f n =
  let rec work i a = 
    if i >= 0 then work (i-1) ((f (i-1))::a) else a 
  in work n []

let xor b1 b2 = (b1 || b2) && (not (b1 && b2))


module Int =
struct 
  type t = int
  let compare = compare
end


