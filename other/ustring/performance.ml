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

(* Simple test program for measure the performance of string
   concatination operator *)

open Ustring.Op
open Printf

let st = "This is a test string"
let ust = us"This is a test string"

let concats = 200
let reps = 100

let rec concat_str s n =
  if n=0 then s else concat_str (s ^ st) (n-1)

let rec concat_ustr s n =
  if n=0 then s else concat_ustr (s ^.. ust) (n-1)

let rec fast_concat_ustr s n =
  if n=0 then s else fast_concat_ustr (s ^. ust) (n-1)

let rec times n f =
    if n=0 then f() else let _ = times (n-1) f in f()

let measure f s1 s2 =
  let t1 = Sys.time() in
  let _ = f() in
  let t2 = Sys.time() in
  printf "%s %d %s %d times takes %f seconds\n" 
    s1 concats s2 reps (t2-.t1)
    
  

let main =
  measure (fun () -> times reps (fun () -> 
     String.length (concat_str st concats))) "Concatination" "string";
  measure (fun () -> times reps (fun () -> 
     Ustring.length (concat_ustr ust concats))) "Concationation" "ustring";
  measure (fun () -> times reps (fun () -> 
     Ustring.length (fast_concat_ustr ust concats))) 
    "Fast concatination" "ustring"

