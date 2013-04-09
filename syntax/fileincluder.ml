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

open Ustring.Op
open Utils
open Ast
open Message

module IdSet = Set.Make(Int)
module IdMap = Map.Make(Int)


let lexercache =
  let cache = ref [] in
  fun lexbuffer -> 
    (match !cache with
     | x::xs -> cache := xs; x
     | [] -> (match Lexer.main lexbuffer with
                | [] -> failwith "Should not happen"
                | x::xs -> cache := xs; x))

let parse_file fi fname  =
  try
    let fs1 = open_in fname in
    Lexer.init (us fname) 8;
    let tlst = Parser.main lexercache <| Ustring.lexing_from_channel fs1 in
    close_in fs1; 
    tlst
  with Sys_error _ -> raise (Mkl_static_error(STATIC_ERROR_OPEN_FILE,
                             ERROR,fi,[us fname]))


let remove_duplicates toplst =
  let rec worker toplst items acc =
      match toplst with
        | ((TopLet(_,id,_,_,_,_),fpos) as tt)::ts | 
          ((TopNu(_,id,_),fpos) as tt)::ts |
          ((TopNewType(_,id),fpos) as tt)::ts |  
          ((TopNameType(_,id,_),fpos) as tt)::ts ->
            if try IdMap.find id items = fpos with Not_found -> false then
              worker ts items acc
            else
              worker ts (IdMap.add id fpos items) (tt::acc)
        | (TopInclude(fi,id),fpos)::_ -> assert false
        | [] -> List.rev acc
  in worker toplst (IdMap.empty) []


let rec read_module fi visited cached nameid =
  if IdSet.mem nameid visited then
    raise (Mkl_static_error(STATIC_CIRCULAR_DEP_INCLUDE,
                            ERROR,fi,[Symtbl.get nameid]))
  else
     try (IdMap.find nameid cached,cached)
     with Not_found ->   
       let toplst1 = parse_file fi (Ustring.to_utf8 (Symtbl.get nameid)) in
       let visited' = IdSet.add nameid visited in
       let toplst2 = expand_top nameid 0 toplst1 visited' cached [] in
       let toplst3 = remove_duplicates toplst2 in
       (toplst3,IdMap.add nameid toplst3 cached)


and expand_top nameid k toplst visited cached acc =
  match toplst with
    | TopInclude(fi,id)::ts -> 
        let (toplst',cached') = read_module fi visited cached id in
        expand_top nameid k ts visited cached' (List.rev_append toplst' acc)
    | t::ts -> 
        let fpos = (nameid,k) in
        expand_top nameid (k+1) ts visited cached ((t,fpos)::acc)
    | [] -> List.rev acc


let read_file_chain filename =     
  filename |> String.lowercase |> us |> Symtbl.add 
           |> read_module Info.NoInfo (IdSet.empty) (IdMap.empty) 
           |> fst |> List.split |> fst
  



