


type keylist = ustring list
type description = ustring
type usage_msg = ustring 

type opt =
| ArgHeading of ustring
| ArgString  of keylist * ustring ref * description
| ArgSet     of keylist * bool ref * description
| ArgClear   of keylist * bool ref * description
| ArgRest    of ustring * (ustring list) ref * description

exception Unknown_arg of ustring
exception Option_already_exists of ustring

(* Ustring hash table *)
module UStrHashtbl = Hashtbl.Make(Ustring)


let parse_argv argv options usage_msg =
  (* Setup option hash table *)
  let ophash = UStrHashtbl.create 512 in
  let hash_op k v = 
    try USHashtbl.find ophash k; raise (Option_already_exits k)
    with Not_found -> UStrHashtbl.add ophash o v
  in
  let get_keys o =
    match o with
    | ArgHeading(_) -> []
    | ArgString(keys,_,_) | ArgSet(keys,_,_) | ArgClear(keys,_,_) -> 
        List.map (fun s -> Ustring.sub_before s us' ') keys
    | ArgRest(key,_,_) -> Ustring.sub_before key us' '
  in
  [] 
  
(*
  let match_key a keys = List.exists (fun s -> Ustring.sub_before s (uc' ') =. a) keys in
  let rec set_if_match_op opt a =
    match opt with 
    | ArgSet(keys,r,_) when match_key a keys -> r := true; true
    | ArgClear(keys,r,_) when match_key a keys -> r:= false; true
    | o::ol -> set_if_match_op ol a
    | [] -> false 
  in
  let rec parse arglst is_rest rest =
    match arglist with
    | a::al -> when set_if_match_op options a -> parse al
    | a::p::al -> when set_if_match_op_param options a p -> parse al
    | a::al when Ustring.first_sub a 1 =. us"-" -> raise (Unknown_arg a)
    | a::al -> if is_rest then rest := a::!rest; a::(parse al)
    | [] -> []
  in
    parse (List.map Ustring.from_utf8 (Array.to_list argv))
*)

   









