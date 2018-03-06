

open Ustring.Op
open Printf

type argtype =
| No      (* No arguments *)
| Str     (* Argument is a normal string *)
| Int     (* The argument is an integer that can be both postive and negative *)
| StrList (* The argument can be a list of strings. The list can be empty. *)

exception Error of ustring

(* ---------------------------------------------------------------------*)
let parse argv options =
  
  (* Function for inserting a new argument *)
  let rec insert oplist op args =
    match oplist with
    | (o,str)::ls ->
      if op = o then (o,List.append str args)::ls
      else (o,str)::(insert ls op args)
    | [] -> [(op,args)]
  in

  (* Work function that iterates through all arguments *)
  let rec work args last_op exp_argtype acc_ops acc_args =
    match args with
    | a::al -> (
      try 
        let (k,opargty,_,_,_) = List.find (fun (_,_,x,_,_) -> x =. a) options  in       
        
        (* We have found an option *)
        (match exp_argtype with
        | No | StrList -> work al k opargty (insert acc_ops k []) acc_args
        | Str | Int -> 
           let (_,_,x,_,_) = List.find (fun (k,_,_,_,_) -> k = last_op) options  in 
           raise (Error (us"Option " ^. x ^. us" needs an option argument.")))
    
      (* Not a known option *)
      with Not_found -> 

        (* Check if starts with dash. If so, error *)
        if Ustring.starts_with (us"-") a then
          raise (Error (us"Incorrect or unknown argument '" ^. a ^. us"'"))
        else
          (match exp_argtype with
          | No ->
            (* This is a normal argument *)
              work al last_op No acc_ops (a::acc_args)
          | Str ->
              (* We have one string argument. Next will not be option argument *)
              work al last_op No (insert acc_ops last_op [a]) acc_args
          | Int ->
              (* We have one int argument. Next will not be option argument *)
             (try let _ = int_of_string (Ustring.to_utf8 a) in
                 work al last_op No (insert acc_ops last_op [a]) acc_args
              with _ -> raise (Error (us"Option argument '" ^. 
                                            a ^. us"' is not an integer.")))
          | StrList ->
              (* We have string argument of list. *)
              work al last_op exp_argtype (insert acc_ops last_op [a]) acc_args))
    
      | [] -> (acc_ops,List.rev acc_args)
            

  in
    if List.length options = 0 
    then ([],List.map us argv)
    else            
      let (dummyop,_,_,_,_) = List.hd options in
      work (List.map us argv) dummyop No [] []

  
(* ---------------------------------------------------------------------*)
let optionstext ?(indent=2) ?(max_op_len=25) ?(line_length=80) options = 
  let extra_space = 2 in

  (* Compute the column length for the options *)
  let oplen = List.fold_left (fun len (_,_,op,ex,_) ->
    max (Ustring.length op + Ustring.length ex) len
  ) 0 options in
  
  (* Start of the description column *)
  let desccol = min (indent + oplen + extra_space) max_op_len  in

  (* Print all options *)
  List.fold_left (fun acc (_,opargty,op,ex,txt) ->
    acc ^.
    Ustring.spaces_before (us"") indent ^.
    Ustring.spaces_after (op ^. ex) (desccol - indent) ^.
    (if Ustring.length (op ^. ex) + indent + extra_space > max_op_len 
     then us"\n" ^. Ustring.spaces_after (us"") desccol else us"") ^. 
  
    (* Format description text. Split new line if space *)
    (snd (List.fold_left (fun (col,acc) s ->
      let len_s = Ustring.length s + 1in
      if col + len_s < line_length then
        (col + len_s, acc ^. s ^. us" ")
      else
        (desccol + len_s, acc ^. us"\n" ^. 
          Ustring.spaces_after (us"") desccol ^. s ^. us" ")
    ) (desccol,us"")  
      (List.map Ustring.trim (Ustring.split txt (us" "))
         |> List.filter (fun x -> x <>. us""))))
    ^. us"\n"
    
  ) (us"") options 
    ^. us"\n"



(* ---------------------------------------------------------------------*)
let has_op op oplst = 
  List.mem_assoc op oplst


(* ---------------------------------------------------------------------*)
let str_op op oplst = 
  try List.assoc op oplst |> List.hd
  with _ -> raise Not_found


(* ---------------------------------------------------------------------*)
let strlst_op op oplst = 
  List.assoc op oplst
        







