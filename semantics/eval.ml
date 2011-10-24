(*
Modeling Kernel Language (MKL) toolchain
Copyright (C) 2010-2011 David Broman

MKL toolchain is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

MKL toolchain is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with MKL toolchain.  If not, see <http://www.gnu.org/licenses/>.
*)

open Ustring.Op
open Utils
open Printf
open Info
open Sundials

type level = int
type index = int
type sym = int
type typeid = int
type specialize = bool

type uenv = (int * int) list
type env = tm list

  

and ty =
  | TyBool      
  | TyInt       
  | TyReal      
  | TyString    
  | TyArrow     of ty * ty 
  | TyUnit      
  | TyList      of ty
  | TyTuple     of ty list
  | TyModel     of ty
  | TyAnyModel  
  | TyBot        
  | TyUserdef   of typeid
  | TyArray     of ty
  | TyMap       of ty * ty
  | TySet       of ty 
  | TyDAESolver


and tm =
  | TmVar         of index
  | TmSymVar      of sym
  | TmLam         of tm 
  | TmClos        of tm * env 
  | TmApp         of tm * tm * specialize
  | TmFix         of tm
  | TmIf          of tm * tm * tm
  | TmConst       of Ast.const
  | TmBrack       of tm
  | TmEsc         of tm
  | TmRun         of tm
  | TmUk          of sym * ty 
  | TmUkGen       of ty
  | TmModApp      of tm * tm 
  | TmModIf       of tm * tm * tm
  | TmModEqual    of tm * tm
  | TmModProj     of int * tm
  | TmVal         of tm * ty
  | TmDecon       of tm * mpat * tm * tm
  | TmEqual       of tm * tm
  | TmLcase       of tm * tm * tm
  | TmCons        of tm * tm
  | TmNil         
  | TmTuple       of tm list
  | TmProj        of int * tm
  | TmArray       of tm array
  | TmArrayOp     of Ast.arrayop * tm list
  | TmMap         of int * (tm,tm) PMap.t
  | TmMapOp       of Ast.mapop * tm list
  | TmSet         of int * (tm,unit) PMap.t
  | TmSetOp       of Ast.setop * tm list
  | TmDAESolver   of Ida.st * tm array * tm array
  | TmDAESolverOp of Ast.daesolverop * tm list
  | TmDPrint      of tm
  | TmDPrintType  of tm
  | TmError       of info * tm
  | TmDebugId     of Ast.ident * tm



and mpat = 
  | MPatUk         of ty
  | MPatModApp     
  | MPatModIfGuard
  | MPatModIfThen
  | MPatModIfElse
  | MPatModEqual     
  | MPatModProj     
  | MPatVal        of ty


type idcount = int
type ident = int
let (symIdCount : (ident,idcount) Hashtbl.t) = Hashtbl.create 1024 
let (symToId : (tm,(ident * idcount)) Hashtbl.t) = Hashtbl.create 1024 


let debugTagTm id t =
  match t with
    | TmUk(s,ty) -> 
        (try 
           let count = Hashtbl.find symIdCount id in
           Hashtbl.add symIdCount id (count+1);
           Hashtbl.add symToId t (id,count+1);
           t
         with Not_found -> 
	   Hashtbl.add symIdCount id 1; 
           Hashtbl.add symToId t (id,1);
           t)
        
    | t -> t

let getDebugSymId t = 
  let (id,count) = Hashtbl.find symToId t in
  let totalCount = Hashtbl.find symIdCount id in
  let s = Symtbl.get id in
  if totalCount = 1 then s ^. us"#"
  else s ^. us"#" ^. ustring_of_int count


let no_pat_vars p = 
  match p with
  | MPatUk(_) -> 0
  | MPatModApp -> 2
  | MPatModIfGuard -> 1
  | MPatModIfThen -> 1
  | MPatModIfElse -> 1
  | MPatModEqual -> 2
  | MPatModProj -> 2
  | MPatVal(_) -> 1

let pprint_ty t =
  let rec pprint_ty left t =
      match t with 
	| TyBool ->  us"Bool"  
	| TyInt ->  us"Int"  
	| TyReal -> us"Real"  
	| TyString ->  us"String"  
	| TyArrow(t1,t2) -> 
	    (if left then us"(" else us"") ^. (pprint_ty true t1) ^. us" " ^.  
	      us"->" ^. us" " ^. (pprint_ty false t2) ^.
	      (if left then us")" else us"")
 	| TyUnit -> us"()"
        | TyList(t) ->  us"[" ^. (pprint_ty false t) ^. us"]"
        | TyTuple(tylst) -> us"(" ^. (tylst |> 
	    List.map (pprint_ty false) |> Ustring.concat (us","))  ^. us")"
        | TyModel(t) ->  us"<" ^. (pprint_ty false t) ^. us">"
	| TyAnyModel -> us"<>"  
	| TyBot ->  us"bot"  
        | TyUserdef(tyid) -> ustring_of_int tyid 
        | TyArray(t) ->  us"{" ^. (pprint_ty false t) ^. us"}"
	| TyMap(t1,t2) -> 
	    us"(" ^. (pprint_ty false t1) ^. us" " ^.  
	      us"=>" ^. us" " ^. (pprint_ty false t2) ^. us")" 
        | TySet(t) -> us"{" ^. (pprint_ty false t) ^. us"}"
	| TyDAESolver ->  us"SimInst"  
  in pprint_ty false t


let pprint_pat p = 
  match p with
  | MPatUk(ty) -> us"uk:" ^. pprint_ty ty 
  | MPatModApp ->   us"app " 
  | MPatModIfGuard ->   us"ifguard " 
  | MPatModIfThen ->   us"ifthen " 
  | MPatModIfElse ->   us"ifelse " 
  | MPatModEqual ->   us"== " 
  | MPatModProj ->   us"proj " 
  | MPatVal(ty) -> us"val:" ^.  pprint_ty ty

let rec pprint t  =
  match t with
    | TmVar(idx) -> ustring_of_int idx ^. us"'"
    | TmSymVar(s) -> us"sym(" ^. ustring_of_int s ^. us")"
    | TmLam(t) -> us"(" ^. us"fun -> " ^. pprint t ^. us")"
    | TmClos(t,e) -> us"(" ^. us"closure[" ^. 
        (*(e |> List.map pprint |> Ustring.concat (us",")) ^.*) us"] -> " ^. 
             pprint t ^. us")"
    | TmApp(t1,t2,fs) -> (if fs then us"specialize(" else  us"(" ) ^. 
        pprint t1 ^. us" " ^. pprint t2 ^. us")"
    | TmFix(t) -> us"fix[" ^. pprint t ^. us"]"
    | TmIf(t1,t2,t3) -> us"if " ^. 
	pprint t1 ^. us" then " ^.pprint t2 ^. us" else " ^. pprint t3 
    | TmConst(c) -> Ast.pprint_const c 0
    | TmBrack(t) -> us".<" ^. pprint t ^. us">."     
    | TmEsc(t) -> us".~(" ^. pprint t ^. us")"
    | TmRun(t) -> us".!(" ^. pprint t ^. us")"
    | TmUk(idx,ty) -> getDebugSymId t
    | TmUkGen(ty) -> us"ukgen(" ^. pprint_ty ty ^. us")"
    | TmModApp(t1,t2) -> us"(" ^. pprint t1  ^. us"@" ^. pprint t2  ^. us")"
    | TmModIf(t1,t2,t3) -> us"<if> " ^. 
	pprint t1 ^. us" then " ^.pprint t2 ^. us" else " ^. pprint t3 
    | TmModEqual(t1,t2) -> pprint t1 ^. us" <==> " ^. pprint t2
    | TmModProj(i,t) -> us"proj " ^. ustring_of_int i ^. us" " ^. pprint t
    | TmVal(t,ty) -> us"val(" ^. pprint t ^. us":" ^. pprint_ty ty ^. us")"
    | TmDecon(t1,pat,t2,t3) -> us"(decon " ^.
         pprint t1 ^. us" with " ^. pprint_pat pat ^. us" then " ^. 
         pprint t2 ^. us" else " ^. pprint t3 ^. us")"
    | TmEqual(t1,t2) -> pprint t1 ^. us" == " ^. pprint t2
    | TmLcase(t,t1,t2) -> us"lcase " ^. pprint t ^. us" of " ^.
              us":: -> (" ^. pprint t1 ^. us") | [] -> (" ^. pprint t2 ^. us")" 
    | TmCons(t1,t2) -> 
        (let rec toList t = 
          match t with 
            | TmCons(t1,t2) -> (pprint t1)::(toList t2)
            | _ -> []
        in
          us"[" ^. (Ustring.concat (us", ") (toList t)) ^. us"]" )
    | TmNil -> us"[]" 
    | TmTuple(tms) -> 
	us"(" ^. (tms |> List.map pprint |> Ustring.concat (us", ")) ^. us")"
    | TmProj(i,t) -> us"proj " ^. ustring_of_int i ^. us" " ^. pprint t
    | TmArray(tms) -> us"[|" ^. 
        (tms |> Array.to_list |> List.map pprint |> Ustring.concat (us",")) ^. us"|]"
    | TmArrayOp(op,tms) -> Ast.pprint_array_op op ^. us" " ^. 
        (tms |> List.map pprint |> Ustring.concat (us" "))
(*    | TmMap(size,tmmap) -> us"map{" ^. 
        (PMap.foldi (fun t1 t2 a -> pprint t1 ^. us"->" ^. pprint t2 ^. us"," ^. a)  
          tmmap (us"")) ^. us"}" *)
    | TmMap(size,tmmap) -> 
        let lst = PMap.foldi (fun t1 t2 ts -> 
                                (pprint t1 ^. us" => " ^. pprint t2)::ts) tmmap [] in
          us"{" ^. (Ustring.concat (us", ") lst) ^. us"}" 
    | TmMapOp(op,tms) -> Ast.pprint_map_op op ^. us" " ^. 
        (tms |> List.map pprint |> Ustring.concat (us" "))
    | TmSet(size,tmset) -> 
        let lst = PMap.foldi (fun t1 _ ts -> (pprint t1)::ts) tmset [] in
          us"{" ^. (Ustring.concat (us", ") lst) ^. us"}" 
    | TmSetOp(op,tms) -> Ast.pprint_set_op op ^. us" " ^. 
        (tms |> List.map pprint |> Ustring.concat (us" "))
    | TmDAESolver(st,_,_) -> us"sim"
    | TmDAESolverOp(op,tms) -> Ast.pprint_daesolver_op op ^. us" " ^. 
        (tms |> List.map pprint |> Ustring.concat (us" "))
    | TmDPrint(t) -> pprint t
    | TmDPrintType(t) -> pprint t
    | TmError(fi,t) -> us"error " ^. pprint t
    | TmDebugId(_,t) -> pprint t

(* State when two terms are equal. Note that comparing variables, closures
   and fix terms always return false *)
let rec tm_equiv t1 t2 = 
  t1 = t2


let rec trans_ty ty =
  match ty with 
  | Ast.TyBool(_,_) -> TyBool
  | Ast.TyInt(_,_) -> TyInt
  | Ast.TyReal(_,_) -> TyReal
  | Ast.TyString(_,_) -> TyString
  | Ast.TyArrow(_,_,ty1,ty2) -> TyArrow(trans_ty ty1,trans_ty ty2)
  | Ast.TyUnit(_,_) -> TyUnit
  | Ast.TyList(_,_,ty) -> TyList(trans_ty ty)    
  | Ast.TyTuple(_,_,tys) -> TyTuple(List.map trans_ty tys)
  | Ast.TyModel(_,_,ty) -> TyModel(trans_ty ty)    
  | Ast.TyAnyModel(_,_) -> TyAnyModel
  | Ast.TyBot(_,_) -> TyBot
  | Ast.TyUserdef(_,_,id,_) -> TyUserdef(id)
  | Ast.TyIdent(_,_,_) -> assert false
  | Ast.TyArray(_,_,ty) -> TyArray(trans_ty ty)
  | Ast.TyMap(_,_,ty1,ty2) -> TyMap(trans_ty ty1,trans_ty ty2)
  | Ast.TySet(_,_,ty) -> TySet(trans_ty ty)
  | Ast.TyDAESolver(_,_) -> TyDAESolver

let trans_pat l p denv =
  match p with
  | Ast.MPatUk(_,ty) -> (MPatUk(trans_ty ty),denv)
  | Ast.MPatModApp(_,id1,id2) -> (MPatModApp,(id1,l)::(id2,l)::denv)
  | Ast.MPatModIfGuard(_,id1) -> (MPatModIfGuard,(id1,l)::denv)
  | Ast.MPatModIfThen(_,id1) -> (MPatModIfThen,(id1,l)::denv)
  | Ast.MPatModIfElse(_,id1) -> (MPatModIfElse,(id1,l)::denv)
  | Ast.MPatModEqual(_,id1,id2) -> (MPatModEqual,(id1,l)::(id2,l)::denv)
  | Ast.MPatModProj(_,id1,id2) -> (MPatModProj,(id1,l)::(id2,l)::denv)
  | Ast.MPatVal(_,id,ty) -> (MPatVal(trans_ty ty),(id,l)::denv)

let translate t =
  let rec nl l d t = 
    if l > d then TmBrack(nl l (d+1) t)
    else if l < d then TmEsc(nl l (d-1) t)
    else t 
  in 
  let rec mkfuns plst t d denv  =
    match plst with
      | [] -> trans t d denv 
      | (x,ty)::res -> TmLam(mkfuns res t d ((x,d)::denv) )
  and trans t d denv  = 
    match t with
      | Ast.TmVar(_,id) -> 
	  (try let (l2,i) = Utils.find_associndex id denv in nl l2 d (TmVar(i)) 
	   with Not_found -> assert false)
      | Ast.TmLam(_,l,id,ty,t) -> 
	  nl l d (TmLam(trans t  l ((id,l)::denv) ))
      | Ast.TmApp(_,l,t1,t2,fs) -> 
	  nl l d (TmApp(trans t1 l denv ,trans t2 l denv,fs))
      | Ast.TmFix(_,l,t) -> nl l d (TmFix(trans t l denv ))
      | Ast.TmLet(_,l,id,Some ty,plst,t1,t2,false) -> 
	  nl l d (TmApp(TmLam(trans t2 l 
		((id,l)::denv) ),mkfuns plst t1 d denv,false)) 
      | Ast.TmLet(fi,l,id,Some ty,plst,t1,t2,true) -> 
	  nl l d (TmApp(TmLam(trans t2 l ((id,l)::denv) ),
             TmFix(TmLam(mkfuns plst t1 l ((id,l)::denv) )),false)) 
      | Ast.TmLet(_,_,_,_,_,_,_,_) -> assert false
      | Ast.TmIf(_,l,t1,t2,t3) -> nl l d (TmIf(trans t1 l denv ,
		trans t2 l denv ,trans t3 l denv )) 
      | Ast.TmConst(_,l,c) -> nl l d (TmConst(c))
      | Ast.TmUp(_,l,t) -> 
	  nl l d (TmApp(TmLam(TmBrack(TmVar(0))),
					    trans t l denv,false))
      | Ast.TmDown(_,l,t) -> nl l d (TmRun(trans t l denv ))
      | Ast.TmBracket(_,_) -> assert false
      | Ast.TmEscape(_,_) -> assert false
      | Ast.TmList(_,_,_) -> assert false
      | Ast.TmMatch(_,_,_,_) -> assert false
      | Ast.TmUk(_,l,id,ty) -> 
	  (try let (l2,i) = Utils.find_associndex id denv in nl l2 d (TmVar(i)) 
	   with Not_found -> assert false)
      | Ast.TmNu(_,l,id,ty,t) -> 
          nl l d (TmApp(TmLam(trans t l ((id,l)::denv)),
                        TmDebugId(id,TmUkGen(trans_ty ty)),false))
      | Ast.TmModApp(_,l,t1,t2) -> 
          nl l d (TmModApp(trans t1 l denv ,trans t2 l denv ))
      | Ast.TmModIf(_,l,t1,t2,t3) -> nl l d (TmModIf(trans t1 l denv ,
		trans t2 l denv ,trans t3 l denv )) 
      | Ast.TmModEqual(_,l,t1,t2) -> 
	  nl l d (TmModEqual(trans t1 l denv ,trans t2 l denv ))
      | Ast.TmModProj(_,l,i,t) -> nl l d (TmModProj(i,trans t l denv ))
      | Ast.TmVal(_,l,t,ty) -> nl l d (TmVal(trans t l denv ,trans_ty ty))
      | Ast.TmDecon(_,l,t1,p,t2,t3) ->
	  let (p',denv') = trans_pat l p denv in
	  nl l d (TmDecon(trans t1 l denv ,p',trans t2 l denv' ,
		   trans t3 l denv ))
      | Ast.TmEqual(_,l,t1,t2) -> 
	  nl l d (TmEqual(trans t1 l denv ,trans t2 l denv ))
      | Ast.TmLcase(_,l,t,id1,id2,t1,t2) -> 
	  nl l d (TmLcase(trans t l denv ,trans t1 l
	         ((id1,l)::(id2,l)::denv) ,trans t2 l denv ))
      | Ast.TmCons(_,l,t1,t2) -> 
	  nl l d (TmCons(trans t1 l denv ,trans t2 l denv ))
      | Ast.TmNil(_,l,ty) -> nl l d (TmNil)
      | Ast.TmTuple(_,l,tms) -> 
	  nl l d (TmTuple(List.map (fun t -> trans t l denv ) tms))
      | Ast.TmProj(_,l,i,t) -> nl l d (TmProj(i,trans t l denv ))
      | Ast.TmArray(_,l,tms) -> 
          nl l d (TmArray(Array.map (fun t -> trans t l denv ) tms))
      | Ast.TmArrayOp(_,l,op,tms) ->
          nl l d (TmArrayOp(op,List.map (fun t -> trans t l denv ) tms))
      | Ast.TmMapOp(_,l,op,tms) ->
          nl l d (TmMapOp(op,List.map (fun t -> trans t l denv ) tms))
      | Ast.TmSetOp(_,l,op,tms) ->
          nl l d (TmSetOp(op,List.map (fun t -> trans t l denv ) tms))
      | Ast.TmDAESolverOp(_,l,op,tms) ->
          nl l d (TmDAESolverOp(op,List.map (fun t -> trans t l denv ) tms))
      | Ast.TmDPrint(t) -> TmDPrint(trans t d denv )
      | Ast.TmDPrintType(t) -> TmDPrintType(trans t d denv )
      | Ast.TmError(fi,l,t) -> nl l d (TmError(fi,trans t l denv ))
  in 
    trans t 0 []


let eval_array_op op array_lst =
  let raise_out_of_bounds() = 
    raise (Ast.Mkl_runtime_error (Message.RUNTIME_ERROR,Message.ERROR, NoInfo, 
                                     [us"Index out of bounds"])) in 
  match op,array_lst with
  | Ast.ArrayOpLength,[TmArray(ar)] -> 
        TmConst(Ast.ConstInt(Array.length ar))
  | Ast.ArrayOpMake,[TmConst(Ast.ConstInt(len));elem] -> 
      TmArray(Array.make len elem)
  | Ast.ArrayOpGet,[TmArray(ar);TmConst(Ast.ConstInt(pos))] ->
      (try Array.get ar pos 
       with Invalid_argument _ -> raise_out_of_bounds())
  | Ast.ArrayOpSet,[TmArray(ar);TmConst(Ast.ConstInt(pos));elem] ->
      (try Array.set ar pos elem;TmConst(Ast.ConstUnit) 
       with Invalid_argument _ -> raise_out_of_bounds())
  | _ -> assert false (* Should be discovered by the type checker *)

let eval_map_op op arg_lst = 
  let raise_key_not_found() = 
    raise (Ast.Mkl_runtime_error (Message.RUNTIME_ERROR,Message.ERROR, NoInfo, 
                                     [us"Key not found"])) in 
  match op,arg_lst with
  | Ast.MapOpSize,[TmMap(size,m)] -> 
      TmConst(Ast.ConstInt(size))            
  | Ast.MapOpEmpty,[] -> 
      TmMap(0,PMap.empty)
  | Ast.MapOpAdd,[key;value;TmMap(size,m)] ->
      if PMap.mem key m 
      then TmMap(size,PMap.add key value m)
      else TmMap(size+1,PMap.add key value m)
  | Ast.MapOpFind,[key;TmMap(size,m)] ->
      (try PMap.find key m with Not_found -> raise_key_not_found())
  | Ast.MapOpMem,[key;TmMap(size,m)] ->
      TmConst(Ast.ConstBool(PMap.mem key m))
  | Ast.MapOpRemove,[key;TmMap(size,m)] ->
      if PMap.mem key m 
      then TmMap(size-1,PMap.remove key m)
      else TmMap(size,m)
  | Ast.MapOpToList,[TmMap(size,m)] ->
      PMap.foldi (fun k v a -> TmCons(TmTuple([k;v]),a)) m TmNil
  | _ -> assert false (* Should be discovered by the type checker *)

let eval_set_op op arg_lst = 
  match op,arg_lst with
  | Ast.SetOpSize,[TmSet(size,m)] -> 
      TmConst(Ast.ConstInt(size))            
  | Ast.SetOpEmpty,[] -> 
      TmSet(0,PMap.empty)
  | Ast.SetOpAdd,[key;TmSet(size,m)] ->
      if PMap.mem key m 
      then TmSet(size,PMap.add key () m)
      else TmSet(size+1,PMap.add key () m)
  | Ast.SetOpMem,[key;TmSet(size,m)] ->
      TmConst(Ast.ConstBool(PMap.mem key m))
  | Ast.SetOpRemove,[key;TmSet(size,m)] ->
      if PMap.mem key m 
      then TmSet(size-1,PMap.remove key m)
      else TmSet(size,m)
  | Ast.SetOpToList,[TmSet(size,m)] ->
      PMap.foldi (fun k _ a -> TmCons(k,a)) m TmNil
  | _ -> assert false (* Should be discovered by the type checker *)

let array_update ar artm =
  Array.iteri (fun i r -> artm.(i) <- TmConst(Ast.ConstReal(r))) ar

let array_to_tm ar  =
  Array.map (fun r -> TmConst(Ast.ConstReal(r))) ar

let array_from_tm ar =
  Array.map (fun t -> match t with TmConst(Ast.ConstReal(r)) -> r 
              | _ -> assert false) ar

let array_from_tmlist tm =
  let rec worker tm =
    match tm with
      | TmCons(TmConst(Ast.ConstReal(r)),ts) -> r::(worker ts)
      | TmNil -> []
      | _ -> assert false
  in Array.of_list (worker tm)



let eval_daesolver_op eval op arg_lst = 
    (* Residual and root finder functions *)
    let resrootfun tmres time yy yp = 
      let tmtime = TmConst(Ast.ConstReal(time)) in  
      let tmyy = TmArray(array_to_tm yy) in
      let tmyp = TmArray(array_to_tm yp) in
      let lst = 
        eval (TmApp(TmApp(TmApp(tmres,tmtime,false),tmyy,false),tmyp,false)) in
        array_from_tmlist lst 
    in
    match op,arg_lst with
    | Ast.DAESolverOpMake,
        [TmArray(tm_yy);TmArray(tm_yp);TmArray(tm_id);tmres] -> 
        let st = Ida.make (array_from_tm tm_yy) (array_from_tm tm_yp)  
                       (array_from_tm tm_id) (resrootfun tmres) in
        array_update (Ida.y st) tm_yy;
        TmDAESolver(st,tm_yy,tm_yp)
    | Ast.DAESolverOpMakeHybrid,
        [TmConst(Ast.ConstReal(time));TmArray(tm_yy);TmArray(tm_yp);
         TmArray(tm_id);tmres;tmrootfinder] -> 
        let rootfun = resrootfun tmrootfinder in
        let (yy,yp) = (array_from_tm tm_yy,array_from_tm tm_yp) in
        let roots = Array.length (rootfun 0. yy yp) in
        let st = Ida.make ~start_time:time ~roots:roots ~rootfun:rootfun yy yp 
                 (array_from_tm tm_id) (resrootfun tmres)  in
        array_update (Ida.y st) tm_yy;
        array_update (Ida.yp st) tm_yp;
        TmDAESolver(st,tm_yy,tm_yp)
    | Ast.DAESolverOpStep,[TmConst(Ast.ConstReal(time));
                              TmDAESolver(st,tm_yy,tm_yp)] -> 
        let time' = Ida.step st time in
        array_update (Ida.y st) tm_yy;
        array_update (Ida.yp st) tm_yp;
        TmConst(Ast.ConstReal(time'))
    | Ast.DAESolverOpReinit,[TmDAESolver(st,tm_yy,tm_yp)] -> 
        Ida.reinit st;
        TmConst(Ast.ConstUnit)
    | Ast.DAESolverOpClose,[TmDAESolver(st,tm_yy,tm_yp)] -> 
        Ida.close st;
        TmConst(Ast.ConstUnit)
    | Ast.DAESolverOpRoots,[TmDAESolver(st,tm_yy,tm_yp)] -> 
        TmArray(Array.map (fun x -> TmConst(Ast.ConstInt(x)))(Ida.roots st))
  | _ -> assert false (* Should be discovered by the type checker *)


(* Creates two model values from a primitive function constant. Used when
   for example build in +. operator is partially applied with one argument
   and the deconstruct operator of modapp wants to take apart the value *)
let mk_primappvalues prim arg args = 
  let c1 = Ast.ConstPrim(prim,args) in
  let ty1 = Ast.deltatype NoInfo c1 0 in
  let v1 = TmVal(TmConst(c1),trans_ty ty1) in
  let ty2 = match ty1 with Ast.TyArrow(_,_,ty,_) -> ty | _ -> assert false in
  let v2 = TmVal(TmConst(arg),trans_ty ty2) in
  (v1,v2)                  


  




let evaluate t = 
  let ukno = ref 0 in
  let gensym_uk() = incr ukno; !ukno in
  let rec eval earg t = 
    let (vd,venv,n) = earg in
      match n,t with 
        | n,TmVar(i) -> 
            (match List.nth venv i with
               | TmFix(t) as tt -> 
                   eval earg tt
               | t -> t) 
        | 0,TmSymVar(s) -> assert false
        | n,TmSymVar(s) -> TmSymVar(s)
        | n,TmLam(t) -> TmClos(t,venv)
        | n,TmClos(t,e) -> TmClos(t,e)
        | 0,TmApp(t1,t2,fs) -> 
            (match eval earg t1,eval earg t2 with
	       | (TmClos(t3,venv2),v2) -> 
                   eval (vd,(v2::venv2),0) t3  
	       | (TmConst(c1),TmConst(c2)) -> TmConst(Ast.delta c1 c2)
	       | t1',t2' -> assert false) 
        | n,TmApp(t1,t2,fs) -> TmApp(eval earg t1,eval earg t2,fs)
        | 0,TmFix(t) ->
            (match eval earg t with
               | TmClos(t2,venv2) as tt ->                  
                   eval (vd,TmFix(tt)::venv2,0) t2 
               | _ -> assert false)
        | n,TmFix(t) -> TmFix(eval earg t)  
        | 0,TmIf(t1,t2,t3) -> 
            (match eval earg t1 with
	       | TmConst(Ast.ConstBool(b)) -> 
                   eval (vd,venv,n) (if b then t2 else t3)
	       | _ -> assert false)
        | n,TmIf(t1,t2,t3) -> TmIf(eval earg t1,eval earg t2,eval earg t3) 
        | n,TmConst(b) -> TmConst(b)
        | n,TmBrack(t) -> TmBrack(eval (vd,venv,(n+1)) t)
        | 0,TmEsc(t) -> assert false
        | 1,TmEsc(t) -> (match eval (vd,venv,0) t with 
			   | TmBrack(v) -> v
			   | _ -> assert false)
        | n,TmEsc(t) -> TmEsc(eval (vd,venv,(n-1)) t)      
        | 0,TmRun(t) -> (match eval earg t with
			   | TmBrack(t2) -> eval earg t2
			   | _ -> assert false)
        | n,TmRun(t) -> TmRun(eval earg t)
        | n,TmUk(s,ty) -> TmUk(s,ty)
        | n,TmUkGen(ty) -> TmUk(gensym_uk(),ty)
        | n,TmModApp(t1,t2) -> TmModApp(eval earg t1,eval earg t2)
        | n,TmModIf(t1,t2,t3) -> TmModIf(eval earg t1,t2,t3)
        | n,TmModEqual(t1,t2) -> TmModEqual(eval earg t1,eval earg t2)
        | n,TmModProj(i,t) -> TmModProj(i,eval earg t)
        | n,TmVal(t,ty) -> TmVal(eval earg t,ty)
        | 0,TmDecon(t1,p,t2,t3) -> 
            (match eval earg t1,p with
	       | TmUk(id,ty1),MPatUk(ty2) 
		   when ty1 = ty2  ->
                   eval (vd,venv,0) t2                  
	       | TmModApp(v1,v2),MPatModApp -> 
                   eval (vd,v1::v2::venv,0) t2            
	       | TmModIf(v1,v2,v3),MPatModIfGuard ->
                   eval (vd,v1::venv,0) t2            
	       | TmModIf(v1,v2,v3),MPatModIfThen ->
                   eval (vd,v2::venv,0) t2            
	       | TmModIf(v1,v2,v3),MPatModIfElse ->
                   eval (vd,v3::venv,0) t2            
	       | TmModEqual(v1,v2),MPatModEqual -> 
                   eval (vd,v1::v2::venv,0) t2            
	       | TmModProj(i,v2),MPatModProj -> 
		   let v1 = TmConst(Ast.ConstInt(i)) in
                     eval (vd,v1::v2::venv,0) t2            
	       | TmVal(v1,ty1),MPatVal(ty2) when ty1 = ty2 ->	
                   eval (vd,v1::venv,0) t2            
               | TmVal(TmConst(Ast.ConstPrim(prim,arg::args)),ty1),MPatModApp ->
                   let (v1,v2) = mk_primappvalues prim arg args in
                   eval (vd,v1::v2::venv,0) t2 
               | _ -> eval (vd,venv,n) t3)
        | n,TmDecon(t1,p,t2,t3) -> 
            TmDecon(eval earg t1,p,eval earg t2,eval earg t3)
        | 0,TmEqual(t1,t2) -> 
            TmConst(Ast.ConstBool(tm_equiv (eval earg t1) 
                                       (eval earg t2)))
        | n,TmEqual(t1,t2) -> TmEqual(eval earg t1,eval earg t2)
        | 0,TmLcase(t,t1,t2) ->           
            (match eval earg t with
               | TmCons(v1,v2) -> 
                   eval (vd,(v1::v2::venv),0) t1                  
	       | TmNil -> eval (vd,venv,n) t2
	       | tt -> assert false)
        | n,TmLcase(t,t1,t2) -> TmLcase(eval earg t,eval earg t1,eval earg t2) 
        | n,TmCons(t1,t2) -> TmCons(eval earg t1,eval earg t2) 
        | n,TmNil -> TmNil
        | n,TmTuple(tms) -> TmTuple(List.map (eval earg) tms)
        | 0,TmProj(i,t) -> 
            (match eval earg t with
	       | TmTuple(tms) -> List.nth tms i
	       | _ -> assert false)
        | n,TmProj(i,t) -> TmProj(i,eval earg t)
        | n,TmArray(tms) -> TmArray(Array.map (eval earg) tms)     
        | 0,TmArrayOp(op,tms) -> eval_array_op op (List.map (eval earg) tms)
        | n,TmArrayOp(op,tms) -> TmArrayOp(op,List.map (eval earg) tms)
        | n,TmMap(size,tms) -> TmMap(size,tms)
        | 0,TmMapOp(op,tms) -> eval_map_op op (List.map (eval earg) tms)
        | n,TmMapOp(op,tms) -> TmMapOp(op,List.map (eval earg) tms)
        | n,TmSet(size,tms) -> TmSet(size,tms)
        | 0,TmSetOp(op,tms) -> eval_set_op op (List.map (eval earg) tms)
        | n,TmSetOp(op,tms) -> TmSetOp(op,List.map (eval earg) tms)
        | n,TmDAESolver(st,yy,yp) -> TmDAESolver(st,yy,yp)
        | 0,TmDAESolverOp(op,tms) -> 
            eval_daesolver_op (eval earg) op (List.map (eval earg) tms)
        | n,TmDAESolverOp(op,tms) -> TmDAESolverOp(op,List.map (eval earg) tms)
        | n,TmDPrint(t) -> let t' = eval earg t  in 
	    pprint t' |> uprint_endline; t'
        | n,TmDPrintType(t) -> pprint t |> uprint_endline; eval earg t
        | 0,TmError(fi,t) ->  
	    (match eval earg t with
	     | TmConst(Ast.ConstString(s)) ->
		 raise (Ast.Mkl_runtime_error (Message.RUNTIME_ERROR,
						  Message.ERROR, fi, [s])) 
	     | _ -> assert false)
        | n,TmError(fi,t) -> TmError(fi,eval earg t)
        | n,TmDebugId(id,t) -> 
            let t'= eval earg t in
            debugTagTm id t'
  in
    eval (0,[],0) t
    


