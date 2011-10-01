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
open Ast
open Message
open Info
open Printf
exception Mkl_type_error of Message.message

let i2s = ustring_of_int
type strip = 
  | StripMetaup   of info * level
  | StripMetadown of info * level
  | StripNo

type tyenv = (ident * (level * ty * strip))

let rec tylst_equiv tylst1 tylst2 = List.combine tylst1 tylst2 
   |> List.fold_left (fun inp (ty1,ty2) -> ty_equiv ty1 ty2) true

let rec ty_constlev ty = 
  match ty with 
    | TyBool(_,l) -> true
    | TyInt(_,l) -> true
    | TyReal(_,l) -> true
    | TyString(_,l) -> true
    | TyArrow(_,l,t1,t2) -> 
	ty_constlev t1 && ty_constlev t2 && ty_lev t1 = l && ty_lev t2 = l
    | TyUnit(_,l) -> true
    | TyList(_,l,t) -> ty_constlev t && ty_lev t == l
    | TyTuple(_,l,tylst) -> List.for_all 
        (fun t -> ty_constlev t && ty_lev t == l) tylst
    | TyModel(_,l,t) -> ty_constlev t && ty_lev t == l
    | TyAnyModel(_,l) -> true
    | TyBot(_,l) -> true
    | TyUserdef(_,l,tyid,id) -> true
    | TyIdent(fi,l1,id1) -> assert false 
    | TyArray(_,l,t) -> ty_constlev t && ty_lev t = l  
    | TyMap(_,l,t1,t2) -> 
	ty_constlev t1 && ty_constlev t2 && ty_lev t1 == l && ty_lev t2 == l
    | TySet(_,l,t) -> ty_constlev t && ty_lev t = l  
    | TyDAESolver(_,l) -> true

let rec ty_mono ty = 
  match ty with 
    | TyBool(_,l) -> true
    | TyInt(_,l) -> true
    | TyReal(_,l) -> true
    | TyString(_,l) -> true
    | TyArrow(_,l,t1,t2) -> 
	ty_mono t1 && ty_mono t2 && ty_lev t1 >= l && ty_lev t2 >= l
    | TyUnit(_,l) -> true
    | TyList(_,l,t) -> ty_mono t && ty_lev t >= l
    | TyTuple(_,l,tylst) -> List.for_all 
        (fun t -> ty_mono t && ty_lev t >= l) tylst
    | TyModel(_,l,t) -> ty_mono t && ty_lev t >= l
    | TyAnyModel(_,l) -> true
    | TyBot(_,l) -> true
    | TyUserdef(_,l,tyid,id) -> true
    | TyIdent(fi,l1,id1) -> assert false 
    | TyArray(_,l,t) -> ty_constlev t && ty_lev t = l  (* Note! Equal *)
    | TyMap(_,l,t1,t2) -> 
	ty_mono t1 && ty_mono t2 && ty_lev t1 >= l && ty_lev t2 >= l
    | TySet(_,l,t) -> ty_mono t && ty_lev t = l  (* Note! Equal *)
    | TyDAESolver(_,l) -> true

(* Greatest level *)
let rec ty_gl ty = 
  match ty with 
    | TyBool(_,l) -> l
    | TyInt(_,l) -> l
    | TyReal(_,l) -> l
    | TyString(_,l) -> l
    | TyArrow(_,l,t1,t2) -> max l (max (ty_gl t1) (ty_gl t2))
    | TyUnit(_,l) -> l
    | TyList(_,l,t) -> max l (ty_gl t)
    | TyTuple(_,l,tylst) -> List.fold_left (fun l t -> max l (ty_gl t)) l tylst
    | TyModel(_,l,t) -> max l (ty_gl t)
    | TyAnyModel(_,l) -> l
    | TyBot(_,l) -> l
    | TyUserdef(_,l,tyid,id) -> l
    | TyIdent(fi,l1,id1) -> l1
    | TyArray(_,l,t) -> max l (ty_gl t)
    | TyMap(_,l,t1,t2) -> max l (max (ty_gl t1) (ty_gl t2))
    | TySet(_,l,t) -> max l (ty_gl t)
    | TyDAESolver(_,l) -> l

let rec ty_up ty = 
  match ty with 
    | TyBool(fi,l) -> TyBool(fi,l+1)
    | TyInt(fi,l) -> TyInt(fi,l+1)
    | TyReal(fi,l) -> TyReal(fi,l+1)
    | TyString(fi,l) -> TyString(fi,l+1)
    | TyArrow(fi,l,t1,t2) -> TyArrow(fi,l+1,ty_up t1,ty_up t2) 
    | TyUnit(fi,l) -> TyUnit(fi,l+1)
    | TyList(fi,l,t) -> TyList(fi,l+1,ty_up t)
    | TyTuple(fi,l,tylst) -> TyTuple(fi,l+1,List.map ty_up tylst)  
    | TyModel(fi,l,t) -> TyModel(fi,l+1,ty_up t)
    | TyAnyModel(fi,l) -> TyAnyModel(fi,l+1)
    | TyBot(fi,l) -> TyBot(fi,l+1)
    | TyUserdef(fi,l,tyid,id) -> TyUserdef(fi,l+1,tyid,id)
    | TyIdent(fi,l,id) -> TyIdent(fi,l+1,id)
    | TyArray(fi,l,t) -> TyArray(fi,l+1,ty_up t)
    | TyMap(fi,l,t1,t2) -> TyMap(fi,l+1,ty_up t1,ty_up t2) 
    | TySet(fi,l,t) -> TySet(fi,l+1,ty_up t)
    | TyDAESolver(fi,l) -> TyDAESolver(fi,l+1)

let rec ty_down ty = 
  match ty with 
    | TyBool(fi,l) -> TyBool(fi,l-1)
    | TyInt(fi,l) -> TyInt(fi,l-1)
    | TyReal(fi,l) -> TyReal(fi,l-1)
    | TyString(fi,l) -> TyString(fi,l-1)
    | TyArrow(fi,l,t1,t2) -> TyArrow(fi,l-1,ty_down t1,ty_down t2) 
    | TyUnit(fi,l) -> TyUnit(fi,l-1)
    | TyList(fi,l,t) -> TyList(fi,l-1,ty_down t)
    | TyTuple(fi,l,tylst) -> TyTuple(fi,l-1,List.map ty_down tylst)
    | TyModel(fi,l,t) -> TyModel(fi,l-1,ty_down t)
    | TyAnyModel(fi,l) -> TyAnyModel(fi,l-1)
    | TyBot(fi,l) -> TyBot(fi,l-1)
    | TyUserdef(fi,l,tyid,id) -> TyUserdef(fi,l-1,tyid,id)
    | TyIdent(fi,l,id) -> TyIdent(fi,l-1,id)
    | TyArray(fi,l,t) -> TyArray(fi,l-1,ty_down t)
    | TyMap(fi,l,t1,t2) -> TyMap(fi,l-1,ty_down t1,ty_down t2) 
    | TySet(fi,l,t) -> TySet(fi,l-1,ty_down t)
    | TyDAESolver(fi,l) -> TyDAESolver(fi,l-1)

let rec ty_lev_up l ty = 
  if l = 0 then ty else ty_lev_up (l-1) (ty_up ty)

let rec ty_lev_down l ty = 
  if l = 0 then ty else ty_lev_down (l-1) (ty_down ty)
    
let rec strip env fi l up = 
  match env with
    | (x,(k,ty,s))::res ->  
  	(if k <= l then (x,(k,ty,s))
	 else if up then (x,(k,ty,StripMetaup(fi,l))) 
	 else (x,(k,ty,StripMetadown(fi,l)))
        )::strip res fi l up
    | [] -> []

	
let rec mk_letenv plst l env =
  match plst with
    | [] -> env
    | (x,ty)::res -> (x,(l,ty,StripNo))::(mk_letenv res l env)


let rec remove_dup_assoc l =
  match l with
    | (k,v)::ls -> (k,v)::(remove_dup_assoc (List.remove_assoc k ls))
    | [] -> []


let ty_ismodel ty = 
  match ty with
    | TyModel(_,_,_) -> true
    | TyAnyModel(_,_) -> true
    | _ -> false

let rel_union rl1 rl2 = remove_dup_assoc (rl1 @ rl2)
            

let desugar_bracket_escape tm =
  let rec ds_ty lev ty = 
    match ty with 
      | TyBool(fi,l) -> TyBool(fi,l+lev)
      | TyInt(fi,l) -> TyInt(fi,l+lev)
      | TyReal(fi,l) -> TyReal(fi,l+lev)
      | TyString(fi,l) -> TyString(fi,l+lev)
      | TyArrow(fi,l,ty1,ty2) -> TyArrow(fi,l+lev,ds_ty lev ty1,ds_ty lev ty2)
      | TyUnit(fi,l) -> TyUnit(fi,l+lev) 
      | TyList(fi,l,t) -> TyList(fi,l+lev,ds_ty lev t)  
      | TyTuple(fi,l,t) -> TyTuple(fi,l+lev,List.map (ds_ty lev) t)
      | TyModel(fi,l,t) -> TyModel(fi,l+lev,ds_ty lev t)
      | TyAnyModel(fi,l) -> TyAnyModel(fi,l+lev)
      | TyBot(fi,l) -> TyBot(fi,l+lev)
      | TyUserdef(fi,l,tyid,id) -> TyUserdef(fi,l+lev,tyid,id)
      | TyIdent(fi,l,id) -> TyIdent(fi,l+lev,id)
      | TyArray(fi,l,t) -> TyArray(fi,l+lev,ds_ty lev t)  
      | TyMap(fi,l,ty1,ty2) -> TyMap(fi,l+lev,ds_ty lev ty1,ds_ty lev ty2)
      | TySet(fi,l,t) -> TySet(fi,l+lev,ds_ty lev t)  
      | TyDAESolver(fi,l) -> TyDAESolver(fi,l+lev)
  in  
  let rec liftlev l lev fi t = 
    if lev > l then TmUp(fi,l,liftlev l (lev-1) fi t) else t in 
  let mpat_env p l lev env = 
    match p with 
      | MPatUk(fi,ty) -> env
      | MPatModApp(fi,x1,x2) -> (x1,l+lev)::(x2,l+lev)::env    
      | MPatModIfGuard(fi,x) -> (x,l+lev)::env    
      | MPatModIfThen(fi,x) -> (x,l+lev)::env     
      | MPatModIfElse(fi,x) -> (x,l+lev)::env   
      | MPatModEqual(fi,x1,x2) -> (x1,l+lev)::(x2,l+lev)::env    
      | MPatModProj(fi,x1,x2) -> (x1,l+lev)::(x2,l+lev)::env    
      | MPatVal(fi,x,ty)  -> (x,l+lev)::env
  in
  let rec ds_pat lev env ukenv p  =
    match p with 
      | PatVar(fi,x) as tt -> tt
      | PatExpr(fi,t) -> PatExpr(fi,ds lev env ukenv t) 
      | PatUk(fi,ty) as tt -> tt 
      | PatModApp(fi,p1,p2) -> 
	  PatModApp(fi,ds_pat lev env ukenv p1,ds_pat lev env ukenv p2)
      | PatModIf(fi,p1,p2,p3) -> 
	  PatModIf(fi,ds_pat lev env ukenv p1,ds_pat lev env ukenv p2,
		   ds_pat lev env ukenv p3)
      | PatModEqual(fi,p1,p2) -> 
	  PatModEqual(fi,ds_pat lev env ukenv p1,ds_pat lev env ukenv p2)
      | PatModProj(fi,p1,p2) -> 
	  PatModProj(fi,ds_pat lev env ukenv p1,ds_pat lev env ukenv p2)
      | PatModVal(fi,x,ty) -> PatModVal(fi,x,ty)
      | PatCons(fi,p1,p2) -> 
	  PatCons(fi,ds_pat lev env ukenv p1,ds_pat lev env ukenv p2)
      | PatNil(fi) as tt -> tt
      | PatTuple(fi,ps) -> PatTuple(fi,List.map (ds_pat lev env ukenv) ps)
      | PatWildcard(fi) as tt -> tt
  and ds lev env ukenv tm =
    match tm with
      | TmVar(fi,x) as tt ->  
	  (try let l = List.assoc x env in
             liftlev l lev fi tt
	   with Not_found -> 
	     if List.mem x ukenv then tt 
	     else 
               raise (Mkl_type_error(TYPE_VAR_NOT_DEFINED,ERROR,
					fi,[Symtbl.get x])))
      | TmLam(fi,l,x,ty1,t2) -> 
	  TmLam(fi,l+lev,x,ds_ty lev ty1,ds lev ((x,l+lev)::env) ukenv t2)
      | TmApp(fi,l,t1,t2,fs) -> 
	  TmApp(fi,l+lev,ds lev env ukenv t1,ds lev env ukenv t2,fs) 
      | TmFix(fi,l,t) -> TmFix(fi,l+lev,ds lev env ukenv t)
      | TmLet(fi,l,x,ty,plst,t1,t2,recu) -> 
	  let ty' = map_option (ds_ty lev) ty in
	  let plst' = plst |> List.map (fun (id,ty) -> (id,ds_ty lev ty)) in
          let env1' = (x,l+lev)::
	    (plst |> List.map (fun (x,_) -> (x,l+lev)))@env in
	  let env2' = (x,l+lev)::env in
	  TmLet(fi,l+lev,x,ty',plst',ds lev env1' ukenv t1,
		ds lev env2' ukenv t2,recu)
      | TmIf(fi,l,t1,t2,t3) -> 
	  TmIf(fi,l+lev,ds lev env ukenv t1,ds lev env ukenv t2,
	       ds lev env ukenv t3) 
      | TmConst(fi,l,c) -> TmConst(fi,l+lev,c)
      | TmUp(fi,l,t) -> TmUp(fi,l+lev,ds lev env ukenv t) 
      | TmDown(fi,l,t) -> TmDown(fi,l+lev,ds lev env ukenv t)
      | TmBracket(fi,t) -> ds (lev+1) env ukenv t
      | TmEscape(fi,t) -> if lev > 0 then ds (lev-1) env ukenv  t else
		raise (Mkl_type_error(TYPE_DESUGAR_ESC_LEV_ZERO,ERROR,fi,[]))
      | TmList(fi,l,tms) ->
	  TmList(fi,l+lev,List.map (ds lev env ukenv) tms)
      | TmMatch(fi,l,t,pcases) -> 
	  let pcases' = pcases |> 
	      List.map (fun (PCase(fi2,ps,t1_op,mlst,t2)) ->
			  let freevar  = ps |> List.map fpv_pat |> 
			      List.fold_left VarSet.union VarSet.empty in 
			  let env' =    
			      VarSet.fold (fun x e -> (x,l+lev)::e) 
                              freevar env in
			  PCase(fi2,List.map (ds_pat lev env ukenv) ps,
				map_option (ds lev env' ukenv) t1_op, mlst,
				ds lev env' ukenv t2)) in
          TmMatch(fi,l+lev,ds lev env  ukenv t,pcases')
      | TmUk(fi,l,x,ty) -> TmUk(fi,l+lev,x,ds_ty lev ty) 
      | TmNu(fi,l,x,ty,t) -> 
           TmNu(fi,l+lev,x,ds_ty lev ty,ds lev env (x::ukenv) t)
      | TmModApp(fi,l,t1,t2) ->
	  TmModApp(fi,l+lev,ds lev env ukenv  t1,ds lev env ukenv t2)
      | TmModIf(fi,l,t1,t2,t3) -> 
	  TmModIf(fi,l+lev,ds lev env ukenv t1,ds lev env ukenv t2,
	       ds lev env ukenv t3) 
      | TmModEqual(fi,l,t1,t2) -> TmModEqual(fi,l+lev,ds lev env ukenv t1,
				       ds lev env ukenv t2)
      | TmModProj(fi,l,i,t) -> TmModProj(fi,l+lev,i,ds lev env ukenv t) 
      | TmVal(fi,l,t,ty) -> TmVal(fi,l+lev,ds lev env ukenv t,ds_ty lev ty)
      | TmDecon(fi,l,t1,p,t2,t3) -> let env' = mpat_env p l lev env in 
	  TmDecon(fi,l+lev,ds lev env ukenv t1,p,ds lev env' ukenv t2,
		  ds lev env' ukenv t3)
      | TmEqual(fi,l,t1,t2) -> TmEqual(fi,l+lev,ds lev env ukenv t1,
				       ds lev env ukenv t2)
      | TmLcase(fi,l,t,id1,id2,t1,t2) -> 
	  TmLcase(fi,l+lev,ds lev env ukenv t,id1,id2,ds lev  
		    ((id1,l+lev)::(id2,l+lev)::env) ukenv t1,
                                   ds lev env ukenv t2)
      | TmCons(fi,l,t1,t2) -> TmCons(fi,l+lev,ds lev env ukenv t1,
				     ds lev env ukenv t2)
      | TmTuple(fi,l,ts) -> TmTuple(fi,l+lev,List.map (ds lev env ukenv) ts)
      | TmProj(fi,l,i,t) -> TmProj(fi,l+lev,i,ds lev env ukenv t) 
      | TmNil(fi,l,ty) -> TmNil(fi,l+lev,ds_ty lev ty)
      | TmArray(fi,l,ts) -> TmArray(fi,l+lev,Array.map (ds lev env ukenv) ts)
      | TmArrayOp(fi,l,op,ts) -> 
          TmArrayOp(fi,l+lev,op,List.map (ds lev env ukenv) ts)
      | TmMapOp(fi,l,op,ts) -> 
          TmMapOp(fi,l+lev,op,List.map (ds lev env ukenv) ts)
      | TmSetOp(fi,l,op,ts) -> 
          TmSetOp(fi,l+lev,op,List.map (ds lev env ukenv) ts)
      | TmDAESolverOp(fi,l,op,ts) -> 
          TmDAESolverOp(fi,l+lev,op,List.map (ds lev env ukenv) ts)
      | TmDpa(t) -> TmDpa(ds lev env ukenv t)
      | TmDpb(t) -> TmDpb(ds lev env ukenv t)
      | TmError(fi,l,t) -> TmError(fi,l+lev,ds lev env ukenv t)
  in ds 0 [] [] tm


let mk_tymodel ty =
  TyModel(ty_info ty,ty_lev ty,ty)


	
let check_istype_array fi l ty_ar =
  match ty_ar with
    | TyArray(_,l',ty) when l = l' -> ty
    | _ -> raise (Mkl_type_error(TYPE_EXPECTED_ARRAY_TYPE,ERROR,fi,
		    [pprint_ty ty_ar; ustring_of_int l]))

let check_istype_map fi l ty_ma =
  match ty_ma with
    | TyMap(_,l',ty1,ty2) when l = l' -> (ty1,ty2)
    | _ -> raise (Mkl_type_error(TYPE_EXPECTED_MAP_TYPE,ERROR,fi,
		    [pprint_ty ty_ma; ustring_of_int l]))

let check_istype_set fi l ty_set =
  match ty_set with
    | TySet(_,l',ty) when l = l' -> ty
    | _ -> raise (Mkl_type_error(TYPE_EXPECTED_SET_TYPE,ERROR,fi,
		    [pprint_ty ty_set; ustring_of_int l]))

let check_istype_daesolver fi l ty_daesolver =
  match ty_daesolver with
    | TyDAESolver(_,l') when l = l' -> ()
    | _ -> raise (Mkl_type_error(TYPE_EXPECTED_DAESOLVER_TYPE,ERROR,fi,
		    [pprint_ty ty_daesolver; ustring_of_int l]))
        
let check_istype_int fi l ty_int =
  match ty_int with 
    | TyInt(_,l') when l = l' -> ()
    | _ -> raise (Mkl_type_error(TYPE_EXPECTED_INT_TYPE,ERROR,fi,
		    [pprint_ty ty_int; ustring_of_int l]))

let check_istype_real fi l ty_real =
  match ty_real with 
    | TyReal(_,l') when l = l' -> ()
    | _ -> raise (Mkl_type_error(TYPE_EXPECTED_REAL_TYPE,ERROR,fi,
		    [pprint_ty ty_real; ustring_of_int l]))

let check_istype_resroot fi l ty_residual =
   let tyexp = TyArrow(fi,l,TyReal(fi,l),
           TyArrow(fi,l,TyArray(fi,l,TyReal(fi,l)),
               TyArrow(fi,l,TyArray(fi,l,TyReal(fi,l)),
                       TyList(fi,l,TyReal(fi,l))))) in
   if not (ty_consistent tyexp ty_residual) then
        raise (Mkl_type_error(TYPE_EXPECTED_RESROOT_TYPE,ERROR,fi,
		    [pprint_ty tyexp; pprint_ty ty_residual]))

     

let check_type_is_constlev fi l ty_elem =
  if ty_lev ty_elem = l && ty_constlev ty_elem then ()
  else raise (Mkl_type_error(TYPE_EXPECTED_CONSTANT_LEV,ERROR,fi,
	       [pprint_ty ty_elem; ustring_of_int l]))
        
let check_arg_type_consistency fi ty' ty_elem =
  if ty_consistent ty' ty_elem then ty_restriction ty' ty_elem
  else raise (Mkl_type_error(TYPE_APP_ARG_MISMATCH,ERROR,fi,
	       [pprint_ty ty'; pprint_ty ty_elem]))


let int2real_coercion t = 
  TmApp(NoInfo,0,TmConst(NoInfo,0,ConstPrim(PrimInt2Real,[])),t,false)


let rec typeof_array_op fi l op ts env ukenv =
  match op,ts with
  | ArrayOpLength,[ar] -> 
      let (ty_ar,ar') = typeof env ukenv ar in
      let _ = check_istype_array (tm_info ar) l ty_ar in
      (TyInt(fi,l),[ar'])
  | ArrayOpMake,[len;elem] ->  
      let (ty_len,len') = typeof env ukenv len in
      let (ty_elem,elem') = typeof env ukenv elem in
      check_istype_int (tm_info len)l ty_len;
      check_type_is_constlev (tm_info elem)l ty_elem;
      (TyArray(fi,l,ty_elem),[len';elem'])
  | ArrayOpGet,[ar;pos] -> 
      let (ty_ar,ar') = typeof env ukenv ar in
      let (ty_pos,pos') = typeof env ukenv pos in
      let ty' = check_istype_array (tm_info ar) l ty_ar in
      check_istype_int (tm_info pos) l ty_pos;
      (ty',[ar';pos'])
  | ArrayOpSet,[ar;pos;elem] ->  
      let (ty_ar,ar') = typeof env ukenv ar in
      let (ty_pos,pos') = typeof env ukenv pos in
      let (ty_elem,elem') = typeof env ukenv elem in
      let ty' = check_istype_array (tm_info ar) l ty_ar in
      check_istype_int (tm_info pos) l ty_pos;
      check_type_is_constlev (tm_info elem) l ty_elem;
      let _ = check_arg_type_consistency (tm_info elem) ty' ty_elem in
      (TyUnit(fi,l),[ar';pos';elem'])
  | _ -> raise (Mkl_type_error
	       (TYPE_UNEXPECTED_NO_ARGS,ERROR,fi,
                [ustring_of_int (List.length ts)]))

and typeof_map_op fi l op ts env ukenv =
  match op,ts with
  | MapOpSize,[ma] -> 
      let (ty_ma,ma') = typeof env ukenv ma in
      let _ = check_istype_map (tm_info ma) l ty_ma in
      (TyInt(fi,l),[ma'])
  | MapOpEmpty,[] -> 
      (TyMap(fi,l,TyBot(fi,l),TyBot(fi,l)),[])
  | MapOpAdd,[key;value;ma] ->
      let (ty_key,key') = typeof env ukenv key in
      let (ty_value,value') = typeof env ukenv value in
      let (ty_ma,ma') = typeof env ukenv ma in
      let (ty_ma1,ty_ma2) = check_istype_map (tm_info ma) l ty_ma in
      let ty_key' = check_arg_type_consistency (tm_info key) ty_key ty_ma1 in
      let ty_value' = 
        check_arg_type_consistency (tm_info value) ty_value ty_ma2 in
      (TyMap(fi,l,ty_key',ty_value'),[key';value';ma'])
  | MapOpFind,[key;ma] ->
      let (ty_key,key') = typeof env ukenv key in
      let (ty_ma,ma') = typeof env ukenv ma in
      let (ty_ma1,ty_ma2) = check_istype_map (tm_info ma) l ty_ma in
      let _ = check_arg_type_consistency (tm_info key) ty_key ty_ma1 in
      (ty_ma2,[key';ma'])
  | MapOpMem,[key;ma] ->
      let (ty_key,key') = typeof env ukenv key in
      let (ty_ma,ma') = typeof env ukenv ma in
      let (ty_ma1,ty_ma2) = check_istype_map (tm_info ma) l ty_ma in
      let _ = check_arg_type_consistency (tm_info key) ty_key ty_ma1 in
      (TyBool(fi,l),[key';ma'])
  | MapOpRemove,[key;ma] ->
      let (ty_key,key') = typeof env ukenv key in
      let (ty_ma,ma') = typeof env ukenv ma in
      let (ty_ma1,ty_ma2) = check_istype_map (tm_info ma) l ty_ma in
      let ty_key' = check_arg_type_consistency (tm_info key) ty_key ty_ma1 in
      (TyMap(fi,l,ty_key',ty_ma2),[key';ma'])
  | MapOpToList,[ma] ->
      let (ty_ma,ma') = typeof env ukenv ma in
      let (ty_ma1,ty_ma2) = check_istype_map (tm_info ma) l ty_ma in
      let ty_lst = TyList(fi,l,TyTuple(fi,l,[ty_ma1;ty_ma2])) in
      (ty_lst,[ma'])
  | _ -> raise (Mkl_type_error
	       (TYPE_UNEXPECTED_NO_ARGS,ERROR,fi,
                [ustring_of_int (List.length ts)]))


and typeof_set_op fi l op ts env ukenv =
  match op,ts with
  | SetOpSize,[set] -> 
      let (ty_set,set') = typeof env ukenv set in
      let _ = check_istype_set (tm_info set) l ty_set in
      (TyInt(fi,l),[set'])
  | SetOpEmpty,[] -> 
      (TySet(fi,l,TyBot(fi,l)),[])
  | SetOpAdd,[key;set] ->
      let (ty_key,key') = typeof env ukenv key in
      let (ty_set,set') = typeof env ukenv set in
      let ty_setkey = check_istype_set (tm_info set) l ty_set in
      let ty_key' = check_arg_type_consistency (tm_info set) ty_key ty_setkey in
      (TySet(fi,l,ty_key'),[key';set'])
  | SetOpMem,[key;set] ->
      let (ty_key,key') = typeof env ukenv key in
      let (ty_set,set') = typeof env ukenv set in
      let ty_setkey = check_istype_set (tm_info set) l ty_set in
      let _ = check_arg_type_consistency (tm_info key) ty_key ty_setkey in
      (TyBool(fi,l),[key';set'])
  | SetOpRemove,[key;set] ->
      let (ty_key,key') = typeof env ukenv key in
      let (ty_set,set') = typeof env ukenv set in
      let ty_setkey = check_istype_set (tm_info set) l ty_set in
      let ty_key' = check_arg_type_consistency (tm_info set) ty_key ty_setkey in
      (TySet(fi,l,ty_key'),[key';set'])
  | SetOpToList,[set] ->
      let (ty_set,set') = typeof env ukenv set in
      let ty_setkey = check_istype_set (tm_info set) l ty_set in
      let ty_lst = TyList(fi,l,ty_setkey) in
      (ty_lst,[set'])
  | _ -> raise (Mkl_type_error
	       (TYPE_UNEXPECTED_NO_ARGS,ERROR,fi,
                [ustring_of_int (List.length ts)]))

and typeof_daesolver_op fi l op ts env ukenv = 
  match op,ts with
  | DAESolverOpMake,[ar_yy;ar_yp;ar_id;tmres] ->
      let (ty_ar_yy,ar_yy') = typeof env ukenv ar_yy in
      let (ty_ar_yp,ar_yp') = typeof env ukenv ar_yp in
      let (ty_ar_id,ar_id') = typeof env ukenv ar_id in
      let (ty_tmres,tmres') = typeof env ukenv tmres in
      let ty_yy' = check_istype_array (tm_info ar_yy) l ty_ar_yy in
      let ty_yp' = check_istype_array (tm_info ar_yp) l ty_ar_yp in
      let ty_id' = check_istype_array (tm_info ar_id) l ty_ar_id in
      check_istype_real (tm_info ar_yy) l ty_yy';
      check_istype_real (tm_info ar_yp) l ty_yp';
      check_istype_real (tm_info ar_id) l ty_id';
      check_istype_resroot (tm_info tmres) l ty_tmres;
      (TyDAESolver(fi,l),[ar_yy';ar_yp';ar_id';tmres'])

  | DAESolverOpMakeHybrid,[time;ar_yy;ar_yp;ar_id;tmres;tmroot] ->
      let (ty_time,time') = typeof env ukenv time in
      let (ty_ar_yy,ar_yy') = typeof env ukenv ar_yy in
      let (ty_ar_yp,ar_yp') = typeof env ukenv ar_yp in
      let (ty_ar_id,ar_id') = typeof env ukenv ar_id in
      let (ty_tmres,tmres') = typeof env ukenv tmres in
      let (ty_tmroot,tmroot') = typeof env ukenv tmroot in
      let ty_yy' = check_istype_array (tm_info ar_yy) l ty_ar_yy in
      let ty_yp' = check_istype_array (tm_info ar_yp) l ty_ar_yp in
      let ty_id' = check_istype_array (tm_info ar_id) l ty_ar_id in
      check_istype_real (tm_info time) l ty_time;
      check_istype_real (tm_info ar_yy) l ty_yy';
      check_istype_real (tm_info ar_yp) l ty_yp';
      check_istype_real (tm_info ar_id) l ty_id';
      check_istype_resroot (tm_info tmres) l ty_tmres;
      check_istype_resroot (tm_info tmroot) l ty_tmroot;
      (TyDAESolver(fi,l),[time';ar_yy';ar_yp';ar_id';tmres';tmroot'])

  | DAESolverOpStep,[time;sun] ->
      let (ty_time,time') = typeof env ukenv time in
      check_istype_real (tm_info time) l ty_time;
      let (ty_sun,sun') = typeof env ukenv sun in
      check_istype_daesolver (tm_info sun) l ty_sun;
      (ty_time,[time';sun'])

  | DAESolverOpReinit,[sun] ->
      let (ty_sun,sun') = typeof env ukenv sun in
      check_istype_daesolver (tm_info sun) l ty_sun;
      (TyUnit(NoInfo,l),[sun'])

  | DAESolverOpClose,[sun] ->
      let (ty_sun,sun') = typeof env ukenv sun in
      check_istype_daesolver (tm_info sun) l ty_sun;
      (TyUnit(NoInfo,l),[sun'])

  | DAESolverOpRoots,[sun] ->
      let (ty_sun,sun') = typeof env ukenv sun in
      check_istype_daesolver (tm_info sun) l ty_sun;
      (TyArray(NoInfo,l,TyInt(NoInfo,l)),[sun'])

  | _ -> raise (Mkl_type_error
	       (TYPE_UNEXPECTED_NO_ARGS,ERROR,fi,
                [ustring_of_int (List.length ts)]))

and typeof env ukenv t =
  match t with
    | TmVar(fi,x) -> begin
	(* Is variable? *)
        try let (l1,ty1,strip) = List.assoc x env in
          begin match strip with
	    | StripMetaup(strip_fi,striplev) ->
		raise (Mkl_type_error(TYPE_META_UP_ON_FREE_VAR,ERROR,fi,
		    [Symtbl.get x; ustring_of_int l1; ustring_of_int striplev;
                     ustring_of_int (info2str_startline strip_fi)]))
	    | StripMetadown(strip_fi,striplev) ->
		raise (Mkl_type_error(TYPE_META_DOWN_ON_FREE_VAR,ERROR,fi,
		     [Symtbl.get x; ustring_of_int l1; ustring_of_int striplev;
                     ustring_of_int (info2str_startline strip_fi)]))
	    | StripNo -> (ty1,TmVar(fi,x))
          end
	with Not_found -> (
	  (* Is unknown? *)
          try let (l1,ty1) = List.assoc x ukenv in
	    (ty1,TmUk(fi,l1,x,ty1))
	  with
	      (* Variable or unknown not found... *)
	      Not_found -> 
                raise (Mkl_type_error
	                 (TYPE_VAR_NOT_DEFINED,ERROR,fi,[Symtbl.get x])))
      end
    | TmLam(fi,l,x,ty1,t2) ->
	if not (ty_mono ty1) then
	  raise (Mkl_type_error(TYPE_LAM_VAR_LEV_MONOTONICITY,ERROR,
				    ty_info ty1,[pprint_ty ty1]))
	else
          let (ty2,t2') = typeof ((x,(l,ty1,StripNo))::env) ukenv t2 in
	    if not (ty_lev ty1 >= l && ty_lev ty2 >= l) then
	      raise (Mkl_type_error(TYPE_LAM_EXP_LEV_MONOTONICITY,ERROR,fi,
			   [ustring_of_int l; pprint_ty ty1; pprint_ty ty2]))
	    else (TyArrow(NoInfo,l,ty1,ty2),TmLam(fi,l,x,ty1,t2'))
    | TmFix(fi,l,t) ->
	let (ty,t') = typeof env ukenv t in
	  begin match ty with
	    | TyArrow(fi,l1,ty1,ty2) -> 
		if not (ty_consistent ty1 ty2) then 
		  raise (Mkl_type_error(TYPE_FIX_MISMATCH,ERROR,tm_info t,
			  [pprint_ty ty1; pprint_ty ty2]))
		else (ty1,TmFix(fi,l,t'))
	    | _ -> raise (Mkl_type_error(TYPE_FIX_ERROR,ERROR,tm_info t,[]))
	  end
    | TmApp(fi,_,t1,t2,fs)  -> 
        let typeof_app fi ty1 t1' ty2 t2' = 
          begin match ty1 with 
	    | TyArrow(_,l,ty11,ty12) -> 
                (* Coercion of int to real *)
                if ty_consistent ty11 (TyReal(NoInfo,0)) &&
                   ty_consistent ty2 (TyInt(NoInfo,0)) 
                then  (ty12,TmApp(fi,l,t1',int2real_coercion t2',fs))
                else
  		  if ty_consistent ty11 ty2 
		  then (ty12,TmApp(fi,l,t1',t2',fs))
		  else 
		    (match ty2 with 
		       | TyModel(_,l3,ty2b) when ty_consistent ty11 ty2b -> 
                          (mk_tymodel ty12,TmModApp(fi,l,TmVal(fi,l,t1',ty1),t2'))
		       | TyAnyModel(_,l3) -> 
                          (mk_tymodel ty12,TmModApp(fi,l,TmVal(fi,l,t1',ty1),t2'))
		       | _ ->  raise (Mkl_type_error(TYPE_APP_ARG_MISMATCH,ERROR,
		             tm_info t2,[pprint_ty ty11; pprint_ty ty2;us"2"])))
	    | TyModel(_,l,TyArrow(_,l3,ty11,ty12)) ->
                if ty_ismodel ty2 then
                  let ty11b = TyModel(ty_info ty11,ty_lev ty11,ty11) in
		  if ty_consistent ty11b ty2 
		  then (TyModel(fi,l,ty12),TmModApp(fi,l,t1',t2'))
		  else raise (Mkl_type_error(TYPE_APP_ARG_MISMATCH,ERROR,
                              tm_info t2,[pprint_ty ty11b; pprint_ty ty2;us"3"]))
		else
                  (* Coercion of int to real *)
                  if ty_consistent ty11 (TyReal(NoInfo,0)) &&
                     ty_consistent ty2 (TyInt(NoInfo,0)) 
                  then (mk_tymodel ty12,
                       TmModApp(fi,l,t1',TmVal(ty_info ty2,ty_lev ty2,
                                        int2real_coercion t2',TyReal(NoInfo,0))))
                  else
		    if ty_consistent ty11 ty2 then 
                      (mk_tymodel ty12,
                       TmModApp(fi,l,t1',TmVal(ty_info ty2,ty_lev ty2,t2',ty2))) 
		    else raise (Mkl_type_error(TYPE_APP_ARG_MISMATCH,ERROR,
                              tm_info t2,[pprint_ty ty11; pprint_ty ty2;us"4"]))
	    | TyAnyModel(fi,l)  ->
		if ty_consistent ty1 ty2 then
		  (TyAnyModel(fi,l),TmModApp(fi,l,t1',t2'))
		else
                  (TyAnyModel(fi,l),TmModApp(fi,l,t1',
                                    TmVal(ty_info ty2,ty_lev ty2,t2',ty2)))
	    | _ -> raise (Mkl_type_error(TYPE_APP_NO_FUNC_TYPE,ERROR,tm_info t1,
					   [pprint_ty ty1]))
	  end
        in
               let (ty1,t1') = typeof env ukenv t1 in
               let (ty2,t2') = typeof env ukenv t2 in
                 typeof_app fi ty1 t1' ty2 t2'  (* )  *)
    | TmLet(fi,l,x,ty,plst,t1,t2,recu) ->
	plst |> List.iter (fun (x,ty) -> if ty_mono ty then () else
		    raise (Mkl_type_error(TYPE_LET_PARAM_LEV_MONOTONICITY,ERROR,
					  ty_info ty, [pprint_ty ty])));
	plst |> List.iter (fun (x,ty) -> if ty_lev ty >= l then () else
		           raise (Mkl_type_error(TYPE_LET_PARAM_LEV_LOWER,ERROR,
			   ty_info ty, [ustring_of_int l;pprint_ty ty])));
	let t1_env = mk_letenv plst l env in
	let (ty1,t1') = 
          begin match (ty,recu) with
	  | (None,true) ->
	      raise (Mkl_type_error(TYPE_LET_REC_MISS_RET_TYPE,
					 ERROR,fi,[]))
	  | (Some ty1def,recu) -> 
	      if not (ty_mono ty1def) then raise (Mkl_type_error(
	                      TYPE_LET_DEF_MONOTONICITY,ERROR,ty_info ty1def,
						       [pprint_ty ty1def]))
	      else if not (ty_lev ty1def >= l) then 
		raise (Mkl_type_error(TYPE_LET_DEF_LOWER,ERROR,ty_info ty1def,
					 [ustring_of_int l;pprint_ty ty1def]))
	      else 		   
		let tyvar = Ast.mk_lettype plst l ty1def in
		let (ty1,t1') = 
		  if recu then 
                    typeof ((x,(l,tyvar,StripNo))::t1_env) ukenv t1
		  else typeof t1_env ukenv t1 in
		  if not (ty_consistent ty1 ty1def) then
		    raise (Mkl_type_error(TYPE_LET_TYPE_DEF_MISMATCH,
			ERROR,fi,[pprint_ty ty1; pprint_ty ty1def]))
		  else (ty1,t1')
	  | (None,false) -> typeof t1_env ukenv t1
	end in
	  if not (ty_lev ty1 >= l) then 
	    raise (Mkl_type_error(TYPE_LET_TM1_LOWER,ERROR,tm_info t1,
				     [ustring_of_int l;pprint_ty ty1]))
	     else 		   
	       let tyvar = Ast.mk_lettype plst l ty1 in
	       let (ty2,t2') = typeof ((x,(l,tyvar,StripNo))::env)
                 ukenv t2 in
		 if not (ty_lev ty2 >= l) then 
		   raise (Mkl_type_error(TYPE_LET_TM2_LOWER,ERROR,tm_info t2,
				     [ustring_of_int l;pprint_ty ty2]))
		 else
		   (ty2,TmLet(fi,l,x,Some ty1,plst,t1',t2',recu)) 
    | TmIf(fi,l,t1,t2,t3) -> 
        let ((ty1,t1'),(ty2,t2'),(ty3,t3')) = 
	  (typeof env ukenv t1,typeof env ukenv t2,
           typeof env ukenv t3) in
	  (match ty1 with
	     | TyModel(_,_l2,TyBool(_,l3)) -> 
		 if ty_ismodel ty2 && ty_ismodel ty3 then
		   typeof env ukenv (TmModIf(fi,l,t1,t2,t3))
	         else if ty_ismodel ty2 then
		   typeof env ukenv 
                     (TmModIf(fi,l,t1,t2,TmVal(fi,l,t3,ty3)))
	         else if ty_ismodel ty3 then 
		   typeof env ukenv 
                     (TmModIf(fi,l,t1,TmVal(fi,l,t2,ty2),t3))
		 else
		   typeof env ukenv 
                     (TmModIf(fi,l,t1,TmVal(fi,l,t2,ty2),TmVal(fi,l,t3,ty3)))
	     | _ -> (
		 if not (ty_consistent ty1 (TyBool(NoInfo,l))) then
		  raise (Mkl_type_error(TYPE_MISMATCH_IF_GUARD,ERROR,tm_info t1,
			[pprint_ty (TyBool(NoInfo,l)); pprint_ty ty1]))
		 else if not (ty_lev ty2 >= l && ty_lev ty3 >= l) then
		   raise (Mkl_type_error(TYPE_IF_EXP_LEV_MONOTONICITY,ERROR,fi,
			 [ustring_of_int l; pprint_ty ty2; pprint_ty ty3]))
		 else
		   if not (ty_consistent ty2 ty3) then
		     raise (Mkl_type_error(TYPE_IF_EXP_DIFF_TYPE,ERROR,fi,
				           [pprint_ty ty2; pprint_ty ty3]))
		   else
		     (ty_restriction ty2 ty3,TmIf(fi,l,t1',t2',t3'))))
    | TmConst(fi,l,c) as tt -> (deltatype fi c l,tt)
    | TmUp(fi,l,t) -> 
	let (ty1,t') = typeof (strip env fi l true) ukenv t in
	  if not (ty_lev ty1 >= l) then
	    raise (Mkl_type_error(TYPE_METAUP_LEV_MONOTONICITY,ERROR,fi,
			          [ustring_of_int l; pprint_ty ty1]))
	  else (ty_up ty1,TmUp(fi,l,t')) 
    | TmDown(fi,l,t) -> 
	let (ty1,t') = typeof (strip env fi l false) ukenv t in
	  if not (ty_lev ty1 > l) then
	    raise (Mkl_type_error(TYPE_METADOWN_LEV_STRICT_MONOTONICITY,
				  ERROR,fi,[ustring_of_int l; pprint_ty ty1]))
	  else (ty_down ty1,TmDown(fi,l,t'))
    | TmBracket(_,_) -> assert false
    | TmEscape(_,_) -> assert false
    | TmList(fi,l,ts) ->
	(match ts with
	   | [] -> assert false
	   | t::_ -> let (ty',t') = typeof env ukenv t in 
               typeof env ukenv (List.fold_left 
			 (fun a t -> TmCons(tm_info t,l,t,a)) 
		         (TmNil(fi,l,ty')) ts))
    | TmMatch(fi,l,t,cases) -> assert false
    | TmUk(fi,l,u,ty) as tt-> (ty,tt)
    | TmNu(fi,l,u,ty1,t2) ->
	if not (ty_mono ty1) then
	  raise (Mkl_type_error(TYPE_NU_LET_VAR_LEV_MONOTONICITY,ERROR,
				    ty_info ty1,[pprint_ty ty1]))
	else if not (ty_ismodel ty1) then
	  raise (Mkl_type_error(TYPE_NU_LET_NOT_MODELTYPE,ERROR,
				    ty_info ty1,[pprint_ty ty1]))
        else
          let (ty2,t2') = typeof env ((u,(l,ty1))::ukenv) t2 in
	    if not (ty_lev ty1 >= l && ty_lev ty2 >= l) then
	      raise (Mkl_type_error(TYPE_NU_LET_EXP_LEV_MONOTONICITY,ERROR,fi,
			    [ustring_of_int l; pprint_ty ty1; pprint_ty ty2]))
	    else (ty2,TmNu(fi,l,u,ty1,t2'))
    | TmModApp(fi,l,t1,t2) ->
        let ((ty1',t1'),(ty2',t2')) = 
	  (typeof env ukenv t1,typeof env ukenv t2) in
          begin match (ty1',ty2') with 
            (* (TT-MODAPP1) *)
            | (TyAnyModel(fi,l2) as any),ty2' when ty_consistent any ty2' ->
		  (TyAnyModel(fi,l2),TmModApp(fi,l,t1',t2'))
            (* (TT-MODAPP2) *)
	    | TyModel(_,l1,TyArrow(_,_,ty11,ty12)),ty2' 
                when ty_consistent (TyModel(NoInfo,l1,ty11)) ty2' ->
		  (TyModel(NoInfo,l1,ty12),TmModApp(fi,l,t1',t2'))
	    | _ -> raise (Mkl_type_error(TYPE_MODAPP_TYPE_MISMATCH,ERROR,
			   fi,[pprint_ty ty1'; pprint_ty ty2']))
	  end
    | TmModIf(fi,l,t1,t2,t3) -> 
	(match typeof env ukenv t1,typeof env ukenv t2,
           typeof env ukenv t3 with
	   |((TyModel(_,l1,ty1) as ty1'),t1'),((TyModel(_,l2,ty2) as ty2'),t2'),
             ((TyModel(_,l3,ty3) as ty3'),t3') ->
	      if not (ty_consistent ty1 (TyBool(NoInfo,l))) then
		raise (Mkl_type_error(TYPE_MISMATCH_IF_GUARD,ERROR,tm_info t1,
			[pprint_ty (TyBool(NoInfo,l)); pprint_ty ty1]))
	      else if not (ty_lev ty1' >= l && ty_lev ty2' >= l && 
			     ty_lev ty3' >= l) then
		raise (Mkl_type_error(TYPE_IF_EXP_LEV_MONOTONICITY,ERROR,fi,
			 [ustring_of_int l; pprint_ty ty2; pprint_ty ty3]))
	      else
		if not (ty_consistent ty2' ty3') then
		  raise (Mkl_type_error(TYPE_IF_EXP_DIFF_TYPE,ERROR,fi,
				        [pprint_ty ty2; pprint_ty ty3]))
		else 
		  (ty_restriction ty2' ty3',TmModIf(fi,l,t1',t2',t3'))
  	   | (ty1,t1'),(ty2,t2'),(ty3,t3') ->  raise (Mkl_type_error(
				TYPE_MODIF_NOT_CONCRETE_MODELTYPE,ERROR,fi,
				[pprint_ty ty1; pprint_ty ty2;pprint_ty ty3])))
    | TmModEqual(fi,l,t1,t2) -> 
	(match typeof env ukenv t1,typeof env ukenv t2 with
	   | ((TyModel(_,l1,ty1) as ty1'),t1'),
	     ((TyModel(_,l2,ty2) as ty2'),t2') ->
	       if not (ty_consistent ty1 ty2) then
		 raise (Mkl_type_error(TYPE_EQUAL_EXP_DIFF_TYPE,ERROR,fi,
					  [pprint_ty ty1; pprint_ty ty2]))
	       else if not (ty_lev ty1' >= l && ty_lev ty2' >= l) then
	         raise (Mkl_type_error(TYPE_EQUAL_EXP_LEV_MONOTONICITY,ERROR,fi,
			[ustring_of_int l; pprint_ty ty1; pprint_ty ty2]))
	       else  
		     (TyModel(NoInfo,l,TyBool(NoInfo,l)),
                      TmModEqual(fi,l,t1',t2'))
	   | (ty1,t1'),(ty2,t2') ->
		 raise (Mkl_type_error(TYPE_MODEQUAL_NOT_CONCRETE_MODELTYPE,
                                       ERROR,fi,
				       [pprint_ty ty1; pprint_ty ty2])))
    | TmModProj(fi,l,i,t) -> 
        (match typeof env ukenv t  with
	   | (TyModel(fi,l2,(TyTuple(fi2,l3,tys) as ty')),t')  ->
               if not (l2 >= l && l3 >= l)  then
		 raise (Mkl_type_error(TYPE_PROJ_LEV_MONOTONICITY,ERROR,
			    fi,[ustring_of_int l; pprint_ty ty']))
	       else if i >= List.length tys then
		 raise (Mkl_type_error(TYPE_PROJ_TUPLE_SIZE,ERROR,
		        fi,[ustring_of_int i; ustring_of_int (List.length tys)])) 
               else (List.nth tys i,TmModProj(fi,l,i,t'))
	   | (ty,t') -> raise (Mkl_type_error(TYPE_MODPROJ_NOT_MODELTUPLE,
				ERROR,ty_info ty,[pprint_ty ty])))
    | TmVal(fi,l,t,_) -> 
	let (ty',t') = typeof env ukenv t in
	  (TyModel(ty_info ty',ty_lev ty',ty'),TmVal(fi,l,t',ty'))
    | TmDecon(fi,l,t1,p,t2,t3) ->
        let ((ty1',t1'),(ty3',t3')) = 
	  (typeof env ukenv t1,typeof env ukenv t3) in
	  (match ty1' with
	     | TyModel(_,l,_) | TyAnyModel(_,l) -> (
		 let (ty2',t2') = 
		   (match p with
		      | MPatUk(_,TyAnyModel(_,_)) -> typeof env ukenv t2 
		      | MPatUk(_,TyModel(_,_,_)) -> typeof env ukenv t2 
		      | MPatUk(_,ty3) -> raise (Mkl_type_error
			    (TYPE_DECON_PAT_UK_NOT_MODEL_TYPE,ERROR,ty_info ty3,
			     [pprint_ty ty3]))   
		      | MPatModApp(_,x1,x2) -> 
			  let tt = TyAnyModel(NoInfo,l) in
			   typeof 
			   ((x1,(l,tt,StripNo))::(x2,(l,tt,StripNo))::env) ukenv
                              t2
		      | MPatModIfGuard(_,x) -> typeof 
			  ((x,(l,TyAnyModel(NoInfo,l),StripNo))::env) ukenv
                            t2
		      | MPatModIfThen(_,x) -> typeof 
			  ((x,(l,TyAnyModel(NoInfo,l),StripNo))::env) ukenv
                            t2
		      | MPatModIfElse(_,x) -> typeof 
			  ((x,(l,TyAnyModel(NoInfo,l),StripNo))::env) ukenv   
                            t2 
		      | MPatModEqual(_,x1,x2) -> 
			  let tt = TyAnyModel(NoInfo,l) in
			    typeof 
			  ((x1,(l,tt,StripNo))::(x2,(l,tt,StripNo))::env) ukenv
                              t2
		      | MPatModProj(_,x1,x2) -> 
			  typeof ((x1,(l,TyInt(NoInfo,l),StripNo))::
			       (x2,(l,TyAnyModel(NoInfo,l),StripNo))::env) ukenv
                                t2
		      | MPatVal(_,x,ty2) -> typeof 
			  ((x,(l,ty2,StripNo))::env) ukenv t2) 
		 in 
		   if not (ty_lev ty1' >= l && ty_lev ty2' >= l 
			   && ty_lev ty3' >= l) then
		     raise (Mkl_type_error(TYPE_DECON_LEV_MONOTONICITY,ERROR,fi,
			    [ustring_of_int l; pprint_ty ty1'; pprint_ty ty2'
			        ;pprint_ty ty3']))
		   else
		     if not (ty_consistent ty2' ty3') then
		       raise (Mkl_type_error(TYPE_DECON_MISMATCH,ERROR,
                                       fi,[pprint_ty ty2'; pprint_ty ty3']))
		     else
		       (ty_restriction ty2' ty3', TmDecon(fi,l,t1',p,t2',t3')))
	     | _ -> raise (Mkl_type_error(TYPE_DECON_TYPE_NOT_MODEL,ERROR,
					  fi,[pprint_ty ty1']))		    
	  )
    | TmEqual(fi,l,t1,t2) ->
        let ((ty1,t1'),(ty2,t2')) = 
	  (typeof env ukenv t1,typeof env ukenv t2) in
          (* (L-EQUAL1) *)
          if (not (ty_consistent (TyAnyModel(fi,l)) ty1)) &&
             (ty_consistent (TyAnyModel(fi,l)) ty2) &&
             (ty_consistent (TyModel(fi,l,ty1)) ty2) 
          then 
              (TyBool(NoInfo,l),(TmEqual(fi,l,TmVal(fi,l,t1',ty1),t2')))
          else           
          (* (L-EQUAL2) *)
          if (ty_consistent (TyAnyModel(fi,l)) ty1) &&
             (not (ty_consistent (TyAnyModel(fi,l)) ty2)) &&
             (ty_consistent ty1 (TyModel(fi,l,ty2))) 
          then 
              (TyBool(NoInfo,l),(TmEqual(fi,l,t1',TmVal(fi,l,t2',ty2))))
          else 
          (* (L-EQUAL3) *)
          if (ty_consistent ty1 ty2)  
          then
            (TyBool(NoInfo,l),TmEqual(fi,l,t1',t2'))
          else
	      raise (Mkl_type_error(TYPE_EQUAL_EXP_DIFF_TYPE,ERROR,fi,
					  [pprint_ty ty1; pprint_ty ty2]))
     | TmLcase(fi,l,t1,id1,id2,t2,t3)  ->
	if id1 == id2 then 
	  raise (Mkl_type_error(TYPE_LCASE_IDENTICAL_IDENTIFIERS,ERROR,fi,
			[Symtbl.get id1; Symtbl.get id2]))
	else 
	  begin match typeof env ukenv t1  with
	    | (TyList(fi1,l1,ty1) as ty1lst,t1') -> 
		if l <> l1 then
		  raise (Mkl_type_error(TYPE_LCASE_LEVEL_MISMATCH,ERROR,
			 tm_info t1,[ustring_of_int l; pprint_ty ty1lst]))
		else
		  let (ty2,t2') = typeof ((id2,(l,ty1lst,StripNo))::
		                   (id1,(l,ty1,StripNo))::env) ukenv t2  in
		  let (ty3,t3') = typeof env ukenv t3 in
		    if not (ty_lev ty2 >= l && ty_lev ty3 >= l) then
		      raise (Mkl_type_error(TYPE_LCASE_LEV_MONOTONICITY,ERROR,fi,
			[ustring_of_int l; pprint_ty ty2; pprint_ty ty3]))
		    else	
	              if ty_consistent ty2 ty3 then
		    (ty_restriction ty2 ty3,TmLcase(fi,l,t1',id1,id2,t2',t3'))
		  else
	            raise (Mkl_type_error(TYPE_LCASE_DIFFERENT_CASE_TYPES,
	    		   ERROR,fi,[pprint_ty ty2; pprint_ty ty3]))
	    | (ty,_) -> raise (Mkl_type_error(
				 TYPE_LCASE_MATCHING_TM_NOT_LIST_TYPE,
				 ERROR,tm_info t1,[pprint_ty ty]))
          end	     
    | TmCons(fi,l,t1,t2) -> 
	let (ty1,t1') = typeof  env ukenv t1 in
	  begin match typeof  env ukenv t2 with
	    | (TyList(fi2,l2,ty2) as ty2all, t2') ->
		if not (ty_consistent ty1 ty2) then
		  raise (Mkl_type_error(TYPE_CONS_TYPE_MISMATCH,ERROR,fi,
					[pprint_ty ty1;pprint_ty ty2]))
		else if not (l2 = l && (ty_lev ty1 >= l)) then 
		  raise (Mkl_type_error(TYPE_CONS_LEV_MONOTONICITY,ERROR,fi,
			 [ustring_of_int l; pprint_ty ty1; pprint_ty ty2all]))
		else
		      (TyList(fi2,l,ty_restriction ty1 ty2),TmCons(fi,l,t1',t2'))
	    | (ty2,t2') -> 
		raise (Mkl_type_error(TYPE_CONS_NOT_A_LIST,ERROR,tm_info t2,
				      [pprint_ty ty2]))
	  end
    | TmNil(fi,l,ty)  -> 
	if not (ty_mono ty) || not (ty_lev ty >= l) then
	  raise (Mkl_type_error(TYPE_NIL_LEV_MONOTONICITY,ERROR,fi,
		                [ustring_of_int l;pprint_ty ty]))
	else (TyList(fi,l,ty), TmNil(fi,l,ty))
    | TmTuple(fi,l,ts) -> 
	let (ty',ts') = ts |> List.map (fun t -> typeof env ukenv t ) 
                           |> List.split in
	  ty' |> List.iter (fun ty -> if not (ty_lev ty >= l) then
		    raise (Mkl_type_error(TYPE_TUPLE_LEV_MONOTONICITY,ERROR,
			ty_info ty,[ustring_of_int l; pprint_ty ty])));
	  (TyTuple(fi,l,ty'),TmTuple(fi,l,ts'))
    | TmProj(fi,l,i,t) -> 
        (match typeof env ukenv t with
	   | (TyModel(fi,l2,ty1),t') ->
	       typeof  env ukenv (TmModProj(fi,l,i,t))
           | ((TyTuple(fi2,l2,tys) as ty'),t')  -> 		   
               if not (l2 >= l) then
		 raise (Mkl_type_error(TYPE_PROJ_LEV_MONOTONICITY,ERROR,
			    fi,[ustring_of_int l; pprint_ty ty']))
	       else if i >= List.length tys then
		 raise (Mkl_type_error(TYPE_PROJ_TUPLE_SIZE,ERROR,
		      fi,[ustring_of_int i; ustring_of_int (List.length tys)])) 
               else (List.nth tys i,TmProj(fi,l,i,t'))
	   | (ty',t') -> raise (Mkl_type_error(TYPE_PROJ_NOT_TUPLE,ERROR,
			             fi,[pprint_ty ty'])))
    | TmArray(fi,l,ts) ->
	let (tys',ts') = ts |> Array.to_list 
                            |> List.map (fun t -> typeof env ukenv t ) 
                            |> List.split in
        let ty' = tys' |> List.fold_left 
          (fun aty ty -> 
             if ty_consistent aty ty then ty_restriction aty ty
             else raise (Mkl_type_error(TYPE_ARRAY_ELEM_NOT_CONSISTENT,ERROR,
	     ty_info ty,[pprint_ty aty; pprint_ty ty]))) (List.hd tys')
        in                                        
	  tys' |> List.iter (fun ty -> if not (ty_lev ty = l) then
		    raise (Mkl_type_error(TYPE_ARRAY_LEV_MONOTONICITY,ERROR,
			   ty_info ty,[ustring_of_int l; pprint_ty ty])));
	  (TyArray(fi,l,ty'),TmArray(fi,l,Array.of_list ts'))
    | TmArrayOp(fi,l,op,ts) ->
        let (ty',ts') = typeof_array_op fi l op ts env ukenv in
        (ty',TmArrayOp(fi,l,op,ts'))
    | TmMapOp(fi,l,op,ts) -> 
        let (ty',ts') = typeof_map_op fi l op ts env ukenv in
        (ty',TmMapOp(fi,l,op,ts'))
    | TmSetOp(fi,l,op,ts) -> 
        let (ty',ts') = typeof_set_op fi l op ts env ukenv in
        (ty',TmSetOp(fi,l,op,ts'))
    | TmDAESolverOp(fi,l,op,ts) -> 
        let (ty',ts') = typeof_daesolver_op fi l op ts env ukenv in
        (ty',TmDAESolverOp(fi,l,op,ts'))
    | TmDpa(t) -> let (ty,t') = typeof env ukenv t in (ty,TmDpa(t'))
    | TmDpb(t) -> let (ty,t') = typeof env ukenv t in (ty,TmDpb(t'))
    | TmError(fi,l,t) -> 
	(match typeof env ukenv t with
	   | ((TyString(fi2,l2) as ty),t') -> 
	       if not (l2 >= l) then
	       raise (Mkl_type_error(TYPE_ERROR_TERM_LEV_MONOTONICITY,ERROR,fi,
					   [ustring_of_int l;pprint_ty ty]))
	       else (TyBot(fi,l),TmError(fi,l,t'))
	   | (ty,t') -> raise (Mkl_type_error(TYPE_ERROR_TERM_NOT_STRING,ERROR,
					      ty_info ty,[pprint_ty ty])))
	    

let typeofterm t = fst (typeof [] [] t)
let typecheck t = snd (typeof [] [] t)

	 



