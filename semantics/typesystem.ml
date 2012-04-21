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

let rec ty_mono ty = true  (* should be removed *)
	
let rec mk_letenv plst l env =
  match plst with
    | [] -> env
    | (x,ty)::res -> (x,ty)::(mk_letenv res l env)

let ty_ismodel ty = 
  match ty with
    | TyModel(_,_,_) -> true
    | _ -> false

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

     
let check_arg_type_consistency fi ty' ty_elem =
  if ty_consistent ty' ty_elem then ty_restriction ty' ty_elem
  else raise (Mkl_type_error(TYPE_APP_ARG_MISMATCH,ERROR,fi,
	       [pprint_ty ty'; pprint_ty ty_elem]))


let int2real_coercion t = 
  TmApp(NoInfo,0,TmConst(NoInfo,0,ConstPrim(PrimInt2Real,[])),t,false)


let rec typeof_array_op fi l op ts env  =
  match op,ts with
  | ArrayOpLength,[ar] -> 
      let (ty_ar,ar') = typeof env  ar in
      let _ = check_istype_array (tm_info ar) l ty_ar in
      (TyInt(fi,l),[ar'])
  | ArrayOpMake,[len;elem] ->  
      let (ty_len,len') = typeof env  len in
      let (ty_elem,elem') = typeof env  elem in
      check_istype_int (tm_info len)l ty_len;
      (TyArray(fi,l,ty_elem),[len';elem'])
  | ArrayOpGet,[ar;pos] -> 
      let (ty_ar,ar') = typeof env  ar in
      let (ty_pos,pos') = typeof env  pos in
      let ty' = check_istype_array (tm_info ar) l ty_ar in
      check_istype_int (tm_info pos) l ty_pos;
      (ty',[ar';pos'])
  | ArrayOpSet,[ar;pos;elem] ->  
      let (ty_ar,ar') = typeof env  ar in
      let (ty_pos,pos') = typeof env  pos in
      let (ty_elem,elem') = typeof env  elem in
      let ty' = check_istype_array (tm_info ar) l ty_ar in
      check_istype_int (tm_info pos) l ty_pos;
      let _ = check_arg_type_consistency (tm_info elem) ty' ty_elem in
      (TyUnit(fi,l),[ar';pos';elem'])
  | _ -> raise (Mkl_type_error
	       (TYPE_UNEXPECTED_NO_ARGS,ERROR,fi,
                [ustring_of_int (List.length ts)]))

and typeof_map_op fi l op ts env  =
  match op,ts with
  | MapOpSize,[ma] -> 
      let (ty_ma,ma') = typeof env  ma in
      let _ = check_istype_map (tm_info ma) l ty_ma in
      (TyInt(fi,l),[ma'])
  | MapOpEmpty,[] -> 
      (TyMap(fi,l,TyBot(fi,l),TyBot(fi,l)),[])
  | MapOpAdd,[key;value;ma] ->
      let (ty_key,key') = typeof env  key in
      let (ty_value,value') = typeof env  value in
      let (ty_ma,ma') = typeof env  ma in
      let (ty_ma1,ty_ma2) = check_istype_map (tm_info ma) l ty_ma in
      let ty_key' = check_arg_type_consistency (tm_info key) ty_key ty_ma1 in
      let ty_value' = 
        check_arg_type_consistency (tm_info value) ty_value ty_ma2 in
      (TyMap(fi,l,ty_key',ty_value'),[key';value';ma'])
  | MapOpFind,[key;ma] ->
      let (ty_key,key') = typeof env  key in
      let (ty_ma,ma') = typeof env  ma in
      let (ty_ma1,ty_ma2) = check_istype_map (tm_info ma) l ty_ma in
      let _ = check_arg_type_consistency (tm_info key) ty_key ty_ma1 in
      (ty_ma2,[key';ma'])
  | MapOpMem,[key;ma] ->
      let (ty_key,key') = typeof env  key in
      let (ty_ma,ma') = typeof env  ma in
      let (ty_ma1,ty_ma2) = check_istype_map (tm_info ma) l ty_ma in
      let _ = check_arg_type_consistency (tm_info key) ty_key ty_ma1 in
      (TyBool(fi,l),[key';ma'])
  | MapOpRemove,[key;ma] ->
      let (ty_key,key') = typeof env  key in
      let (ty_ma,ma') = typeof env  ma in
      let (ty_ma1,ty_ma2) = check_istype_map (tm_info ma) l ty_ma in
      let ty_key' = check_arg_type_consistency (tm_info key) ty_key ty_ma1 in
      (TyMap(fi,l,ty_key',ty_ma2),[key';ma'])
  | MapOpToList,[ma] ->
      let (ty_ma,ma') = typeof env  ma in
      let (ty_ma1,ty_ma2) = check_istype_map (tm_info ma) l ty_ma in
      let ty_lst = TyList(fi,l,TyTuple(fi,l,[ty_ma1;ty_ma2])) in
      (ty_lst,[ma'])
  | _ -> raise (Mkl_type_error
	       (TYPE_UNEXPECTED_NO_ARGS,ERROR,fi,
                [ustring_of_int (List.length ts)]))


and typeof_set_op fi l op ts env  =
  match op,ts with
  | SetOpSize,[set] -> 
      let (ty_set,set') = typeof env  set in
      let _ = check_istype_set (tm_info set) l ty_set in
      (TyInt(fi,l),[set'])
  | SetOpEmpty,[] -> 
      (TySet(fi,l,TyBot(fi,l)),[])
  | SetOpAdd,[key;set] ->
      let (ty_key,key') = typeof env  key in
      let (ty_set,set') = typeof env  set in
      let ty_setkey = check_istype_set (tm_info set) l ty_set in
      let ty_key' = check_arg_type_consistency (tm_info set) ty_key ty_setkey in
      (TySet(fi,l,ty_key'),[key';set'])
  | SetOpMem,[key;set] ->
      let (ty_key,key') = typeof env  key in
      let (ty_set,set') = typeof env  set in
      let ty_setkey = check_istype_set (tm_info set) l ty_set in
      let _ = check_arg_type_consistency (tm_info key) ty_key ty_setkey in
      (TyBool(fi,l),[key';set'])
  | SetOpRemove,[key;set] ->
      let (ty_key,key') = typeof env  key in
      let (ty_set,set') = typeof env  set in
      let ty_setkey = check_istype_set (tm_info set) l ty_set in
      let ty_key' = check_arg_type_consistency (tm_info set) ty_key ty_setkey in
      (TySet(fi,l,ty_key'),[key';set'])
  | SetOpToList,[set] ->
      let (ty_set,set') = typeof env  set in
      let ty_setkey = check_istype_set (tm_info set) l ty_set in
      let ty_lst = TyList(fi,l,ty_setkey) in
      (ty_lst,[set'])
  | _ -> raise (Mkl_type_error
	       (TYPE_UNEXPECTED_NO_ARGS,ERROR,fi,
                [ustring_of_int (List.length ts)]))

and typeof_daesolver_op fi l op ts env  = 
  match op,ts with
  | DAESolverOpMake,[ar_yy;ar_yp;ar_id;tmres] ->
      let (ty_ar_yy,ar_yy') = typeof env  ar_yy in
      let (ty_ar_yp,ar_yp') = typeof env  ar_yp in
      let (ty_ar_id,ar_id') = typeof env  ar_id in
      let (ty_tmres,tmres') = typeof env  tmres in
      let ty_yy' = check_istype_array (tm_info ar_yy) l ty_ar_yy in
      let ty_yp' = check_istype_array (tm_info ar_yp) l ty_ar_yp in
      let ty_id' = check_istype_array (tm_info ar_id) l ty_ar_id in
      check_istype_real (tm_info ar_yy) l ty_yy';
      check_istype_real (tm_info ar_yp) l ty_yp';
      check_istype_real (tm_info ar_id) l ty_id';
      check_istype_resroot (tm_info tmres) l ty_tmres;
      (TyDAESolver(fi,l),[ar_yy';ar_yp';ar_id';tmres'])

  | DAESolverOpMakeHybrid,[time;ar_yy;ar_yp;ar_id;tmres;tmroot] ->
      let (ty_time,time') = typeof env  time in
      let (ty_ar_yy,ar_yy') = typeof env  ar_yy in
      let (ty_ar_yp,ar_yp') = typeof env  ar_yp in
      let (ty_ar_id,ar_id') = typeof env  ar_id in
      let (ty_tmres,tmres') = typeof env  tmres in
      let (ty_tmroot,tmroot') = typeof env  tmroot in
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
      let (ty_time,time') = typeof env  time in
      check_istype_real (tm_info time) l ty_time;
      let (ty_sun,sun') = typeof env  sun in
      check_istype_daesolver (tm_info sun) l ty_sun;
      (ty_time,[time';sun'])

  | DAESolverOpReinit,[sun] ->
      let (ty_sun,sun') = typeof env  sun in
      check_istype_daesolver (tm_info sun) l ty_sun;
      (TyUnit(NoInfo,l),[sun'])

  | DAESolverOpClose,[sun] ->
      let (ty_sun,sun') = typeof env  sun in
      check_istype_daesolver (tm_info sun) l ty_sun;
      (TyUnit(NoInfo,l),[sun'])

  | DAESolverOpRoots,[sun] ->
      let (ty_sun,sun') = typeof env  sun in
      check_istype_daesolver (tm_info sun) l ty_sun;
      (TyArray(NoInfo,l,TyInt(NoInfo,l)),[sun'])

  | _ -> raise (Mkl_type_error
	       (TYPE_UNEXPECTED_NO_ARGS,ERROR,fi,
                [ustring_of_int (List.length ts)]))

and typeof env t =
  match t with
    | TmVar(fi,x) -> ( 
        try let ty1 = List.assoc x env in (ty1,TmVar(fi,x))
	with Not_found -> (
          raise (Mkl_type_error (TYPE_VAR_NOT_DEFINED,ERROR,fi,[Symtbl.get x]))))
    | TmLam(fi,l,x,ty1,t2) ->
          let (ty2,t2') = typeof ((x,ty1)::env)  t2 in
	    (TyArrow(NoInfo,l,ty1,ty2),TmLam(fi,l,x,ty1,t2'))
    | TmFix(fi,l,t) ->
	let (ty,t') = typeof env  t in
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
            | TyDynamic(_,l) ->
                (TyDynamic(NoInfo,0), TmApp(fi,l,t1',t2',fs))
	    | TyArrow(_,l,ty11,ty12) -> 
                (* Coercion of int to real *)
                if (match ty11 with TyReal(_,_) -> true | _ -> false) &&
                   (match ty2 with TyInt(_,_) -> true | _ -> false) 
                then  (ty12,TmApp(fi,l,t1',int2real_coercion t2',fs)) 
                else 
  		  if ty_consistent ty11 ty2 
		  then (ty12,TmApp(fi,l,t1',t2',fs))
		  else 
		    (match ty2 with 
		       | TyModel(_,l3,ty2b) when ty_consistent ty11 ty2b -> 
                          (mk_tymodel ty12,TmModApp
                             (fi,l,TmVal(fi,l,t1',ty1),t2'))
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
                  if  (match ty11 with TyReal(_,_) -> true | _ -> false) &&
                      (match ty2 with TyInt(_,_) -> true | _ -> false) 
                  then (mk_tymodel ty12,
                       TmModApp(fi,l,t1',TmVal(ty_info ty2,ty_lev ty2,
                                        int2real_coercion t2',TyReal(NoInfo,0))))
                  else
		    if ty_consistent ty11 ty2 then 
                      (mk_tymodel ty12,
                       TmModApp(fi,l,t1',TmVal(ty_info ty2,ty_lev ty2,t2',ty2))) 
		    else raise (Mkl_type_error(TYPE_APP_ARG_MISMATCH,ERROR,
                              tm_info t2,[pprint_ty ty11; pprint_ty ty2;us"4"]))
	    | TyModel(fi,l,TyDynamic(_,_))  ->
		if ty_consistent ty1 ty2 then
		  (TyModel(fi,l,TyDynamic(fi,l)),TmModApp(fi,l,t1',t2'))
		else
                  (TyModel(fi,l,TyDynamic(fi,l)),TmModApp(fi,l,t1',
                                    TmVal(ty_info ty2,ty_lev ty2,t2',ty2)))  
	    | _ -> raise (Mkl_type_error(TYPE_APP_NO_FUNC_TYPE,ERROR,tm_info t1,
					   [pprint_ty ty1]))
	  end
        in
               let (ty1,t1') = typeof env  t1 in
               let (ty2,t2') = typeof env  t2 in
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
                    typeof ((x,tyvar)::t1_env)  t1
		  else typeof t1_env  t1 in
                  (* Coercion of int to real *)
                  if  (match ty1def with TyReal(_,_) -> true | _ -> false) &&
                      (match ty1 with TyInt(_,_) -> true | _ -> false) 
                  then (ty1def, int2real_coercion t1')
		  else if not (ty_consistent ty1 ty1def) then
		    raise (Mkl_type_error(TYPE_LET_TYPE_DEF_MISMATCH,
			ERROR,fi,[pprint_ty ty1; pprint_ty ty1def]))
		  else (ty1,t1')
	  | (None,false) -> typeof t1_env  t1
	end in
	  if not (ty_lev ty1 >= l) then 
	    raise (Mkl_type_error(TYPE_LET_TM1_LOWER,ERROR,tm_info t1,
				     [ustring_of_int l;pprint_ty ty1]))
	     else 		   
	       let tyvar = Ast.mk_lettype plst l ty1 in
	       let (ty2,t2') = typeof ((x,tyvar)::env)
                  t2 in
		 if not (ty_lev ty2 >= l) then 
		   raise (Mkl_type_error(TYPE_LET_TM2_LOWER,ERROR,tm_info t2,
				     [ustring_of_int l;pprint_ty ty2]))
		 else
		   (ty2,TmLet(fi,l,x,Some ty1,plst,t1',t2',recu)) 
      | TmIf(fi,l,t1,t2,t3) -> 
        let ((ty1,t1'),(ty2,t2'),(ty3,t3')) = 
	  (typeof env  t1,typeof env  t2,typeof env  t3) in
	  if not (ty_consistent ty1 (TyBool(NoInfo,l))) then
            raise (Mkl_type_error(TYPE_MISMATCH_IF_GUARD,ERROR,tm_info t1,
	                          [pprint_ty (TyBool(NoInfo,l)); pprint_ty ty1]))
          else 
          if ty_consistent ty2 ty3 then 
            (ty_restriction ty2 ty3, TmIf(fi,0,t1',t2',t3'))
          else
          if (not (ty_ismodel ty2)) && ty_consistent (TyModel(fi,l,ty2)) ty3 then 
              (ty_restriction (TyModel(fi,l,ty2)) ty3,
               TmIf(fi,0,t1',TmVal(fi,l,t2',ty2),t3'))
          else           
          if (not (ty_ismodel ty3)) && ty_consistent  ty2 (TyModel(fi,l,ty3)) then 
              (ty_restriction  ty2 (TyModel(fi,l,ty3)),
               TmIf(fi,0,t1',t2',TmVal(fi,l,t3',ty3)))
          else   
          if  (match ty2 with TyReal(_,_) -> true | _ -> false) &&
            (match ty3 with TyInt(_,_) -> true | _ -> false) 
          then  (ty2,TmIf(fi,l,t1',t2',int2real_coercion t3'))
          else if  (match ty3 with TyReal(_,_) -> true | _ -> false) &&
            (match ty2 with TyInt(_,_) -> true | _ -> false) 
          then  (ty3,TmIf(fi,l,t1',int2real_coercion t2',t3'))
          else raise (Mkl_type_error(TYPE_IF_EXP_DIFF_TYPE,ERROR,fi,
		                   [pprint_ty ty2; pprint_ty ty3]))
            

    | TmConst(fi,l,c) as tt -> (deltatype fi c l,tt)
    | TmList(fi,l,ts) ->
	(match ts with
	   | [] -> assert false
	   | t::_ -> let (ty',t') = typeof env  t in 
               typeof env  (List.fold_left 
			 (fun a t -> TmCons(tm_info t,l,t,a)) 
		         (TmNil(fi,l,ty')) ts))
    | TmMatch(fi,l,t,cases) -> assert false
    | TmUk(fi,l,u,ty) -> failwith "Only in the internal language."
    | TmNu(fi,l,u,ty1,t2) ->
	if not (ty_ismodel ty1) then
	  raise (Mkl_type_error(TYPE_NU_LET_NOT_MODELTYPE,ERROR,
				    ty_info ty1,[pprint_ty ty1]))
        else
          let (ty2,t2') = typeof ((u,ty1)::env)  t2 in
	  (ty2,TmNu(fi,l,u,ty1,t2'))

    | TmModApp(fi,l,t1,t2) -> failwith "Only in internal language."
    | TmVal(fi,l,t,_) -> 
	let (ty',t') = typeof env  t in
	  (TyModel(ty_info ty',ty_lev ty',ty'),TmVal(fi,l,t',ty'))
    | TmDecon(fi,l,t1,p,t2,t3) ->
        let ((ty1',t1'),(ty3',t3')) = 
	  (typeof env  t1,typeof env  t3) in
	  (match ty1' with
	     | TyModel(_,l,_)  -> (
                 let anymod = TyModel(NoInfo,l,TyDynamic(NoInfo,l)) in
		 let (ty2',t2') = 
		   (match p with
		      | MPatUk(_,TyModel(_,_,_)) -> typeof env  t2 
		      | MPatUk(_,ty3) -> raise (Mkl_type_error
			    (TYPE_DECON_PAT_UK_NOT_MODEL_TYPE,ERROR,ty_info ty3,
			     [pprint_ty ty3]))   
		      | MPatModApp(_,x1,x2) -> 
			   typeof 
			   ((x1,anymod)::(x2,anymod)::env) 
                              t2
		      | MPatModIfGuard(_,x) -> typeof 
			  ((x,anymod)::env) 
                            t2
		      | MPatModIfThen(_,x) -> typeof 
			  ((x,anymod)::env) 
                            t2
		      | MPatModIfElse(_,x) -> typeof 
			  ((x,anymod)::env)    
                            t2 
		      | MPatModEqual(_,x1,x2) -> 
			    typeof 
			  ((x1,anymod)::(x2,anymod)::env) 
                              t2
		      | MPatModProj(_,x1,x2) -> 
			  typeof ((x1,TyInt(NoInfo,l))::
			       (x2,anymod)::env) 
                                t2
		      | MPatVal(_,x,ty2) -> typeof 
			  ((x,ty2)::env)  t2) 
		 in 
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
	  (typeof env  t1,typeof env  t2) in
          (* (L-EQUAL1) *)
          if (not (ty_ismodel ty1)) && ty_consistent (TyModel(fi,l,ty1)) ty2
          then 
              (TyBool(NoInfo,l),(TmEqual(fi,l,TmVal(fi,l,t1',ty1),t2')))
          else           
          (* (L-EQUAL2) *)
          if ty_consistent ty1 (TyModel(fi,l,ty2))  && (not (ty_ismodel ty2)) 
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
	  begin match typeof env  t1  with
	    | (TyList(fi1,l1,ty1) as ty1lst,t1') -> 
		if l <> l1 then
		  raise (Mkl_type_error(TYPE_LCASE_LEVEL_MISMATCH,ERROR,
			 tm_info t1,[ustring_of_int l; pprint_ty ty1lst]))
		else
		  let (ty2,t2') = typeof ((id2,ty1lst)::
		                   (id1,ty1)::env)  t2  in
		  let (ty3,t3') = typeof env  t3 in
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
	let (ty1,t1') = typeof  env  t1 in
	  begin match typeof  env  t2 with
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
	let (ty',ts') = ts |> List.map (fun t -> typeof env  t ) 
                           |> List.split in
	  ty' |> List.iter (fun ty -> if not (ty_lev ty >= l) then
		    raise (Mkl_type_error(TYPE_TUPLE_LEV_MONOTONICITY,ERROR,
			ty_info ty,[ustring_of_int l; pprint_ty ty])));
	  (TyTuple(fi,l,ty'),TmTuple(fi,l,ts'))
    | TmProj(fi,l,i,t) -> 
        (match typeof env  t with
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
                            |> List.map (fun t -> typeof env  t ) 
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
        let (ty',ts') = typeof_array_op fi l op ts env  in
        (ty',TmArrayOp(fi,l,op,ts'))
    | TmMapOp(fi,l,op,ts) -> 
        let (ty',ts') = typeof_map_op fi l op ts env  in
        (ty',TmMapOp(fi,l,op,ts'))
    | TmSetOp(fi,l,op,ts) -> 
        let (ty',ts') = typeof_set_op fi l op ts env  in
        (ty',TmSetOp(fi,l,op,ts'))
    | TmDAESolverOp(fi,l,op,ts) -> 
        let (ty',ts') = typeof_daesolver_op fi l op ts env  in
        (ty',TmDAESolverOp(fi,l,op,ts'))
    | TmDPrint(t) -> let (ty,t') = typeof env  t in (ty,TmDPrint(t'))
    | TmDPrintType(t) -> let (ty,t') = typeof env  t in (ty,TmDPrintType(t'))
    | TmError(fi,l,t) -> 
	(match typeof env t with
	   | ((TyString(fi2,l2) as ty),t') -> 
	       if not (l2 >= l) then
	       raise (Mkl_type_error(TYPE_ERROR_TERM_LEV_MONOTONICITY,ERROR,fi,
					   [ustring_of_int l;pprint_ty ty]))
	       else (TyBot(fi,l),TmError(fi,l,t'))
	   | (ty,t') -> raise (Mkl_type_error(TYPE_ERROR_TERM_NOT_STRING,ERROR,
					      ty_info ty,[pprint_ty ty])))
	    

let typeofterm t = fst (typeof [] t)
let typecheck t = snd (typeof [] t)

	 



