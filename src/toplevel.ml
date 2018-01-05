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
open Info


(* Normalize by making sure that there is just one level
   of symbolic brackets *)
let ty_normalize ty =
  let rec norm isSym ty =
    match ty with
      | TyBool(_,_) -> ty
      | TyInt(_,_) -> ty
      | TyReal(_,_) -> ty
      | TyString(_,_) -> ty
      | TyArrow(fi,l,ty1,ty2) ->
          TyArrow(fi,l,norm isSym ty1, norm isSym ty2)
      | TyUnit(_,_) -> ty
      | TyList(fi,l,ty1) -> TyList(fi,l,norm false ty1)
      | TyTuple(fi,l,tys) -> TyTuple(fi,l,List.map (norm false) tys)
      | TySym(fi,l,(TySym(_,_,TyDyn(_,_)) as tty)) -> tty
      | TySym(fi,l,ty1) ->
          if isSym then norm true ty1 else TySym(fi,l,norm true ty1)
      | TyDyn(_,_) -> ty
      | TySymData(_,_,_,_) -> ty
      | TyIdent(fi,l,id) -> ty
      | TyArray(fi,l,ty1) -> TyArray(fi,l,norm isSym ty1)
      | TyMap(fi,l,ty1,ty2) ->
          TyMap(fi,l,norm isSym ty1, norm isSym ty2)
      | TySet(fi,l,ty1) -> TySet(fi,l, norm isSym ty1)
      | TyDAESolver(_,_) -> ty
      | TyEnv(_,_,_) -> ty
  in
    norm false ty

let ty_typesubst typemap numap ty =
  let rec subst ty =
    match ty with
      | TyBool(_,_) as tt -> tt
      | TyInt(_,_) as tt -> tt
      | TyReal(_,_) as tt -> tt
      | TyString(_,_) as tt -> tt
      | TyArrow(fi,l,ty1,ty2) ->
          TyArrow(fi,l,subst ty1,subst ty2)
      | TyUnit(_,_) as tt -> tt
      | TyList(fi,l,ty) -> TyList(fi,l,subst ty)
      | TyTuple(fi,l,tys) -> TyTuple(fi,l,List.map subst tys)
      | TySym(fi,l,ty) -> TySym(fi,l,subst ty)
      | TyDyn(_,_) as tt -> tt
      | TySymData(_,_,_,_) as tt -> tt
      | TyIdent(fi,l,id) ->
          (try set_ty_info fi (List.assoc id typemap)
           with Not_found -> raise (Mkl_static_error(TYPE_UNKNOWN_TYPE,ERROR,
				                     fi,[Symtbl.get id])))
      | TyArray(fi,l,ty) -> TyArray(fi,l,subst ty)
      | TyMap(fi,l,ty1,ty2) ->
          TyMap(fi,l,subst ty1,subst ty2)
      | TySet(fi,l,ty) -> TySet(fi,l,subst ty)
      | TyDAESolver(_,_) as tt -> tt
      | TyEnv(_,_,_) -> ty
  in
    ty_normalize (subst ty)

let mpat_typesubst typemap numap pat  =
  let tysub = ty_typesubst typemap numap in
  match pat with
    | MPatSym(fi,ty) -> MPatSym(fi,tysub ty)
    | MPatSymApp(fi,id1,id2) as mp -> mp
    | MPatLift(fi,id,ty) -> MPatLift(fi,id,tysub ty)

let rec pat_typesubst typemap numap pat =
  let psub = pat_typesubst typemap numap in
  let tysub = ty_typesubst typemap numap in
  match pat with
    (* Introduce auto escape for global symbol definitions. *)
  | PatVar(fi,id,auto_esc_allowed) as pp ->
     if auto_esc_allowed && List.mem id numap then
        PatExpr(fi,TmVar(fi,id,0))
      else pp
  | PatExpr(fi,t) -> PatExpr(fi,tm_typesubst typemap numap t)
  | PatSym(fi,ty) -> PatSym(fi,tysub ty)
  | PatSymApp(fi,p1,p2) -> PatSymApp(fi,psub p1,psub p2)
  | PatLift(fi,id,ty) -> PatLift(fi,id,tysub ty)
  | PatCons(fi,p1,p2) -> PatCons(fi,psub p1,psub p2)
  | PatNil(fi) -> PatNil(fi)
  | PatTuple(fi,pats) -> PatTuple(fi,List.map psub pats)
  | PatWildcard(fi) -> PatWildcard(fi)

and vtrans_typesubst typemap numap vtrans =
  match vtrans with
  | VTransExpr(fi,id,t) -> VTransExpr(fi,id,tm_typesubst typemap numap t)
  | VTransModUk(fi,id,ty) ->
      VTransModUk(fi,id,ty_typesubst typemap numap ty)
  | VTransModVal(fi,id1,id2,ty) ->
      VTransModVal(fi,id1,id2,ty_typesubst typemap numap ty)

and tm_typesubst typemap numap tm =
  let tmsub = tm_typesubst typemap numap in
  let tysub = ty_typesubst typemap numap in
  let mpatsub = mpat_typesubst typemap numap in
  let patsub = pat_typesubst typemap numap in
  let vtranssub = vtrans_typesubst typemap numap in
  match tm with
    | TmVar(fi,x,_) as tt -> tt
    | TmLam(fi,l,y,ty,t) -> TmLam(fi,l,y,tysub ty,tmsub t)
    | TmApp(fi,l,t1,t2,fs) -> TmApp(fi,l,tmsub t1,tmsub t2,fs)
    | TmFix(fi,l,t) -> TmFix(fi,l,tmsub t)
    | TmLet(fi,l,y,tyop,plst,t1,t2,recu) ->
        let tyop' = map_option tysub tyop in
        let plst' = List.map (fun (id,ty) -> (id,tysub ty)) plst in
        TmLet(fi,l,y,tyop',plst',tmsub t1,tmsub t2,recu)
    | TmIf(fi,l,t1,t2,t3) -> TmIf(fi,l,tmsub t1,tmsub t2,tmsub t3)
    | TmConst(fi,l,c) as tt -> tt
    | TmList(fi,l,tms) -> TmList(fi,l,List.map tmsub tms)
    | TmMatch(fi,l,t,cases) ->
        let cases' = List.map
          (fun (PCase(fi,plst,t1op,vtrans,t2)) ->
                  let plst' = List.map patsub plst in
                  let t1op' = map_option tmsub t1op in
                  let vtrans' = List.map vtranssub vtrans in
                  PCase(fi,plst',t1op',vtrans',tmsub t2)) cases in
        TmMatch(fi,l,tmsub t,cases')
    | TmSym(fi,l,id,ty) -> TmSym(fi,l,id,tysub ty)
    | TmNu(fi,l,id,ty,t) -> TmNu(fi,l,id,tysub (TySym(NoInfo,0,ty)),tmsub t)
    | TmSymApp(fi,l,t1,t2) -> TmSymApp(fi,l,tmsub t1,tmsub t2)
    | TmLift(fi,l,t,ty) -> TmLift(fi,l,tmsub t,tysub ty)
    | TmCase(fi,l,t1,p,t2,t3) ->
        TmCase(fi,l,tmsub t1,mpatsub p,tmsub t2,tmsub t3)
    | TmEqual(fi,l,t1,t2) -> TmEqual(fi,l,tmsub t1,tmsub t2)
    | TmLcase(fi,l,t,x,y,t1,t2) ->
        TmLcase(fi,l,tmsub t,x,y,tmsub t1,tmsub t2)
    | TmCons(fi,l,t1,t2) -> TmCons(fi,l,tmsub t1,tmsub t2)
    | TmNil(fi,l,ty) -> TmNil(fi,l,tysub ty)
    | TmTuple(fi,l,ts) -> TmTuple(fi,l,List.map tmsub ts)
    | TmProj(fi,l,i,t1) -> TmProj(fi,l,i,tmsub t1)
    | TmArray(fi,l,tms) -> TmArray(fi,l,Array.map tmsub tms)
    | TmArrayOp(fi,l,op,tms) -> TmArrayOp(fi,l,op,List.map tmsub tms)
    | TmMapOp(fi,l,op,tms) -> TmMapOp(fi,l,op,List.map tmsub tms)
    | TmSetOp(fi,l,op,tms) -> TmSetOp(fi,l,op,List.map tmsub tms)
    | TmDAESolverOp(fi,l,op,tms) -> TmDAESolverOp(fi,l,op,List.map tmsub tms)
    | TmDPrint(t) -> TmDPrint(tmsub t)
    | TmDPrintType(t) -> TmDPrintType(tmsub t)
    | TmSymStr(fi,t) -> TmSymStr(fi,tmsub t)
    | TmError(fi,l,t) -> TmError(fi,l,tmsub t)
    | TmPEval(t) -> TmPEval(tmsub t)


let desugar tlst =
  (* numap is used for auto escape of global symbol definitions
  when doing pattern matching *)
  let rec ds tlst tyno typemap numap =
    match tlst with
      | TopLet(fi,id,ty,plst,t1,recu)::ts ->
          let ty' = map_option (ty_typesubst typemap numap) ty in
          let plst' = List.map
            (fun (id,ty) -> (id,ty_typesubst typemap numap ty)) plst in
          let t1' = tm_typesubst typemap numap t1 in
          let ts' = ds ts tyno typemap numap  in
            TmLet(fi,0,id,ty',plst',t1',ts',recu)
      | TopNu(fi,id,ty)::ts ->
          let ty' = ty_typesubst typemap numap (TySym(NoInfo,0,ty)) in
          let ts' = ds ts tyno typemap (id::numap) in
	    TmNu(fi,0,id,ty',ts')
      | TopNewType(fi,id)::ts ->
          ds ts (tyno+1) ((id,TySym(NoInfo,0,TySymData(fi,0,tyno,id)))::typemap) numap
      | TopNameType(fi,id,ty)::ts ->
          ds ts tyno ((id,ty_typesubst typemap numap ty)::typemap) numap
      | TopInclude(fi,id)::ts ->
          assert false (* Should be removed in the import pass *)
      | [] -> TmConst(NoInfo,0,ConstUnit)
  in ds tlst 0 [] []
