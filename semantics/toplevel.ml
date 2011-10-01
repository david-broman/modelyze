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


let rec ty_typesubst typemap ty =
   match ty with
  | TyBool(_,_) as tt -> tt
  | TyInt(_,_) as tt -> tt 
  | TyReal(_,_) as tt -> tt   
  | TyString(_,_) as tt -> tt    
  | TyArrow(fi,l,ty1,ty2) -> 
      TyArrow(fi,l,ty_typesubst typemap ty1,ty_typesubst typemap ty2)
  | TyUnit(_,_) as tt -> tt
  | TyList(fi,l,ty) -> TyList(fi,l,ty_typesubst typemap ty)
  | TyTuple(fi,l,tys) -> TyTuple(fi,l,List.map (ty_typesubst typemap) tys)
  | TyModel(fi,l,ty) -> TyModel(fi,l,ty_typesubst typemap ty)
  | TyAnyModel(_,_) as tt -> tt 
  | TyBot(_,_) as tt -> tt 
  | TyUserdef(_,_,_,_) as tt -> tt 
  | TyIdent(fi,l,id) -> 
      (try set_ty_info fi (List.assoc id typemap)
       with Not_found -> raise (Mkl_static_error(TYPE_UNKNOWN_TYPE,ERROR,
				              fi,[Symtbl.get id])))
  | TyArray(fi,l,ty) -> TyArray(fi,l,ty_typesubst typemap ty)
  | TyMap(fi,l,ty1,ty2) -> 
      TyMap(fi,l,ty_typesubst typemap ty1,ty_typesubst typemap ty2)
  | TySet(fi,l,ty) -> TySet(fi,l,ty_typesubst typemap ty)
  | TyDAESolver(_,_) as tt -> tt

let mpat_typesubst typemap pat  = 
  let tysub = ty_typesubst typemap in
  match pat with
    | MPatUk(fi,ty) -> MPatUk(fi,tysub ty)        
    | MPatModApp(fi,id1,id2) as mp -> mp
    | MPatModIfGuard(fi,id) as mp -> mp 
    | MPatModIfThen(fi,id) as mp -> mp
    | MPatModIfElse(fi,id) as mp -> mp
    | MPatModEqual(fi,id1,id2) as mp -> mp  
    | MPatModProj(fi,id1,id2) as mp -> mp 
    | MPatVal(fi,id,ty) -> MPatVal(fi,id,tysub ty)       

let rec pat_typesubst typemap pat =
  let psub = pat_typesubst typemap in
  let tysub = ty_typesubst typemap in
  match pat with
  | PatVar(fi,id) as pp -> pp 
  | PatExpr(fi,t) -> PatExpr(fi,tm_typesubst typemap t)
  | PatUk(fi,ty) -> PatUk(fi,tysub ty)
  | PatModApp(fi,p1,p2) -> PatModApp(fi,psub p1,psub p2)
  | PatModIf(fi,p1,p2,p3) -> PatModIf(fi,psub p1,psub p2,psub p3)
  | PatModEqual(fi,p1,p2) -> PatModEqual(fi,psub p1,psub p2)
  | PatModProj(fi,p1,p2) -> PatModProj(fi,psub p1,psub p2)  
  | PatModVal(fi,id,ty) -> PatModVal(fi,id,tysub ty) 
  | PatCons(fi,p1,p2) -> PatCons(fi,psub p1,psub p2)
  | PatNil(fi) -> PatNil(fi)
  | PatTuple(fi,pats) -> PatTuple(fi,List.map psub pats)     
  | PatWildcard(fi) -> PatWildcard(fi)

and vtrans_typesubst typemap vtrans =
  match vtrans with
  | VTransExpr(fi,id,t) -> VTransExpr(fi,id,tm_typesubst typemap t)    
  | VTransModUk(fi,id,ty) -> 
      VTransModUk(fi,id,ty_typesubst typemap ty)
  | VTransModVal(fi,id1,id2,ty) -> 
      VTransModVal(fi,id1,id2,ty_typesubst typemap ty)

and tm_typesubst typemap tm = 
  let tmsub = tm_typesubst typemap in
  let tysub = ty_typesubst typemap in
  let mpatsub = mpat_typesubst typemap in
  let patsub = pat_typesubst typemap in
  let vtranssub = vtrans_typesubst typemap in
  match tm with
    | TmVar(fi,x) as tt -> tt 
    | TmLam(fi,l,y,ty,t) -> TmLam(fi,l,y,tysub ty,tmsub t)
    | TmApp(fi,l,t1,t2,fs) -> TmApp(fi,l,tmsub t1,tmsub t2,fs)
    | TmFix(fi,l,t) -> TmFix(fi,l,tmsub t)
    | TmLet(fi,l,y,tyop,plst,t1,t2,recu) -> 
        let tyop' = map_option tysub tyop in
        let plst' = List.map (fun (id,ty) -> (id,tysub ty)) plst in
        TmLet(fi,l,y,tyop',plst',tmsub t1,tmsub t2,recu)
    | TmIf(fi,l,t1,t2,t3) -> TmIf(fi,l,tmsub t1,tmsub t2,tmsub t3)
    | TmConst(fi,l,c) as tt -> tt
    | TmUp(fi,l,t) -> TmUp(fi,l,tmsub t)
    | TmDown(fi,l,t) -> TmDown(fi,l,tmsub t)
    | TmBracket(fi,t) -> TmBracket(fi,tmsub t)
    | TmEscape(fi,t) -> TmEscape(fi,tmsub t)
    | TmList(fi,l,tms) -> TmList(fi,l,List.map tmsub tms)
    | TmMatch(fi,l,t,cases) ->
        let cases' = List.map 
          (fun (PCase(fi,plst,t1op,vtrans,t2)) ->
                  let plst' = List.map patsub plst in
                  let t1op' = map_option tmsub t1op in
                  let vtrans' = List.map vtranssub vtrans in
                  PCase(fi,plst',t1op',vtrans',tmsub t2)) cases in
        TmMatch(fi,l,tmsub t,cases')
    | TmUk(fi,l,id,ty) -> TmUk(fi,l,id,tysub ty)
    | TmNu(fi,l,id,ty,t) -> TmNu(fi,l,id,tysub ty,tmsub t)
    | TmModApp(fi,l,t1,t2) -> TmModApp(fi,l,tmsub t1,tmsub t2)
    | TmModIf(fi,l,t1,t2,t3) -> 
        TmModIf(fi,l,tmsub t1,tmsub t2,tmsub t3)
    | TmModEqual(fi,l,t1,t2) -> TmModEqual(fi,l,tmsub t1,tmsub t2)
    | TmModProj(fi,l,i,t1) -> TmModProj(fi,l,i,tmsub t1)
    | TmVal(fi,l,t,ty) -> TmVal(fi,l,tmsub t,tysub ty)
    | TmDecon(fi,l,t1,p,t2,t3) -> 
        TmDecon(fi,l,tmsub t1,mpatsub p,tmsub t2,tmsub t3)
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
    | TmDpa(t) -> TmDpa(tmsub t)
    | TmDpb(t) -> TmDpb(tmsub t)
    | TmError(fi,l,t) -> TmError(fi,l,tmsub t)

let desugar tlst =
  let rec ds tlst tyno typemap =
    match tlst with  
      | TopLet(fi,id,ty,plst,t1,recu)::ts -> 
          let ty' = map_option (ty_typesubst typemap) ty in
          let plst' = List.map 
            (fun (id,ty) -> (id,ty_typesubst typemap ty)) plst in
          let t1' = tm_typesubst typemap t1 in
          let ts' = ds ts tyno typemap in
            TmLet(fi,0,id,ty',plst',t1',ts',recu)
      | TopNu(fi,id,ty)::ts -> 
          let ty' = ty_typesubst typemap ty in
          let ts' = ds ts tyno typemap in
	    TmNu(fi,0,id,ty',ts')
      | TopNewType(fi,id)::ts -> 
          ds ts (tyno+1) ((id,TyUserdef(fi,0,tyno,id))::typemap) 
      | TopNameType(fi,id,ty)::ts -> 
          ds ts tyno ((id,ty_typesubst typemap ty)::typemap) 
      | TopInclude(fi,id)::ts -> 
          assert false (* Should be removed in the import pass *)
      | [] -> TmConst(NoInfo,0,ConstUnit)
  in ds tlst 0 []
