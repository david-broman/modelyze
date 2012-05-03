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

open Evalast



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
  | Ast.TySym(_,_,ty) -> TySym(trans_ty ty)    
  | Ast.TyDyn(_,_) -> TyDyn
  | Ast.TySymData(_,_,id,_) -> TySymData(id)
  | Ast.TyIdent(_,_,_) -> assert false
  | Ast.TyArray(_,_,ty) -> TyArray(trans_ty ty)
  | Ast.TyMap(_,_,ty1,ty2) -> TyMap(trans_ty ty1,trans_ty ty2)
  | Ast.TySet(_,_,ty) -> TySet(trans_ty ty)
  | Ast.TyDAESolver(_,_) -> TyDAESolver

let trans_pat l p denv =
  match p with
  | Ast.MPatSym(_,ty) -> (MPatSym(trans_ty ty),denv)
  | Ast.MPatSymApp(_,id1,id2) -> (MPatSymApp,(id1,l)::(id2,l)::denv)
  | Ast.MPatModIfGuard(_,id1) -> (MPatModIfGuard,(id1,l)::denv)
  | Ast.MPatModIfThen(_,id1) -> (MPatModIfThen,(id1,l)::denv)
  | Ast.MPatModIfElse(_,id1) -> (MPatModIfElse,(id1,l)::denv)
  | Ast.MPatModEqual(_,id1,id2) -> (MPatModEqual,(id1,l)::(id2,l)::denv)
  | Ast.MPatModProj(_,id1,id2) -> (MPatModProj,(id1,l)::(id2,l)::denv)
  | Ast.MPatLift(_,id,ty) -> (MPatLift(trans_ty ty),(id,l)::denv)

let translate t =
  let rec nl l d t = t in
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
		((id,l)::denv) ),TmDebugId(id,mkfuns plst t1 d denv),false)) 
      | Ast.TmLet(fi,l,id,Some ty,plst,t1,t2,true) -> 
	  nl l d (TmApp(TmLam(trans t2 l ((id,l)::denv) ),
           TmDebugId(id,TmFix(TmLam(mkfuns plst t1 l ((id,l)::denv) ))),false)) 
      | Ast.TmLet(_,_,_,_,_,_,_,_) -> assert false
      | Ast.TmIf(_,l,t1,t2,t3) -> nl l d (TmIf(trans t1 l denv ,
		trans t2 l denv ,trans t3 l denv )) 
      | Ast.TmConst(_,l,c) -> nl l d (TmConst(c))
      | Ast.TmList(_,_,_) -> assert false
      | Ast.TmMatch(_,_,_,_) -> assert false
      | Ast.TmSym(_,l,id,ty) -> 
	  (try let (l2,i) = Utils.find_associndex id denv in nl l2 d (TmVar(i)) 
	   with Not_found -> assert false)
      | Ast.TmNu(_,l,id,ty,t) -> 
          nl l d (TmApp(TmLam(trans t l ((id,l)::denv)),
                        TmDebugId(id,TmGenSym(trans_ty ty)),false))
      | Ast.TmSymApp(_,l,t1,t2) -> 
          nl l d (TmSymApp(trans t1 l denv ,trans t2 l denv ))
      | Ast.TmLift(_,l,t,ty) -> nl l d (TmLift(trans t l denv ,trans_ty ty))
      | Ast.TmCase(_,l,t1,p,t2,t3) ->
	  let (p',denv') = trans_pat l p denv in
	  nl l d (TmCase(trans t1 l denv ,p',trans t2 l denv' ,
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
