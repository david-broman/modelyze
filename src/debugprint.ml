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

open Evalast
open Ustring.Op
open Utils
open Printf


type idcount = int
type ident = int
let (symIdCount : (ident,idcount) Hashtbl.t) = Hashtbl.create 1024
let (symToId : (tm,(ident * idcount)) Hashtbl.t) = Hashtbl.create 1024
let (primToId : (Ast.primitive,ident) Hashtbl.t) = Hashtbl.create 512

let isSymInfix s = Ustring.sub s 0 1 =. us"("

let debugTagTm id t =
  match t with
    | TmSym(s,ty) ->
        if Hashtbl.mem symToId t then
           if isSymInfix (Symtbl.get id) then
             (Hashtbl.add symToId t (id,0); t)
           else t
        else
          (try
             let count = Hashtbl.find symIdCount id in
             Hashtbl.add symIdCount id (count+1);
             Hashtbl.add symToId t (id,count+1);t
           with Not_found ->
	     Hashtbl.add symIdCount id 1;
             Hashtbl.add symToId t (id,1);t)
    | TmClos(t,env,_) -> TmClos(t,env,id)
    | TmConst(Ast.ConstPrim(prim,_)) ->
        Hashtbl.add primToId prim id; t
    | TmByteCode(code,apiid,_,args) -> TmByteCode(code,apiid,id,args)
    | t -> t


let getDebugSymId t =
  try
    let (id,count) = Hashtbl.find symToId t in
    let s = Symtbl.get id in
    if isSymInfix s then s
    else
    (*let totalCount = Hashtbl.find symIdCount id in
      if totalCount = 1
      then s ^. us"#"
      else *)
      s ^. us"_" ^. ustring_of_int count
  with Not_found -> us"NOT_A_SYMBOL"


let no_pat_vars p =
  match p with
  | MPatSym(_) -> 0
  | MPatSymApp -> 2
  | MPatLift(_) -> 1

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
        | TySym(t) ->  us"<" ^. (pprint_ty false t) ^. us">"
	| TyDyn -> us"?"
        | TySymData(tyid) -> ustring_of_int tyid
        | TyArray(t) ->  us"{" ^. (pprint_ty false t) ^. us"}"
	| TyMap(t1,t2) ->
	    us"(" ^. (pprint_ty false t1) ^. us" " ^.
	      us"=>" ^. us" " ^. (pprint_ty false t2) ^. us")"
        | TySet(t) -> us"{" ^. (pprint_ty false t) ^. us"}"
	| TyDAESolver ->  us"SimInst"
	| TyNLEQSolver ->  us"NLEQSolverInst"
        | TyEnv -> us"TyEnv"
  in pprint_ty false t

let pprint_pat p =
  match p with
  | MPatSym(ty) -> us"uk:" ^. pprint_ty ty
  | MPatSymApp ->   us"app "
  | MPatLift(ty) -> us"val:" ^.  pprint_ty ty

type primStyle =
  | InfixSpace
  | InfixNoSpace
  | InfixNewline
  | Prefix
  | PrefixFun
  | Postfix

let opMap str =
  match Ustring.to_utf8 str with
    | "(=)"   -> (InfixSpace,us"=",5)
    | "(~=)"  -> (InfixSpace,us"~=",5)
    | "(<-)"  -> (InfixSpace,us"<-",5)
    | "(mod)" -> (InfixSpace,us"mod",10)
    | "(+)"   -> (InfixSpace,us"+",8)
    | "(-)"   -> (InfixSpace,us"-",8)
    | "(*)"   -> (InfixSpace,us"*",9)
    | "(/)"   -> (InfixSpace,us"/",9)
    | "(<)"   -> (InfixSpace,us"<",6)
    | "(<=)"  -> (InfixSpace,us"<=",6)
    | "(>)"   -> (InfixSpace,us">",6)
    | "(>=)"  -> (InfixSpace,us">=",6)
    | "(==)"  -> (InfixSpace,us"==",6)
    | "(!=)"  -> (InfixSpace,us"!=",6)
    | "(+.)"  -> (InfixSpace,us"+.",8)
    | "(-.)"  -> (InfixSpace,us"-.",8)
    | "(*.)"  -> (InfixSpace,us"*.",9)
    | "(/.)"  -> (InfixSpace,us"/.",9)
    | "(<.)"  -> (InfixSpace,us"<.",7)
    | "(<=.)" -> (InfixSpace,us"<=.",7)
    | "(>.)"  -> (InfixSpace,us">.",7)
    | "(>=.)" -> (InfixSpace,us">=.",7)
    | "(==.)" -> (InfixSpace,us"==.",7)
    | "(!=.)" -> (InfixSpace,us"!=.",7)
    | "(!)"   -> (Prefix,us"!",8)
    | "(&&)"  -> (InfixSpace,us"&&",3)
    | "(||)"  -> (InfixSpace,us"||",2)
    | "(;)"   -> (InfixNewline,us";",1)
    | "(;;)"  -> (InfixNewline,us";;",1)
    | "(++)"  -> (InfixNewline,us"++",5)
    | "(^)"   -> (InfixNoSpace,us"^",11)
    | "(^.)"  -> (InfixNoSpace,us"^.",11)
    | "(-->)"  -> (InfixNoSpace,us"-->",6)
    | "(')"   -> (Postfix,us"'",13)
    | "(--)"  -> (Prefix,us"-",12)
    | "(--.)" -> (Prefix,us"-.",12)
    | s       -> (PrefixFun,us s,0)


let rec pprint_op prec opStr argsStrs  =
  let (style,s,newPrec) = opMap opStr in
  let paren s =
    if newPrec < prec then us"(" ^. s ^. us")" else s
  in
    match (style,argsStrs) with
    | (InfixSpace,[t1;t2]) ->
        paren (pp newPrec t1 ^. us" " ^. s ^. us" " ^. pp newPrec t2)
    | (InfixNoSpace,[t1;t2]) ->
        paren (pp newPrec t1 ^. s ^. pp newPrec t2)
    | (InfixNewline,[t1;t2]) ->
        pp newPrec t1 ^. s ^. us"\n" ^. pp newPrec t2
    | (Postfix,[t1]) -> paren (pp newPrec t1 ^. s)
    | (Prefix,[t1]) -> paren (s ^. pp newPrec t1)
    | (_,ts) ->
         s ^. us"(" ^.
           (Ustring.concat (us", ") (List.map (pp 0) ts)) ^. us")"

and pprint_app prec t =
  match t with
    | TmSymApp(TmSymApp(TmLift(TmConst(_) as c,_),t1),t2) ->
        pprint_op prec (pp 0 c) [t1;t2]
    | TmSymApp(TmLift(TmConst(Ast.ConstPrim(prim,[t1c])) as c,_),t2) ->
        pprint_op prec (pp 0 c) [TmConst(t1c);t2]
    | TmSymApp(TmLift(TmConst(_) as c,_),t2) ->
        pprint_op prec (pp 0 c) [t2]
    | _ ->
       (let rec collect t acc =
           match t with
            | TmSymApp(t1,t2) -> collect t1 (t2::acc)
            | t -> (t,acc)
        in
         let (f,ts) = collect t [] in
           pprint_op 0 (pp 0 f) ts)


and pp prec t  =
  match t with
  | TmClos(t,e,id) ->  us"function " ^. Symtbl.get id ^. us"{" ^. pp 0 t ^. us"}"
    (* Symtbl.get id  *)
    | TmConst(Ast.ConstPrim(primop,_) as c) ->
          (try
             Symtbl.get (Hashtbl.find primToId primop)
           with Not_found ->
             Ast.pprint_const c 0)
    | TmConst(c) ->
      Ast.pprint_const c 0
    | TmSym(idx,ty) ->
        getDebugSymId t
    | TmSymApp(t1,t2) -> pprint_app prec t
    | TmLift(t,ty) -> us"sval(" ^. pp 0 t ^. us")"
    | TmCons(t1,t2) ->
        (let rec toList t =
          match t with
            | TmCons(t1,t2) -> (pp 0 t1)::(toList t2)
            | _ -> []
        in
          us"[" ^. (Ustring.concat (us", ") (toList t)) ^. us"]" )
    | TmNil -> us"[]"
    | TmTuple(tms) ->
	us"(" ^. (tms |> List.map (pp 0) |> Ustring.concat (us", ")) ^. us")"
    | TmArray(tms) -> us"[|" ^.
        (tms |> Array.to_list |> List.map (pp 0) |>
             Ustring.concat (us",")) ^. us"|]"
    | TmMap(size,tmmap) ->
        let lst = PMap.foldi
          (fun t1 t2 ts ->
             (pp 0 t1 ^. us" => " ^. pp 0 t2)::ts) tmmap [] in
          us"{" ^. (Ustring.concat (us", ") lst) ^. us"}"
    | TmSet(size,tmset) ->
        let lst = PMap.foldi (fun t1 _ ts -> (pp 0 t1)::ts) tmset [] in
          us"{" ^. (Ustring.concat (us", ") lst) ^. us"}"
    | TmDAESolver(_) -> us"DAEsolver"
    | TmNLEQSolver(_) -> us"NLEQsolver"
    | TmDPrint(t) | TmDPrintType(t) | TmDebugId(_,t) | TmPEval(t) -> pp prec t
    | TmError(fi,t) -> us"error " ^. pp 0 t
    | TmSpecSym(i) -> us"TmSpecSym(" ^. ustring_of_int i ^. us")"
    | TmVar(i) -> us"TmVar(" ^. ustring_of_int i ^. us")"
    | TmCase(t1,p,t2,t3) -> us"TmCase(" ^.
        pp 0 t1 ^. us",_," ^. pp 0 t2 ^. us"," ^. pp 0 t3 ^. us")"
    | TmEqual(t1,t2) -> us"TmEqual(" ^.
        pp 0 t1 ^. us"," ^. pp 0 t2 ^. us")"
    | TmLcase(t1,t2,t3) -> us"TmLcase(" ^.
        pp 0 t1 ^. us"," ^. pp 0 t2 ^. us"," ^. pp 0 t3 ^. us")"
    | TmLam(t1) -> us"TmLam(" ^. pp 0 t1 ^. us")"
    | TmProj(i,t1) -> us"TmProj(_," ^. pp 0 t1 ^. us")"
    | TmArrayOp(op,tms) -> us"TmArrayOp()"
    | TmMapOp(op,tms) -> us"TmMapOp()"
    | TmApp(t1,t2,s) -> us"TmApp(" ^. pp 0 t1 ^. us"," ^. pp 0 t2 ^.
        (* us"," ^. (if s then us"true" else us"false")  ^. *) us")"
    | TmFix(t1) -> us"TmFix(" ^. pp 0 t1 ^. us")"
    | TmIf(t1,t2,t3) -> us"TmIf(" ^.
        pp 0 t1 ^. us"," ^. pp 0 t2 ^. us"," ^. pp 0 t3 ^. us")"
    | TmGenSym(ty) -> us"TmGenSym()"
    | TmSetOp(op,tms) -> us"TmSetOp()"
    | TmSymStr(t) -> us"TmSetOp(" ^. pp 0 t ^. us")"
    | TmDAESolverOp(op,tms) -> us"TmDAESolverOp()"
    | TmNLEQSolverOp(op,tms) -> us"TmNLEQSolverOp()"
    | TmByteCode(code,extid,ident,args) -> Symtbl.get ident
    | TmTheta(t) -> us"thera(" ^. pp 0 t ^. us")"

let pprint t = pp 0 t

let pprint_env env =
  Ustring.concat (us"\n") (List.map pprint env)
