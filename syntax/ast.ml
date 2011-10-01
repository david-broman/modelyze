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
open Message
open Info
open Sundials

(** Modeling Kernel Language Abstract Syntax Trees (ASTs) *)

(** {6 Complete AST (C-AST)} *)

exception Mkl_runtime_error of Message.message

module VarSet = Set.Make(Int)

type exeception

type level = int
type ident = int
type typeid = int
type valueid = int
type fname = int
type recursive = bool 
type specialize = bool

(** Definition of types in the language *)
type ty = 
  | TyBool      of info * level
  | TyInt       of info * level
  | TyReal      of info * level
  | TyString    of info * level
  | TyArrow     of info * level * ty * ty 
  | TyUnit      of info * level
  | TyList      of info * level * ty
  | TyTuple     of info * level * ty list
  | TyModel     of info * level * ty
  | TyAnyModel  of info * level 
  | TyBot       of info * level
  | TyUserdef   of info * level * typeid * ident
  | TyIdent     of info * level * ident
  | TyArray     of info * level * ty
  | TyMap       of info * level * ty * ty
  | TySet       of info * level * ty 
  | TyDAESolver of info * level

(** Primitive, built-in functions *)
type primitive = 
  | PrimIntMod 
  | PrimIntAdd
  | PrimIntSub
  | PrimIntMul
  | PrimIntDiv
  | PrimIntLess 
  | PrimIntLessEqual 
  | PrimIntGreat 
  | PrimIntGreatEqual 
  | PrimIntEqual
  | PrimIntNotEqual
  | PrimIntNeg
  | PrimRealAdd
  | PrimRealSub
  | PrimRealMul
  | PrimRealDiv
  | PrimRealLess 
  | PrimRealLessEqual 
  | PrimRealGreat 
  | PrimRealGreatEqual 
  | PrimRealEqual
  | PrimRealNotEqual
  | PrimRealNeg
  | PrimBoolAnd
  | PrimBoolOr
  | PrimBoolNot
  | PrimPrint
  | PrimBool2String
  | PrimInt2String
  | PrimReal2String
  | PrimInt2Real
  | PrimReal2Int
  | PrimString2Bool
  | PrimString2Int
  | PrimString2Real
  | PrimIsBoolString
  | PrimIsRealString
  | PrimIsIntString
  | PrimSin
  | PrimCos
  | PrimTan
  | PrimASin
  | PrimACos
  | PrimATan
  | PrimSinh
  | PrimCosh
  | PrimTanh
  | PrimCeil
  | PrimFloor
  | PrimLog
  | PrimLog10
  | PrimSqrt
  | PrimExp
  | PrimExponentiation
  | PrimStringConcat
  | PrimStringStrlen
  | PrimStringSubstr

(** Constants. Models e.g. integers, strings, built-in 
    functions and model relations *)
type const =
  | ConstBool     of bool
  | ConstInt      of int
  | ConstReal     of float
  | ConstString   of ustring
  | ConstUnit     
  | ConstPrim     of primitive * const list

(** Model calculus patterns *)
and mpat = 
  | MPatUk         of info * ty  
  | MPatModApp     of info * ident * ident 
  | MPatModIfGuard of info * ident  
  | MPatModIfThen  of info * ident  
  | MPatModIfElse  of info * ident  
  | MPatModEqual   of info * ident * ident 
  | MPatModProj    of info * ident * ident 
  | MPatVal        of info * ident * ty

and pat =
  | PatVar        of  info * ident
  | PatExpr       of  info * tm
  | PatUk         of  info * ty
  | PatModApp     of  info * pat * pat
  | PatModIf      of  info * pat * pat * pat 
  | PatModEqual   of  info * pat * pat
  | PatModProj    of  info * pat * pat
  | PatModVal     of  info * ident * ty
  | PatCons       of  info * pat * pat
  | PatNil        of  info
  | PatTuple      of  info * pat list
  | PatWildcard   of  info

and vartrans =
  | VTransExpr    of  info * ident * tm
  | VTransModUk   of  info * ident * ty 
  | VTransModVal  of  info * ident * ident * ty

and patcase = PCase of info * pat list * tm option * vartrans list * tm 

and arrayop =
  | ArrayOpLength
  | ArrayOpMake
  | ArrayOpGet
  | ArrayOpSet

and mapop = 
  | MapOpSize
  | MapOpEmpty
  | MapOpAdd
  | MapOpFind
  | MapOpMem
  | MapOpRemove
  | MapOpToList

and setop = 
  | SetOpSize 
  | SetOpEmpty
  | SetOpAdd
  | SetOpMem
  | SetOpRemove
  | SetOpToList

and daesolverop = 
  | DAESolverOpMake
  | DAESolverOpMakeHybrid
  | DAESolverOpStep
  | DAESolverOpReinit
  | DAESolverOpClose
  | DAESolverOpRoots
      
(** Top elements of a source code file *)
and top = 
  | TopLet        of info * ident * ty option * (ident * ty) list * 
                     tm   * recursive
  | TopNu         of info * ident * ty 
  | TopNewType    of info * ident  
  | TopNameType   of info * ident * ty
  | TopInclude    of info * ident 

(** Terms of the abstract syntax tree. This tree is used for desugaring
    terms, type checking and then translating to another term which 
    is interpreted. *)
and tm = 
    (* Basic terms *)
  | TmVar         of info * ident
  | TmLam         of info * level * ident * ty * tm
  | TmApp         of info * level * tm * tm * specialize
  | TmFix         of info * level * tm
  | TmLet         of info * level * ident * ty option * (ident * ty) list * 
                                    tm * tm * recursive
  | TmIf          of info * level * tm * tm * tm
  | TmConst       of info * level * const 
    (* Meta lambda calculus *)
  | TmUp          of info * level * tm
  | TmDown        of info * level * tm
    (* Syntactic sugar *)
  | TmBracket     of info * tm                          
  | TmEscape      of info * tm                          
  | TmList        of info * level * tm list 
  | TmMatch       of info * level * tm * patcase list
    (* Model calculus *)
  | TmUk          of info * level * ident * ty 
  | TmNu          of info * level * ident * ty * tm
  | TmModApp      of info * level * tm * tm 
  | TmModIf       of info * level * tm * tm * tm
  | TmModEqual    of info * level * tm * tm
  | TmModProj     of info * level * int * tm
  | TmVal         of info * level * tm * ty
  | TmDecon       of info * level * tm * mpat * tm * tm
    (* Polymorphic equality *)
  | TmEqual       of info * level * tm * tm
    (* List *)
  | TmLcase       of info * level * tm * ident * ident * tm * tm
  | TmCons        of info * level * tm * tm
  | TmNil         of info * level * ty
    (* Tuple *)
  | TmTuple       of info * level * tm list
  | TmProj        of info * level * int * tm  
    (* Array *)
  | TmArray       of info * level * tm array
  | TmArrayOp     of info * level * arrayop * tm list
    (* Map *)
  | TmMapOp       of info * level * mapop * tm list
    (* Set *)
  | TmSetOp       of info * level * setop * tm list
    (* Simulation *)
  | TmDAESolverOp      of  info * level * daesolverop * tm list
    (* Debugging and errors *)
  | TmDpa         of tm
  | TmDpb         of tm
  | TmError       of info * level * tm


(** Test equivalence of types. Ignores the info field. *)
let rec ty_equiv ty1 ty2 = 
  match ty1,ty2 with
  | TyBool(_,l1),TyBool(_,l2) -> l1 = l2
  | TyInt(_,l1),TyInt(_,l2) -> l1 = l2
  | TyReal(_,l1),TyReal(_,l2) -> l1 = l2
  | TyString(_,l1),TyString(_,l2) -> l1 = l2
  | TyArrow(_,l1,ty1a,ty1b), TyArrow(_,l2,ty2a,ty2b) ->
      (ty_equiv ty1a ty2a) && (ty_equiv ty1b ty2b) && l1 = l2
  | TyUnit(_,l1),TyUnit(_,l2) -> l1 = l2
  | TyList(_,l1,ty1),TyList(_,l2,ty2) -> l1 = l2 && ty_equiv ty1 ty2
  | TyTuple(_,l1,tylst1),TyTuple(_,l2,tylst2) -> l1 = l2 &&
      List.fold_left2 (fun a ty1 ty2 -> a && ty_equiv ty1 ty2) 
      true tylst1 tylst2
  | TyModel(_,l1,ty1),TyModel(_,l2,ty2) -> l1 = l2 && ty_equiv ty1 ty2
  | TyAnyModel(_,l1),TyAnyModel(_,l2) -> l1 = l2  
  | TyBot(_,l1),TyBot(_,l2) -> l1 = l2  
  | TyUserdef(_,l1,tyid1,_),TyUserdef(_,l2,tyid2,_) -> l1 = l2 && tyid1 = tyid2
  | TyArray(_,l1,ty1),TyArray(_,l2,ty2) -> l1 = l2 && ty_equiv ty1 ty2
  | TyMap(_,l1,ty1a,ty1b),TyMap(_,l2,ty2a,ty2b) -> 
      l1 = l2 && ty_equiv ty1a ty2a  && ty_equiv ty1b ty2b
  | TySet(_,l1,ty1),TySet(_,l2,ty2) -> l1 = l2 && ty_equiv ty1 ty2
  | TyDAESolver(_,l1),TyDAESolver(_,l2) -> l1 = l2
  | _ -> false 

let rec ty_restriction ty1 ty2 =
  match ty1,ty2 with
    | TyModel(_,_,_),TyAnyModel(fi,ty) -> TyAnyModel(fi,ty)
    | TyBot(_,_),_ -> ty2
    | TyArrow(fi,l,ty1,ty2),TyArrow(fi',l',ty1',ty2') -> 
	TyArrow(fi,l,ty_restriction ty1 ty1',ty_restriction ty2 ty2')
    | TyList(fi,l,ty),TyList(fi',l',ty') -> 
	TyList(fi,l,ty_restriction ty ty')
    | TyTuple(fi,l,tys),TyTuple(fi',l',tys') -> 
         TyTuple(fi,l,map2sc ty_restriction tys tys')
    | TyModel(fi,l,ty),TyModel(fi',l',ty') -> 
	TyModel(fi,l,ty_restriction ty ty')
    | TyArray(fi,l,ty),TyArray(fi',l',ty') -> 
	TyArray(fi,l,ty_restriction ty ty')
    | TyMap(fi,l,ty1,ty2),TyMap(fi',l',ty1',ty2') -> 
	TyMap(fi,l,ty_restriction ty1 ty1',ty_restriction ty2 ty2')
    | TySet(fi,l,ty),TySet(fi',l',ty') -> 
	TySet(fi,l,ty_restriction ty ty')
    | _,_ -> ty1


let rec ty_consistent ty1 ty2 =
  ty_equiv (ty_restriction ty1 ty2) (ty_restriction ty2 ty1)

let delta c1 c2 =
  match (c1,c2) with
    | (ConstPrim(PrimIntMod,[]),c2) -> ConstPrim(PrimIntMod,[c2])
    | (ConstPrim(PrimIntMod,[ConstInt(i1)]),ConstInt(i2)) -> ConstInt(i1 mod i2)
    | (ConstPrim(PrimIntAdd,[]),c2) -> ConstPrim(PrimIntAdd,[c2])
    | (ConstPrim(PrimIntAdd,[ConstInt(i1)]),ConstInt(i2)) -> ConstInt(i1 + i2)
    | (ConstPrim(PrimIntSub,[]),c2) -> ConstPrim(PrimIntSub,[c2])
    | (ConstPrim(PrimIntSub,[ConstInt(i1)]),ConstInt(i2)) -> ConstInt(i1 - i2)
    | (ConstPrim(PrimIntMul,[]),c2) -> ConstPrim(PrimIntMul,[c2])
    | (ConstPrim(PrimIntMul,[ConstInt(i1)]),ConstInt(i2)) -> ConstInt(i1 * i2)
    | (ConstPrim(PrimIntDiv,[]),c2) -> ConstPrim(PrimIntDiv,[c2])
    | (ConstPrim(PrimIntDiv,[ConstInt(i1)]),ConstInt(i2)) -> ConstInt(i1 / i2)
    | (ConstPrim(PrimIntLess,[]),c2) -> ConstPrim(PrimIntLess,[c2])
    | (ConstPrim(PrimIntLess,[ConstInt(i1)]),ConstInt(i2)) -> ConstBool(i1 < i2)
    | (ConstPrim(PrimIntLessEqual,[]),c2) -> ConstPrim(PrimIntLessEqual,[c2])
    | (ConstPrim(PrimIntLessEqual,[ConstInt(i1)]),ConstInt(i2)) -> 
        ConstBool(i1 <= i2)
    | (ConstPrim(PrimIntGreat,[]),c2) -> ConstPrim(PrimIntGreat,[c2])
    | (ConstPrim(PrimIntGreat,[ConstInt(i1)]),ConstInt(i2)) -> 
        ConstBool(i1 > i2)
    | (ConstPrim(PrimIntGreatEqual,[]),c2) -> ConstPrim(PrimIntGreatEqual,[c2])
    | (ConstPrim(PrimIntGreatEqual,[ConstInt(i1)]),ConstInt(i2)) -> 
        ConstBool(i1 >= i2)
    | (ConstPrim(PrimIntEqual,[]),c2) -> ConstPrim(PrimIntEqual,[c2])
    | (ConstPrim(PrimIntEqual,[ConstInt(i1)]),ConstInt(i2)) -> 
        ConstBool(i1 = i2)
    | (ConstPrim(PrimIntNotEqual,[]),c2) -> ConstPrim(PrimIntNotEqual,[c2])
    | (ConstPrim(PrimIntNotEqual,[ConstInt(i1)]),ConstInt(i2)) -> 
        ConstBool(i1 <> i2)
    | (ConstPrim(PrimIntNeg,[]),ConstInt(i1)) -> ConstInt(-i1)
    | (ConstPrim(PrimRealAdd,[]),c2) -> ConstPrim(PrimRealAdd,[c2])
    | (ConstPrim(PrimRealAdd,[ConstReal(i1)]),ConstReal(i2)) 
      -> ConstReal(i1 +. i2)
    | (ConstPrim(PrimRealSub,[]),c2) -> ConstPrim(PrimRealSub,[c2])
    | (ConstPrim(PrimRealSub,[ConstReal(i1)]),ConstReal(i2)) -> 
        ConstReal(i1 -. i2)
    | (ConstPrim(PrimRealMul,[]),c2) -> ConstPrim(PrimRealMul,[c2])
    | (ConstPrim(PrimRealMul,[ConstReal(i1)]),ConstReal(i2)) -> 
        ConstReal(i1 *. i2)
    | (ConstPrim(PrimRealDiv,[]),c2) -> ConstPrim(PrimRealDiv,[c2])
    | (ConstPrim(PrimRealDiv,[ConstReal(i1)]),ConstReal(i2)) -> 
        ConstReal(i1 /. i2)
    | (ConstPrim(PrimRealLess,[]),c2) -> ConstPrim(PrimRealLess,[c2])
    | (ConstPrim(PrimRealLess,[ConstReal(i1)]),ConstReal(i2)) -> 
        ConstBool(i1 < i2)
    | (ConstPrim(PrimRealLessEqual,[]),c2) -> ConstPrim(PrimRealLessEqual,[c2])
    | (ConstPrim(PrimRealLessEqual,[ConstReal(i1)]),ConstReal(i2)) -> 
        ConstBool(i1 <= i2)
    | (ConstPrim(PrimRealGreat,[]),c2) -> ConstPrim(PrimRealGreat,[c2])
    | (ConstPrim(PrimRealGreat,[ConstReal(i1)]),ConstReal(i2)) -> 
        ConstBool(i1 > i2)
    | (ConstPrim(PrimRealGreatEqual,[]),c2) -> 
        ConstPrim(PrimRealGreatEqual,[c2])
    | (ConstPrim(PrimRealGreatEqual,[ConstReal(i1)]),ConstReal(i2)) ->
        ConstBool(i1 >= i2)
    | (ConstPrim(PrimRealEqual,[]),c2) -> ConstPrim(PrimRealEqual,[c2])
    | (ConstPrim(PrimRealEqual,[ConstReal(i1)]),ConstReal(i2)) -> 
        ConstBool(i1 = i2)
    | (ConstPrim(PrimRealNotEqual,[]),c2) -> ConstPrim(PrimRealNotEqual,[c2])
    | (ConstPrim(PrimRealNotEqual,[ConstReal(i1)]),ConstReal(i2)) -> 
        ConstBool(i1 <> i2)
    | (ConstPrim(PrimRealNeg,[]),ConstReal(i1)) -> ConstReal(-.i1)
    | (ConstPrim(PrimBoolAnd,[]),c2) -> ConstPrim(PrimBoolAnd,[c2])
    | (ConstPrim(PrimBoolAnd,[ConstBool(i1)]),ConstBool(i2)) -> 
        ConstBool(i1 && i2)
    | (ConstPrim(PrimBoolOr,[]),c2) -> ConstPrim(PrimBoolOr,[c2])
    | (ConstPrim(PrimBoolOr,[ConstBool(i1)]),ConstBool(i2)) -> 
        ConstBool(i1 || i2)
    | (ConstPrim(PrimBoolNot,[]),ConstBool(i1)) -> ConstBool(not i1)
    | (ConstPrim(PrimPrint,[]),ConstString(s)) -> uprint_string s; ConstUnit
    | (ConstPrim(PrimBool2String,[]),ConstBool(i)) -> 
        ConstString(ustring_of_bool i)
    | (ConstPrim(PrimInt2String,[]),ConstInt(i)) -> 
        ConstString(ustring_of_int i)
    | (ConstPrim(PrimReal2String,[]),ConstReal(i)) -> 
        ConstString(ustring_of_float i)
    | (ConstPrim(PrimInt2Real,[]),ConstInt(i)) -> ConstReal(float_of_int i)
    | (ConstPrim(PrimReal2Int,[]),ConstReal(f)) -> ConstInt(int_of_float f)
    | (ConstPrim(PrimString2Bool,[]),ConstString(s)) -> 
        (try ConstBool(bool_of_ustring s) with Invalid_argument _ -> 
           ConstBool(false))
    | (ConstPrim(PrimString2Int,[]),ConstString(s)) -> 
        (try ConstInt(int_of_ustring s) with Failure _ -> ConstInt(0))
    | (ConstPrim(PrimString2Real,[]),ConstString(s)) -> 
        (try ConstReal(float_of_ustring s) with Failure _ -> ConstReal(0.))
    | (ConstPrim(PrimIsBoolString,[]),ConstString(s)) -> 
        (try let _ = ConstBool(bool_of_ustring s) in ConstBool(true)
         with Invalid_argument _ -> ConstBool(false))
    | (ConstPrim(PrimIsIntString,[]),ConstString(s)) -> 
        (try let _ = int_of_ustring s in ConstBool(true)
         with Failure _ -> ConstBool(false))
    | (ConstPrim(PrimIsRealString,[]),ConstString(s)) -> 
        (try let _ = float_of_ustring s in ConstBool(true) 
         with Failure _ -> ConstBool(false))
    | (ConstPrim(PrimSin,[]),ConstReal(e)) -> ConstReal(sin e)
    | (ConstPrim(PrimCos,[]),ConstReal(e)) -> ConstReal(cos e)
    | (ConstPrim(PrimTan,[]),ConstReal(e)) -> ConstReal(tan e)
    | (ConstPrim(PrimASin,[]),ConstReal(e)) -> ConstReal(asin e)
    | (ConstPrim(PrimACos,[]),ConstReal(e)) -> ConstReal(acos e)
    | (ConstPrim(PrimATan,[]),ConstReal(e)) -> ConstReal(atan e)
    | (ConstPrim(PrimSinh,[]),ConstReal(e)) -> ConstReal(sinh e)
    | (ConstPrim(PrimCosh,[]),ConstReal(e)) -> ConstReal(cosh e)
    | (ConstPrim(PrimTanh,[]),ConstReal(e)) -> ConstReal(tanh e)
    | (ConstPrim(PrimCeil,[]),ConstReal(e)) -> ConstReal(ceil e)
    | (ConstPrim(PrimFloor,[]),ConstReal(e)) -> ConstReal(floor e)
    | (ConstPrim(PrimLog,[]),ConstReal(e)) -> ConstReal(log e)
    | (ConstPrim(PrimLog10,[]),ConstReal(e)) -> ConstReal(log10 e)
    | (ConstPrim(PrimSqrt,[]),ConstReal(e)) -> ConstReal(sqrt e)
    | (ConstPrim(PrimExp,[]),ConstReal(e)) -> ConstReal(exp e)
    | (ConstPrim(PrimExponentiation,[]),c2) -> 
        ConstPrim(PrimExponentiation,[c2])
    | (ConstPrim(PrimExponentiation,[ConstReal(i1)]),ConstReal(i2)) -> 
        ConstReal(i1 ** i2)
    | (ConstPrim(PrimStringConcat,[]),c2) -> ConstPrim(PrimStringConcat,[c2])
    | (ConstPrim(PrimStringConcat,[ConstString(s1)]),ConstString(s2)) -> 
	ConstString(s1 ^. s2)
    | (ConstPrim(PrimStringStrlen,[]),ConstString(s1)) -> 
        ConstInt(Ustring.length s1)
    | (ConstPrim(PrimStringSubstr,[]),s) -> ConstPrim(PrimStringSubstr,[s])
    | (ConstPrim(PrimStringSubstr,([s] as ls)),pos) -> 
	ConstPrim(PrimStringSubstr,pos::ls)  
    | (ConstPrim(PrimStringSubstr,[ConstInt(pos);ConstString(s)]),
       ConstInt(len)) -> 
	ConstString(try Ustring.sub s pos len with Invalid_argument _ -> us"")  
    | _ -> assert false 

let deltatype fi c1 l  =
  let tybool = TyBool(fi,l) in
  let tyint = TyInt(fi,l) in
  let tyreal = TyReal(fi,l) in
  let tystring = TyString(fi,l) in
  let tyunit = TyUnit(fi,l) in 
  let tyfun2 ty1 ty2 = TyArrow(fi,l,ty1,ty2) in
  let tyfun3 ty1 ty2 ty3 = TyArrow(fi,l,ty1,TyArrow(fi,l,ty2,ty3)) in
  let tyfun4 ty1 ty2 ty3 ty4 = 
    TyArrow(NoInfo,l,ty1,(TyArrow(fi,l,ty2,TyArrow(fi,l,ty3,ty4)))) in
  match c1 with
    | ConstBool(_) -> tybool 
    | ConstInt(_) -> tyint
    | ConstReal(_) -> tyreal
    | ConstString(_) -> tystring
    | ConstUnit -> tyunit
    | ConstPrim(PrimIntMod,[]) -> tyfun3 tyint tyint tyint
    | ConstPrim(PrimIntMod,[_]) -> tyfun2 tyint tyint 
    | ConstPrim(PrimIntAdd,[]) -> tyfun3 tyint tyint tyint
    | ConstPrim(PrimIntAdd,[_]) -> tyfun2 tyint tyint 
    | ConstPrim(PrimIntSub,[]) -> tyfun3 tyint tyint tyint
    | ConstPrim(PrimIntSub,[_]) -> tyfun2 tyint tyint 
    | ConstPrim(PrimIntMul,[]) -> tyfun3 tyint tyint tyint
    | ConstPrim(PrimIntMul,[_]) -> tyfun2 tyint tyint 
    | ConstPrim(PrimIntDiv,[]) -> tyfun3 tyint tyint tyint
    | ConstPrim(PrimIntDiv,[_]) -> tyfun2 tyint tyint 
    | ConstPrim(PrimIntLess,[]) -> tyfun3 tyint tyint tybool
    | ConstPrim(PrimIntLess,[_]) -> tyfun2 tyint tybool 
    | ConstPrim(PrimIntLessEqual,[]) -> tyfun3 tyint tyint tybool
    | ConstPrim(PrimIntLessEqual,[_]) -> tyfun2 tyint tybool 
    | ConstPrim(PrimIntGreat,[]) -> tyfun3 tyint tyint tybool
    | ConstPrim(PrimIntGreat,[_]) -> tyfun2 tyint tybool 
    | ConstPrim(PrimIntGreatEqual,[]) -> tyfun3 tyint tyint tybool
    | ConstPrim(PrimIntGreatEqual,[_]) -> tyfun2 tyint tybool 
    | ConstPrim(PrimIntEqual,[]) -> tyfun3 tyint tyint tybool
    | ConstPrim(PrimIntEqual,[_]) -> tyfun2 tyint tybool 
    | ConstPrim(PrimIntNotEqual,[]) -> tyfun3 tyint tyint tybool
    | ConstPrim(PrimIntNotEqual,[_]) -> tyfun2 tyint tybool 
    | ConstPrim(PrimIntNeg,[]) -> tyfun2 tyint tyint 
    | ConstPrim(PrimRealAdd,[]) -> tyfun3 tyreal tyreal tyreal
    | ConstPrim(PrimRealAdd,[_]) -> tyfun2 tyreal tyreal 
    | ConstPrim(PrimRealSub,[]) -> tyfun3 tyreal tyreal tyreal
    | ConstPrim(PrimRealSub,[_]) -> tyfun2 tyreal tyreal 
    | ConstPrim(PrimRealMul,[]) -> tyfun3 tyreal tyreal tyreal
    | ConstPrim(PrimRealMul,[_]) -> tyfun2 tyreal tyreal 
    | ConstPrim(PrimRealDiv,[]) -> tyfun3 tyreal tyreal tyreal
    | ConstPrim(PrimRealDiv,[_]) -> tyfun2 tyreal tyreal 
    | ConstPrim(PrimRealLess,[]) -> tyfun3 tyreal tyreal tybool
    | ConstPrim(PrimRealLess,[_]) -> tyfun2 tyreal tybool 
    | ConstPrim(PrimRealLessEqual,[]) -> tyfun3 tyreal tyreal tybool
    | ConstPrim(PrimRealLessEqual,[_]) -> tyfun2 tyreal tybool 
    | ConstPrim(PrimRealGreat,[]) -> tyfun3 tyreal tyreal tybool
    | ConstPrim(PrimRealGreat,[_]) -> tyfun2 tyreal tybool 
    | ConstPrim(PrimRealGreatEqual,[]) -> tyfun3 tyreal tyreal tybool
    | ConstPrim(PrimRealGreatEqual,[_]) -> tyfun2 tyreal tybool 
    | ConstPrim(PrimRealEqual,[]) -> tyfun3 tyreal tyreal tybool
    | ConstPrim(PrimRealEqual,[_]) -> tyfun2 tyreal tybool 
    | ConstPrim(PrimRealNotEqual,[]) -> tyfun3 tyreal tyreal tybool
    | ConstPrim(PrimRealNotEqual,[_]) -> tyfun2 tyreal tybool 
    | ConstPrim(PrimRealNeg,[]) -> tyfun2 tyreal tyreal 
    | ConstPrim(PrimBoolAnd,[]) -> tyfun3 tybool tybool tybool
    | ConstPrim(PrimBoolAnd,[_]) -> tyfun2 tybool tybool 
    | ConstPrim(PrimBoolOr,[]) -> tyfun3 tybool tybool tybool
    | ConstPrim(PrimBoolOr,[_]) -> tyfun2 tybool tybool 
    | ConstPrim(PrimBoolNot,[]) -> tyfun2 tybool tybool 
    | ConstPrim(PrimPrint,[]) -> tyfun2 tystring tyunit
    | ConstPrim(PrimBool2String,[]) -> tyfun2 tybool tystring
    | ConstPrim(PrimInt2String,[]) -> tyfun2 tyint tystring
    | ConstPrim(PrimReal2String,[]) -> tyfun2 tyreal tystring
    | ConstPrim(PrimInt2Real,[]) -> tyfun2 tyint tyreal
    | ConstPrim(PrimReal2Int,[]) -> tyfun2 tyreal tyint
    | ConstPrim(PrimString2Bool,[]) -> tyfun2 tystring tybool
    | ConstPrim(PrimString2Int,[]) -> tyfun2 tystring tyint
    | ConstPrim(PrimString2Real,[]) -> tyfun2 tystring tyreal
    | ConstPrim(PrimIsBoolString,[]) -> tyfun2 tystring tybool
    | ConstPrim(PrimIsRealString,[]) -> tyfun2 tystring tybool
    | ConstPrim(PrimIsIntString,[]) -> tyfun2 tystring tybool
    | ConstPrim(PrimSin,[]) -> tyfun2 tyreal tyreal
    | ConstPrim(PrimCos,[]) -> tyfun2 tyreal tyreal
    | ConstPrim(PrimTan,[]) -> tyfun2 tyreal tyreal
    | ConstPrim(PrimASin,[]) -> tyfun2 tyreal tyreal
    | ConstPrim(PrimACos,[]) -> tyfun2 tyreal tyreal
    | ConstPrim(PrimATan,[]) -> tyfun2 tyreal tyreal
    | ConstPrim(PrimSinh,[]) -> tyfun2 tyreal tyreal
    | ConstPrim(PrimCosh,[]) -> tyfun2 tyreal tyreal
    | ConstPrim(PrimTanh,[]) -> tyfun2 tyreal tyreal
    | ConstPrim(PrimCeil,[]) -> tyfun2 tyreal tyreal
    | ConstPrim(PrimFloor,[]) -> tyfun2 tyreal tyreal
    | ConstPrim(PrimLog,[]) -> tyfun2 tyreal tyreal
    | ConstPrim(PrimLog10,[]) -> tyfun2 tyreal tyreal
    | ConstPrim(PrimSqrt,[]) -> tyfun2 tyreal tyreal
    | ConstPrim(PrimExp,[]) -> tyfun2 tyreal tyreal
    | ConstPrim(PrimExponentiation,[]) -> tyfun3 tyreal tyreal tyreal
    | ConstPrim(PrimStringConcat,[]) -> tyfun3 tystring tystring tystring
    | ConstPrim(PrimStringConcat,[_]) -> tyfun2 tystring tystring
    | ConstPrim(PrimStringStrlen,[]) -> tyfun2 tystring tyint
    | ConstPrim(PrimStringSubstr,[]) -> tyfun4 tystring tyint tyint tystring 
    | ConstPrim(PrimStringSubstr,[_]) -> tyfun3 tyint tyint tystring
    | ConstPrim(PrimStringSubstr,[_;_]) -> tyfun2 tyint tystring
    | _ -> assert false 

let primitive_arity p =
  match p with
  | PrimIntMod -> 2
  | PrimIntAdd -> 2
  | PrimIntSub -> 2
  | PrimIntMul -> 2
  | PrimIntDiv -> 2
  | PrimIntLess -> 2
  | PrimIntLessEqual -> 2 
  | PrimIntGreat -> 2
  | PrimIntGreatEqual -> 2  
  | PrimIntEqual -> 2
  | PrimIntNotEqual -> 2
  | PrimIntNeg -> 1
  | PrimRealAdd -> 2
  | PrimRealSub -> 2
  | PrimRealMul -> 2
  | PrimRealDiv -> 2
  | PrimRealLess -> 2
  | PrimRealLessEqual -> 2 
  | PrimRealGreat -> 2
  | PrimRealGreatEqual -> 2 
  | PrimRealEqual -> 2
  | PrimRealNotEqual -> 2
  | PrimRealNeg -> 1
  | PrimBoolAnd -> 2
  | PrimBoolOr -> 2
  | PrimBoolNot -> 1
  | PrimPrint -> 1
  | PrimBool2String -> 1
  | PrimInt2String -> 1
  | PrimReal2String -> 1
  | PrimInt2Real -> 1
  | PrimReal2Int -> 1
  | PrimString2Bool -> 1
  | PrimString2Int -> 1
  | PrimString2Real -> 1
  | PrimIsBoolString -> 1
  | PrimIsRealString -> 1
  | PrimIsIntString -> 1
  | PrimSin -> 1
  | PrimCos -> 1
  | PrimTan -> 1
  | PrimASin -> 1
  | PrimACos -> 1
  | PrimATan -> 1
  | PrimSinh -> 1
  | PrimCosh -> 1
  | PrimTanh -> 1
  | PrimCeil -> 1
  | PrimFloor -> 1
  | PrimLog -> 1
  | PrimLog10 -> 1
  | PrimSqrt -> 1
  | PrimExp -> 1
  | PrimExponentiation -> 1
  | PrimStringConcat -> 2
  | PrimStringStrlen -> 1
  | PrimStringSubstr -> 3


let rec tm_info t =
  match t with
    | TmVar(fi,_) -> fi
    | TmLam(fi,_,_,_,_) -> fi
    | TmApp(fi,_,_,_,_) -> fi
    | TmFix(fi,_,_) -> fi
    | TmLet(fi,_,_,_,_,_,_,_) -> fi
    | TmIf(fi,_,_,_,_) -> fi
    | TmConst(fi,_,_) -> fi
    | TmUp(fi,_,_) -> fi
    | TmDown(fi,_,_) -> fi
    | TmBracket(fi,_) -> fi
    | TmEscape(fi,_) -> fi
    | TmList(fi,_,_) -> fi
    | TmMatch(fi,_,_,_) -> fi
    | TmUk(fi,_,_,_) -> fi
    | TmNu(fi,_,_,_,_) -> fi    
    | TmModApp(fi,_,_,_) -> fi    
    | TmModIf(fi,_,_,_,_) -> fi
    | TmModEqual(fi,_,_,_) -> fi
    | TmModProj(fi,_,_,_) -> fi
    | TmVal(fi,_,_,_) -> fi     
    | TmDecon(fi,_,_,_,_,_) -> fi    
    | TmEqual(fi,_,_,_) -> fi
    | TmLcase(fi,_,_,_,_,_,_) -> fi
    | TmCons(fi,_,_,_) -> fi
    | TmNil(fi,_,_) -> fi
    | TmTuple(fi,_,_) -> fi
    | TmProj(fi,_,_,_) -> fi
    | TmArray(fi,_,_) -> fi
    | TmArrayOp(fi,_,_,_) -> fi
    | TmMapOp(fi,_,_,_) -> fi
    | TmSetOp(fi,_,_,_) -> fi
    | TmDAESolverOp(fi,_,_,_) -> fi
    | TmDpa(t) -> tm_info t
    | TmDpb(t) -> tm_info t
    | TmError(fi,_,_) -> fi

let rec set_tm_info newfi tm = 
  match tm with
    | TmVar(_,x) -> TmVar(newfi,x)
    | TmLam(_,l,y,ty,t) -> TmLam(newfi,l,y,ty,t)
    | TmApp(_,l,t1,t2,fs) -> TmApp(newfi,l,t1,t2,fs)
    | TmFix(_,l,t) -> TmFix(newfi,l,t)
    | TmLet(_,l,y,tyop,plst,t1,t2,recu) -> 
         TmLet(newfi,l,y,tyop,plst,t1,t2,recu)
    | TmIf(_,l,t1,t2,t3) -> TmIf(newfi,l,t1,t2,t3)
    | TmConst(_,l,c) -> TmConst(newfi,l,c) 
    | TmUp(_,l,t) -> TmUp(newfi,l,t)
    | TmDown(_,l,t) -> TmDown(newfi,l,t)
    | TmBracket(_,t) -> TmBracket(newfi,t)
    | TmEscape(_,t) -> TmEscape(newfi,t)
    | TmList(_,l,tms) -> TmList(newfi,l,tms)
    | TmMatch(_,l,t,cases) -> TmMatch(newfi,l,t,cases)
    | TmUk(_,l,id,ty) -> TmUk(newfi,l,id,ty)
    | TmNu(_,l,id,ty,t) -> TmNu(newfi,l,id,ty,t)
    | TmModApp(_,l,t1,t2) -> TmModApp(newfi,l,t1,t2)
    | TmModIf(_,l,t1,t2,t3) -> TmModIf(newfi,l,t1,t2,t3)
    | TmModEqual(_,l,t1,t2) -> TmModEqual(newfi,l,t1,t2)
    | TmModProj(_,l,i,t1) -> TmModProj(newfi,l,i,t1)
    | TmVal(_,l,t,ty) -> TmVal(newfi,l,t,ty)
    | TmDecon(_,l,t1,p,t2,t3) -> TmDecon(newfi,l,t1,p,t2,t3)
    | TmEqual(_,l,t1,t2) -> TmEqual(newfi,l,t1,t2)
    | TmLcase(_,l,t,x,y,t1,t2) -> TmLcase(newfi,l,t,x,y,t1,t2)  
    | TmCons(_,l,t1,t2) -> TmCons(newfi,l,t1,t2)
    | TmNil(_,l,ty) -> TmNil(newfi,l,ty)
    | TmTuple(_,l,ts) -> TmTuple(newfi,l,ts) 
    | TmProj(_,l,i,t1) -> TmProj(newfi,l,i,t1)
    | TmArray(_,l,tms) -> TmArray(newfi,l,tms)
    | TmArrayOp(_,l,op,tms) -> TmArrayOp(newfi,l,op,tms)
    | TmMapOp(_,l,op,tms) -> TmMapOp(newfi,l,op,tms)
    | TmSetOp(_,l,op,tms) -> TmSetOp(newfi,l,op,tms)
    | TmDAESolverOp(_,l,op,tms) -> TmDAESolverOp(newfi,l,op,tms)
    | TmDpa(t) -> TmDpa(set_tm_info newfi t)
    | TmDpb(t) -> TmDpb(set_tm_info newfi t)
    | TmError(_,l,t) -> TmError(newfi,l,t)


let pat_info p =
  match p with
  | PatVar(fi,_) -> fi
  | PatExpr(fi,_) -> fi
  | PatUk(fi,_) -> fi
  | PatModApp(fi,_,_) -> fi
  | PatModIf(fi,_,_,_) -> fi
  | PatModEqual(fi,_,_) -> fi
  | PatModProj(fi,_,_) -> fi
  | PatModVal(fi,_,_) -> fi
  | PatCons(fi,_,_) -> fi
  | PatNil(fi) -> fi       
  | PatTuple(fi,_) -> fi
  | PatWildcard(fi) -> fi

let ty_info ty = 
  match ty with 
    | TyBool(fi,_) -> fi
    | TyInt(fi,_) -> fi
    | TyReal(fi,_) -> fi
    | TyString(fi,_) -> fi
    | TyArrow(fi,_,_,_) -> fi 
    | TyUnit(fi,_) -> fi
    | TyList(fi,_,_) -> fi
    | TyTuple(fi,_,_) -> fi
    | TyModel(fi,_,_) -> fi
    | TyAnyModel(fi,_) -> fi
    | TyBot(fi,_) -> fi
    | TyUserdef(fi,_,_,_) -> fi
    | TyIdent(fi,_,_) -> fi
    | TyArray(fi,_,_) -> fi
    | TyMap(fi,_,_,_) -> fi 
    | TySet(fi,_,_) -> fi
    | TyDAESolver(fi,_) -> fi

let rec set_ty_info newfi ty = 
  match ty with 
    | TyBool(_,l) -> TyBool(newfi,l)
    | TyInt(_,l) -> TyInt(newfi,l)
    | TyReal(_,l) -> TyReal(newfi,l)
    | TyString(_,l) -> TyString(newfi,l)
    | TyArrow(_,l,ty1,ty2) -> 
        TyArrow(newfi,l,set_ty_info newfi ty1,set_ty_info newfi ty2)
    | TyUnit(_,l) -> TyUnit(newfi,l)
    | TyList(_,l,ty) -> TyList(newfi,l,set_ty_info newfi ty)
    | TyTuple(_,l,tys) -> TyTuple(newfi,l,List.map (set_ty_info newfi) tys)
    | TyModel(_,l,ty) -> TyModel(newfi,l,set_ty_info newfi ty)
    | TyAnyModel(_,l) -> TyAnyModel(newfi,l)
    | TyBot(_,l) -> TyBot(newfi,l)
    | TyUserdef(_,l,tyid,id) -> TyUserdef(newfi,l,tyid,id)
    | TyIdent(_,l,id) -> TyIdent(newfi,l,id)
    | TyArray(_,l,ty) -> TyArray(newfi,l,set_ty_info newfi ty)
    | TyMap(_,l,ty1,ty2) -> 
        TyMap(newfi,l,set_ty_info newfi ty1,set_ty_info newfi ty2)
    | TySet(_,l,ty) -> TySet(newfi,l,set_ty_info newfi ty)
    | TyDAESolver(_,l) -> TyDAESolver(newfi,l)

let ty_lev ty =
  match ty with 
    | TyBool(_,l) -> l
    | TyInt(_,l) -> l
    | TyReal(_,l) -> l
    | TyString(_,l) -> l
    | TyArrow(_,l,_,_) -> l
    | TyUnit(_,l) -> l
    | TyList(_,l,_) -> l
    | TyTuple(_,l,_) -> l
    | TyModel(_,l,_) -> l
    | TyAnyModel(_,l) -> l
    | TyBot(_,l) -> l
    | TyUserdef(_,l,_,_) -> l
    | TyIdent(_,l,_) -> l
    | TyArray(_,l,_) -> l
    | TyMap(_,l,_,_) -> l
    | TySet(_,l,_) -> l
    | TyDAESolver(_,l) -> l


let rec metastr n = 
  if n == 0 then us"" else us"#" ^. metastr (n-1)      

let pprint_primitive p = 
  match p with
    | PrimIntMod -> us"int_mod"
    | PrimIntAdd -> us"int_add"
    | PrimIntSub -> us"int_sub"
    | PrimIntMul -> us"int_mul"
    | PrimIntDiv -> us"int_div"
    | PrimIntLess -> us"int_less"
    | PrimIntLessEqual -> us"int_less_equal"
    | PrimIntGreat -> us"int_great"
    | PrimIntGreatEqual -> us"int_great_equal"
    | PrimIntEqual -> us"int_equal"
    | PrimIntNotEqual -> us"int_not_equal"
    | PrimIntNeg -> us"int_neg"
    | PrimRealAdd -> us"real_add"
    | PrimRealSub -> us"real_sub"
    | PrimRealMul -> us"real_mul"
    | PrimRealDiv -> us"real_div"
    | PrimRealLess -> us"real_less"
    | PrimRealLessEqual -> us"real_less_equal"
    | PrimRealGreat -> us"real_great"
    | PrimRealGreatEqual -> us"real_great_equal" 
    | PrimRealEqual -> us"real_equal"
    | PrimRealNotEqual -> us"real_not_equal"
    | PrimRealNeg -> us"real_neg"
    | PrimBoolAnd -> us"bool_and"
    | PrimBoolOr -> us"bool_or"
    | PrimBoolNot -> us"bool_not"
    | PrimPrint -> us"print"
    | PrimBool2String -> us"bool2string"
    | PrimInt2String -> us"int2string"
    | PrimReal2String -> us"real2string"
    | PrimInt2Real -> us"int2real"
    | PrimReal2Int -> us"real2int"
    | PrimString2Bool -> us"string2bool"
    | PrimString2Int -> us"string2int"
    | PrimString2Real -> us"string2real"
    | PrimIsBoolString -> us"isboolstring"
    | PrimIsRealString -> us"isrealstring"
    | PrimIsIntString -> us"isintstring"
    | PrimSin -> us"sin"
    | PrimCos -> us"cos"
    | PrimTan -> us"tan"
    | PrimASin -> us"asin"
    | PrimACos -> us"acos"
    | PrimATan -> us"atan"
    | PrimSinh -> us"sinh"
    | PrimCosh -> us"cosh"
    | PrimTanh -> us"tanh"
    | PrimCeil -> us"ceil"
    | PrimFloor -> us"floor"
    | PrimLog -> us"log"
    | PrimLog10 -> us"log10"
    | PrimSqrt -> us"sqrt"
    | PrimExp -> us"exp"
    | PrimExponentiation -> us"exponentiation"
    | PrimStringConcat -> us"string_concat"
    | PrimStringStrlen -> us"string_strlen"
    | PrimStringSubstr -> us"string_substr"

let pprint_const c l =
  match c with
    | ConstBool(b) -> metastr l ^. if b then us"true" else us"false"
    | ConstInt(i) -> metastr l ^. ustring_of_int i
    | ConstReal(f) -> metastr l ^. ustring_of_float f
    | ConstString(s) -> metastr l ^. us"\"" ^. s ^. us"\""
    | ConstUnit -> metastr l ^. us"()"
    | ConstPrim(p,_) -> metastr l ^. pprint_primitive p

let pprint_ty t =
  let rec pprint_ty left t =
      match t with 
	| TyBool(_,l) -> metastr l ^. us"Bool"  
	| TyInt(_,l) -> metastr l ^. us"Int"  
	| TyReal(_,l) -> metastr l ^. us"Real"  
	| TyString(_,l) -> metastr l ^. us"String"  
	| TyArrow(_,l,t1,t2) -> 
	    (if left then us"(" else us"") ^. (pprint_ty true t1) ^. us" " ^.  
	      metastr l ^. us"->" ^. us" " ^. (pprint_ty false t2) ^.
	      (if left then us")" else us"")
 	| TyUnit(_,l) -> metastr l ^. us"()"
        | TyList(_,l,t) -> metastr l ^. us"[" ^. (pprint_ty false t) ^. us"]"
        | TyTuple(_,l,tylst) -> metastr l ^. us"(" ^. (tylst |> 
	    List.map (pprint_ty false) |> Ustring.concat (us","))  ^. us")"
        | TyModel(_,l,t) -> metastr l ^. us"<" ^. (pprint_ty false t) ^. us">"
	| TyAnyModel(_,l) -> metastr l ^. us"<>"  
	| TyBot(_,l) -> metastr l ^. us"bot"  
        | TyUserdef(_,l,tyid,id) -> metastr l ^. Symtbl.get id 
        | TyIdent(_,l,id) -> metastr l ^. us"typeident(" ^. 
            Symtbl.get id ^. us")"
        | TyArray(_,l,t) -> metastr l ^. us"{" ^. (pprint_ty false t) ^. us"}"
	| TyMap(_,l,t1,t2) -> 
	    us"(" ^. (pprint_ty false t1) ^. us" " ^.  
	      metastr l ^. us"=>" ^. us" " ^. (pprint_ty false t2) ^. us")" 
        | TySet(_,l,t) -> metastr l ^. us"Set(" ^. (pprint_ty false t) ^. us")"
	| TyDAESolver(_,l) -> metastr l ^. us"SimInst"  
  in pprint_ty false t

let pprint_array_op op =
  match op with
  | ArrayOpLength -> us"length"
  | ArrayOpMake -> us"make"
  | ArrayOpGet -> us"get"
  | ArrayOpSet -> us"set"

let pprint_map_op op =
  match op with
  | MapOpSize -> us"size"
  | MapOpEmpty -> us"empty"
  | MapOpAdd -> us"add"
  | MapOpFind -> us"find"
  | MapOpMem -> us"mem"
  | MapOpRemove -> us"remove"
  | MapOpToList -> us"fold"

let pprint_set_op op =
  match op with
  | SetOpSize -> us"size"
  | SetOpEmpty -> us"empty"
  | SetOpAdd -> us"add"
  | SetOpMem -> us"mem"
  | SetOpRemove -> us"remove"
  | SetOpToList -> us"toList"

let pprint_daesolver_op op =
  match op with
  | DAESolverOpMake -> us"make"
  | DAESolverOpMakeHybrid -> us"makehybrid"
  | DAESolverOpStep -> us"step"
  | DAESolverOpReinit -> us"reinit"
  | DAESolverOpClose -> us"close"
  | DAESolverOpRoots -> us"roots"

let pprint_mpat p = 
  match p with
  | MPatUk(_,ty) -> us"uk:" ^. pprint_ty ty 
  | MPatModApp(_,x,y) -> us"app " ^. Symtbl.get x ^. us" " ^. Symtbl.get y
  | MPatModIfGuard(_,x) -> us"ifguard " ^. Symtbl.get x 
  | MPatModIfThen(_,x) -> us"ifthen " ^. Symtbl.get x
  | MPatModIfElse(_,x) -> us"ifelse " ^. Symtbl.get x
  | MPatModEqual(_,x,y) -> Symtbl.get x ^. us"== " ^. Symtbl.get y
  | MPatModProj(_,x,y) ->   
      us"proj " ^. Symtbl.get x ^. us" from " ^. Symtbl.get y
  | MPatVal(_,x,ty) -> us"val: " ^. Symtbl.get x ^. us":" ^. pprint_ty ty


let rec pprint_pat p =
  match p with 
  | PatVar(_,x) -> Symtbl.get x
  | PatExpr(_,t) -> pprint t
  | PatUk(_,ty) -> pprint_ty ty
  | PatModApp(_,p1,p2) -> pprint_pat p1 ^. us" " ^. pprint_pat p2 
  | PatModIf(_,p1,p2,p3) -> us"if " ^. pprint_pat p1 ^. us" then " ^.
      pprint_pat p2 ^. us" else " ^. pprint_pat p3
  | PatModEqual(_,p1,p2) -> pprint_pat p1 ^. us"==" ^. pprint_pat p2 
  | PatModProj(_,p1,p2) -> us"proj " ^. pprint_pat p1 ^. us" from " ^. 
      pprint_pat p2 
  | PatModVal(_,x,ty) -> us"val " ^. Symtbl.get x ^. us":" ^. pprint_ty ty
  | PatCons(_,p1,p2) ->  pprint_pat p1 ^. us"::" ^. pprint_pat p2 
  | PatNil(_) -> us"[]"
  | PatTuple(_,ps) -> us"(" ^. 
      (ps |> List.map pprint_pat |> Ustring.concat (us",")) ^. us")"
  | PatWildcard(_) -> us"_"

and pprint_vartrans_list vts = 
  us"{" ^. (vts |> List.map (function 
     | VTransExpr(_,x,t) -> 
         us"VTExpr(" ^. Symtbl.get x ^. us"," ^. pprint t ^. us")"
     | VTransModUk(_,x,ty) ->
         us"VTModUk(" ^. Symtbl.get x ^. us"," ^. pprint_ty ty ^. us")"
     | VTransModVal(_,x,y,ty) ->
         us"VTModVal(" ^. Symtbl.get x ^. us"," ^. Symtbl.get x ^. us"," ^.
           pprint_ty ty ^. us")") 
     |>  Ustring.concat (us".")) ^. us"}"


and pprint_case (PCase(_,ps,tm1op,vtrans,t2)) = 
  us"| " ^. (ps |> List.map pprint_pat |> Ustring.concat (us",")) ^. 
  (match tm1op with Some t -> us" when " ^. pprint t | None -> us" ") ^.
  pprint_vartrans_list vtrans ^. us"-> " ^. pprint t2

and pprint_cases cases =
  cases |> List.map pprint_case |> Ustring.concat (us" ")

and pprint tm = 
  match tm with
  | TmVar(_,id) -> Symtbl.get id        
  | TmLam(_,l,x,ty,t) -> us"(" ^.metastr l ^. us"fun " ^. Symtbl.get x ^.  
      us":" ^. pprint_ty ty ^. us" -> " ^. pprint t ^. us")"
  | TmApp(_,l,t1,t2,fs) -> (if fs then us"specialize(" else us"(" )
      ^. pprint t1 ^. us" " ^. 
      (if l != 0 then metastr l ^. us" " else us"") ^. 
        pprint t2 ^. us")"
  | TmFix(_,l,t) -> metastr l ^. us"fix[" ^. pprint t ^. us"]"
  | TmLet(_,l,id,tyop,arglst,t1,t2,recursive) ->
      let params = us" " ^. (arglst 
        |> List.map (fun (x,ty) -> Symtbl.get x ^. us":" ^. pprint_ty ty) 
        |> Ustring.concat (us" -> ")) in
      metastr l ^. us"let " ^.  Symtbl.get id ^. params ^. us" = " ^. 
      pprint t1 ^. us" in " ^. pprint t2
  | TmIf(_,l,t1,t2,t3) -> metastr l ^. us"(if " ^. 
	pprint t1 ^. us" then " ^.pprint t2 ^. us" else " ^. pprint t3 ^. us")"
  | TmConst(_,l,c) -> pprint_const c l  
  | TmUp(_,l,t) -> us"(" ^. metastr l ^. us"up " ^. pprint t ^. us")"
  | TmDown(_,l,t) -> us"(" ^. metastr l ^. us"down " ^. pprint t ^. us")"
  | TmBracket(_,t) -> us"#(" ^. pprint t ^. us")" 
  | TmEscape(_,t) -> us"~(" ^. pprint t ^. us")" 
  | TmList(_,l,ts) -> metastr l ^. us"[" ^. 
      (ts |> List.map pprint |> Ustring.concat (us",")) ^. us"]"
  | TmMatch(_,l,t,cases) ->
      metastr l ^. us"match " ^. pprint t ^. us" with " ^. pprint_cases cases
  | TmUk(_,l,idx,ty) -> metastr l ^. ustring_of_int idx ^. 
      us"':" ^. pprint_ty ty
  | TmNu(_,l,x,ty,t) -> 
      us"(" ^. metastr l ^. us"nu " ^. Symtbl.get x ^. us":" ^. 
      pprint_ty ty ^. us". "^. pprint t ^. us")"  
  | TmModApp(_,l,t1,t2) -> us"(" ^. pprint t1  ^. metastr l ^. us" " ^. 
      pprint t2  ^. us")"
  | TmModIf(_,l,t1,t2,t3) -> metastr l ^. us"<if> " ^. 
      pprint t1 ^. us" then " ^.pprint t2 ^. us" else " ^. pprint t3 
  | TmModEqual(_,l,t1,t2) -> pprint t1 ^. us" " ^. 
      metastr l ^. us"<==> " ^. pprint t2
  | TmModProj(_,l,i,t1) -> metastr l ^. us"<proj> " ^. ustring_of_int i ^. 
      us" " ^. pprint t1
  | TmVal(_,l,t,ty) -> metastr l ^. us"val(" ^. pprint t ^.
        us":" ^. pprint_ty ty ^. us")"
  | TmDecon(_,l,t1,pat,t2,t3) -> metastr l ^. us"(decon " ^.
      pprint t1 ^. us" with " ^. pprint_mpat pat ^. us" then " ^. 
        pprint t2 ^. us" else " ^. pprint t3 ^. us")"
  | TmEqual(_,l,t1,t2) -> pprint t1 ^. us" " ^. 
      metastr l ^. us"== " ^. pprint t2        
  | TmLcase(_,l,t,x,y,t1,t2) -> metastr l ^.
      us"lcase " ^. pprint t ^.  us" of " ^. Symtbl.get x ^. us"::"  ^.
        Symtbl.get y ^. us" -> (" ^. pprint t1 ^. us") | [] -> (" 
      ^. pprint t2 ^. us")" 
  | TmCons(_,l,t1,t2) ->  pprint t1 ^. metastr l ^. us"::" ^. pprint t2
  | TmNil(_,l,ty) -> metastr l ^. us"[" ^. pprint_ty ty ^. us"]"
  | TmTuple(_,l,ts) -> metastr l ^. us"(" ^. 
      (ts |> List.map pprint |> Ustring.fast_concat (us",")) ^. us")"
  | TmProj(_,l,i,t1) -> metastr l ^. us"proj " ^. ustring_of_int i ^. 
      us" " ^. pprint t1
  | TmArray(_,l,tms) -> metastr l ^. us"[|" ^.
      (tms |> Array.to_list |> List.map pprint |> 
           Ustring.fast_concat (us",")) ^. us"|]"
  | TmArrayOp(_,l,op,tms) -> metastr l ^. pprint_array_op op ^. us" " ^.
      (tms |> List.map pprint |> Ustring.fast_concat (us" ")) 
  | TmMapOp(_,l,op,tms) -> metastr l ^. pprint_map_op op ^. us" " ^.
      (tms |> List.map pprint |> Ustring.fast_concat (us" ")) 
  | TmSetOp(_,l,op,tms) -> metastr l ^. pprint_set_op op ^. us" " ^.
      (tms |> List.map pprint |> Ustring.fast_concat (us" ")) 
  | TmDAESolverOp(_,l,op,tms) -> metastr l ^. pprint_daesolver_op op ^. us" " ^.
      (tms |> List.map pprint |> Ustring.fast_concat (us" ")) 
  | TmDpa(t) -> pprint t
  | TmDpb(t) -> pprint t
  | TmError(fi,l,t) -> metastr l ^. us"error " ^. pprint t


(** Free pattern variables in model patterns *)
let rec fpv_mpat p = 
  match p with
  | MPatUk(_,_) -> VarSet.empty
  | MPatModApp(_,x,y) -> VarSet.singleton x |> VarSet.add y
  | MPatModIfGuard(_,x) -> VarSet.singleton x
  | MPatModIfThen(_,x) -> VarSet.singleton x
  | MPatModIfElse(_,x) -> VarSet.singleton x
  | MPatModEqual(_,x,y) -> VarSet.singleton x |> VarSet.add y
  | MPatModProj(_,x,y) -> VarSet.singleton x |> VarSet.add y
  | MPatVal(_,x,_) -> VarSet.singleton x

(** Free pattern variables in patterns *)
let rec fpv_pat p =
  match p with
  | PatVar(_,x) -> VarSet.singleton(x)
  | PatExpr(_,t) -> VarSet.empty
  | PatUk(_,ty) -> VarSet.empty
  | PatModApp(_,p1,p2) -> VarSet.union (fpv_pat p1) (fpv_pat p2)
  | PatModIf(_,p1,p2,p3) -> 
      VarSet.union (fpv_pat p1) (VarSet.union (fpv_pat p2) (fpv_pat p3))
  | PatModEqual(_,p1,p2) -> VarSet.union (fpv_pat p1) (fpv_pat p2)
  | PatModProj(_,p1,p2) -> VarSet.union (fpv_pat p1) (fpv_pat p2)
  | PatModVal(_,x,ty) -> VarSet.singleton(x)
  | PatCons(_,p1,p2) -> VarSet.union (fpv_pat p1) (fpv_pat p2)
  | PatNil(_) -> VarSet.empty
  | PatTuple(_,ps) -> 
      ps |> List.map fpv_pat |> List.fold_left VarSet.union VarSet.empty
  | PatWildcard(_) -> VarSet.empty

(** Free variables in patterns *)
let rec fv_pat p =
  match p with
  | PatVar(_,x) -> VarSet.empty
  | PatExpr(_,t) -> fv_tm t
  | PatUk(_,ty) -> VarSet.empty
  | PatModApp(_,p1,p2) -> VarSet.union (fv_pat p1) (fv_pat p2)
  | PatModIf(_,p1,p2,p3) -> 
      VarSet.union (fv_pat p1) (VarSet.union (fv_pat p2) (fv_pat p3))
  | PatModEqual(_,p1,p2) -> VarSet.union (fv_pat p1) (fv_pat p2)
  | PatModProj(_,p1,p2) -> VarSet.union (fv_pat p1) (fv_pat p2)
  | PatModVal(_,x,ty) -> VarSet.empty
  | PatCons(_,p1,p2) -> VarSet.union (fv_pat p1) (fv_pat p2)
  | PatNil(_) -> VarSet.empty
  | PatTuple(_,ps) -> 
      ps |> List.map fv_pat |> List.fold_left VarSet.union VarSet.empty
  | PatWildcard(_) -> VarSet.empty

(** Free variables in a pattern case *)
and fv_patcases cases =
  match cases with 
    | PCase(fi2,ps,t1op,mlst,t2)::qs ->
        let fv_ps = ps |> List.map fv_pat 
                       |> List.fold_left VarSet.union VarSet.empty in 
	let fpv_ps = ps |> List.map fpv_pat 
                        |> List.fold_left VarSet.union VarSet.empty in 
        let fv_t1op = match t1op with Some t -> fv_tm t | None -> 
          VarSet.empty in
        let fv_tn = VarSet.diff (VarSet.union fv_t1op (fv_tm t2)) fpv_ps in
        let fv_case = VarSet.union fv_tn fv_ps in
	VarSet.union fv_case (fv_patcases qs)
    | [] -> VarSet.empty

(** Free variables in a term *)
and fv_tm t = 
  match t with
    | TmVar(fi,x) -> VarSet.singleton(x)
    | TmLam(fi,l,y,ty,t) -> VarSet.diff (fv_tm t) (VarSet.singleton y)
    | TmApp(fi,l,t1,t2,_) -> VarSet.union (fv_tm t1) (fv_tm t2)
    | TmFix(fi,l,t) -> fv_tm t
    | TmLet(fi,l,y,_,_,t1,t2,_) -> VarSet.union (fv_tm t1) 
	(VarSet.diff (fv_tm t2) (VarSet.singleton y))
    | TmIf(fi,l,t1,t2,t3) -> 
	VarSet.union (fv_tm t1) (VarSet.union (fv_tm t2) (fv_tm t3))
    | TmConst(fi,_,_) -> VarSet.empty
    | TmUp(fi,_,t) -> fv_tm t
    | TmDown(fi,_,t) -> fv_tm t
    | TmBracket(fi,t) -> fv_tm t
    | TmEscape(fi,t) -> fv_tm t
    | TmList(fi,l,tms) ->
        tms |> List.map fv_tm |> List.fold_left VarSet.union VarSet.empty
    | TmMatch(fi,l,t,cases) -> 
	VarSet.union (fv_tm t) (fv_patcases cases)
    | TmUk(fi,_,_,_) -> VarSet.empty       
    | TmNu(fi,_,_,_,t) -> fv_tm t
    | TmModApp(fi,l,t1,t2) -> VarSet.union (fv_tm t1) (fv_tm t2)
    | TmModIf(fi,l,t1,t2,t3) -> 
	VarSet.union (fv_tm t1) (VarSet.union (fv_tm t2) (fv_tm t3))
    | TmModEqual(fi,l,t1,t2) -> VarSet.union (fv_tm t1) (fv_tm t2)
    | TmModProj(fi,l,i,t1) -> fv_tm t1
    | TmVal(fi,_,t,_) -> fv_tm t
    | TmDecon(fi,l,t1,p,t2,t3) -> 
	let fv_t = VarSet.union (fv_tm t1) 
          (VarSet.union (fv_tm t2) (fv_tm t3)) in
	VarSet.diff fv_t (fpv_mpat p) 
    | TmEqual(fi,l,t1,t2) -> VarSet.union (fv_tm t1) (fv_tm t2)
    | TmLcase(fi,l,t,x,y,t1,t2) -> 
	let fv_tt = VarSet.union (fv_tm t1) (fv_tm t2) in
        let fv_tt' = VarSet.diff fv_tt (VarSet.singleton x |> VarSet.add y) in
        VarSet.union (fv_tm t) fv_tt'
    | TmCons(fi,l,t1,t2) -> VarSet.union (fv_tm t1) (fv_tm t2)
    | TmNil(fi,_,_) -> VarSet.empty
    | TmTuple(fi,l,ts) -> 
	ts |> List.map fv_tm |> List.fold_left VarSet.union VarSet.empty
    | TmProj(fi,l,i,t1) -> fv_tm t1
    | TmArray(fi,l,tms) -> tms |> Array.to_list 
        |> List.map fv_tm |> List.fold_left VarSet.union VarSet.empty
    | TmArrayOp(fi,l,op,tms) ->  
        tms |> List.map fv_tm |> List.fold_left VarSet.union VarSet.empty
    | TmMapOp(fi,l,op,tms) -> 
        tms |> List.map fv_tm |> List.fold_left VarSet.union VarSet.empty
    | TmSetOp(fi,l,op,tms) -> 
        tms |> List.map fv_tm |> List.fold_left VarSet.union VarSet.empty
    | TmDAESolverOp(fi,l,op,tms) -> 
        tms |> List.map fv_tm |> List.fold_left VarSet.union VarSet.empty
    | TmDpa(t) -> fv_tm t
    | TmDpb(t) -> fv_tm t
    | TmError(fi,l,t) -> fv_tm t

(** Function [free x t] returns true if [x] is free in term [x], else false *)
let freein_tm x t = 
  VarSet.mem x (fv_tm t) 


(** Create an arrow type from a let parameter list *)
let rec mk_lettype plst l endty =
  match plst with
    | [] -> endty
    | (x,ty)::res -> TyArrow(NoInfo,l,ty,mk_lettype res l endty)

let mk_arrayop fi sid =
  let s = Symtbl.get sid in
  match Ustring.to_latin1 s with
  | "length" -> ArrayOpLength
  | "make" -> ArrayOpMake
  | "get" -> ArrayOpGet
  | "set" -> ArrayOpSet
  | _ -> raise (Mkl_lex_error (LEX_UNKNOWN_FUNCTION,ERROR, fi, [s])) 

let mk_mapop fi sid = 
  let s = Symtbl.get sid in
  match Ustring.to_latin1 s with
  | "size" -> MapOpSize
  | "empty" -> MapOpEmpty
  | "add" -> MapOpAdd
  | "find" -> MapOpFind
  | "mem" -> MapOpMem
  | "remove" -> MapOpRemove
  | "toList" -> MapOpToList
  | _ -> raise (Mkl_lex_error (LEX_UNKNOWN_FUNCTION,ERROR, fi, [s])) 

let mk_setop fi sid = 
  let s = Symtbl.get sid in
  match Ustring.to_latin1 s with
  | "size" -> SetOpSize
  | "empty" -> SetOpEmpty
  | "add" -> SetOpAdd
  | "mem" -> SetOpMem
  | "remove" -> SetOpRemove
  | "toList" -> SetOpToList
  | _ -> raise (Mkl_lex_error (LEX_UNKNOWN_FUNCTION,ERROR, fi, [s])) 


let mk_daesolverop fi sid = 
  let s = Symtbl.get sid in
  match Ustring.to_latin1 s with
  | "make" -> DAESolverOpMake
  | "makehybrid" -> DAESolverOpMakeHybrid
  | "step" -> DAESolverOpStep
  | "reinit" -> DAESolverOpReinit
  | "close" -> DAESolverOpClose
  | "roots" -> DAESolverOpRoots
  | _ -> raise (Mkl_lex_error (LEX_UNKNOWN_FUNCTION,ERROR, fi, [s])) 


type 'a tokendata = {i:info; l:int; v:'a}
