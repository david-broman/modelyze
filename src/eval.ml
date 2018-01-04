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
open Printf
open Info
open Evalast
open Debugprint

exception Cannot_eval

(* State when two terms are equal using operator <==>.
   Note that comparing variables, closures and fix terms always return false *)
let rec tm_equiv t1 t2 =
  match t1,t2 with
  | TmConst(c1),TmConst(c2) -> c1 = c2
  | TmSym(s1,_),TmSym(s2,_) -> s1 = s2
  | TmSymApp(t1a,t1b),TmSymApp(t2a,t2b) -> tm_equiv t1a t2a && tm_equiv t1b t2b
  | TmLift(t1,ty1),TmLift(t2,ty2) -> tm_equiv t1 t2
  | TmCons(t1a,t1b),TmCons(t2a,t2b) -> tm_equiv t1a t2a && tm_equiv t1b t2b
  | TmNil,TmNil -> true
  | TmTuple(ts1),TmTuple(ts2) ->
       List.length ts1 = List.length ts2 && List.for_all2 tm_equiv ts1 ts2
  | TmArray(ta1),TmArray(ta2) -> (
      if Array.length ta1 <> Array.length ta2 then false else
        let (i,eq) = (ref 0,ref true) in
        while !i < Array.length ta1 && !eq do
          if ta1.(!i) <> ta2.(!i) then eq := false;
          i := !i + 1;
        done;
        !eq)
  | TmMap(i1,s1),TmMap(i2,s2) ->
      i1 = i2 && (PMap.foldi (fun k v eq -> eq &&
                     (try PMap.find k s2 = v with _ -> false)) s1 true)
  | TmSet(i1,s1),TmSet(i2,s2) ->
      i1 = i2 && (PMap.foldi (fun k v eq -> eq && PMap.mem k s2) s1 true)
  | _ -> false


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
  | _ -> TmArrayOp(op, array_lst)


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
  | _ -> TmMapOp(op,arg_lst)

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
  | _ -> TmSetOp(op,arg_lst)

let from_tm ar =
  let ar' = Array.map (fun t -> match t with TmConst(Ast.ConstReal(r)) -> r
                                           | _ -> assert false) ar in
  Sundials.RealArray.of_array ar'

let from_realArray ar =
  let ar' = Sundials.RealArray.to_array ar in
  Array.map (fun r -> TmConst(Ast.ConstReal(r))) ar'

let into_tm ar artm =
  Sundials.RealArray.iteri (fun i r -> artm.(i) <- TmConst(Ast.ConstReal(r))) ar

let bigarray_from_tmlist tm rr =
  let rec worker tm =
    match tm with
      | TmCons(TmConst(Ast.ConstReal(r)),ts) -> r::(worker ts)
      | TmNil -> []
      | _ -> raise Cannot_eval
  in List.iteri (fun i e -> rr.{i} <- e) (worker tm)

let eval_daesolver_op eval op arg_lst =
    (* Residual and root finder functions *)
    let resrootfun tmres time yy yp rr =
      let tmtime = TmConst(Ast.ConstReal(time)) in
      let tmyy = TmArray(from_realArray yy) in
      let tmyp = TmArray(from_realArray yp) in
      let lst =
        eval (TmApp(TmApp(TmApp(tmres,tmtime,false),tmyy,false),tmyp,false)) in
        bigarray_from_tmlist lst rr
    in
    let solver_result_to_rccode sr =
      match sr with
      | Ida.Success -> 0
      | Ida.RootsFound -> 1
      | Ida.StopTimeReached -> 2
    in
    match op,arg_lst with
    | Ast.DAESolverOpInit,
      [tmres;TmConst(Ast.ConstReal(t0));TmArray(tm_yy0);TmArray(tm_yp0)] ->
       let resf = resrootfun tmres in
       let yy = Nvector_serial.wrap (from_tm tm_yy0) in
       let yp = Nvector_serial.wrap (from_tm tm_yp0) in
       let st = Ida.(init (Dls.dense ()) (SStolerances (1e-9, 1e-9)) resf t0 yy yp) in
       TmDAESolver(st,yy,yp)

    | Ast.DAESolverOpInitWithRootf,
      [tmres;TmConst(Ast.ConstInt(nroots));tmroot;TmConst(Ast.ConstReal(t0));
       TmArray(tm_yy0);TmArray(tm_yp0)] ->
       let resf = resrootfun tmres in
       let rootf = resrootfun tmroot in
       let yy = Nvector_serial.wrap (from_tm tm_yy0) in
       let yp = Nvector_serial.wrap (from_tm tm_yp0) in
       let st = Ida.(init (Dls.dense ()) (SStolerances (1e-9, 1e-9))
                       resf ~roots:(nroots, rootf) t0 yy yp) in
       TmDAESolver(st,yy,yp)

    | Ast.DAESolverOpCalcICYYYP,
      [TmDAESolver(st,yy,yp);TmArray(tm_varids);TmArray(tm_yyout);TmArray(tm_ypout);
       TmConst(Ast.ConstReal(t0))] ->
       let vids = Nvector_serial.wrap (from_tm tm_varids) in
       Ida.set_suppress_alg st ~varid:vids true;
       Ida.calc_ic_ya_yd' st ~y:yy ~y':yp t0;
       into_tm (Nvector_serial.unwrap yy) tm_yyout;
       into_tm (Nvector_serial.unwrap yp) tm_ypout;
       TmConst(Ast.ConstUnit)

    | Ast.DAESolverOpSolveNormal,
      [TmDAESolver(st,yy,yp);TmConst(Ast.ConstReal(tout));TmArray(tm_yyout);
       TmArray(tm_ypout)] ->
       let tret, r = Ida.solve_normal st tout yy yp in
       let rc = solver_result_to_rccode r in
       into_tm (Nvector_serial.unwrap yy) tm_yyout;
       into_tm (Nvector_serial.unwrap yp) tm_ypout;
       TmTuple([TmConst(Ast.ConstReal(tret));TmConst(Ast.ConstInt(rc))])

    | Ast.DAESolverOpRootInfo, [TmDAESolver(st,yy,yp)] ->
       let nroots = Ida.get_num_roots st in
       let roots = Sundials.Roots.create nroots in
       Ida.get_root_info st roots;
       let a = Array.map Sundials.Roots.int_of_root (Sundials.Roots.to_array roots) in
       TmArray(Array.map (fun e -> TmConst(Ast.ConstInt(e))) a)

  | _ -> TmDAESolverOp(op,arg_lst)


(* Creates two model values from a primitive function constant. Used when
   for example build in +. operator is partially applied with one argument
   and the deconstruct operator of modapp wants to take apart the value *)
let mk_primappvalues prim arg args =
  let c1 = Ast.ConstPrim(prim,args) in
  let ty1 = Ast.deltatype NoInfo c1 0 in
  let v1 = TmLift(TmConst(c1),Translate.trans_ty ty1) in
  let ty2 = match ty1 with Ast.TyArrow(_,_,ty,_) -> ty | _ -> assert false in
  let v2 = TmLift(TmConst(arg),Translate.trans_ty ty2) in
  (v1,v2)



let rec readback syms d tm =
  let rec getidx syms s d =
    match syms with
      | x::xs -> if x = s then d else getidx xs s (d+1)
      | [] -> failwith "Error in readback"
  in
    match tm with
      | TmVar(i) -> tm
      | TmSpecSym(s) -> TmVar(getidx syms s d)
      | TmLam(t) -> TmLam(readback syms (d+1) t)
      | TmClos(t,e,id) -> TmLam(readback syms (d+1) t) (* TODO, update index *)
      | TmByteCode(c,ext,ident,argc) -> tm
      | TmApp(TmApp(TmConst(Ast.ConstPrim(Ast.PrimIntMul,[])),
              TmConst(Ast.ConstInt(1)),_),t1,_) -> readback syms d t1
      | TmApp(TmApp(TmConst(Ast.ConstPrim(Ast.PrimIntMul,[])),t1,_),
              TmConst(Ast.ConstInt(1)),_) -> readback syms d t1
      | TmApp(TmApp(TmConst(Ast.ConstPrim(Ast.PrimIntAdd,[])),
              TmConst(Ast.ConstInt(0)),_),t1,_) -> readback syms d t1
      | TmApp(TmApp(TmConst(Ast.ConstPrim(Ast.PrimIntAdd,[])),t1,_),
              TmConst(Ast.ConstInt(0)),_) -> readback syms d t1
      | TmApp(t1,t2,specialize) ->
          TmApp(readback syms d t1,readback syms d t2,specialize)
      | TmFix(t1) -> TmFix(readback syms d t1)
      | TmIf(t1,t2,t3) ->
          TmIf(readback syms d t1,readback syms d t2,readback syms d t3)
      | TmConst(b) -> tm
      | TmSym(s,ty) -> tm
      | TmGenSym(ty) -> tm
      | TmSymApp(t1,t2) -> TmSymApp(readback syms d t1,readback syms d t2)
      | TmLift(t,ty) -> TmLift(readback syms d t,ty)
      | TmCase(t1,p,t2,t3) -> TmCase(readback syms d t1,p,
                                       readback syms d t2,readback syms d t3)
      | TmEqual(t1,t2) -> TmEqual(readback syms d t1,readback syms d t2)
      | TmLcase(t,t1,t2) -> TmLcase(readback syms d t,readback syms d t1,
                                    readback syms d t2)
      | TmCons(t1,t2) -> TmCons(readback syms d t1,readback syms d t2)
      | TmNil -> tm
      | TmTuple(tms) -> TmTuple(List.map (readback syms d) tms)
      | TmProj(i,t) -> TmProj(i,readback syms d t)
      | TmArray(tms) -> tm
      | TmArrayOp(op,tms) -> TmArrayOp(op,List.map (readback syms d) tms)
      | TmMap(size,tms) -> tm
      | TmMapOp(op,tms) -> TmMapOp(op,List.map (readback syms d) tms)
      | TmSet(size,tms) -> tm
      | TmSetOp(op,tms) -> TmSetOp(op,List.map (readback syms d) tms)
      | TmDAESolver(st,_,_) -> tm
      | TmDAESolverOp(op,tms) -> TmDAESolverOp(op,List.map (readback syms d) tms)
      | TmDPrint(t) -> TmDPrint(readback syms d t)
      | TmDPrintType(t) -> TmDPrintType(readback syms d t)
      | TmSymStr(t) -> TmSymStr(readback syms d t)
      | TmError(fi,t) ->  TmError(fi,readback syms d t)
      | TmDebugId(id,t) -> TmDebugId(id,readback syms d t)
      | TmPEval(t) -> TmPEval(readback syms d t)
      | TmTheta(t) -> TmTheta(readback syms d t)



let rec consistent ty_a ty_b =
  match ty_a,ty_b with
    | ty,TyDyn -> true
    | TyDyn,ty -> true
    | TyBool,TyBool -> true
    | TyInt,TyInt -> true
    | TyReal,TyReal -> true
    | TyString,TyString -> true
    | TyArrow(ty1,ty2), TyArrow(ty3,ty4) ->
        consistent ty1 ty3 && consistent ty2 ty4
    | TyUnit,TyUnit -> true
    | TyList(ty1),TyList(ty2) ->
        consistent ty1 ty2
    | TyTuple(tys1),TyTuple(tys2) ->
        List.for_all2 consistent tys1 tys2
    | TySym(ty1),TySym(ty2) ->
        consistent ty1 ty2
    | TySymData(tyid1),TySymData(tyid2)
        when tyid1 = tyid2 -> true
    | TyArray(ty1),TyArray(ty2) ->
        consistent ty1 ty2
    | TyMap(ty1,ty2), TyMap(ty3,ty4) ->
        consistent ty1 ty3 && consistent ty2 ty4
    | TySet(ty1),TySet(ty2) ->
        consistent ty1 ty2
    | TyDAESolver,TyDAESolver -> true
    | _ , _ ->  false


let symcount = ref 0
let gensym() = incr symcount; !symcount
let funtext = Symtbl.add (us"fun")
type norec = bool

let is_pe_value tm =
    match tm with
    | TmApp(_,_,_) -> false
    | _ -> true



let rec specializeParams t venv syms norec =
    match t with
      | TmLam(t1) ->
          let s = gensym() in
          let ts = TmSpecSym(s) in
            TmLam(specializeParams t1 (ts::venv) (s::syms) norec)
      | _ ->
          let t' = eval venv norec t in
          readback syms 0 t'



and eval venv norec t =
    match t with
      | TmVar(i) ->
          (match List.nth venv i with
             | TmFix(t) as tt -> eval venv norec tt
             | t -> t)
      | TmSpecSym(s) -> TmSpecSym(s)
      | TmLam(t) -> TmClos(t,venv,funtext)
      | TmClos(t,e,id) -> TmClos(t,e,id)
      | TmByteCode(code,extid,ident,args) -> t
      | TmApp(t1,t2,specialize) ->
          (match eval venv norec t1,eval venv norec t2 with
	     | (TmClos(t3,venv2,ident),v2)  ->
               if specialize then (
                   let t3' = specializeParams t3 (v2::venv2) [] norec in
                     (eval venv norec t3') )
                 else
                 eval(v2::venv2) norec t3
             | (TmLam(t3),v2) -> eval(v2::venv) norec t3
	     | (TmConst(c1),TmConst(c2)) -> TmConst(Ast.delta c1 c2)
             | (TmByteCode((co,rc,argc) as code ,extid,ident,args),v2) ->
                  if argc = (List.length args) + 1
                  then Bytecode.run code (v2::args)
                  else TmByteCode(code,extid,ident,v2::args)
	     | t1',t2' -> TmApp(t1',t2',specialize))
      | TmFix(t1) ->
          if norec then t
          else
            (match eval venv norec t1 with
               | TmClos(t2,venv2,_) as tt ->
                   eval(TmFix(tt)::venv2) false t2
               | _ -> TmFix(t1))
      | TmIf(t1,t2,t3) ->
          (match eval venv norec t1 with
	     | TmConst(Ast.ConstBool(b)) ->
                   eval venv norec (if b then t2 else t3)
	     | t1' ->  TmIf(t1',eval venv true t2,eval venv true t3))
      | TmConst(b) -> TmConst(b)
      | TmSym(s,ty) -> TmSym(s,ty)
      | TmGenSym(ty) -> TmSym(gensym(),ty)
      | TmSymApp(t1,t2) -> TmSymApp(eval venv norec t1,eval venv norec t2)
      | TmLift(t,ty) -> TmLift(eval venv norec t,ty)
      | TmCase(t1,p,t2,t3) ->
          (match eval venv norec t1,p with
	     | TmSym(id,ty1),MPatSym(ty2)
		 when ty1 = ty2  ->
                 eval venv norec  t2
	     | TmSymApp(v1,v2),MPatSymApp ->
                 eval(v1::v2::venv) norec t2
	     | TmLift(v1,ty1),MPatLift(ty2) when consistent ty1 ty2 ->
                 eval(v1::venv) norec t2
	     | TmLift(v1,ty1),MPatLift(TyDyn)  ->
                 eval(v1::venv) norec t2
             | TmLift(TmConst(Ast.ConstPrim(prim,arg::args)),ty1),MPatSymApp ->
                 let (v1,v2) = mk_primappvalues prim arg args in
                   eval(v1::v2::venv) norec t2
             | _ -> eval venv norec t3)
      | TmEqual(t1,t2) ->
          TmConst(Ast.ConstBool(tm_equiv (eval venv norec t1)
                                  (eval venv norec t2)))
      | TmLcase(t,t1,t2) ->
          (match eval venv norec t with
             | TmCons(v1,v2) ->
                   eval(v1::v2::venv) norec t1
	     | TmNil -> eval venv norec t2
	     | tt -> TmLcase(t,t1,t2))
      | TmCons(t1,t2) -> TmCons(eval venv norec t1,eval venv norec t2)
      | TmNil -> TmNil
      | TmTuple(tms) -> TmTuple(List.map (eval venv norec) tms)
      | TmProj(i,t) ->
          (match eval venv norec t with
	       | TmTuple(tms) -> List.nth tms i
	       | _ -> TmProj(i,t))
      | TmArray(tms) -> TmArray(Array.map (eval venv norec) tms)
      | TmArrayOp(op,tms) -> eval_array_op op (List.map (eval venv norec) tms)
      | TmMap(size,tms) -> TmMap(size,tms)
      | TmMapOp(op,tms) -> eval_map_op op (List.map (eval venv norec) tms)
      | TmSet(size,tms) -> TmSet(size,tms)
      | TmSetOp(op,tms) -> eval_set_op op (List.map (eval venv norec) tms)
      | TmDAESolver(st,yy,yp) -> TmDAESolver(st,yy,yp)
      | TmDAESolverOp(op,tms) ->
          eval_daesolver_op (eval venv norec) op (List.map (eval venv norec) tms)
      | TmDPrint(t) -> let t' = eval venv norec t  in
	  pprint t' |> uprint_endline; t'
      | TmDPrintType(t) -> us"[Printing types is not supported]"
                           |> uprint_endline; eval venv norec t
      | TmSymStr(t) -> let t' = eval venv norec t  in
            TmConst(Ast.ConstString(Debugprint.getDebugSymId t'))
      | TmError(fi,t) ->
	  (match eval venv norec t with
	     | TmConst(Ast.ConstString(s)) ->
		 raise (Ast.Mkl_runtime_error (Message.RUNTIME_ERROR,
					       Message.ERROR, fi, [s]))
	     | _ -> assert false)
      | TmDebugId(id,t) ->
            let t'= eval venv norec t in
              debugTagTm id t'
      | TmPEval(t) ->
         (match eval venv norec t with
           | TmClos(t2,venv2,ident) ->
              specializeParams (TmLam(t2)) venv2 [] norec
           | t -> t)
      | TmTheta(t) -> failwith "TODO Theta"



let evaluate t = eval [] false t
