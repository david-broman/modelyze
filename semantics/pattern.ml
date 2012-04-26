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


let var_count = ref 0

let fresh_var() = 
  incr var_count;
  Symtbl.add (us"@fresh" ^. ustring_of_int (!var_count - 1))   

let rec fresh_var_list n =
  if n == 0 then [] else (fresh_var())::(fresh_var_list (n-1))

let mktm_and_equal fi l t t1 t2 =
  let candop = TmConst(fi,l,ConstPrim(PrimBoolAnd,[])) in
  let capp t1 t2 = TmApp(fi,l,t1,t2,false) in
  let cand t1 t2 = capp (capp candop t1) t2 in
  let cequal t1 t2 = TmEqual(fi,l,t1,t2) in
    cand t (cequal t1 t2) 

let tm_true l = TmConst(NoInfo,l,ConstBool(true))

let is_var patcase = 
  match patcase with
    | PCase(_,PatVar(_,_,_)::ps,_,_,_) -> true
    | PCase(_,PatWildcard(_)::ps,_,_,_) -> true
    | _ -> false 

let rec subst_var_pat (x:int) (y:int) (p:pat)   =
  match p with 
  | PatVar(fi,x,_) as tt -> tt
  | PatExpr(fi,t) -> PatExpr(fi,subst_var x y t) 
  | PatUk(fi,ty) as tt -> tt 
  | PatModApp(fi,p1,p2) -> 
      PatModApp(fi,subst_var_pat x y p1,subst_var_pat x y p2)
  | PatModIf(fi,p1,p2,p3) -> 
      PatModIf(fi,subst_var_pat x y p1,subst_var_pat x y p2,
	       subst_var_pat x y p3)
  | PatModEqual(fi,p1,p2) -> 
      PatModEqual(fi,subst_var_pat x y p1,subst_var_pat x y p2)
  | PatModProj(fi,p1,p2) -> 
      PatModProj(fi,subst_var_pat x y p1,subst_var_pat x y p2)
  | PatModVal(fi,x,ty) -> PatModVal(fi,x,ty)
  | PatCons(fi,p1,p2) -> 
      PatCons(fi,subst_var_pat x y p1,subst_var_pat x y p2)
  | PatNil(fi) as tt -> tt
  | PatTuple(fi,ps) -> PatTuple(fi,List.map (subst_var_pat x y) ps)
  | PatWildcard(fi) as tt -> tt

and subst_var_pateqs x y eqs =
  eqs |> List.map (fun (PCase(fi,patlst,op_tm,mlst,e)) -> 
	let freevar  = patlst |> List.map fpv_pat |> 
	   List.fold_left VarSet.union VarSet.empty in
	let dosubst = not (VarSet.mem x freevar) in 
	PCase(fi,List.map (subst_var_pat x y) patlst, 
	 (if dosubst then map_option (subst_var x y) op_tm else op_tm), mlst,
         (if dosubst then subst_var x y e else e)))

and subst_var_mtrans x y mtrans = 
   match mtrans with 
     | VTransExpr(fi,z,t) -> 
         VTransExpr(fi,(if z = x then y else z),(subst_var x y t)) 
     | VTransModUk(fi,z,ty) ->  VTransModUk(fi,(if z = x then y else z),ty)
     | VTransModVal(fi,z1,z2,ty) -> 
         VTransModVal(fi,(if z1 = x then y else z1),
                      (if z2 = x then y else z2),ty)

and subst_var (x:int) (y:int) (tm:tm)  =
    match tm with
      | TmVar(fi,z) as tt -> if x = z then TmVar(fi,y) else tt
      | TmLam(fi,l,z,ty1,t2) as tt -> 
	  if x = z then tt else TmLam(fi,l,z,ty1,subst_var x y t2)
      | TmApp(fi,l,t1,t2,fs) -> TmApp(fi,l,subst_var x y t1, subst_var x y t2,fs)
      | TmFix(fi,l,t) -> TmFix(fi,l,subst_var x y t)  
      | TmLet(fi,l,z,ty,plst,t1,t2,recu) ->
          let in_p = List.exists (fun (p,_) -> p = x) plst in 
	  TmLet(fi,l,z,ty,plst,
                (if in_p || (x = z && recu) then t1 else subst_var x y t1),
		(if x = z then t2 else subst_var x y t2),recu)
      | TmIf(fi,l,t1,t2,t3) -> 
	  TmIf(fi,l,subst_var x y t1,subst_var x y t2,subst_var x y t3)
      | TmConst(fi,l,c) as tt -> tt
      | TmList(fi,l,tms) -> TmList(fi,l,List.map (subst_var x y) tms)
      | TmMatch(fi,l,t,eqs) -> 
	  TmMatch(fi,l,subst_var x y t,(subst_var_pateqs x y eqs)) 
      | TmSym(fi,l,x,ty) as tt -> tt
      | TmNu(fi,l,z,ty,t) -> TmNu(fi,l,z,ty,subst_var x y t) 
      | TmSymApp(fi,l,t1,t2) ->
	  TmSymApp(fi,l,subst_var x y t1,subst_var x y t2)
      | TmLift(fi,l,t,ty) -> TmLift(fi,l,subst_var x y t,ty)
      | TmCase(fi,l,t1,p,t2,t3) ->
	  let dosubst = not (VarSet.mem x (fpv_mpat p)) in
	  TmCase(fi,l,subst_var x y t1,p,
		  (if dosubst then subst_var x y t2 else t2),
		  (if dosubst then subst_var x y t3 else t3))
      | TmEqual(fi,l,t1,t2) -> 
	  TmEqual(fi,l,subst_var x y t1,subst_var x y t2)
      | TmLcase(fi,l,t,id1,id2,t1,t2) -> 
	  TmLcase(fi,l,t,id1,id2,
		  (if not (x = id1 && x = id2) then subst_var x y t1 else t1),
		  subst_var x y t2) 
      | TmCons(fi,l,t1,t2) -> 
	  TmCons(fi,l,subst_var x y t1,subst_var x y t2)
      | TmNil(fi,l,ty) as tt  -> tt
      | TmTuple(fi,l,ts) -> TmTuple(fi,l,List.map (subst_var x y) ts)
      | TmProj(fi,l,i,t) -> TmProj(fi,l,i,subst_var x y t)
      | TmArray(fi,l,ts) -> TmArray(fi,l,Array.map (subst_var x y) ts)
      | TmArrayOp(fi,l,op,ts) -> TmArrayOp(fi,l,op,List.map (subst_var x y) ts)
      | TmMapOp(fi,l,op,ts) -> TmMapOp(fi,l,op,List.map (subst_var x y) ts)
      | TmSetOp(fi,l,op,ts) -> TmSetOp(fi,l,op,List.map (subst_var x y) ts)
      | TmDAESolverOp(fi,l,op,ts) -> 
          TmDAESolverOp(fi,l,op,List.map (subst_var x y) ts)
      | TmDPrint(t) -> TmDPrint(subst_var x y t) 
      | TmDPrintType(t) -> TmDPrintType(subst_var x y t)
      | TmError(fi,l,t) -> TmError(fi,l,subst_var x y t)


let rec partition f lst =
  let tack x xss = 
    match xss with
      | (xs::xss) -> (x::xs) :: xss
      | _ -> assert false  
  in
    match lst with
    | [] -> []
    | [x] -> [[x]]
    | x::x'::xs ->
	if f x = f x' then tack x (partition f (x'::xs))
	else [x] :: partition f (x'::xs)

let eqs_info eqs = 
  match eqs with 
    | PCase(fi1,_,_,_,_)::_ -> 
        let PCase(fi2,_,_,_,_) = last eqs in mkinfo fi1 fi2
    | _ -> NoInfo

let rec dsmatch_var l u us eqs default =
  let eqs' = filtermap (function 
     | PCase(fi,PatVar(_,id,_)::ps,g,mtrans,e) ->
	 Some (PCase(fi,ps,map_option (subst_var id u) g,
         List.map (subst_var_mtrans id u) mtrans,subst_var id u e))
     | PCase(fi,PatWildcard(_)::ps,g,mlst,e) -> Some (PCase(fi,ps,g,mlst,e)) 
     | _ -> None) eqs
  in dsmatch l us eqs' default

and dsmatch_list l u us eqs default =
  let eqs_cons = filtermap (function
	| PCase(fi,PatCons(fi2,p1,p2)::ps,g,mlst,e) -> 
            Some(PCase(fi,p1::p2::ps,g,mlst,e))
	| _ -> None) eqs in
  let eqs_nil = filtermap (function
	| PCase(fi,PatNil(fi2)::ps,g,mlst,e) -> Some(PCase(fi2,ps,g,mlst,e))
	| _ -> None) eqs in 
  let fi = eqs_info eqs in
  if List.length eqs_cons + List.length eqs_nil <> List.length eqs then
    raise (Mkl_static_error(PATMATCH_MIXING_PATTERN_TYPES,ERROR,fi,[])) 
  else
  let x1 = fresh_var() in
  let x2 = fresh_var() in
  let t1 = dsmatch l (x1::x2::us) eqs_cons default in
  let t2 = dsmatch l us eqs_nil default in
    TmLcase(fi,l,TmVar(fi,u),x1,x2,t1,t2)

and dsmatch_tuple l u us eqs default len =
  let eqs_tuple = filtermap (function
	| PCase(fi,PatTuple(fi2,ps2)::ps,g,mlst,e) 
            when List.length ps2 = len -> Some(PCase(fi,ps2@ps,g,mlst,e))
	| _ -> None) eqs in
  let fi = eqs_info eqs in
  if List.length eqs_tuple <> List.length eqs then
    raise (Mkl_static_error(PATMATCH_MIXING_PATTERN_TYPES,ERROR,fi,[])) 
  else
  let xn = fresh_var_list len in
  let rec mkproj vars n = 
    match vars with
      | x::xs -> 
	  TmLet(fi,l,x,None,[],TmProj(fi,l,n,TmVar(fi,u)),mkproj xs (n+1),false)
      | [] -> dsmatch l (xn@us) eqs_tuple default 
  in mkproj xn 0

and partion_patmodel eqs =
  let rec sep eqs (pmapp,pmif,pmeql,pmproj) =
    match eqs with
      | PCase(fi,PatModApp(fi2,p1,p2)::ps,g,vtrans,e)::qs -> 
	  sep qs (PCase(fi,p1::p2::ps,g,vtrans,e)::pmapp,pmif,pmeql,pmproj) 
      | PCase(fi,PatModIf(fi2,p1,p2,p3)::ps,g,vtrans,e)::qs -> 
          failwith "Patten matching of <if> is not yet implemented"
      | PCase(fi,PatModEqual(fi2,p1,p2)::ps,g,vtrans,e)::qs -> 
	  sep qs (pmapp,pmif,PCase(fi,p1::p2::ps,g,vtrans,e)::pmeql,pmproj)
      | PCase(fi,PatModProj(fi2,p1,p2)::ps,g,vtrans,e)::qs -> 
	  sep qs (pmapp,pmif,pmeql,PCase(fi,p1::p2::ps,g,vtrans,e)::pmproj)
      | PCase(fi,_,_,_,_)::qs -> 
          raise (Mkl_static_error(PATMATCH_MIXING_PATTERN_TYPES,ERROR,fi,[]))   
      | [] -> (List.rev pmapp,List.rev pmif,List.rev pmeql,List.rev pmproj)
  in sep eqs ([],[],[],[])

and dsmatch_model l u us eqs default =
    let (qsapp,qsif,qseql,qsproj) = partion_patmodel eqs in
    let mk_mod eqs mpatcon def =
      if List.length eqs = 0 then def else 
        let fi = eqs_info eqs in
        let (x1,x2) = (fresh_var(), fresh_var ()) in
        let tm1 = dsmatch l (x1::x2::us) eqs def in
          TmCase(fi,l,TmVar(fi,u),mpatcon fi x1 x2,tm1,def) 
    in
      mk_mod qsapp (fun fi x1 x2 -> MPatModApp(fi,x1,x2)) 
        (mk_mod qseql (fun fi x1 x2 -> MPatModEqual(fi,x1,x2))
          (mk_mod qsproj (fun fi x1 x2 -> MPatModProj(fi,x1,x2)) default))

and dsmatch_pat l uvars eqs default = 
  match uvars,eqs with 
    | u::us,(PCase(_,p::ps,g,mlst,e))::qs ->
        (match p with
	  | PatVar(_,_,_) | PatWildcard(_) -> dsmatch_var l u us eqs default
          | PatCons(_,_,_) | PatNil(_) -> dsmatch_list l u us eqs default 
          | PatTuple(_,ps) -> dsmatch_tuple l u us eqs default (List.length ps)
          | PatModApp(_,_,_) | PatModIf(_,_,_,_) |
            PatModEqual(_,_,_) | PatModProj(_,_,_) -> 
              dsmatch_model l u us eqs default 
          | PatModVal(_,_,_) | PatUk(_,_) | PatExpr(_,_) -> 
              assert false (* Already removed at an earlier desugar phase *) )          
    | _,_ -> assert false
        
and generate_conditional fi l vtranss t1op e default =
  let rec collapse vtranss (btm,vts) = 
      match vtranss with
        | VTransExpr(fi,x,t)::vs -> 
            collapse vs ((mktm_and_equal fi l btm (TmVar(fi,x)) t),vts)       
        | (VTransModUk(fi,x,ty) as v)::vs -> collapse vs (btm,v::vts)
        | (VTransModVal(fi,x,y,ty) as v)::vs -> collapse vs (btm,v::vts)
        | [] -> (btm,List.rev vts)
  in
  let (btm,vst) = (match t1op with 
    | Some t -> collapse vtranss (t,[])
    | None -> collapse vtranss (tm_true l,[])) in
  let (deffun,defval) = 
    let x = Symtbl.add (us"@gencond") in
    let y = Symtbl.add (us"@genconempty") in
    if List.length vst + (if btm = tm_true l then 1 else 0) <= 1 
    then ((fun e -> e),default)
    else ((fun e -> TmLet(fi,l,x,None,[],TmLam(fi,l,y,TyUnit(fi,l),default),e,false)),
          TmApp(fi,l,TmVar(fi,x),TmConst(fi,l,ConstUnit),false))
  in 
  let e' =  if btm = tm_true l then e else TmIf(fi,l,btm,e,defval) in
  let e'' = List.fold_right 
    (fun vt e -> match vt with
       | VTransExpr(_,_,_) -> assert false
       | VTransModUk(fi,u,ty) -> 
            TmCase(fi,l,TmVar(fi,u),MPatUk(fi,ty),e,defval)
       | VTransModVal(fi,u,x,ty) ->
            TmCase(fi,l,TmVar(fi,u),MPatVal(fi,x,ty),e,defval)) vst e' in
  deffun e''

and dsmatch l uvars eqs default  = 
  match uvars,eqs with
    | [],(PCase(fi,[],t1op,vtrans,e)::xs) -> 
        generate_conditional fi l vtrans t1op e (dsmatch l [] xs default)
    | [], _::(PCase(fi,_,_,_,_))::_ -> 
	raise (Mkl_static_error(PATMATCH_UNUSED_PATTERN,ERROR,fi,[])) 
    | _,_ -> 
	List.fold_right (dsmatch_pat l uvars)  (partition is_var eqs) default 

let rec trans_pat_varexpr vtrans p =
  match p with 
  | PatVar(fi,x,_) as pp -> (vtrans,pp)
  | PatExpr(fi,t) ->       
      let x = fresh_var() in 
        (VTransExpr(fi,x,desugar t)::vtrans,PatVar(fi,x,false))
  | PatUk(fi,ty) ->  
      let x = fresh_var() in (VTransModUk(fi,x,ty)::vtrans,PatVar(fi,x,false))
  | PatModApp(fi,p1,p2) ->
      let (vtrans1,p1') = trans_pat_varexpr vtrans p1 in
      let (vtrans2,p2') = trans_pat_varexpr vtrans1 p2 in
      (vtrans2,PatModApp(fi,p1',p2'))
  | PatModIf(fi,p1,p2,p3) -> 
      let (vtrans1,p1') = trans_pat_varexpr vtrans p1 in
      let (vtrans2,p2') = trans_pat_varexpr vtrans1 p2 in
      let (vtrans3,p3') = trans_pat_varexpr vtrans2 p3 in
      (vtrans3,PatModIf(fi,p1',p2',p3'))
  | PatModEqual(fi,p1,p2) -> 
      let (vtrans1,p1') = trans_pat_varexpr vtrans p1 in
      let (vtrans2,p2') = trans_pat_varexpr vtrans1 p2 in
      (vtrans2,PatModEqual(fi,p1',p2'))
  | PatModProj(fi,p1,p2) -> 
      let (vtrans1,p1') = trans_pat_varexpr vtrans p1 in
      let (vtrans2,p2') = trans_pat_varexpr vtrans1 p2 in
      (vtrans2,PatModProj(fi,p1',p2'))
  | PatModVal(fi,y,ty) ->  
      let x = fresh_var() in (VTransModVal(fi,x,y,ty)::vtrans,PatVar(fi,x,false))
  | PatCons(fi,p1,p2) -> 
      let (vtrans1,p1') = trans_pat_varexpr vtrans p1 in
      let (vtrans2,p2') = trans_pat_varexpr vtrans1 p2 in
      (vtrans2,PatCons(fi,p1',p2'))
  | PatNil(fi) as pp -> (vtrans,pp)
  | PatTuple(fi,ps) ->
      let (vtransn,ps') = 
	ps |> List.fold_left 
	    (fun (vtrans,ps) p -> 
               let (vtrans',p') = 
                 trans_pat_varexpr vtrans p in (vtrans',p'::ps))
	    (vtrans,[])
      in (vtransn,PatTuple(fi,List.rev ps'))
  | PatWildcard(fi) as pp -> (vtrans,pp)

and trans_patcases_vartrans cases =
  let rec trans_cases cases acc =
      match cases with
        | (PCase(fi,[p],t1op,_,t2))::cs ->
            let (vtrans,p') = trans_pat_varexpr [] p in
            trans_cases cs 
              (PCase(fi,[p'],map_option desugar t1op,vtrans,desugar t2)::acc)
        | [] -> (List.rev acc)
        | _ -> assert false
  in trans_cases cases []


and desugar_match fi l t cases  =
  let errdef = 
    TmError(fi,l,TmConst(fi,l,ConstString(us"Pattern match error"))) in
  let x = fresh_var() in
  let cases' = trans_patcases_vartrans cases in
    TmLet(fi,l,x,None,[],t,dsmatch l [x] cases' errdef,false)  


and desugar tm =
  let rec ds tm =
    match tm with
      | TmVar(fi,x) as tt -> tt 
      | TmLam(fi,l,x,ty1,t2) -> TmLam(fi,l,x,ty1,ds t2)
      | TmApp(fi,l,t1,t2,fs) -> TmApp(fi,l,ds t1,ds t2,fs) 
      | TmFix(fi,l,t) -> TmFix(fi,l,ds t) 
      | TmLet(fi,l,x,ty,plst,t1,t2,recu) ->
          TmLet(fi,l,x,ty,plst,ds t1,ds t2,recu)
      | TmIf(fi,l,t1,t2,t3) -> TmIf(fi,l,ds t1,ds t2,ds t3) 
      | TmConst(fi,l,c) -> TmConst(fi,l,c)
      | TmList(fi,l,tms) -> TmList(fi,l,List.map ds tms)
      | TmMatch(fi,l,t,cases) -> 
          desugar_match fi l (ds t) cases 
      | TmSym(fi,l,x,ty) -> TmSym(fi,l,x,ty) 
      | TmNu(fi,l,x,ty,t) -> TmNu(fi,l,x,ty,ds t)
      | TmSymApp(fi,l,t1,t2) -> TmSymApp(fi,l,ds t1,ds t2)
      | TmLift(fi,l,t,ty) -> TmLift(fi,l,ds t,ty)
      | TmCase(fi,l,t1,p,t2,t3) -> TmCase(fi,l,ds t1,p,ds t2,ds t3)
      | TmEqual(fi,l,t1,t2) -> TmEqual(fi,l,ds t1,ds t2)
      | TmLcase(fi,l,t,id1,id2,t1,t2) -> 
	  TmLcase(fi,l,ds t,id1,id2,ds t1,ds t2)
      | TmCons(fi,l,t1,t2) -> TmCons(fi,l,ds t1,ds t2)
      | TmTuple(fi,l,ts) -> TmTuple(fi,l,List.map ds ts)
      | TmProj(fi,l,i,t) -> TmProj(fi,l,i,ds t) 
      | TmNil(fi,l,ty) -> TmNil(fi,l,ty)
      | TmArray(fi,l,ts) -> TmArray(fi,l,Array.map ds ts)
      | TmArrayOp(fi,l,op,ts) -> TmArrayOp(fi,l,op,List.map ds ts)
      | TmMapOp(fi,l,op,ts) -> TmMapOp(fi,l,op,List.map ds ts)
      | TmSetOp(fi,l,op,ts) -> TmSetOp(fi,l,op,List.map ds ts)
      | TmDAESolverOp(fi,l,op,ts) -> TmDAESolverOp(fi,l,op,List.map ds ts)
      | TmDPrint(t) -> TmDPrint(ds t)
      | TmDPrintType(t) -> TmDPrintType(ds t)
      | TmError(fi,l,t) -> TmError(fi,l,ds t)
  in 
    ds tm


