type level = int
type index = int
type sym = int
type typeid = int
type specialize = bool
type argc = int
type code = int
type bcode = code list * float list * argc
type apiid = int ref
type uenv = (int * int) list
type env = tm list
and ty =
    TyBool
  | TyInt
  | TyReal
  | TyString
  | TyArrow of ty * ty
  | TyUnit
  | TyList of ty
  | TyTuple of ty list
  | TySym of ty
  | TyDyn
  | TySymData of typeid
  | TyArray of ty
  | TyMap of ty * ty
  | TySet of ty
  | TyDAESolver
and tm =
    TmVar of index
  | TmSpecSym of sym
  | TmLam of tm
  | TmClos of tm * env * Ast.ident
  | TmByteCode of bcode * apiid * Ast.ident * tm list
  | TmApp of tm * tm * specialize
  | TmFix of tm
  | TmIf of tm * tm * tm
  | TmConst of Ast.const
  | TmSym of sym * ty
  | TmGenSym of ty
  | TmSymApp of tm * tm
  | TmLift of tm * ty
  | TmCase of tm * mpat * tm * tm
  | TmEqual of tm * tm
  | TmLcase of tm * tm * tm
  | TmCons of tm * tm
  | TmNil
  | TmTuple of tm list
  | TmProj of int * tm
  | TmArray of tm array
  | TmArrayOp of Ast.arrayop * tm list
  | TmMap of int * (tm, tm) PMap.t
  | TmMapOp of Ast.mapop * tm list
  | TmSet of int * (tm, unit) PMap.t
  | TmSetOp of Ast.setop * tm list
  | TmDAESolver of Sundials.Ida.st * tm array * tm array
  | TmDAESolverOp of Ast.daesolverop * tm list
  | TmDPrint of tm
  | TmDPrintType of tm
  | TmSymStr of tm
  | TmError of Info.info * tm
  | TmDebugId of Ast.ident * tm
and mpat = MPatSym of ty | MPatSymApp | MPatLift of ty
