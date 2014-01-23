exception Mkl_runtime_error of Message.message
module VarSet :
  sig
    type elt = Utils.Int.t
    type t = Set.Make(Utils.Int).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
  end
type exeception
type level = int
type ident = int
type typeid = int
type valueid = int
type fname = int
type recursive = bool
type specialize = bool
type ty =
    TyBool of Info.info * level
  | TyInt of Info.info * level
  | TyReal of Info.info * level
  | TyString of Info.info * level
  | TyArrow of Info.info * level * ty * ty
  | TyUnit of Info.info * level
  | TyList of Info.info * level * ty
  | TyTuple of Info.info * level * ty list
  | TySym of Info.info * level * ty
  | TyDyn of Info.info * level
  | TySymData of Info.info * level * typeid * ident
  | TyIdent of Info.info * level * ident
  | TyArray of Info.info * level * ty
  | TyMap of Info.info * level * ty * ty
  | TySet of Info.info * level * ty
  | TyDAESolver of Info.info * level
type primitive =
    PrimIntMod
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
type const =
    ConstBool of bool
  | ConstInt of int
  | ConstReal of float
  | ConstString of Ustring.Op.ustring
  | ConstUnit
  | ConstPrim of primitive * const list
and mpat =
    MPatSym of Info.info * ty
  | MPatSymApp of Info.info * ident * ident
  | MPatLift of Info.info * ident * ty
and pat =
    PatVar of Info.info * ident * bool
  | PatExpr of Info.info * tm
  | PatSym of Info.info * ty
  | PatSymApp of Info.info * pat * pat
  | PatLift of Info.info * ident * ty
  | PatCons of Info.info * pat * pat
  | PatNil of Info.info
  | PatTuple of Info.info * pat list
  | PatWildcard of Info.info
and vartrans =
    VTransExpr of Info.info * ident * tm
  | VTransModUk of Info.info * ident * ty
  | VTransModVal of Info.info * ident * ident * ty
and patcase = PCase of Info.info * pat list * tm option * vartrans list * tm
and arrayop = ArrayOpLength | ArrayOpMake | ArrayOpGet | ArrayOpSet
and mapop =
    MapOpSize
  | MapOpEmpty
  | MapOpAdd
  | MapOpFind
  | MapOpMem
  | MapOpRemove
  | MapOpToList
and setop =
    SetOpSize
  | SetOpEmpty
  | SetOpAdd
  | SetOpMem
  | SetOpRemove
  | SetOpToList
and daesolverop =
    DAESolverOpMake
  | DAESolverOpMakeHybrid
  | DAESolverOpStep
  | DAESolverOpReinit
  | DAESolverOpClose
  | DAESolverOpRoots
and top =
    TopLet of Info.info * ident * ty option * (ident * ty) list * tm *
      recursive
  | TopNu of Info.info * ident * ty
  | TopNewType of Info.info * ident
  | TopNameType of Info.info * ident * ty
  | TopInclude of Info.info * ident
and tm =
    TmVar of Info.info * ident
  | TmLam of Info.info * level * ident * ty * tm
  | TmApp of Info.info * level * tm * tm * specialize
  | TmFix of Info.info * level * tm
  | TmLet of Info.info * level * ident * ty option * (ident * ty) list * 
      tm * tm * recursive
  | TmIf of Info.info * level * tm * tm * tm
  | TmConst of Info.info * level * const
  | TmList of Info.info * level * tm list
  | TmMatch of Info.info * level * tm * patcase list
  | TmSym of Info.info * level * ident * ty
  | TmNu of Info.info * level * ident * ty * tm
  | TmSymApp of Info.info * level * tm * tm
  | TmLift of Info.info * level * tm * ty
  | TmCase of Info.info * level * tm * mpat * tm * tm
  | TmEqual of Info.info * level * tm * tm
  | TmLcase of Info.info * level * tm * ident * ident * tm * tm
  | TmCons of Info.info * level * tm * tm
  | TmNil of Info.info * level * ty
  | TmTuple of Info.info * level * tm list
  | TmProj of Info.info * level * int * tm
  | TmArray of Info.info * level * tm array
  | TmArrayOp of Info.info * level * arrayop * tm list
  | TmMapOp of Info.info * level * mapop * tm list
  | TmSetOp of Info.info * level * setop * tm list
  | TmDAESolverOp of Info.info * level * daesolverop * tm list
  | TmDPrint of tm
  | TmDPrintType of tm
  | TmSymStr of Info.info * tm
  | TmError of Info.info * level * tm
val metastr : int -> Ustring.Op.ustring
val pprint_primitive : primitive -> Ustring.Op.ustring
val pprint_const : const -> 'a -> Ustring.Op.ustring
val pprint_ty : ty -> Ustring.ustring
val pprint_array_op : arrayop -> Ustring.Op.ustring
val pprint_map_op : mapop -> Ustring.Op.ustring
val pprint_set_op : setop -> Ustring.Op.ustring
val pprint_daesolver_op : daesolverop -> Ustring.Op.ustring
val pprint_mpat : mpat -> Ustring.Op.ustring
val pprint_pat : pat -> Ustring.t
val pprint_vartrans_list : vartrans list -> Ustring.Op.ustring
val pprint_case : patcase -> Ustring.ustring
val pprint_cases : patcase list -> Ustring.ustring
val pprint : tm -> Ustring.t
val ty_equiv : ty -> ty -> bool
val ty_restriction : ty -> ty -> ty
val ty_consistent : ty -> ty -> bool
val delta : const -> const -> const
val deltatype : Info.info -> const -> level -> ty
val primitive_arity : primitive -> int
val tm_info : tm -> Info.info
val set_tm_info : Info.info -> tm -> tm
val pat_info : pat -> Info.info
val ty_info : ty -> Info.info
val set_ty_info : Info.info -> ty -> ty
val ty_lev : ty -> level
val no_auto_esc : pat -> pat
val fpv_mpat : mpat -> VarSet.t
val fpv_pat : pat -> VarSet.t
val fv_pat : pat -> VarSet.t
val fv_patcases : patcase list -> VarSet.t
val fv_tm : tm -> VarSet.t
val freein_tm : VarSet.elt -> tm -> bool
val mk_lettype : ('a * ty) list -> level -> ty -> ty
val mk_arrayop : Info.info -> int -> arrayop
val mk_mapop : Info.info -> int -> mapop
val mk_setop : Info.info -> int -> setop
val mk_daesolverop : Info.info -> int -> daesolverop
type 'a tokendata = { i : Info.info; l : int; v : 'a; }
