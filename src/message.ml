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
open Info
  

(** Handling of Modelica messages (errors and warnings) *)

type id =
  | LEX_UNKNOWN_CHAR
      (* arg1 - String representing the unknown char *)
  | LEX_STRING_NOT_TERMINATED 
      (* arg1 = Unterminated string including newlines *)
  | LEX_INVALID_ESCAPE
      (* arg1 = The escape string, e.g., "\m" *)
  | LEX_COMMENT_NOT_TERMINATED
      (* arg1 = The comment string *)
  | LEX_UNKOWN_PRIMITIVE
      (* arg2 = The primitive string *)
  | LEX_UNKNOWN_FUNCTION
      (* arg1 = function name that is unknown *)
  | PARSE_ERROR
      (* *)
  | TYPE_MISMATCH_IF_GUARD
      (* arg1 = expected type
         arg2 = current type *)
  | TYPE_IF_EXP_DIFF_TYPE
      (* arg1 = type of true-branch 
         arg2 = type of false-branch *)
  | TYPE_IF_EXP_LEV_MONOTONICITY
      (* arg1 = level of if-expression
         arg2 = type of true-branch
         arg3 = type of false-branch *)
  | TYPE_LAM_VAR_LEV_MONOTONICITY
      (* arg1 = type of the binding variable *)
  | TYPE_LAM_EXP_LEV_MONOTONICITY
      (* arg1 = level of lamda term 
         arg2 = type of binding variable
         arg3 = type of abstracted term *)
  | TYPE_VAR_NOT_DEFINED
      (* arg1 = variable that is not found *)
  | TYPE_META_UP_ON_FREE_VAR 
      (* arg1 = variable that is free 
         arg2 = level that the variable was bound
         arg3 = level at the variable was stripped 
         arg4 = line number of the meta-up operation *)
  | TYPE_META_DOWN_ON_FREE_VAR 
      (* arg1 = variable that is free 
         arg2 = level that the variable was bound
         arg3 = level at the variable was stripped 
         arg4 = line number of the meta-down operation *)
  | TYPE_SYMAPP_MISSING_ARG
  | TYPE_APP_ARG_MISMATCH 
      (* arg0 = extra info
         arg1 = type of application argument 
         arg2 = expected type *)
  | TYPE_APP_ABS_MISMATCH 
      (* arg1 = type left hand side 
         arg2 = type of application argument *)
  | TYPE_APP_NO_FUNC_TYPE
      (* arg1 = type of term 1 *)
  | TYPE_LET_REC_MISS_RET_TYPE
      (* *)
  | TYPE_LET_TYPE_DEF_MISMATCH
      (* arg1 = type of explicitly defined variable
         arg2 = derived type from term *)
  | TYPE_LET_PARAM_LEV_MONOTONICITY
      (* arg1 = type of the param *)
  | TYPE_LET_DEF_MONOTONICITY
      (* arg1 = type of the type defintion *) 
  | TYPE_LET_PARAM_LEV_LOWER     
      (* arg1 = level of let term 
         arg2 = type of the param *)
  | TYPE_LET_DEF_LOWER 
      (* arg1 = level of let term 
         arg2 = type of defition or return type *)
  | TYPE_LET_TM1_LOWER 
      (* arg1 = level of let term 
         arg2 = type of term tm1 *)
  | TYPE_LET_TM2_LOWER 
      (* arg1 = level of let term 
         arg2 = type of term tm2 *)
  | TYPE_METAUP_LEV_MONOTONICITY
      (* arg1 = level of meta up term
         arg2 = type of the operand term *)
  | TYPE_METADOWN_LEV_STRICT_MONOTONICITY
      (* arg1 = level of meta down term
         arg2 = type of the operand term *)
  | TYPE_REL_OP_NOT_DEFINED
      (* arg1 = binop *)
  | TYPE_FIX_MISMATCH
      (* arg1 = first type in arrow type
         arg2 = second type in arrow type *)
  | TYPE_FIX_ERROR
      (* *)
  | TYPE_REL_TYPE_PARAM_MISMATCH
      (* arg1 = type of the param derived from term
         arg2 = type of param according to relation definition *)
  | TYPE_REL_LENGTH_MISMATCH
      (* arg1 = length of type from term
         arg2 = length of the defined relation *)
  | TYPE_REL_UNDEF
      (* arg1 = name of the relation *)
  | TYPE_REL_LEV_MONOTONICITY
      (* arg1 = type of the relation *)
  | TYPE_CONJ_DIFF_REL_LEV
      (* arg1 = type of term 1
         arg2 = type of term 2 *)
  | TYPE_CONJ_LEV_MISMATCH
      (* arg1 = level of the conjunction term
         arg2 = level of one of the types of the operands *)
  | TYPE_CONJ_OP1_NOT_REL
      (* arg1 = type of the term that is not a relation *)
  | TYPE_CONJ_OP2_NOT_REL
      (* arg1 = type of the term that is not a relation *)
  | TYPE_UK_OP_NOT_REL
      (* arg1 = type of the unknown term *)
  | TYPE_UK_LEV_MISMATCH
      (* arg1 = type of the unknown term
         arg2 = type of the body term *)
  | TYPE_DESUGAR_ESC_LEV_ZERO
      (* *)
  | TYPE_NIL_LEV_MONOTONICITY
      (* arg1 = Level of the nil term
         arg2 = Type of the list element *)
  | TYPE_CONS_TYPE_MISMATCH
      (* arg1 = type of the left cons term
         arg2 = type of the element of the list (right hand side) *)
  | TYPE_CONS_LEV_MONOTONICITY
      (* arg1 = level of the cons term  
         arg2 = type of the left hand side 
         arg3 = type of the right hand side *)
  | TYPE_CONS_NOT_A_LIST
      (* arg1 = Argument that is not a list *)
  | TYPE_LCASE_IDENTICAL_IDENTIFIERS
      (* arg1 = identifier 1
         arg2 = identifier 2 *)
  | TYPE_LCASE_DIFFERENT_CASE_TYPES
      (* arg1 = type of the cons case 
         arg2 = type of the nil case *)
  | TYPE_LCASE_LEVEL_MISMATCH
      (* arg1 = level of the lcase term
         arg2 = type of the matching term *)
  | TYPE_LCASE_LEV_MONOTONICITY
      (* arg1 = level of the lcase term
         arg2 = type of the cons case
         arg3 = type of the nil case *)
  | TYPE_LCASE_MATCHING_TM_NOT_LIST_TYPE
      (* arg1 = type of the matching term *)
  | TYPE_NU_LET_VAR_LEV_MONOTONICITY
      (* arg1 = type of the binding variable *)
  | TYPE_NU_LET_EXP_LEV_MONOTONICITY
      (* arg1 = level of nu term 
         arg2 = type of binding variable
         arg3 = type of abstracted term *)
  | TYPE_MODAPP_ARG_MISMATCH 
      (* arg1 = type of function parameter 
         arg2 = type of application argument *)
  | TYPE_MODAPP_TYPE_MISMATCH 
      (* arg1 = type of left term 
         arg2 = type of right term *)
  | TYPE_EQUAL_EXP_DIFF_TYPE
      (* arg1 = type of left term 
         arg2 = type of right term *)
  | TYPE_EQUAL_EXP_LEV_MONOTONICITY
      (* arg1 = level of equal-expression
         arg2 = type of left term
         arg3 = type of right term *)
  | TYPE_DECON_TYPE_NOT_MODEL
      (* arg1 = type of the term *)
  | TYPE_DECON_LEV_MONOTONICITY
      (* arg1 = level of term
         arg2 = type of term 1 
         arg3 = type of term 2
         arg4 = type of term 3 *)
  | TYPE_DECON_MISMATCH
      (* arg1 = type of second term 
	 arg2 = type of third term *)
  | TYPE_TUPLE_LEV_MONOTONICITY
      (* arg1 = tuple level 
         arg2 = element type *)
  | TYPE_PROJ_LEV_MONOTONICITY
      (* arg1 = level of projection term
         arg2 = type of sub-term *)
  | TYPE_PROJ_TUPLE_SIZE
      (* arg1 = projection element number 
         arg2 = tuple size *)
  | TYPE_PROJ_NOT_TUPLE
      (* arg1 = type of element that is not a tuple *)
  | TYPE_DECON_PAT_UK_NOT_MODEL_TYPE
      (* arg1 = the type in the pattern that is not of model type *)
  | TYPE_MODEQUAL_NOT_CONCRETE_MODELTYPE
      (* arg1 = term 1
         arg2 = term 2 *) 
  | TYPE_MODIF_NOT_CONCRETE_MODELTYPE
      (* arg1 = guard term 
         arg2 = true term 
         arg3 = false term *)
  | TYPE_MODPROJ_NOT_MODELTUPLE
      (* arg1 = proj term *)
  | TYPE_ERROR_TERM_LEV_MONOTONICITY
      (* arg1 = level 
         arg2 = type of the term that should be a string *)
  | TYPE_ERROR_TERM_NOT_STRING 
      (* arg1 = type of the term that should be a string *)
  | TYPE_NU_LET_NOT_MODELTYPE
      (* arg1 = type *)
  | TYPE_UNKNOWN_TYPE 
      (* arg0 = the type *)
  | RUNTIME_ERROR
      (* arg0 = user defined error message *)
  | PATMATCH_UNUSED_PATTERN
  | PATMATCH_MIXING_PATTERN_TYPES
  | TYPE_MATCH_MONOTONICITY
      (* arg1 = level of term
         arg2 = type of the operand term *)
  | TYPE_ARRAY_LEV_MONOTONICITY
      (* arg1 = array level 
         arg2 = element type *)
  | TYPE_ARRAY_ELEM_NOT_CONSISTENT
      (* arg1 = element 1
         arg2 = element 2 *)
  | TYPE_EXPECTED_ARRAY_TYPE
      (* arg1 = type of the type that is not an array 
         arg2 = expected level of the type *)
  | TYPE_EXPECTED_MAP_TYPE
      (* arg1 = type of the type that is not an map 
         arg2 = expected level of the type *)
  | TYPE_EXPECTED_SET_TYPE
      (* arg1 = type of the type that is not an set 
         arg2 = expected level of the type *)
  | TYPE_EXPECTED_DAESOLVER_TYPE
      (* arg1 = type of the type that is not a dae solver
         arg2 = expected level of the type *)
  | TYPE_EXPECTED_INT_TYPE
      (* arg1 = type of the type that is not an int
         arg2 = expected level of the type *)
  | TYPE_EXPECTED_REAL_TYPE
      (* arg1 = type of the type that is not an real
         arg2 = expected level of the type *)
  | TYPE_EXPECTED_CONSTANT_LEV
      (* arg1 = type of the type that is expected constant level
         arg2 = the expected level *)
  | TYPE_UNEXPECTED_NO_ARGS
  | TYPE_EXPECTED_RESROOT_TYPE 
      (* arg1 = The expected type
         arg2 = The current type *)
  | TYPE_ERROR_TERM_NOT_SYM
      (* arg1 = Current type *)
  | STATIC_CIRCULAR_DEP_INCLUDE
      (* arg1 = name of the include *)
  | STATIC_ERROR_OPEN_FILE
     (*  arg0 = file name *)
  | RUNTIME_TYPE_ERROR
     (*  arg1 = operation
         arg2 = argument *)
  | TYPE_NO_OVERLOADING 
     (*  arg1 = variable name *)

type severity =
  | ERROR
  | WARNING

type arguments = ustring list

(** Error and warning messages. Created by the lexer, parser,
    and type checker.  *)
type message = id * severity * info * arguments


exception Mkl_lex_error of message
exception Mkl_static_error of message



(** [id2str id] returns the identifier string for [id], e.g., 
    "LEX_UNKNOWN_CHAR" *)
let id2str id args =
  match id  with
    | LEX_UNKNOWN_CHAR -> us"Illegal character '" ^. (List.nth args 0) ^. us"'."
    | LEX_STRING_NOT_TERMINATED -> us"String is not terminated."
    | LEX_INVALID_ESCAPE -> us"LEX_INVALID_ESCAPE"
    | LEX_COMMENT_NOT_TERMINATED -> us"Comment is not terminated. Missing */"
    | LEX_UNKOWN_PRIMITIVE -> us"LEX_UNKOWN_PRIMITIVE"
    | LEX_UNKNOWN_FUNCTION -> us"LEX_UNKNOWN_FUNCTION"

    | PARSE_ERROR -> us"Syntax error"
    | TYPE_MISMATCH_IF_GUARD -> us"TYPE_MISMATCH_IF_GUARD" 
    | TYPE_IF_EXP_DIFF_TYPE -> us"TYPE_IF_EXP_DIFF_TYPE"
    | TYPE_IF_EXP_LEV_MONOTONICITY -> us"TYPE_IF_EXP_LEV_MONOTONICITY"
    | TYPE_LAM_VAR_LEV_MONOTONICITY -> us"TYPE_LAM_VAR_LEV_MONOTONICITY"
    | TYPE_LAM_EXP_LEV_MONOTONICITY -> us"TYPE_LAM_EXP_LEV_MONOTONICITY"
    | TYPE_VAR_NOT_DEFINED -> us"Unknown identifier '" ^. (List.nth args 0) ^. us"'."
    | TYPE_META_UP_ON_FREE_VAR -> us"TYPE_META_UP_ON_FREE_VAR"
    | TYPE_META_DOWN_ON_FREE_VAR -> us"TYPE_META_DOWN_ON_FREE_VAR"
    | TYPE_SYMAPP_MISSING_ARG -> us"Missing argument of type '" ^. (List.nth args 0) ^. us"'."
    | TYPE_APP_ARG_MISMATCH -> (List.nth args 0) ^.
                               us"Illegal argument type. Expected an argument of type " ^.
                               (List.nth args 1) ^. us", but the argument has type " ^. (List.nth args 2) ^. us"."
    | TYPE_APP_ABS_MISMATCH -> (List.nth args 1) ^.
                               us"The expression of type '" ^. (List.nth args 0) ^. 
                               us"' is not a function and shall not have any arguments."
    | TYPE_APP_NO_FUNC_TYPE -> us"TYPE_APP_NO_FUNC_TYPE"
    | TYPE_LET_REC_MISS_RET_TYPE -> us"TYPE_LET_REC_MISS_RET_TYPE"
    | TYPE_LET_TYPE_DEF_MISMATCH -> us"TYPE_LET_TYPE_DEF_MISMATCH"
    | TYPE_LET_PARAM_LEV_MONOTONICITY -> us"TYPE_LET_PARAM_LEV_MONOTONICITY"
    | TYPE_LET_DEF_MONOTONICITY -> us"TYPE_LET_DEF_MONOTONICITY"
    | TYPE_LET_PARAM_LEV_LOWER -> us"TYPE_LET_PARAM_LEV_LOWER"
    | TYPE_LET_DEF_LOWER -> us"TYPE_LET_DEF_LOWER"
    | TYPE_LET_TM1_LOWER -> us"TYPE_LET_TM1_LOWER"
    | TYPE_LET_TM2_LOWER -> us"TYPE_LET_TM2_LOWER"
    | TYPE_METAUP_LEV_MONOTONICITY -> us"TYPE_METAUP_LEV_MONOTONICITY"
    | TYPE_METADOWN_LEV_STRICT_MONOTONICITY ->
	us"TYPE_METADOWN_LEV_STRICT_MONOTONICITY"
    | TYPE_REL_OP_NOT_DEFINED -> us"TYPE_REL_OP_NOT_DEFINED"
    | TYPE_FIX_MISMATCH -> us"TYPE_FIX_MISMATCH"
    | TYPE_FIX_ERROR -> us"TYPE_FIX_ERROR"
    | TYPE_REL_TYPE_PARAM_MISMATCH -> us"TYPE_REL_TYPE_PARAM_MISMATCH"
    | TYPE_REL_LENGTH_MISMATCH -> us"TYPE_REL_LENGTH_MISMATCH"
    | TYPE_REL_UNDEF -> us"TYPE_REL_UNDEF"
    | TYPE_REL_LEV_MONOTONICITY -> us"TYPE_REL_LEV_MONOTONICITY"
    | TYPE_CONJ_DIFF_REL_LEV -> us"TYPE_CONJ_DIFF_REL_LEV"
    | TYPE_CONJ_LEV_MISMATCH -> us"TYPE_CONJ_LEV_MISMATCH"
    | TYPE_CONJ_OP1_NOT_REL -> us"TYPE_CONJ_OP1_NOT_REL"
    | TYPE_CONJ_OP2_NOT_REL -> us"TYPE_CONJ_OP2_NOT_REL"
    | TYPE_UK_OP_NOT_REL -> us"TYPE_UK_OP_NOT_REL"
    | TYPE_UK_LEV_MISMATCH -> us"TYPE_UK_LEV_MISMATCH"
    | TYPE_DESUGAR_ESC_LEV_ZERO -> us"TYPE_DESUGAR_ESC_LEV_ZERO"
    | TYPE_NIL_LEV_MONOTONICITY -> us"TYPE_NIL_LEV_MONOTONICITY"
    | TYPE_CONS_TYPE_MISMATCH -> 
         us"The types of the cons operator do not match. The type on the left-hand side is " ^.
          (List.nth args 0) ^. us", whereas the type on the right hand side is " ^.
          (List.nth args 1) ^. us"."
    | TYPE_CONS_LEV_MONOTONICITY -> us"TYPE_CONS_LEV_MONOTONICITY"
    | TYPE_CONS_NOT_A_LIST -> us"The type of right hand side of the cons expression is " ^.
         (List.nth args 0) ^. us", which is not a list type."        
    | TYPE_LCASE_IDENTICAL_IDENTIFIERS -> us"TYPE_LCASE_IDENTICAL_IDENTIFIERS"
    | TYPE_LCASE_DIFFERENT_CASE_TYPES -> us"TYPE_LCASE_DIFFERENT_CASE_TYPES"
    | TYPE_LCASE_LEVEL_MISMATCH -> us"TYPE_LCASE_LEVEL_MISMATCH"
    | TYPE_LCASE_LEV_MONOTONICITY -> us"TYPE_LCASE_LEV_MONOTONICITY"
    | TYPE_LCASE_MATCHING_TM_NOT_LIST_TYPE -> 
	us"TYPE_LCASE_MATCHING_TM_NOT_LIST_TYPE"
    | TYPE_NU_LET_VAR_LEV_MONOTONICITY -> us"TYPE_NU_LET_VAR_LEV_MONOTONICITY"
    | TYPE_NU_LET_EXP_LEV_MONOTONICITY -> us"TYPE_NU_LET_EXP_LEV_MONOTONICITY"
    | TYPE_MODAPP_ARG_MISMATCH -> us"TYPE_MODAPP_ARG_MISMATCH" 
    | TYPE_MODAPP_TYPE_MISMATCH -> us"TYPE_MODAPP_TYPE_MISMATCH" 
    | TYPE_EQUAL_EXP_DIFF_TYPE -> us"TYPE_EQUAL_EXP_DIFF_TYPE"
    | TYPE_EQUAL_EXP_LEV_MONOTONICITY -> us"TYPE_EQUAL_EXP_LEV_MONOTONICITY"
    | TYPE_DECON_TYPE_NOT_MODEL -> us"TYPE_DECON_TYPE_NOT_MODEL"
    | TYPE_DECON_LEV_MONOTONICITY -> us"TYPE_DECON_LEV_MONOTONICITY"
    | TYPE_DECON_MISMATCH -> us"TYPE_DECON_MISMATCH"
    | TYPE_TUPLE_LEV_MONOTONICITY -> us"TYPE_TUPLE_LEV_MONOTONICITY"
    | TYPE_PROJ_LEV_MONOTONICITY -> us"TYPE_PROJ_LEV_MONOTONICITY"
    | TYPE_PROJ_TUPLE_SIZE -> us"TYPE_PROJ_TUPLE_SIZE"
    | TYPE_PROJ_NOT_TUPLE -> us"TYPE_PROJ_NOT_TUPLE"   
    | TYPE_DECON_PAT_UK_NOT_MODEL_TYPE -> us"TYPE_DECON_PAT_UK_NOT_MODEL_TYPE"
    | TYPE_MODEQUAL_NOT_CONCRETE_MODELTYPE -> 
	us"TYPE_MODEQUAL_NOT_CONCRETE_MODELTYPE"
    | TYPE_MODIF_NOT_CONCRETE_MODELTYPE ->
	us"TYPE_MODIF_NOT_CONCRETE_MODELTYPE"
    | TYPE_MODPROJ_NOT_MODELTUPLE -> us"TYPE_MODPROJ_NOT_MODELTUPLE"
    | TYPE_ERROR_TERM_LEV_MONOTONICITY -> us"TYPE_ERROR_TERM_LEV_MONOTONICITY"
    | TYPE_ERROR_TERM_NOT_STRING -> us"TYPE_ERROR_TERM_NOT_STRING"
    | TYPE_NU_LET_NOT_MODELTYPE -> us"TYPE_NU_LET_NOT_MODELTYPE"
    | TYPE_UNKNOWN_TYPE -> us"Unkown type '" ^. (List.nth args 0) ^. us"'."
    | RUNTIME_ERROR -> (List.nth args 0)
    | PATMATCH_UNUSED_PATTERN -> us"PATMATCH_UNUSED_PATTERN"
    | PATMATCH_MIXING_PATTERN_TYPES -> us"PATMATCH_MIXING_PATTERN_TYPES"
    | TYPE_MATCH_MONOTONICITY -> us"TYPE_MATCH_MONOTONICITY"
    | TYPE_ARRAY_LEV_MONOTONICITY -> us"TYPE_ARRAY_LEV_MONOTONICITY"
    | TYPE_ARRAY_ELEM_NOT_CONSISTENT -> us"TYPE_ARRAY_ELEM_NOT_CONSISTENT"
    | TYPE_EXPECTED_ARRAY_TYPE -> us"TYPE_EXPECTED_ARRAY_TYPE"
    | TYPE_EXPECTED_MAP_TYPE -> us"TYPE_EXPECTED_MAP_TYPE"
    | TYPE_EXPECTED_SET_TYPE -> us"TYPE_EXPECTED_SET_TYPE"
    | TYPE_EXPECTED_DAESOLVER_TYPE -> us"TYPE_EXPECTED_DAESOLVER_TYPE"
    | TYPE_EXPECTED_INT_TYPE -> us"TYPE_EXPECTED_INT_TYPE"
    | TYPE_EXPECTED_REAL_TYPE -> us"TYPE_EXPECTED_REAL_TYPE"
    | TYPE_EXPECTED_CONSTANT_LEV -> us"TYPE_EXPECTED_CONSTANT_LEV"
    | TYPE_UNEXPECTED_NO_ARGS -> us"TYPE_UNEXPECTED_NO_ARGS"
    | TYPE_EXPECTED_RESROOT_TYPE -> us"TYPE_EXPECTED_RESROOT_TYPE"
    | TYPE_ERROR_TERM_NOT_SYM -> us"The supplied term is not a symbol type but of type " ^.
                                 (List.nth args 0) ^. us"'."
    | STATIC_CIRCULAR_DEP_INCLUDE -> us"STATIC_CIRCULAR_DEP_INCLUDE"
    | STATIC_ERROR_OPEN_FILE -> us"Cannot open file '" ^. (List.nth args 0) ^. us"'" 
    | RUNTIME_TYPE_ERROR -> us"RUNTIME_TYPE_ERROR"
    | TYPE_NO_OVERLOADING -> us"There is no overloaded function '" ^.
                             (List.nth args 0) ^. us"' for the given arguments."




(** [severity2str s] returns the severity strings ["ERROR"] or 
    ["WARNING"]. *)
let severity2str s =
  match s with
    | ERROR -> us"error"
    | WARNING -> us"warning"


let info2str_startline fi =
  match fi with 
    | Info(filename,l1,c1,l2,c2) -> l1
    | NoInfo -> assert false
  

(** [message2str m] returns a string representation of message [m].
    Is message is not intended to be read by humans. *)
let message2str (id,sev,info,args)  = 
  match info with 
    | Info(filename,l1,c1,l2,c2) -> 
	  filename ^. us" " ^.
	  (ustring_of_int l1) ^. us":" ^.
	  (ustring_of_int c1) ^. us"-" ^.
	  (ustring_of_int l2) ^. us":" ^.
	  (ustring_of_int c2) ^. us" " ^.
	  (severity2str sev) ^. us": " ^. 
	  (id2str id args) 
    |  NoInfo -> us"NO INFO: " ^. (id2str id args)
     






