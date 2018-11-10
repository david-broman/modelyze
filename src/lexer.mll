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

{
  open Parser
  open Printf
  open Ustring.Op
  open Message
  open Info
  open Ast


let reserved_strings = [
  (* Keywords *)
  ("fun",           fun(i,l) -> Parser.FUN{i=i;l=l;v=()}); 
  ("def",           fun(i,l) -> Parser.DEF{i=i;l=l;v=()}); 
  ("in",            fun(i,l) -> Parser.IN{i=i;l=l;v=()}); 
  ("if",            fun(i,l) -> Parser.IF{i=i;l=l;v=()}); 
  ("then",          fun(i,l) -> Parser.THEN{i=i;l=l;v=()}); 
  ("else",          fun(i,l) -> Parser.ELSE{i=i;l=l;v=()}); 
  ("true",          fun(i,l) -> Parser.TRUE{i=i;l=l;v=()}); 
  ("false",         fun(i,l) -> Parser.FALSE{i=i;l=l;v=()}); 
  ("Int",           fun(i,l) -> Parser.INT{i=i;l=l;v=()}); 
  ("Real",          fun(i,l) -> Parser.REAL{i=i;l=l;v=()}); 
  ("Bool",          fun(i,l) -> Parser.BOOL{i=i;l=l;v=()}); 
  ("String",        fun(i,l) -> Parser.TYSTRING{i=i;l=l;v=()}); 
  ("dprint",        fun(i,l) -> Parser.DPRINT{i=i;l=l;v=()}); 
  ("dprinttype",    fun(i,l) -> Parser.DPRINTTYPE{i=i;l=l;v=()}); 
  ("symstr",        fun(i,l) -> Parser.SYMSTR{i=i;l=l;v=()}); 
  ("lcase",         fun(i,l) -> Parser.LCASE{i=i;l=l;v=()}); 
  ("of",            fun(i,l) -> Parser.OF{i=i;l=l;v=()}); 
  ("decon",         fun(i,l) -> Parser.DECON{i=i;l=l;v=()}); 
  ("with",          fun(i,l) -> Parser.WITH{i=i;l=l;v=()}); 
  ("sym",           fun(i,l) -> Parser.SYM{i=i;l=l;v=()}); 
  ("lift",          fun(i,l) -> Parser.LIFT{i=i;l=l;v=()}); 
  ("sval",          fun(i,l) -> Parser.LIFT{i=i;l=l;v=()}); 
  ("proj",          fun(i,l) -> Parser.PROJ{i=i;l=l;v=()}); 
  ("fst",           fun(i,l) -> Parser.FST{i=i;l=l;v=()});   
  ("snd",           fun(i,l) -> Parser.SND{i=i;l=l;v=()});  
  ("ifguard",       fun(i,l) -> Parser.IFGUARD{i=i;l=l;v=()}); 
  ("ifthen",        fun(i,l) -> Parser.IFTHEN{i=i;l=l;v=()}); 
  ("ifelse",        fun(i,l) -> Parser.IFELSE{i=i;l=l;v=()}); 
  ("error",         fun(i,l) -> Parser.ERROR{i=i;l=l;v=()}); 
  ("match",         fun(i,l) -> Parser.MATCH{i=i;l=l;v=()}); 
  ("from",          fun(i,l) -> Parser.FROM{i=i;l=l;v=()}); 
  ("type",          fun(i,l) -> Parser.TYPE{i=i;l=l;v=()}); 
  ("Array",         fun(i,l) -> Parser.ARRAY{i=i;l=l;v=()}); 
  ("Map",           fun(i,l) -> Parser.MAP{i=i;l=l;v=()}); 
  ("List",          fun(i,l) -> Parser.LIST{i=i;l=l;v=()}); 
  ("Set",           fun(i,l) -> Parser.SET{i=i;l=l;v=()}); 
  ("DAESolver",     fun(i,l) -> Parser.DAESOLVER{i=i;l=l;v=()}); 
  ("NLEQSolver",    fun(i,l) -> Parser.NLEQSOLVER{i=i;l=l;v=()});
  ("include",       fun(i,l) -> Parser.INCLUDE{i=i;l=l;v=()}); 
  ("begin",         fun(i,l) -> Parser.BEGIN{i=i;l=l;v=()}); 
  ("end",           fun(i,l) -> Parser.END{i=i;l=l;v=()}); 
  ("specialize",    fun(i,l) -> Parser.SPECIALIZE{i=i;l=l;v=()}); 
  ("peval",         fun(i,l) -> Parser.PEVAL{i=i;l=l;v=()}); 

  (* v *)
  ("=",             fun(i,l) -> Parser.EQ{i=i;l=l;v=()});
  ("~=",            fun(i,l) -> Parser.APXEQ{i=i;l=l;v=()});
  ("<-",            fun(i,l) -> Parser.LEFTARROW{i=i;l=l;v=()});
  ("mod",           fun(i,l) -> Parser.MOD{i=i;l=l;v=()});
  ("+",             fun(i,l) -> Parser.ADD{i=i;l=l;v=()});
  ("-",             fun(i,l) -> Parser.SUB{i=i;l=l;v=()});
  ("*",             fun(i,l) -> Parser.MUL{i=i;l=l;v=()});
  ("/",             fun(i,l) -> Parser.DIV{i=i;l=l;v=()});
  ("<",             fun(i,l) -> Parser.LESS{i=i;l=l;v=()});
  ("<=",            fun(i,l) -> Parser.LESSEQUAL{i=i;l=l;v=()});
  (">",             fun(i,l) -> Parser.GREAT{i=i;l=l;v=()});
  (">=",            fun(i,l) -> Parser.GREATEQUAL{i=i;l=l;v=()});
  ("==",            fun(i,l) -> Parser.EQUAL{i=i;l=l;v=()});
  ("!=",            fun(i,l) -> Parser.NOTEQUAL{i=i;l=l;v=()});
  ("+.",            fun(i,l) -> Parser.DOTADD{i=i;l=l;v=()});
  ("-.",            fun(i,l) -> Parser.DOTSUB{i=i;l=l;v=()});
  ("*.",            fun(i,l) -> Parser.DOTMUL{i=i;l=l;v=()});
  ("/.",            fun(i,l) -> Parser.DOTDIV{i=i;l=l;v=()});
  ("<.",            fun(i,l) -> Parser.DOTLESS{i=i;l=l;v=()});
  ("<=.",           fun(i,l) -> Parser.DOTLESSEQUAL{i=i;l=l;v=()});
  (">.",            fun(i,l) -> Parser.DOTGREAT{i=i;l=l;v=()});
  (">=.",           fun(i,l) -> Parser.DOTGREATEQUAL{i=i;l=l;v=()});
  ("==.",           fun(i,l) -> Parser.DOTEQUAL{i=i;l=l;v=()});
  ("!=.",           fun(i,l) -> Parser.DOTNOTEQUAL{i=i;l=l;v=()});
  ("!",             fun(i,l) -> Parser.NOT{i=i;l=l;v=()});
  ("&&",            fun(i,l) -> Parser.AND{i=i;l=l;v=()});
  ("||",            fun(i,l) -> Parser.OR{i=i;l=l;v=()});
  (";",             fun(i,l) -> Parser.SEMI{i=i;l=l;v=()});
  ("++",            fun(i,l) -> Parser.PLUSPLUS{i=i;l=l;v=()});
  ("^",             fun(i,l) -> Parser.EXP{i=i;l=l;v=()});
  ("^.",            fun(i,l) -> Parser.DOTEXP{i=i;l=l;v=()});

  (* Symbolic Tokens *)
  ("(",             fun(i,l) -> Parser.LPAREN{i=i;l=l;v=()});
  (")",             fun(i,l) -> Parser.RPAREN{i=i;l=l;v=()});
  ("[",             fun(i,l) -> Parser.LSQUARE{i=i;l=l;v=()});
  ("]",             fun(i,l) -> Parser.RSQUARE{i=i;l=l;v=()});
  ("{",             fun(i,l) -> Parser.LCURLY{i=i;l=l;v=()});
  ("}",             fun(i,l) -> Parser.RCURLY{i=i;l=l;v=()});
  ("::",            fun(i,l) -> Parser.CONS{i=i;l=l;v=()});
  (":",             fun(i,l) -> Parser.COLON{i=i;l=l;v=()});
  (",",             fun(i,l) -> Parser.COMMA{i=i;l=l;v=()});
  (".",             fun(i,l) -> Parser.DOT{i=i;l=l;v=()});
  ("|",             fun(i,l) -> Parser.BAR{i=i;l=l;v=()});
  ("-->",           fun(i,l) -> Parser.LONGARROW{i=i;l=l;v=()});
  ("->",            fun(i,l) -> Parser.ARROW{i=i;l=l;v=()});
  ("=>",            fun(i,l) -> Parser.DARROW{i=i;l=l;v=()});
  ("<==>",          fun(i,l) -> Parser.POLYEQUAL{i=i;l=l;v=()});
  ("_",             fun(i,l) -> Parser.USCORE{i=i;l=l;v=()});
  ("~",             fun(i,l) -> Parser.ESCAPE{i=i;l=l;v=()});
  ("'",             fun(i,l) -> Parser.SQUOTE{i=i;l=l;v=()});
  (")(",            fun(i,l) -> Parser.PARENAPP{i=i;l=l;v=()});
  (";;",            fun(i,l) -> Parser.EQSEMI{i=i;l=l;v=()});
  ("?",             fun(i,l) -> Parser.QUESTIONMARK{i=i;l=l;v=()});

]

let str2primitive fi s =
  match s with
  | "@@int_mod"  -> PrimIntMod
  | "@@int_add"  -> PrimIntAdd
  | "@@int_sub"  -> PrimIntSub
  | "@@int_mul"  -> PrimIntMul
  | "@@int_div"  -> PrimIntDiv
  | "@@int_less" -> PrimIntLess
  | "@@int_less_equal" -> PrimIntLessEqual
  | "@@int_great" -> PrimIntGreat
  | "@@int_great_equal" -> PrimIntGreatEqual
  | "@@int_equal" -> PrimIntEqual
  | "@@int_not_equal" -> PrimIntNotEqual
  | "@@int_neg" -> PrimIntNeg
  | "@@real_add" -> PrimRealAdd
  | "@@real_sub" -> PrimRealSub
  | "@@real_mul" -> PrimRealMul
  | "@@real_div" -> PrimRealDiv
  | "@@real_less" -> PrimRealLess
  | "@@real_less_equal" -> PrimRealLessEqual
  | "@@real_great" -> PrimRealGreat
  | "@@real_great_equal" -> PrimRealGreatEqual
  | "@@real_equal" -> PrimRealEqual
  | "@@real_not_equal" -> PrimRealNotEqual
  | "@@real_neg" -> PrimRealNeg
  | "@@bool_and" -> PrimBoolAnd
  | "@@bool_or" -> PrimBoolOr
  | "@@bool_not" -> PrimBoolNot
  | "@@print" -> PrimPrint
  | "@@bool2string" -> PrimBool2String
  | "@@int2string" -> PrimInt2String
  | "@@real2string" -> PrimReal2String
  | "@@int2real" -> PrimInt2Real
  | "@@real2int" -> PrimReal2Int
  | "@@string2bool" -> PrimString2Bool
  | "@@string2int" -> PrimString2Int
  | "@@string2real" -> PrimString2Real
  | "@@isboolstring" -> PrimIsBoolString
  | "@@isrealstring" -> PrimIsRealString
  | "@@isintstring" -> PrimIsIntString
  | "@@sin" -> PrimSin
  | "@@cos" -> PrimCos
  | "@@tan" -> PrimTan
  | "@@asin" -> PrimASin
  | "@@acos" -> PrimACos
  | "@@atan" -> PrimATan
  | "@@sinh" -> PrimSinh
  | "@@cosh" -> PrimCosh
  | "@@tanh" -> PrimTanh
  | "@@ceil" -> PrimCeil
  | "@@floor" -> PrimFloor
  | "@@log" -> PrimLog
  | "@@log10" -> PrimLog10
  | "@@sqrt" -> PrimSqrt
  | "@@exp" -> PrimExp
  | "@@exponentiation" -> PrimExponentiation
  | "@@string_concat" -> PrimStringConcat
  | "@@string_strlen" -> PrimStringStrlen
  | "@@string_substr" -> PrimStringSubstr
  | _ -> raise (Mkl_lex_error (LEX_UNKOWN_PRIMITIVE,ERROR, fi, [us s]))


(* Info handling : filename and position
   Functions ending with name _fast assumes that no return
   characters are in the string *)
let tabsize = ref 8
let filename = ref (us"")
let rowno = ref 1
let colno = ref 0
let last_info = ref NoInfo
let utf8strlen s = Ustring.length (Ustring.from_utf8 s)
let newrow() =
  incr rowno;
  colno := 0
(* Updates both columns and rows in a safe way *)
let count_ustring s =
  rowno := !rowno + (Ustring.count s (uc '\n'));
  colno := try Ustring.length s - Ustring.rindex s (uc '\n') - 1
	     with Not_found -> !colno + Ustring.length s
let count_utf8 s = count_ustring (Ustring.from_utf8 s)
let colcount_fast s = colno := !colno + (String.length s)
let colcount_utf8 s = colno := !colno + (utf8strlen s)
let add_colno i = colno := !colno + i
let mkinfo_fast s =
  last_info := Info(!filename,!rowno,!colno,!rowno,!colno+(String.length s));
  colcount_fast s; !last_info
let mkinfo_utf8_fast s =
  last_info := Info(!filename,!rowno,!colno,!rowno,!colno + (utf8strlen s));
  colcount_utf8 s; !last_info
(* mkinfo_ustring also counts newlines correctly in string [s] *)
let mkinfo_ustring s =
  let row = !rowno in
  let col = !colno in
  count_ustring s;
  last_info := Info(!filename,row,col,!rowno,!colno);
  !last_info

(* Init the lexer with file name and tab-size. *)
let init file_name tab_size=
  filename := file_name;
  rowno := 1;
  colno := 0;
  tabsize := tab_size

(* Handle identifiers, keywords, and operators *)
type buildfun = (info * int) -> Parser.token
let (str_tab : (string,buildfun) Hashtbl.t) =
  Hashtbl.create 1024
let _ = List.iter (fun (str,f) -> Hashtbl.add str_tab str f)
  reserved_strings

(* Make identfier, keyword, or operator  *)
let mkid fullstr withparen =
  try
    let f = Hashtbl.find str_tab fullstr in
    let fi = mkinfo_fast fullstr in
    let kw = f (fi,0) in
    if withparen then [kw;Parser.LPAREN{i=fi;l=0;v=()}] else [kw]
  with Not_found ->
    let s2 = Ustring.from_utf8 fullstr in
    if withparen then
      [Parser.IDENTPAREN {i=mkinfo_ustring s2; l=0; v=Symtbl.add s2}]
    else
      [Parser.IDENT {i=mkinfo_ustring s2; l=0; v=Symtbl.add s2}]

(* String handling *)
let string_buf = Buffer.create 80

(* Parse error message *)
let parse_error_message() =
  (PARSE_ERROR,ERROR,!last_info,[])


}

let utf8_1byte = ['\x00'-'\x7F']
let utf8_2byte = ['\xC0'-'\xDF'] ['\x80'-'\xBF']
let utf8_3byte = ['\xE0'-'\xEF'] ['\x80'-'\xBF'] ['\x80'-'\xBF']
let utf8_4byte = ['\xF0'-'\xF7'] ['\x80'-'\xBF'] ['\x80'-'\xBF'] ['\x80'-'\xBF']

let ascii = utf8_1byte
let noascii =  utf8_2byte | utf8_3byte | utf8_4byte
let utf8 = ascii | noascii
let us_letter = ['A'-'Z'] | ['a'-'z']
let newline = ('\013' | '\010' | "\013\010")
let whitespace = (' '| '\012')
let tab = '\t'
let white = whitespace | newline | tab
let digit = ['0'-'9']
let s_escape = "\\'" | "\\\"" | "\\?"  | "\\\\" |
               "\\a"  | "\\b" | "\\f"  | "\\n" | "\\r" | "\\t" | "\\v"
let unsigned_integer = digit+
let unsigned_number = unsigned_integer ('.' (unsigned_integer)?)?
                      (('e'|'E') ("+"|"-")? unsigned_integer)?
let nondigit = ('_' | us_letter)
let q_char = [^ '\\' '\'']
let ident = (nondigit (digit | nondigit)*)
let operator = "="  | "~="  | "<-"  | "mod" |
               "+"  | "-"   | "*"   | "/"   |
               "<"  |"<="   | ">"   | ">="  | "=="  | "!="  |
               "+." | "-."  | "*."  | "/."  |
               "<." | "<=." | ">."  | ">=." | "==." | "!=." |
               "!"  | "&&"  | "||"  | ";"   | "++"  |
	      "--"  | "--." | "^"   | "^."  | "'" | "-->"

let symtok  =  "(" | ")" | "["  | "]" | "{"  | "}" | "::" | ":" |
                "," | "." | "|" | "->" |  "=>" | "~" | "<==>" | "_" |
                 "~" | ")(" | ";;" | "?"



let line_comment = "//" [^ '\013' '\010']*



(* Main lexing *)
rule main = parse
  | whitespace+ as s
      { colcount_fast s; main lexbuf }
  | line_comment
      { main lexbuf }
  | "/*" as s
      { Buffer.reset string_buf ;
	 Buffer.add_string string_buf s; end_section_comment lexbuf;
	 count_utf8 (Buffer.contents string_buf);
	 main lexbuf}
  | tab
      { add_colno !tabsize; main lexbuf }
  | newline
      { newrow(); main lexbuf }
  | "(" operator ")" as s
      { mkid s false }
  | (ident as s) "("
      { mkid s true }
  | ";" (white+ as s) "}"   (* Not a perfect solution. We cannot have comments
                               on the same line... *)
      { count_utf8 s; mkid "}" false }
  | ident | operator | symtok as s
      { mkid s false }
  | "@@" ident as s
      { [let fi = mkinfo_fast s in
	 Parser.PRIMITIVE{i=fi; l=0; v=str2primitive fi s}] }
  | (unsigned_integer as str)
      { [Parser.UINT{i=mkinfo_fast str; l=0; v=int_of_string str}] }
  | unsigned_number as str
      { [Parser.UFLOAT{i=mkinfo_fast str; l=0; v=float_of_string str}] }
  | '"'
      { Buffer.reset string_buf ;  mklstring lexbuf;
	 let s = Ustring.from_utf8 (Buffer.contents string_buf) in
         let esc_s = Ustring.convert_escaped_chars s in
	 let rval = Parser.STRING{i=mkinfo_ustring (s ^. us"  "); l=0; v=esc_s} in
	 add_colno 2; [rval]}
  | eof
      { [Parser.EOF] }
  | utf8 as c
      { [let s = Ustring.from_utf8 c in
	 raise (Mkl_lex_error (LEX_UNKNOWN_CHAR,ERROR,mkinfo_utf8_fast c,[s]))]}

and mklstring = parse
  | '"'
      { }
  | eof
      { let s = Ustring.from_utf8 ("\"" ^ (Buffer.contents string_buf)) in
	raise (Mkl_lex_error (LEX_STRING_NOT_TERMINATED,ERROR,
		 mkinfo_ustring s, [s])) }
  | s_escape as s
      { Buffer.add_string string_buf s; mklstring lexbuf }
  | '\\' utf8 as s
      { count_ustring  (Ustring.from_utf8 ("\""^(Buffer.contents string_buf)));
        let s2 = Ustring.from_utf8 s in
	raise (Mkl_lex_error (LEX_INVALID_ESCAPE,ERROR,
		 mkinfo_ustring s2, [s2])) }
  | [^ '\\' '\"'] as c
      { Buffer.add_char string_buf c; mklstring lexbuf }

(* Section comment *)

and end_section_comment = parse
  | "*/" as s
      { Buffer.add_string string_buf s }
  | eof
      { let s = Ustring.from_utf8 ("/*" ^ (Buffer.contents string_buf)) in
	raise (Mkl_lex_error (LEX_COMMENT_NOT_TERMINATED,ERROR,
	 	 mkinfo_ustring s, [s])) }
  | _ as c
      { Buffer.add_char string_buf c; end_section_comment lexbuf }
