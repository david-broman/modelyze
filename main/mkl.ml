

open Utils
open Ustring.Op
open Printf
open Message


let mkllex filename = 
  let fs1 = open_in filename in
  try 
    let lexbuf = Ustring.lexing_from_channel fs1 in
    Lexer.init (us filename) 8;
    let tokens = Parser.tokens Lexer.main lexbuf in
    let tokensstr = Ustring.concat (us"\n") tokens in 
    uprint_endline tokensstr  
  with
    | Message.Mkl_lex_error m -> fprintf stderr "%s\n" 
	(Ustring.to_utf8 (Message.message2str m));
  close_in fs1



let mkleval filename =   
  (try 
         Fileincluder.read_file_chain filename 
      |> Toplevel.desugar 
      |> Pattern.desugar
      |> Typesystem.typecheck 
      |> Eval.translate 
      |> Eval.evaluate 
      |> ignore
  with
    | Ast.Mkl_runtime_error m -> fprintf stderr "%s\n" 
	(Ustring.to_utf8 (Message.message2str m))
    | Message.Mkl_static_error m -> fprintf stderr "%s\n" 
	(Ustring.to_utf8 (Message.message2str m))
    | Message.Mkl_lex_error m -> fprintf stderr "%s\n" 
	(Ustring.to_utf8 (Message.message2str m))
    | Parsing.Parse_error -> fprintf stderr "%s\n" 
	(Ustring.to_utf8 (Message.message2str (Lexer.parse_error_message())))  
    | Typesystem.Mkl_type_error m -> fprintf stderr "%s\n" 
	(Ustring.to_utf8 (Message.message2str m))
  )
  

let menu() =
    printf "Usage: mkl <file.mkl>\n"


let main =
  if Array.length Sys.argv < 2 then menu()
  else
    let filename = Sys.argv.(1) in
    mkleval filename









