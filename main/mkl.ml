

open Utils
open Ustring.Op
open Printf
open Message



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









