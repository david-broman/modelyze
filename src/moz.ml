

open Utils
open Ustring.Op
open Printf
open Message


  

let mkleval libpath filename =   
  (try 
         Fileincluder.read_file_chain libpath filename 
      |> Toplevel.desugar 
      |> Pattern.desugar
      |> Typesystem.typecheck 
      |> Translate.translate 
      |> Eval.evaluate 
      |> ignore
  with
    | Ast.Mkl_runtime_error m -> fprintf stderr "%s\n" 
	(Ustring.to_utf8 (Message.message2str m)); exit 1
    | Message.Mkl_static_error m -> fprintf stderr "%s\n" 
	(Ustring.to_utf8 (Message.message2str m)); exit 1
    | Message.Mkl_lex_error m -> fprintf stderr "%s\n" 
	(Ustring.to_utf8 (Message.message2str m)); exit 1
    | Parsing.Parse_error -> fprintf stderr "%s\n" 
	(Ustring.to_utf8 (Message.message2str (Lexer.parse_error_message()))); exit 1
    | Typesystem.Mkl_type_error m -> fprintf stderr "%s\n" 
	(Ustring.to_utf8 (Message.message2str m)); exit 1
  )
  

let menu() =
    printf "Modelyze Interpreter. (C) Copyright David Broman 2010-2014\n";
    printf "Usage: moz [OPTIONS] <file.moz>\n";
    printf "\n";
    printf "OPTIONS:\n";
    printf "    --libpaths=<path>  Colon separated paths to Modelyze library files.\n"

let extract_libpaths str = 
  let str2 = Ustring.trim (us str) in
  let switch = us"--libpaths=" in
  let switch_len = Ustring.length switch in
  if not (Ustring.starts_with switch str2) then None 
  else
    let str3 = Ustring.sub str2 switch_len (Ustring.length str2 - switch_len) in
    let slash = us"/" in
    let str4 = List.map (fun s -> if Ustring.ends_with s slash 
                                  then s else s ^. slash) (
                         Ustring.split str3 (us":")) in
    Some(List.map Ustring.to_utf8 str4)
  


let main =
  match Array.length Sys.argv with
  | 2 -> (match extract_libpaths Sys.argv.(1) with
         | Some(paths) -> menu()
         | None -> mkleval [] (Sys.argv.(1)))
  | 3 -> (match extract_libpaths Sys.argv.(1) with 
         | Some(paths) -> mkleval paths (Sys.argv.(2)) 
         | None -> menu())
  | _ -> menu()









