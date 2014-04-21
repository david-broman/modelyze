

let shellcmd cmd =
  let uid = "e42ff037-67dc-4189-bb23-d40649b5c11b" in
  let tmpout = "tmp_" ^ uid ^ "_out" in
  let tmperr = "tmp_" ^ uid ^ "_err" in
  let retcode = Sys.command (cmd ^ " 1> " ^ tmpout ^ " 2> " ^ tmperr) in
  let msgout = Ustring.read_file tmpout in
  let msgerr = Ustring.read_file tmperr in
  Sys.remove tmpout; Sys.remove tmperr; 
  (retcode, msgout, msgerr)

