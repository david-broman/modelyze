

open Ustring.Op


val shellcmd : string -> (int * ustring * ustring)
(** [shellcmd cmd] executes shell command [cmd] and returns a triple,
    where the first element is the exit code, the second element is the
    standard output, and the third element is the standard error output. *)

