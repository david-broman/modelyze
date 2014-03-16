
open Ustring.Op

type keylist = ustring list
type description = ustring
type usage_msg = ustring 

type opt =
| ArgHeading of ustring
| ArgString  of keylist * ustring ref * description
| ArgSet     of keylist * bool ref * description
| ArgClear   of keylist * bool ref * description
| ArgInt     of keylist * int ref * description
| ArgRest    of ustring * (ustring list) ref * description

(** [parse_argv a o] takes an array [a] of utf8 encoded strings and a list
    of command options [o] as input. *)
val parse_argv_basic : string array -> opt list -> ustring list


