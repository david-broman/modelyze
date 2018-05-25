


open Ustring.Op


type argtype =
| No      (* No arguments *)
| Str     (* Argument is a normal string *)
| Int     (* The argument is an integer that can be both postive and negative *)
| StrList (* The argument can be a list of strings. The list can be empty. *)
| IntList (* The argument is a list of integers. The list can be empty. *) 

exception Error of ustring


val parse : string list -> ('a * argtype * ustring * ustring * ustring) list 
    -> (('a * ustring list) list * ustring list) 
(** [parse argv options] parses the argument options [argv] using the
    information described in the [options] parameter. The function
    returns an associative list, where the keys are options and the
    values are lists of option arguments. Exception [Error] is
    raised if there is a parse error. See example code for more
    information *)

val optionstext : ?indent:int -> ?max_op_len:int -> ?line_length:int 
    -> ('a * argtype * ustring * ustring * ustring) list -> ustring
(** [optionstext options] takes the list of [options] and create a
    readable text that can be used as part of a help text. There are
    also three optional arguments: [indent] states how many spaces
    that each option line should start with. Default is
    2. [max_op_length] is the longest option text that should be able
    to be on the first column, without creating a new line. Default
    value is 25. Finally, the optional argument [line_length] is the
    maximal length in characters of a line.  The default value is
    80. *)

val has_op : 'a -> ('a * ustring list) list -> bool
(** [has_op op oplst] returns true if option [op] exits in [op] list *)
        
val str_op : 'a -> ('a * ustring list) list -> ustring
(** [str_op op oplst] returns a ustring of the option argument for
    option [op]. If no such argument is found, exception [Not_found]
    is raised. *)

val strlist_op : 'a -> ('a * ustring list) list -> ustring list
(** [strlist_op op oplst] returns a ustring list of the option argument for
    option [op]. If no such argument is found, exception [Not_found]
    is raised. *)


val int_op : 'a -> ('a * ustring list) list -> int
(** [int_op op oplst] returns an integer of the option argument for
    option [op]. If no such argument is found, exception [Not_found]
    is raised. *)

val intlist_op : 'a -> ('a * ustring list) list -> int list
(** [intlist_op op oplst] returns an integer list of the option argument for
    option [op]. If no such argument is found, exception [Not_found]
    is raised. *)
  
  


