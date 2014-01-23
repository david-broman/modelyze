type row = int
type col = int
type filename = Ustring.Op.ustring
type info = Info of filename * row * col * row * col | NoInfo
val mkinfo : info -> info -> info
val mk_right_info : info -> info
val mk_left_info : info -> info
val mkinfo_lst : info list -> info
val get_filename : info -> Ustring.Op.ustring
val info2str : info -> Ustring.Op.ustring
