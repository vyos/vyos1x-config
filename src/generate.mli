exception Load_error of string
exception Write_error of string

val load_interface_definitions : string -> (Reference_tree.t, string) result
val reference_tree_to_json : string -> string -> unit
