exception Malformed_path of string

val subtree_from_partial : ?descent:bool -> Reference_tree.t -> Config_tree.t -> Config_tree.t -> string list -> Config_tree.t
[@@alert exn "Derived.Malformed_path"]

val subtree_values_of_path : Reference_tree.t -> Config_tree.t -> string list -> (string list * string list) list
[@@alert exn "Derived.Malformed_path"]

val subtree_values_of_path_yojson : Reference_tree.t -> Config_tree.t -> string list -> string
[@@alert exn "Derived.Malformed_path"]
