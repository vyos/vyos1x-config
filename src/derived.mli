exception Malformed_path of string

val subtree_from_partial : ?descent:bool -> Reference_tree.t -> Config_tree.t -> Config_tree.t -> string list -> Config_tree.t
[@@alert exn "Derived.Malformed_path"]
