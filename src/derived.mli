exception Malformed_path of string

val subtree_from_partial : Reference_tree.t -> Config_tree.t -> Config_tree.t -> string list -> Config_tree.t
[@@alert exn "Derived.Malformed_path"]
