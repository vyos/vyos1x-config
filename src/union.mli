val tree_union : Config_tree.t -> Config_tree.t -> Config_tree.t
[@@alert exn "Tree_alg.Incompatible_union"]
[@@alert exn "Tree_alg.Nonexistent_child"]

val tree_merge : ?destructive:bool -> Config_tree.t -> Config_tree.t -> Config_tree.t
[@@alert exn "Tree_alg.Incompatible_union"]
[@@alert exn "Tree_alg.Nonexistent_child"]
