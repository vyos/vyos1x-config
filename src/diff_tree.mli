
val diff_tree_data: ?diff_comments:bool -> string list -> Config_tree.t -> Config_tree.t ->
    Config_tree.t * ((string list * string option) list)
val diff_tree: ?diff_comments:bool -> string list -> Config_tree.t -> Config_tree.t -> Config_tree.t
[@@alert exn "Diff.Incommensurable"]
[@@alert exn "Diff.Empty_comparison"]

val get_tagged_delete_tree : Config_tree.t -> Config_tree.t

