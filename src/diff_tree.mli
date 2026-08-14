
val diff_tree: string list -> Config_tree.t -> Config_tree.t -> Config_tree.t
[@@alert exn "Diff.Incommensurable"]
[@@alert exn "Diff.Empty_comparison"]

val get_tagged_delete_tree : Config_tree.t -> Config_tree.t

