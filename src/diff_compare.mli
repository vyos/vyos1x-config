
val diff_compare : ?cmds:bool -> string list -> Config_tree.t -> Config_tree.t -> string
[@@alert exn "Diff.Incommensurable"]
[@@alert exn "Diff.Empty_comparison"]
