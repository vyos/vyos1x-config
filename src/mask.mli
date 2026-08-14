val mask_inclusive : Config_tree.t -> Config_tree.t -> Config_tree.t
[@@alert exn "Diff.Incommensurable"]
[@@alert exn "Diff.Empty_comparison"]

val mask_exclusive : Config_tree.t -> Config_tree.t -> Config_tree.t
[@@alert exn "Diff.Incommensurable"]
[@@alert exn "Diff.Empty_comparison"]
