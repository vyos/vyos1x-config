type change = Unchanged | Added | Subtracted | Updated of string list

module Diff_tree : sig
    type t = { left: Config_tree.t;
               right: Config_tree.t;
               add: Config_tree.t;
               sub: Config_tree.t;
               del: Config_tree.t;
               inter: Config_tree.t;
             }
end

module Diff_string : sig
    type t = { left: Config_tree.t;
               right: Config_tree.t;
               skel: Config_tree.t;
               ppath: string list;
               udiff: string;
             }
end

type _ diff_result =
    | Diff_tree : Diff_tree.t -> Diff_tree.t diff_result
    | Diff_string : Diff_string.t -> Diff_string.t diff_result

val eval_diff_result : 'a diff_result -> 'a

type 'a diff_func = ?recurse:bool -> string list -> 'a diff_result -> change -> 'a diff_result
val diff : string list -> 'a diff_func -> 'a diff_result -> Config_tree.t option * Config_tree.t option -> 'a diff_result

exception Incommensurable
exception Empty_comparison
exception Nonexistent_child

val clone : ?recurse:bool -> ?set_values:string list option -> Config_tree.t -> Config_tree.t ->string list -> Config_tree.t
[@@alert exn "Vytree.Nonexistent_path"]

val diff_tree : string list -> Config_tree.t -> Config_tree.t -> Config_tree.t
[@@alert exn "Config_diff.Incommensurable"]
[@@alert exn "Config_diff.Empty_comparison"]

val show_diff : ?cmds:bool -> string list -> Config_tree.t -> Config_tree.t -> string
[@@alert exn "Config_diff.Incommensurable"]
[@@alert exn "Config_diff.Empty_comparison"]

val tree_union : Config_tree.t -> Config_tree.t -> Config_tree.t
[@@alert exn "Tree_alg.Incompatible_union"]
[@@alert exn "Tree_alg.Nonexistent_child"]

val tree_merge : ?destructive:bool -> Config_tree.t -> Config_tree.t -> Config_tree.t
[@@alert exn "Tree_alg.Incompatible_union"]
[@@alert exn "Tree_alg.Nonexistent_child"]

val mask_tree : Config_tree.t -> Config_tree.t -> Config_tree.t
[@@alert exn "Config_diff.Incommensurable"]
[@@alert exn "Config_diff.Empty_comparison"]

val get_tagged_delete_tree : Config_tree.t -> Config_tree.t
