type change = Unchanged | Added | Subtracted | Updated of string list

type diff_func = ?recurse:bool -> string list -> change -> unit

type diff_trees = {
    left: Config_tree.t;
    right: Config_tree.t;
    add: Config_tree.t ref;
    sub: Config_tree.t ref;
    inter: Config_tree.t ref;
}

exception Incommensurable
exception Empty_comparison

val make_diff_trees : Config_tree.t -> Config_tree.t -> diff_trees
val clone : ?recurse:bool -> ?set_values:(string list) option -> Config_tree.t -> Config_tree.t -> string list -> Config_tree.t
val decorate_trees : diff_trees -> ?recurse:bool -> string list -> change -> unit
val trim_trees : diff_trees -> ?recurse:bool -> string list -> change -> unit
val compare : string list -> Config_tree.t -> Config_tree.t -> diff_trees
val diff_tree : string list -> Config_tree.t -> Config_tree.t -> Config_tree.t
val trim_tree : Config_tree.t -> Config_tree.t -> Config_tree.t

