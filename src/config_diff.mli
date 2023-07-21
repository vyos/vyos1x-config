
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
               right : Config_tree.t;
               skel: Config_tree.t;
               ppath: string list;
               udiff: string;
             }
end

module Diff_cstore : sig
    type t = { left: Config_tree.t;
               right: Config_tree.t;
               handle: int;
             }
end

type _ result =
    | Diff_tree : Diff_tree.t -> Diff_tree.t result
    | Diff_string : Diff_string.t -> Diff_string.t result
    | Diff_cstore : Diff_cstore.t -> Diff_cstore.t result

exception Incommensurable
exception Empty_comparison
exception Nonexistent_child

val diff_tree : string list -> Config_tree.t -> Config_tree.t -> Config_tree.t
val show_diff : ?cmds:bool -> string list -> Config_tree.t -> Config_tree.t -> string
val tree_union : Config_tree.t -> Config_tree.t -> Config_tree.t
