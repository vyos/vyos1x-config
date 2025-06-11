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

module Diff_cstore : sig
    type t = { left: Config_tree.t;
               right: Config_tree.t;
               handle: int;
               out: string;
             }
end

type _ result =
    | Diff_tree : Diff_tree.t -> Diff_tree.t result
    | Diff_string : Diff_string.t -> Diff_string.t result
    | Diff_cstore : Diff_cstore.t -> Diff_cstore.t result

val eval_result : 'a result -> 'a

type 'a diff_func = ?recurse:bool -> string list -> 'a result -> change -> 'a result
val diff : string list -> 'a diff_func -> 'a result -> Config_tree.t option * Config_tree.t option -> 'a result

exception Incommensurable
exception Empty_comparison
exception Nonexistent_child

val clone : ?recurse:bool -> ?set_values:string list option -> Config_tree.t -> Config_tree.t ->string list -> Config_tree.t
val diff_tree : string list -> Config_tree.t -> Config_tree.t -> Config_tree.t
val show_diff : ?cmds:bool -> string list -> Config_tree.t -> Config_tree.t -> string
val tree_union : Config_tree.t -> Config_tree.t -> Config_tree.t
val tree_merge : ?destructive:bool -> Config_tree.t -> Config_tree.t -> Config_tree.t
val mask_tree : Config_tree.t -> Config_tree.t -> Config_tree.t
val make_diff_cstore : Config_tree.t -> Config_tree.t -> int -> Diff_cstore.t result
val get_tagged_delete_tree : Config_tree.t -> Config_tree.t
