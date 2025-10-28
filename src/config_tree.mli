type value_behaviour = AddValue | ReplaceValue [@@deriving yojson]
type command = Set | Delete

exception Duplicate_value
exception Node_has_no_value
exception No_such_value
exception Useless_set

type config_node_data = {
  values : string list;
  comment : string option;
  tag : bool;
  leaf: bool;
} [@@deriving yojson]

type t = config_node_data Vytree.t [@@deriving yojson]

val default_data : config_node_data

val default : t

val make : string -> t

val create_node : t -> string list -> t
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Config_tree.Useless_set"]

val set : t -> string list -> string option -> value_behaviour -> t
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Config_tree.Useless_set"]
[@@alert exn "Config_tree.Duplicate_value"]

val delete : t -> string list -> string option -> t
[@@alert exn "Vytree.Nonexistent_path"]
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Config_tree.No_such_value"]

val replace_value : t -> string list -> string -> t
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val prune_delete : t -> string list -> t
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val get_values : t -> string list -> string list
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val get_value : t -> string list -> string
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]
[@@alert exn "Config_tree.Node_has_no_value"]

val value_exists : t -> string list -> string -> bool
[@@alert exn "Vytree.Empty_path"]

val set_comment : t -> string list -> string option -> t
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val get_comment : t -> string list -> string option
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val set_tag : t -> string list -> bool -> t
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val is_tag : t -> string list -> bool
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val is_tag_value : t -> string list -> bool
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val set_leaf : t -> string list -> bool -> t
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val is_leaf : t -> string list -> bool
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val get_subtree : ?with_node:bool -> t -> string list -> t

val value_paths_of_tree : t -> string list list

val render_commands : ?op:command -> t -> string list -> string
[@@alert exn "Vytree.Nonexistent_path"]

val render_config : ?ord_val:bool -> t -> string

val render_json : t -> string

val render_json_ast : t -> string

val render_at_level : t -> string list -> string
[@@alert exn "Vytree.Nonexistent_path"]
