type value_behaviour = AddValue | ReplaceValue
type command = Set | Delete

exception Duplicate_value
exception Node_has_no_value
exception No_such_value
exception Useless_set

type config_node_data = {
  values : string list;
  comment : string option;
  tag : bool;
} [@@deriving yojson]

type t = config_node_data Vytree.t [@@deriving yojson]

val default_data : config_node_data

val make : string -> t

val set : t -> string list -> string option -> value_behaviour -> t

val delete : t -> string list -> string option -> t

val get_values : t -> string list -> string list

val get_value : t -> string list -> string

val set_comment : t -> string list -> string option -> t

val get_comment : t -> string list -> string option

val set_tag : t -> string list -> bool -> t

val is_tag : t -> string list -> bool

val get_subtree : ?with_node:bool -> t -> string list -> t

val render_commands : ?op:command -> t -> string list -> string

val render_config : ?ord_val:bool -> t -> string

val render_json : t -> string

val render_json_ast : t -> string
