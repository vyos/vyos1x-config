
open Reference_tree

val get_multi_nodes : ?tag_value_placeholder:string -> t -> string list list
val get_tag_nodes : ?tag_value_placeholder:string -> t -> string list list
val get_nodes_of_kind : ?tag_value_placeholder:string -> t -> string -> string list list
val get_rdeps_of_kind : ?tag_value_placeholder:string -> t -> string -> string list list
val get_rdeps_of_kind_data : ?tag_value_placeholder:string -> t -> string -> (string list * string) list

val get_multi_nodes_yojson : ?tag_value_placeholder:string -> t -> string
val get_tag_nodes_yojson: ?tag_value_placeholder:string -> t -> string
val get_nodes_of_kind_yojson : ?tag_value_placeholder:string -> t -> string -> string
val get_rdeps_of_kind_yojson : ?tag_value_placeholder:string -> t -> string -> string
val get_rdeps_of_kind_data_yojson : ?tag_value_placeholder:string -> t -> string -> string

val get_path_owner : t -> string list -> string option
