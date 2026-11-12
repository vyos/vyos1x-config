module type C = sig
    val render_values : ?ord_val:bool -> string -> string -> string list -> string
    val render_comment : string -> string option -> string
end

module type R = sig
    val render_node : ?ord_val:bool -> int -> int -> Config_tree.t -> string
    val render_relative_node : ?ord_val:bool -> Config_tree.t -> string list -> int -> int -> string
    val render_config : ?ord_val: bool -> Config_tree.t -> string
end

module Make : functor (X : C) -> R

module Renderer :
  sig
    val render_node : ?ord_val:bool -> int -> int -> Config_tree.t -> string
    val render_config : ?ord_val:bool -> Config_tree.t -> string
  end

val make_indent : int -> int -> string
val render_relative_node : ?ord_val:bool -> Config_tree.t -> string list -> int -> int -> string
val render_config : ?ord_val:bool -> Config_tree.t -> string
val render_values : ?ord_val:bool -> string -> string -> string list -> string
val render_comment : string -> string option -> string

val make : (?ord_val:bool -> string -> string -> string list -> string) ->
    (string -> string option -> string) -> (module R)
