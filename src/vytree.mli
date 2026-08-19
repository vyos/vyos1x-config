type 'a t [@@deriving yojson]

exception Empty_path
exception Duplicate_child
exception Nonexistent_path
exception Insert_error of string

type position = Before of string | After of string | Lexical | End | Default

val make : 'a -> string -> 'a t
val make_full : 'a -> string -> ('a t) list -> 'a t

val name_of_node : 'a t -> string
val data_of_node : 'a t -> 'a
val children_of_node : 'a t -> 'a t list

val find : 'a t -> string -> 'a t option

val adopt : 'a t -> 'a t -> 'a t

val replace : 'a t -> 'a t -> 'a t
[@@alert exn "Not_found"]

val insert : ?position:position -> ?children:('a t list) -> 'a t -> string list -> 'a -> 'a t
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Not_found possible if position Before/After"]
[@@alert exn "Vytree.Duplicate_child"]
[@@alert exn "Vytree.Insert_error"]

val insert_maybe : ?position:position -> 'a t -> string list -> 'a -> 'a t
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Not_found possible if position Before/After"]
[@@alert exn "Vytree.Insert_error"]

val insert_or_update : ?position:position -> 'a t -> string list -> 'a -> 'a t
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Not_found possible if position Before/After"]
[@@alert exn "Vytree.Insert_error"]

val insert_multi_level : ?position:position -> 'a -> 'a t -> string list -> string list -> 'a -> 'a t
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Not_found possible if position Before/After"]
[@@alert exn "Vytree.Duplicate_child possible if path_remaining not complement"]
[@@alert exn "Vytree.Insert_error possible if path_done existence not guaranteed"]

val merge_children : ('a -> 'a -> 'a) -> (string -> string -> int) -> 'a t -> 'a t

val delete : 'a t -> string list -> 'a t
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val update : 'a t -> string list -> 'a -> 'a t
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val rename : 'a t -> string list -> string -> 'a t
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Not_found"]
[@@alert exn "Vytree.Nonexistent_path"]

val list_children : 'a t -> string list

val get : 'a t -> string list -> 'a t
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val get_existent_path : 'a t -> string list -> string list

val get_data : 'a t -> string list -> 'a
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val exists : 'a t -> string list -> bool
[@@alert exn "Vytree.Empty_path"]

val children_of_path : 'a t -> string list -> string list
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val sorted_children_of_node : (string -> string -> int) -> 'a t -> ('a t) list

val sort_children : (string -> string -> int) -> 'a t -> 'a t

val copy : 'a t -> string list -> string list -> 'a t
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]
[@@alert exn "Vytree.Insert_error"]

val move : 'a t -> string list -> position -> 'a t
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]
[@@alert exn "Not_found possible if position Before/After"]
[@@alert exn "Vytree.Insert_error"]

val is_terminal_path : 'a t -> string list -> bool
[@@alert exn "Vytree.Empty_path"]

val fold_tree_with_path: (string list * 'acc -> 'b t -> string list * 'acc) -> string list * 'acc -> 'b t -> 'acc

val fold_tree_with_path_and_stack: ((string list * 'a list) * 'acc -> 'b t -> (string list * 'a list) * 'acc) -> (string list * 'a list) * 'acc -> 'b t -> 'acc
