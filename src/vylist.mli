val find : ('a -> bool) -> 'a list -> 'a option

val remove : ('a -> bool) -> 'a list -> 'a list

val replace : ?force:bool -> ('a -> bool) -> 'a -> 'a list -> 'a list
[@@alert exn "Not_found"]

val insert_before : ('a -> bool) -> 'a -> 'a list -> 'a list
[@@alert exn "Not_found"]

val insert_after : ('a -> bool) -> 'a -> 'a list -> 'a	list
[@@alert exn "Not_found"]

val insert_compare : ('a -> 'a -> int) -> 'a -> 'a list -> 'a list

val complement : 'a list -> 'a list -> 'a list

val in_list : 'a list -> 'a -> bool
