exception Syntax_error of ((int * int) option * string)

val get_lexing_position : Lexing.lexbuf -> int * int

val escape_string : string -> string

val default : 'a -> 'a option -> 'a

val lexical_numeric_compare : string -> string -> int

val absolute_path : FilePath.filename -> FilePath.filename

val string_of_list : string list -> string

val json_of_list : string list -> string

val list_of_path : string -> string list

val drop_last : 'a list -> 'a list

val drop_last_n : 'a list -> int -> 'a list

val drop_first : 'a list -> 'a list

val get_last : 'a list -> 'a option

val get_last_n : 'a list -> int -> 'a option

val lex_order : string list -> string list -> int

val colex_order : string list -> string list -> int

val is_empty : 'a list -> bool

val is_sublist : 'a list -> 'a list -> bool

val flag : 'a list -> 'a list list
