exception Syntax_error of ((int * int) option * string)

val get_lexing_position : Lexing.lexbuf -> int * int

val escape_string : string -> string

val default : 'a -> 'a option -> 'a

val lexical_numeric_compare : string -> string -> int

val absolute_path : FilePath.filename -> FilePath.filename
