exception Syntax_error of ((int * int) option * string)

val get_lexing_position : Lexing.lexbuf -> int * int

val default : 'a -> 'a option -> 'a
