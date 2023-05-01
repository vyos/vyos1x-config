exception Syntax_error of ((int * int) option * string)

external lex_numeric_compare: string -> string -> int = "caml_lex_numeric_compare"

external length : string -> int = "%string_length"
external unsafe_get : string -> int -> char = "%string_unsafe_get"

module B = Bytes

let bts = B.unsafe_to_string
let bos = B.unsafe_of_string

let get_lexing_position lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  let line_number = p.Lexing.pos_lnum in
  let column = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
  (line_number, column)

(* Modification of String.escaped to leave UTF-8 bytes unescaped *)
let escape_string s =
  let rec escape_if_needed s n i =
    if i >= n then s else
      match unsafe_get s i with
      | '\"' | '\\' | '\000'..'\031' | '\127' ->
          bts (B.escaped (bos s))
      | _ -> escape_if_needed s n (i+1)
  in
  escape_if_needed s (length s) 0

let default default_value opt =
  match opt with
  | None -> default_value
  | Some value -> value

let lexical_numeric_compare s t =
    lex_numeric_compare s t

(** Convert a relative path to an absolute path based on the current working directory *)
let absolute_path relative_path =
    FilePath.make_absolute (Sys.getcwd ()) relative_path
