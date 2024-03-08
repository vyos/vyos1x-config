exception Syntax_error of ((int * int) option * string)

external lex_numeric_compare: string -> string -> int = "caml_lex_numeric_compare"

external length : string -> int = "%string_length"
external unsafe_get : string -> int -> char = "%string_unsafe_get"
external char_code: char -> int = "%identity"
external char_chr: int -> char = "%identity"

module B = Bytes

let bts = B.unsafe_to_string
let bos = B.unsafe_of_string

let get_lexing_position lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  let line_number = p.Lexing.pos_lnum in
  let column = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
  (line_number, column)

(* Modification of Bytes.escaped to leave UTF-8 bytes unescaped *)
let escape_bytes s =
  let char_code_zero = 48 in
  let high_bit_set = 128 in
  let n = ref 0 in
  for i = 0 to B.length s - 1 do
    n := !n +
      (match B.unsafe_get s i with
       | '\"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
       | ' ' .. '~' -> 1
       | c when (char_code c >= high_bit_set) -> 1
       | _ -> 4)
  done;
  if !n = B.length s then B.copy s else begin
    let s' = B.create !n in
    n := 0;
    for i = 0 to B.length s - 1 do
      begin match B.unsafe_get s i with
      | ('\"' | '\\') as c ->
          B.unsafe_set s' !n '\\'; incr n; B.unsafe_set s' !n c
      | '\n' ->
          B.unsafe_set s' !n '\\'; incr n; B.unsafe_set s' !n 'n'
      | '\t' ->
          B.unsafe_set s' !n '\\'; incr n; B.unsafe_set s' !n 't'
      | '\r' ->
          B.unsafe_set s' !n '\\'; incr n; B.unsafe_set s' !n 'r'
      | '\b' ->
          B.unsafe_set s' !n '\\'; incr n; B.unsafe_set s' !n 'b'
      | (' ' .. '~') as c -> B.unsafe_set s' !n c
      | c when (char_code c >= high_bit_set ) -> B.unsafe_set s' !n c
      | c ->
          let a = char_code c in
          B.unsafe_set s' !n '\\';
          incr n;
          B.unsafe_set s' !n (char_chr (char_code_zero + a / 100));
          incr n;
          B.unsafe_set s' !n (char_chr (char_code_zero + (a / 10) mod 10));
          incr n;
          B.unsafe_set s' !n (char_chr (char_code_zero + a mod 10));
      end;
      incr n
    done;
    s'
  end

(* Modification of String.escaped to leave UTF-8 bytes unescaped *)
let escape_string s =
  let rec escape_if_needed s n i =
    if i >= n then s else
      match unsafe_get s i with
      | '\"' | '\\' | '\000'..'\031' | '\127' ->
          bts (escape_bytes (bos s))
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
