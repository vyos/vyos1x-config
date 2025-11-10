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

(** Convert a list of strings to a string of unquoted, space separated words *)
let string_of_list ss =
    let rec aux xs acc =
        match xs with
        | [] -> acc
        | x :: xs' -> aux xs' (Printf.sprintf "%s %s" acc x)
    in
    match ss with
    | [] -> ""
    | x :: xs -> Printf.sprintf "%s%s" x (aux xs "")

(** Convert a list of strings to JSON *)
let json_of_list ss =
    let ss = List.map (fun x -> `String x) ss in
    Yojson.Safe.to_string (`List ss)

(** Split string on whitespace, excluding single-quoted phrases,
    as needed for parsing vyconf request path option **)
let list_of_path p =
    let seg = String.trim p |> String.split_on_char '\'' in
    match seg with
    | [h] -> Pcre2.split ~pat:"\\s+" h
    | h :: h' :: _ -> (Pcre2.split ~pat:"\\s+" h) @ [h']
    | _ -> []


let drop_last l =
    let rec aux acc l =
        match l with
        | [] | [_] -> List.rev acc
        | hd :: tl ->
            let acc' = hd :: acc in
            aux acc' tl
    in
    aux [] l

let drop_last_n l n =
    let rec aux k l =
        match l with
        | [] -> []
        | _ -> if k <= 0 then l else aux (k - 1) (drop_last l)
    in aux n l

let drop_first l =
    match l with
    | [] -> []
    | _ :: tl -> tl

let rec get_last l =
    match l with
    | [] -> None
    | h :: [] -> Some h
    | _ :: tl -> get_last tl

let get_last_n l n =
    get_last (drop_last_n l n)

let lex_order l k =
    let c = compare (get_last l) (get_last k) in
    match c with
    | 0 -> compare (drop_last l) (drop_last k)
    | _ as r -> r

let colex_order l k =
    let rec comp x y =
        let c = compare (get_last x) (get_last y) in
        match c with
        | 0 -> comp (drop_last x) (drop_last y)
        | _ as r -> r
    in comp l k

let is_empty l =
    List.compare_length_with l 0 = 0

let rec is_sublist l k =
    match l, k with
    | [], _ -> true
    | _, [] -> false
    | hl::tl, hk::tk -> hl = hk && is_sublist tl tk
