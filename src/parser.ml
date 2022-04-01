open Util
open Lexing

module I = Vyos1x_parser.MenhirInterpreter

let rec parse vy_inside_node lexbuf (checkpoint : Config_tree.t I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env ->
      let vy_inside_node, token = Vyos1x_lexer.token vy_inside_node lexbuf in
      let startp = lexbuf.lex_start_p
      and endp = lexbuf.lex_curr_p in
      let checkpoint = I.offer checkpoint (token, startp, endp) in
      parse vy_inside_node lexbuf checkpoint
  | I.Shifting _
  | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      parse vy_inside_node lexbuf checkpoint
  | I.HandlingError _env ->
      let line, pos = Util.get_lexing_position lexbuf in
      raise (Syntax_error (Some (line, pos), "Invalid syntax."))
  | I.Accepted v -> v
  | I.Rejected ->
       raise (Syntax_error (None, "invalid syntax (parser rejected the input)"))

let from_string s =
  let vy_inside_node = false in
  let lexbuf = Lexing.from_string s in
  parse vy_inside_node lexbuf (Vyos1x_parser.Incremental.config lexbuf.lex_curr_p)
