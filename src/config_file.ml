
let rx =
    let p =
        {|("[^"]*[\][^"]*"\n|'[^']*[\][^']*'\n)|}
    in
    Pcre2.regexp p

let escape_backslash s =
    let func s =
        Pcre2.qreplace ~pat:{|\\|} ~templ:{|\\|} s
    in
    Pcre2.substitute ~rex:rx ~subst:func s

let unescape_backslash s =
    let defunc s =
        Pcre2.qreplace ~pat:{|\\\\|} ~templ:{|\|} s
    in
    Pcre2.substitute ~rex:rx ~subst:defunc s


(* strip commponent version string *)
let strip_version s =
    let rex = Pcre2.regexp ~flags:[`MULTILINE;`DOTALL] "(^//.*)" in
    let res = Pcre2.split ~max:0 ~rex s in
    match res with
    | h :: _ -> Ok h
    | [] -> Error "Failure stripping version string from config"

let load_config file =
    (* alert exn Parser.from_string:
        [Util.Syntax_error] caught
     *)
    try
        let chan = open_in file in
        let s = really_input_string chan (in_channel_length chan) in
        let () = close_in chan in
        let prep = strip_version s in
        let s = match prep with
            | Ok t -> escape_backslash t
            | Error msg -> raise (Sys_error msg)
        in
        let config = (Parser.from_string[@alert "-exn"]) s in
        Ok config
    with
        | Sys_error msg -> Error msg
        | Util.Syntax_error (opt, msg) ->
            begin
                match opt with
                | None ->
                    let out = Printf.sprintf "Parse error: %s\n" msg
                    in Error out
                | Some (line, pos) ->
                    let out = Printf.sprintf "Parse error: %s line %d pos %d\n" msg line pos
                    in Error out
            end

let save_config ct file =
    try
        let t = Config_tree.render_config ct in
        let s = unescape_backslash t in
        let chan = open_out file in
        let () = output_string chan s in
        let () = close_out chan in
        let () = Unix.chmod file 0o664 in
        Ok ()
    with
        Sys_error msg -> Error msg

