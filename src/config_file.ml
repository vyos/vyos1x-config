(* strip commponent version string *)
let strip_version s =
    let rex = Pcre.regexp ~flags:[`MULTILINE;`DOTALL] "(^//.*)" in
    let res = Pcre.split ~max:0 ~rex s in
    match res with
    | h :: _ -> Ok h
    | [] -> Error "Failure stripping version string from config"

let load_config file =
    try
        let chan = open_in file in
        let s = really_input_string chan (in_channel_length chan) in
        let () = close_in chan in
        let prep = strip_version s in
        let s = match prep with
            | Ok s -> s
            | Error msg -> raise (Sys_error msg)
        in
        let config = Parser.from_string s in
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
        let s = Config_tree.render_config ct in
        let chan = open_out file in
        let () = output_string chan s in
        let () = close_out chan in
        let () = Unix.chmod file 0o664 in
        Ok ()
    with
        Sys_error msg -> Error msg

