(* Constraint script for a value of reference tree path. *)


module I = Vyos1x.Internal.Make(Vyos1x.Reference_tree)

let cache_path = "/usr/share/vyos/reftree.cache"

let rt =
    try
        (I.read_internal[@alert "-exn"]) cache_path
    with Vyos1x.Internal.Read_error msg ->
        print_endline msg;
        exit 1

let is_path reftree words =
    match words with
    | "" -> true
    | _ ->
        let path = Vyos1x.Util.list_of_string words in
        if (Vyos1x.Vytree.exists[@alert "-exn"]) reftree path then true
        else false

let usage = Printf.sprintf "Usage: %s <path>" Sys.argv.(0)

let path_str = ref ""

let () =
    let () = Arg.parse [] (fun s -> path_str := s) usage in
    if is_path rt !path_str then ()
    else
        let () =
            Printf.fprintf stdout "%s is not a path of the reference tree" !path_str
        in exit 1
