(* Incremental completion helper for a value of reference tree path.

   argv.(1) = the value typed so far, if any, as passed by the template
   (${COMP_WORDS[COMP_CWORD]@Q}) with/without a leading and/or trailing '"'.
   Prints one shell-quoted reference path value per line for use by the
   modified bash completion hooks. *)


module I = Vyos1x.Internal.Make(Vyos1x.Reference_tree)

let cache_path = "/usr/share/vyos/reftree.cache"

let rt =
    try
        (I.read_internal[@alert "-exn"]) cache_path
    with Vyos1x.Internal.Read_error msg ->
        print_endline msg;
        exit 1

let get_children reftree words =
    try
        let node =
            match words with
            | "" -> reftree
            | _ ->
                let path = Vyos1x.Util.list_of_string words in
                (Vyos1x.Vytree.get[@alert "-exn"]) reftree path
        in
        Vyos1x.Vytree.list_children node
    with Vyos1x.Vytree.Nonexistent_path -> []

let strip_quotes s =
    let s =
        if String.length s > 0 && s.[0] = '"' then
            String.sub s 1 (String.length s - 1)
        else s
    in
    let n = String.length s in
    if n > 0 && s.[n - 1] = '"' then String.sub s 0 (n - 1) else s

let shell_quote s =
    "'" ^ String.concat "'\\''" (String.split_on_char '\'' s) ^ "'"

let join head w = if head = "" then w else head ^ " " ^ w

let () =
    let cur =
        strip_quotes (if Array.length Sys.argv > 1 then Sys.argv.(1) else "")
    in
    (* split at the last space: [head] = complete words, [last] = word being typed *)
    let head, last =
        match String.rindex_opt cur ' ' with
        | None -> ("", cur)
        | Some i -> (String.sub cur 0 i, String.sub cur (i + 1) (String.length cur - i - 1))
    in
    let pre_list = get_children rt head in
    (* a unique prefix of a child counts as that child *)
    let last =
        if last = "" then last
        else
            match List.filter (String.starts_with ~prefix:last) pre_list with
            | [w] -> w
            | _ -> last
    in
    let head, full = if List.mem last pre_list then (join head last, true) else (head, false) in
    let path_list = if full then begin
        print_endline (shell_quote head); get_children rt head
    end else pre_list in
    List.iter (fun w -> print_endline (shell_quote (join head w))) path_list
