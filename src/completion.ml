type op_type = Set | Delete | Show | Comment | Unknown

let op_of_string op_str =
    match op_str with
    | "set" -> Set
    | "delete" -> Delete
    | "show" -> Show
    | "comment" -> Comment
    | _ -> Unknown

type completion_env = {
    name: string;
    path_typ: Reference_tree.path_type;
    values: string list;
    completion_help: Reference_tree.completion_help_type list;
    help: string;
    value_help: (string * string) list;
    multi: bool;
} [@@deriving yojson]

type completion_env_list = completion_env list [@@deriving yojson]

let get_completion_data (node: Reference_tree.t) =
    let name = Vytree.name_of_node node in
    let data = Vytree.data_of_node node in
    { name=name;
      values=[];
      path_typ=`Other;
      multi=data.multi;
      completion_help=data.completion_help;
      help=data.help;
      value_help=data.value_help;
    }

let execute_completion_script cmd =
    let () = Unix.putenv "vyos_completion_dir" "/usr/libexec/vyos/completion" in
    let chan = Unix.open_process_in cmd in
    let out = In_channel.input_all chan in
    let result = Unix.close_process_in chan in
    match result with
    | Unix.WEXITED 0 -> out
    | _ -> ""

let read_completion_help ctree (lst: Reference_tree.completion_help_type list) =
    let read_elem chelp =
        match chelp with
        | Reference_tree.List l -> Util.list_of_string l
        | Reference_tree.Path p ->
            begin
            let path = Util.list_of_string p in
            try (Vytree.children_of_path[@alert "-exn"]) ctree path
            with Vytree.Empty_path | Vytree.Nonexistent_path -> []
            end
        | Reference_tree.Script s ->
            Util.list_of_string (execute_completion_script s)
    in
    let func acc elem =
        acc @ (read_elem elem)
    in
    List.fold_left func [] lst

let get_completion_env rtree ctree op cpath =
    let op = op_of_string op in
    match op with
    | Unknown -> Error {|Unknown operation|}
    | _ ->
    let restricted =
        match op with
        | Delete | Show | Comment -> true
        | _ -> false
    in
    let last = Util.get_last cpath in
    let last_elt =
        match last with
        | None -> ""
        | Some c -> c
    in
    let path = Util.drop_last cpath in
    let path_typ = Reference_tree.get_path_type rtree path in
    let rpath = Reference_tree.refpath rtree path in
    if restricted && not (Util.is_empty path) && not ((Vytree.exists[@alert "-exn"]) ctree path)
    then Error {|Nonexistent path|}
    else
    match path_typ with
    | `Invalid -> Error {|Invalid path|}
    | `Leaf_value -> Error {|Leaf value|}
    | `Leaf | `Multi ->
        let compl_env =
            get_completion_data ((Vytree.get[@alert "-exn"]) rtree rpath) in
        let comp_help = read_completion_help ctree compl_env.completion_help
        in
        let values =
            match comp_help with
            | [] ->
                begin
                try
                    (Config_tree.get_values[@alert "-exn"]) ctree path
                with Vytree.Nonexistent_path -> []
                end
            | _ as l -> l
        in
        Ok [{ compl_env with values = values; path_typ = `Leaf_value }]
    | `Tag ->
        let compl_env =
            get_completion_data ((Vytree.get[@alert "-exn"]) rtree rpath)
        in
        let comp_help = read_completion_help ctree compl_env.completion_help
        in
        let values =
            match comp_help with
            | [] ->
                begin
                try
                    Vytree.list_children ((Vytree.get[@alert "-exn"]) ctree path)
                with Vytree.Nonexistent_path -> []
                end
            | _ as l -> l
        in
        Ok [{ compl_env with values = values; path_typ = `Tag_value }]
    | _ ->
        let rnode =
            match rpath with
            | [] -> rtree
            | _ -> (Vytree.get[@alert "-exn"]) rtree rpath
        in
        let cnode =
            match path with
            | [] -> ctree
            | _ -> try (Vytree.get[@alert "-exn"]) ctree path
                   with Vytree.Nonexistent_path -> Config_tree.default
        in
        let children =
            let child_set = Vytree.children_of_node rnode in
            if not restricted then child_set
            else
            let extant_children = Vytree.list_children cnode in
            let is_extant s =
                List.mem (Vytree.name_of_node s) extant_children
            in
            List.filter is_extant child_set
        in
        let children' =
            let get_match s =
                String.starts_with ~prefix:last_elt (Vytree.name_of_node s)
            in
            List.filter get_match children
        in
        let aux node' =
            let compl_env = get_completion_data node' in
            let name = Vytree.name_of_node node' in
            let path_typ = Reference_tree.get_path_type rtree (path @ [name]) in
            { compl_env with values = [name]; path_typ = path_typ }
        in
        Ok (List.map aux children')

let get_completion_env_str ?(legacy_format=false) rtree ctree op cpath =
    let compl_env = get_completion_env rtree ctree op cpath in
    match compl_env with
    | Error e -> Error e
    | Ok comp ->
        if not legacy_format then
            Ok (completion_env_list_to_yojson comp |> Yojson.Safe.to_string)
        else
        (* produce the strings expected by vbash completion *)
        let path_typ = Reference_tree.get_path_type rtree (Util.drop_last cpath) in
        let func (compl_vals, compl_help, help, value_help) comp_env =
            compl_vals @ comp_env.values,
            compl_help @ comp_env.completion_help,
            help @ [comp_env.help],
            value_help @ comp_env.value_help
        in
        let (comp_vals, comp_val, comp_help, help_format, help_string) =
        match path_typ with
        | `Tag | `Leaf | `Multi ->
            let (compl_vals, _, help, value_help) =
                let a, b, c, d =
                    List.fold_left func ([], [], [], []) comp in
                List.rev a, b, c, List.rev d
            in
            let value_help_fmt, value_help_string = List.split value_help in
            begin
            match value_help_string with
            | [] ->
                (compl_vals, true, "", ["txt"], help)
            | _ ->
                (compl_vals, true, "", value_help_fmt, value_help_string)
            end
        | `Other | `Tag_value ->
            let (compl_vals, _, help, _) =
                let sorted_comp =
                    let sort s t = Util.lexical_numeric_compare s.name t.name
                    in List.sort sort comp
                in List.fold_left func ([], [], [], []) sorted_comp
            in
            (compl_vals, false, "", compl_vals, help)
        | _ -> ([], false, "", [], []) (* never reached *)
        in
        let print_help_list l =
            {|(|}^
            (String.concat " " (List.map (Printf.sprintf {|'%s'|}) l))^
            {|)|}
        in
        let res =
        (Printf.sprintf {|_cli_shell_api_comp_values=%s; |} (print_help_list comp_vals))^
        (Printf.sprintf {|_cli_shell_api_last_comp_val=%b; |} comp_val)^
        (Printf.sprintf {|_cli_shell_api_comp_help='%s'; |} comp_help)^
        (Printf.sprintf {|_cli_shell_api_hitems=%s; |} (print_help_list help_format))^
        (Printf.sprintf {|_cli_shell_api_hstrs=%s;|} (print_help_list help_string))
        in Ok res

