(* Load interface definitions from a directory into a reference tree *)
exception Load_error of string
exception Write_error of string

let load_interface_definitions dir =
    let open Reference_tree in
    let relative_paths = FileUtil.ls dir in
    let absolute_paths =
        try Ok (List.map Util.absolute_path relative_paths)
        with Sys_error no_dir_msg -> Error no_dir_msg
    in
    let load_aux tree file =
        load_from_xml tree file
    in
    try begin match absolute_paths with
        | Ok paths  -> Ok (List.fold_left load_aux default paths)
        | Error msg -> Error msg end
    with Bad_interface_definition msg -> Error msg

let reference_tree_to_json from_dir to_file =
    let ref_tree_result =
        load_interface_definitions from_dir
    in
    let ref_tree =
    match ref_tree_result with
        | Ok ref -> ref
        | Error msg -> raise (Load_error msg)
    in
    let out = Reference_tree.render_json ref_tree in
    let oc =
        try
            open_out to_file
        with Sys_error msg -> raise (Write_error msg)
    in
    Printf.fprintf oc "%s" out;
    close_out oc
