(* Load interface definitions from a directory into a reference tree *)
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

