(* Load interface definitions from a directory into a reference tree *)

exception Load_error of string
exception Write_error of string

module I = Internal.Make(Reference_tree)

let load_interface_definitions dir =
    (* alert exn Reference_tree.load_from_xml:
        [Reference_tree.Bad_interface_definition] caught
     *)
    let open Reference_tree in
    let dir_paths = FileUtil.ls dir in
    let relative_paths =
      List.filter (fun x -> Filename.extension x = ".xml") dir_paths
    in
    let absolute_paths =
        try Ok (List.map Util.absolute_path relative_paths)
        with Sys_error no_dir_msg -> Error no_dir_msg
    in
    let load_aux tree file =
        (load_from_xml[@alert "-exn"]) tree file
    in
    try begin match absolute_paths with
        | Ok paths  -> Ok (List.fold_left load_aux default paths)
        | Error msg -> Error msg end
    with Bad_interface_definition msg -> Error msg

let interface_definitions_to_cache from_dir cache_path =
    (* raises:
        [Write_error]
       alert exn Internal.write_internal:
        [Internecl.Write_error] caught
     *)
    let ref_tree_result =
        load_interface_definitions from_dir
    in
    let ref_tree =
    match ref_tree_result with
        | Ok ref -> ref
        | Error msg -> raise (Load_error msg)
    in
    try
        (I.write_internal[@alert "-exn"]) ref_tree cache_path
    with Internal.Write_error msg -> raise (Write_error msg)

let reference_tree_cache_to_json cache_path render_file =
    (* raises:
        [Load_error]
        [Write_error]
       alert exn Internal.read_internal:
        [Internal.Read_error] caught
     *)
    let ref_tree =
        try
            (I.read_internal[@alert "-exn"]) cache_path
        with Internal.Read_error msg -> raise (Load_error msg)
    in
    let out = Reference_tree.render_json ref_tree in
    let oc =
        try
            open_out render_file
        with Sys_error msg -> raise (Write_error msg)
    in
    Printf.fprintf oc "%s" out;
    close_out oc

let merge_reference_tree_cache cache_dir primary_name result_name =
    (* raises:
        [Tree_alg.Incompatible_union],
        [Tree_alg.Nonexistent_child] from Tree_alg.RefAlg.tree_union
        [Load_error]
        [Write_error]
       alert exn Internal.read_internal:
        [Internal.Read_error] caught
       alert exn Internal.write_internal:
        [Internal.Write_error] caught
       alert exn Tree_alg.RefAlg.tree_union:
        [Tree_alg.Incompatible_union] allow raise
        [Tree_alg.Nonexistent_child] allow raise
     *)
    let file_arr = Sys.readdir cache_dir in
    let file_list' = Array.to_list file_arr in
    let file_list =
        List.filter (fun x -> x <> primary_name && x <> result_name) file_list' in
    let file_path_list =
        List.map (FilePath.concat cache_dir) file_list in
    let primary_tree =
        try
            (I.read_internal[@alert "-exn"]) (FilePath.concat cache_dir primary_name)
        with Internal.Read_error msg -> raise (Load_error msg)
    in
    let ref_trees =
        try
            List.map (I.read_internal[@alert "-exn"]) file_path_list
        with Internal.Read_error msg -> raise (Load_error msg)
    in
    match ref_trees with
    | [] ->
        begin
        try
            (I.write_internal[@alert "-exn"])
            primary_tree
            (FilePath.concat cache_dir result_name)
        with Internal.Write_error msg -> raise (Write_error msg)
        end
    | _ ->
        let f _ v = v in
        let res =
            List.fold_left
            (fun p r -> (Tree_alg.RefAlg.tree_union[@alert "-exn"]) r p f)
            primary_tree
            ref_trees
        in
        try
            (I.write_internal[@alert "-exn"])
            res
            (FilePath.concat cache_dir result_name)
        with Internal.Write_error msg -> raise (Write_error msg)

let reference_tree_to_json ?(internal_cache="") from_dir to_file =
    (* raises:
        [Load_error]
        [Write_error]
       alert exn Internal.write_internal:
        [Internal.Write_error] caught
     *)
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
    close_out oc;
    match internal_cache with
    | "" -> ()
    | _ ->
        try
            (I.write_internal[@alert "-exn"]) ref_tree internal_cache
        with Internal.Write_error msg -> raise (Write_error msg)
