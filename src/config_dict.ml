(** The goal here is to produce a JSON rendering to load into a Python dict.
    By abuse of language, the entry point is named config_dict, the Python
    wrapper for which will perform the json.loads().

    The idea here is simple, and abstracts the construction in vyos-1x using
    the xml_ref module:
        fold over the config tree 'a fold over the reference tree'
    The required details to make this work:
        (1) skip tag nodes in the config tree, so as to call the
        corresponding reference tree below the tag node value in the
        depth-first fold.
        (2) use a modified fold_tree_with_path to store a boolean for each
        level, in order to ignore non-sensical (non-existent tag node value)
        or excluded (masked) reference paths below a given level.
        (3) make appropriate use of local vs global paths in the respective
        trees.
 *)

let tree_with_defaults ?(with_first_node=true) ref_tree config_tree mask path =
    let ct_at_path =
        Config_tree.get_subtree ~with_node:with_first_node config_tree path
    in
    let continue l =
        match l with
        | [] -> true
        | x :: _ -> x
    in
    let add_defaults ct p' =
        let ref_path =
            match with_first_node with
            | false -> Reference_tree.refpath ref_tree (path @ p')
            | true -> Reference_tree.refpath ref_tree ((Util.drop_last path) @ p')
        in
        let relative_ref_tree = Reference_tree.get_subtree ref_tree ref_path in
        let ref_tree_walk ((p, c), acc) node =
            let cont = continue c in
            if not cont then ((p, false::c), acc)
            else
            let rev_p = List.rev p in
            let sub_path = p' @ rev_p in
            if not (Util.is_empty rev_p) &&
                (Vytree.is_terminal_path[@alert "-exn"]) mask (ref_path @ rev_p) &&
                not ((Vytree.exists[@alert "-exn"]) ct sub_path)
            then ((p, false::c), acc)
            else
            let data = Vytree.data_of_node node in
            match data.Reference_tree.node_type with
            | `Tag -> ((p, false::c), acc)
            | `Leaf ->
                if (Vytree.exists[@alert "-exn"]) acc sub_path then ((p, cont::c), acc)
                else
                begin
                match data.default_value with
                | None -> ((p, cont::c), acc)
                | Some v ->
                    let acc' =
                        (* The behaviour variant ReplaceValue has no effect in this case
                           as the path does not exist in branch. *)
                        (Config_tree.set[@alert "-exn"]) acc sub_path (Some v) ReplaceValue
                    in ((p, cont::c), acc')
                end
            | _ -> ((p, cont::c), acc)
        in
        Vytree.fold_tree_with_path_and_stack ref_tree_walk (([], []), ct) relative_ref_tree
    in
    let config_tree_walk (p, acc) ct =
        let (data: Config_tree.config_node_data) = Vytree.data_of_node ct in
        if with_first_node && Util.is_empty p then (p, acc) else
        if data.tag then (p, acc) else
        let rev_p = List.rev p in
        let ct' = add_defaults acc rev_p
        in (p, ct')
    in Vytree.fold_tree_with_path config_tree_walk ([], ct_at_path) ct_at_path


(* Simple check of config dict result. Modifications 'multi_to_list' and
   'key_mangling' should belong to an extended config tree JSONRenderer *)
let config_dict ?(with_defaults=true) ?(with_first_node=true) rt ct mask path =
    let ht =
        match with_defaults with
        | true -> tree_with_defaults ~with_first_node rt ct mask path
        | false -> Config_tree.get_subtree ~with_node:with_first_node ct path
    in
    Config_tree.render_json ht
