(* Convert from a path that may or may not include intervening tag_values,
   returning a subtree of matches *)
exception Malformed_path of string

let subtree_from_partial ?(descent=true) reftree ctree result path =
    if Util.is_empty path then result
    else
    let check_reftree p =
        match p with
        | [] -> false
        | _ ->
            let rpath =
                Reference_tree.refpath_from_partial reftree p
            in
            match rpath with
            | [] -> false
            | _ -> true
    in
    let check_ctree p =
        match p with
        | [] -> false
        | _ -> (Vytree.exists[@alert "-exn"]) ctree p
    in
    let spurious_value p =
        Reference_tree.refpath_from_partial reftree p =
        Reference_tree.refpath_from_partial reftree (Util.drop_last p)
    in
    let clone_node ?(descent=false) tree p =
        if (Vytree.exists[@alert "-exn"]) tree p then
            tree
        else
        if not ((Vytree.exists[@alert "-exn"]) ctree p) then
            tree
        else
            (Config_tree.clone[@alert "-exn"]) ~descent:descent ctree tree p
    in
    let clone_children tree p =
        let children = Vytree.list_children ((Vytree.get[@alert "-exn"]) ctree p) in
        let paths = List.map (fun n -> p @ [n]) children in
        List.fold_left (clone_node ~descent:true) tree paths
    in
    let (complete, res) =
        let rec aux (tail, acc) path_done p =
            if not (check_reftree (path_done @ p)) then
                raise (Malformed_path (Util.string_of_list (path_done @ p)))
            else
            match path_done, p with
            | [], h :: tl ->
                    if check_ctree [h] then aux (tail, (clone_node acc [h])) [h] tl
                    else (false, result)
            | _, h :: tl ->
                    let p' = path_done @ [h] in
                    (* case: path_done @ [h] exists in config tree *)
                    if check_ctree p' then aux (tail, (clone_node acc p')) p' tl
                    else
                    (* case: path_done @ [h] is not a tag value, but is not set in config tree *)
                    if not ((Config_tree.is_tag[@alert "-exn"]) ctree path_done)
                    then (tail, acc)
                    else
                    (* case: path_done is tag node, [h] is not a false tag value *)
                    if not (spurious_value p')
                    then
                    let children =
                        Vytree.list_children ((Vytree.get[@alert "-exn"]) ctree path_done)
                    in
                    let func (tail', accum) child =
                        let path = path_done @ [child] @ [h] in
                        if check_ctree path then
                            aux (tail', (clone_node accum path)) path tl
                        else (tail', accum)
                    in
                    List.fold_left func (tail, acc) children
                    else
                    (* case: [h] is a tag_value not present in the config tree *)
                    raise (Malformed_path (Util.string_of_list p'))
            | _, [] ->
                if descent then
                    (true, clone_children acc path_done)
                else
                    (true, acc)
        in aux (false, result) [] path
    in match complete, res with
    | true, tree -> tree
    | false, __ -> result


let subtree_values_of_path rt ct path =
    (* raises:
        [Malformed_path] from subtree_from_partial
     *)
    let subtree = subtree_from_partial ~descent:false rt ct Config_tree.default path
    in
    if Util.is_empty path || subtree = Config_tree.default then
        [([], [])]
    else
    let func (p, (acc, ct')) ft =
        let path = List.rev p in
        if Vytree.is_terminal_node ft then
            let node = (Vytree.get[@alert "-exn"]) (ct': Config_tree.t) path in
            let data = Vytree.data_of_node node in
            let acc' =
                if data.tag then ((Vytree.list_children node, path) :: acc)
                else
                if data.leaf then ((data.values, path) :: acc)
                else acc
            in (p, (acc', ct'))
        else (p, (acc, ct'))
    in fst (Vytree.fold_tree_with_path func ([], ([], ct)) subtree)

let subtree_values_of_path_yojson rt ct path =
    let ret = subtree_values_of_path rt ct path in
    [%to_yojson: (string list * string list) list] ret |> Yojson.Safe.to_string
