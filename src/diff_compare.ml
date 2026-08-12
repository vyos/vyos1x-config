open Diff

module ValueS = Config_tree.ValueS

let tree_at_path path node =
    (* raises:
        [Vytree.Empty_path]
        [Empty_comparison]
       alert exn Vytree.get:
        [Vytree.Empty_path] allow raise
        [Vytree.Nonexistent_path] catch and raise Empty_comparison
     *)
    try
        let node = (Vytree.get[@alert "-exn"]) node path in
        Vytree.make_full Config_tree.default_data "" [node]
    with Vytree.Nonexistent_path -> raise Empty_comparison

let path_to_string (path: string list) =
    Printf.sprintf "[%s]\n" (Util.string_of_list path)

let marked_render mark node =
    let lines = Config_tree.render_config node in
    let l = String.split_on_char '\n' lines in
    let m =
        List.map (fun s -> if (String.length s) > 0 then mark ^ s else s) l in
    String.concat "\n" m

let added_lines ?(cmds=false) node path =
    (* alert exn Config_tree.render_commands:
        [Vytree.Nonexistent_path] not possible on root path
     *)
    if not cmds then marked_render "+ " (tree_at_path path node)
    else
        ((Config_tree.render_commands[@alert "-exn"]) ~op:Set node []) ^ "\n"

let removed_lines ?(cmds=false) node path =
    (* alert exn Config_tree.render_commands:
        [Vytree.Nonexistent_path] not possible on root path
     *)
    if not cmds then marked_render "- " (tree_at_path path node)
    else
        ((Config_tree.render_commands[@alert "-exn"]) ~op:Delete node []) ^ "\n"

let order_commands (strl: string) =
    let l = String.split_on_char '\n' strl in
    let del = List.filter (fun s -> (s <> "") && (s.[0] = 'd')) l in
    let set = List.filter (fun s -> (s <> "") && (s.[0] = 's')) l in
    (String.concat "\n" del) ^ "\n" ^ (String.concat "\n" set) ^ "\n"

module Diff_compare = struct
    type t = { left: Config_tree.t;
               right: Config_tree.t;
               skel: Config_tree.t;
               ppath: string list;
               udiff: string;
               cmds: bool;
             }

    let make_init l r cmds = { left = l;
                               right = r;
                               skel = Config_tree.default;
                               ppath = [];
                               udiff = "";
                               cmds = cmds;
                             }

    let diff_func ?descent:_ (path : string list) res (m : change) =
        (* raises no exception:
            clone will always be called on extant path of left or right
           alert exn Vytree.get_values:
            [Vytree.Empty_path] not possible as pattern Updated implies non-empty path
            [Vytree.Nonexistent_path] not possible as pattern Updated implies path exists
         *)
        let ppath_l = Util.drop_last path
        in
        let ppath_s =
            if (ppath_l <> res.ppath) then path_to_string ppath_l
            else ""
        in
        let str_diff =
            if not res.cmds then res.udiff ^ ppath_s
            else res.udiff
        in
        match m with
        | Added ->
                let str_diff =
                    let add_tree = (Config_tree.clone[@alert "-exn"]) res.right res.skel path in
                    str_diff ^ (added_lines ~cmds:res.cmds add_tree path)
                in
                { res with ppath = ppath_l; udiff = str_diff; }
        | Subtracted ->
                let str_diff =
                    let sub_tree = (Config_tree.clone[@alert "-exn"]) res.left res.skel path in
                    str_diff ^ (removed_lines ~cmds:res.cmds sub_tree path)
                in
                { res with ppath = ppath_l; udiff = str_diff; }
        | Unchanged -> res
        | Updated v ->
                let ov = (Config_tree.get_values[@alert "-exn"]) res.left path in
                match ov, v with
                | [_], [_] ->
                        let str_diff =
                            let sub_tree = (Config_tree.clone[@alert "-exn"]) res.left res.skel path in
                            str_diff ^ (removed_lines ~cmds:res.cmds sub_tree path)
                        in
                        let str_diff =
                            let add_tree = (Config_tree.clone[@alert "-exn"]) res.right res.skel path in
                            str_diff ^ (added_lines ~cmds:res.cmds add_tree path)
                        in
                        { res with ppath = ppath_l; udiff = str_diff; }
                | _, _ -> let ov_set = ValueS.of_list ov in
                          let v_set = ValueS.of_list v in
                          let sub_vals = ValueS.elements (ValueS.diff ov_set v_set) in
                          let add_vals = ValueS.elements (ValueS.diff v_set ov_set) in
                          let str_diff =
                              if not (Util.is_empty sub_vals) then
                                  let sub_tree =
                                      (Config_tree.clone[@alert "-exn"]) ~set_values:(Some sub_vals) res.left res.skel path
                                  in str_diff ^ (removed_lines ~cmds:res.cmds sub_tree path)
                              else str_diff
                          in
                          let str_diff =
                              if not (Util.is_empty add_vals) then
                                  let add_tree =
                                      (Config_tree.clone[@alert "-exn"]) ~set_values:(Some add_vals) res.right res.skel path
                                  in str_diff ^ (added_lines ~cmds:res.cmds add_tree path)
                              else str_diff
                          in
                          { res with ppath = ppath_l; udiff = str_diff; }
end

module D = Diff(Diff_compare)

let add_empty_path src_node dest_node path =
    (Config_tree.clone[@alert "-exn"]) ~descent:false ~set_values:(Some []) src_node dest_node path

let compare_at_path_maybe_empty left right path =
    let left =
        try
            tree_at_path path left
        with Empty_comparison ->
            try
                let left = add_empty_path right left path in
                tree_at_path path left
             with Vytree.Nonexistent_path ->
                 raise Empty_comparison
     and right =
        try
            tree_at_path path right
        with Empty_comparison ->
            try
                let right = add_empty_path left right path in
                tree_at_path path right
             with Vytree.Nonexistent_path ->
                 raise Empty_comparison
    in (left, right)

let diff_compare ?(cmds=false) path left right =
    (* raises:
        [Incommensurable],
        [Empty_comparison] from compare_at_path_maybe_empty
     *)
    if (Vytree.name_of_node left) <> (Vytree.name_of_node right) then
        raise Incommensurable
    else
        let (left, right) =
            if (path <> []) then
                compare_at_path_maybe_empty left right path
            else (left, right) in
        let init = Diff_compare.make_init left right cmds in
        let diff_strs = D.diff init left right in
        let strs =
            if cmds then order_commands diff_strs.udiff
            else diff_strs.udiff
        in
        strs

