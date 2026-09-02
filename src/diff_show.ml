open Diff

module ValueS = Config_tree.ValueS

let annotate_rendered change rendered =
    let mark =
        match change with
        | Unchanged -> " "
        | Added -> "+"
        | Subtracted -> "-"
        | Updated _ -> ">"
    in
    let lst = String.split_on_char '\n' rendered in
    let marked = List.map (fun x -> match x with "" -> x | _ -> mark ^ x) lst in
    String.concat "\n" marked

let get_level_at_path node path =
    (* alert exn Config_tree.is_tag_value:
        [Vytree.Empty_path] called in pattern path non-empty
        [Vytree.Nonexistent_path] function diff never calls diff_func on nonexistent path
     *)
    let f level p =
        if (Config_tree.is_tag_value[@alert "-exn"]) node p then level
        else level + 1
    in
    match path with
    | [] -> 0
    | _ ->
        List.fold_left f 0 (Util.flag path) - 1

let render_level_open indent node path =
    (* alert exn Config_tree.is_tag, Config_tree.is_tag_value:
        [Vytree.Empty_path] called in branch path non-empty
        [Vytree.Nonexistent_path] function diff never calls diff_func on nonexistent path
     *)
    if Util.is_empty path || (Config_tree.is_tag[@alert "-exn"]) node path then
        ""
    else
    let level = get_level_at_path node path in
    let indent_str = Config_tree.make_indent indent level in
    if (Config_tree.is_tag_value[@alert "-exn"]) node path then
        let tag_node =
            match Util.get_last_n path 1 with
            | None -> (* not possible as path non-empty *) "none"
            | Some n -> n
        in
        let tag_value =
            match Util.get_last path with
            | None -> (* not possible as path non-empty *) "none"
            | Some v -> v
        in
        Printf.sprintf "%s%s %s {\n" indent_str tag_node tag_value
    else
        let name =
            match Util.get_last path with
            | None -> (* not possible as path non-empty *) "none"
            | Some v -> v
        in
        Printf.sprintf "%s%s {\n" indent_str name

let render_level_close indent node path =
    (* alert exn Config_tree.is_tag:
        [Vytree.Empty_path] called in branch path non-empty
        [Vytree.Nonexistent_path] function diff never calls diff_func on nonexistent path
     *)
    if Util.is_empty path || (Config_tree.is_tag[@alert "-exn"]) node path then
        ""
    else
    let level = get_level_at_path node path in
    let indent_str = Config_tree.make_indent indent level in
    Printf.sprintf "%s}\n" indent_str

module Diff_show = struct
    type t = { left: Config_tree.t;
               right: Config_tree.t;
               rt: Reference_tree.t;
               base_path: string list;
               open_blocks: string list list;
               config_diff: string;
             }

    let make_init l r rt path = { left = l;
                                  right = r;
                                  rt = rt;
                                  base_path = path;
                                  open_blocks = [];
                                  config_diff = "";
                                }


    let diff_func ?(descent=true) (path : string list) res (m : change) =
        (* alert exn Vytree.get, Reference_tree.refpath, Config_tree.get_values,
           Reference_tree.is_multi, Config_tree.is_tag_value:
            [Vytree.Empty_path] checked at only point possible (Unchanged)
            [Vytree.Nonexistent_path] function diff never calls diff_func on nonexistent path
         *)

        let indent = 4 in
        (* the only subtlety in all this is the bookkeeping of closing open
           braces at correct level:
           (1) a rendered line with open brace will occur before the next
           depth-first step
           (2) at each return to (local) root, the path is checked for the
           matching closing brace
           explicitly: the record field open_blocks is a list of open paths
           ordered by reverse inclusion, which are closed when the path being
           passed to config_diff no longer contains that element
         *)
        let rec close_blocks s l =
            match l with
            | [] -> s, []
            | h :: tl ->
                if Util.is_sublist h path then
                    s, l
                else
                    let rendered = render_level_close indent res.left h in
                    let s' = s ^ annotate_rendered Unchanged rendered
                    in close_blocks s' tl
        in
        let diff_str, rev_blocks = close_blocks res.config_diff res.open_blocks
        in
        match m with
        | Added ->
            let node = (Vytree.get[@alert "-exn"]) res.right path in
            let level = get_level_at_path res.right path in
            let rendered =
                Config_tree.render_node indent level node
            in
            let rev_diff = diff_str ^ annotate_rendered m rendered in
            {res with config_diff = rev_diff; open_blocks = rev_blocks;}
        | Subtracted ->
            let node = (Vytree.get[@alert "-exn"]) res.left path in
            let level = get_level_at_path res.left path in
            let rendered =
                Config_tree.render_node indent level node
            in
            let rev_diff = diff_str ^ annotate_rendered m rendered in
            {res with config_diff = rev_diff; open_blocks = rev_blocks;}
        | Unchanged ->
            begin
            match descent with
            | false ->
                let rendered = render_level_open indent res.left path in
                let rev_diff = diff_str ^ annotate_rendered m rendered in
                {res with config_diff = rev_diff; open_blocks = path::rev_blocks}
            | true ->
                match path with
                | [] -> (* case left = right *)
                    let rendered =
                        Config_tree.render_config res.left
                    in
                    let rev_diff = diff_str ^ annotate_rendered m rendered in
                    {res with config_diff = rev_diff; open_blocks = rev_blocks;}
                | _ ->
                    let level = get_level_at_path res.left path in
                    let node = (Vytree.get[@alert "-exn"]) res.left path in
                    let rendered =
                        Config_tree.render_node indent level node
                    in
                    let rev_diff = diff_str ^ annotate_rendered m rendered in
                    {res with config_diff = rev_diff; open_blocks = rev_blocks;}
            end
        | Updated v ->
            let refp =
                (Reference_tree.refpath[@alert "-exn"]) res.rt (res.base_path @ path) in
            let multi = (Reference_tree.is_multi[@alert "-exn"]) res.rt refp in
            let level = get_level_at_path res.left path in
            let indent_str =
                Config_tree.make_indent indent level
            in
            let name =
                match Util.get_last path with
                | None -> (* not possible *) "none"
                | Some n -> n
            in
            match multi with
            | false ->
                let rendered =
                    Config_tree.render_values indent_str name v
                in
                let rev_diff = diff_str ^ annotate_rendered m rendered in
                {res with config_diff = rev_diff; open_blocks = rev_blocks;}
            | true ->
                let ov = (Config_tree.get_values[@alert "-exn"]) res.left path in
                let ov_set = ValueS.of_list ov in
                let v_set = ValueS.of_list v in
                let sub_vals = ValueS.elements (ValueS.diff ov_set v_set) in
                let add_vals = ValueS.elements (ValueS.diff v_set ov_set) in
                let inter_vals = ValueS.elements (ValueS.inter ov_set v_set) in
                let sub_rendered =
                    match sub_vals with
                    | [] -> ""
                    | _ ->
                        Config_tree.render_values indent_str name sub_vals
                in
                let sub_diff = annotate_rendered Subtracted sub_rendered in
                let add_rendered =
                    match add_vals with
                    | [] -> ""
                    | _ ->
                        Config_tree.render_values indent_str name add_vals
                in
                let add_diff = annotate_rendered Added add_rendered in
                let inter_rendered =
                    match inter_vals with
                    | [] -> ""
                    | _ ->
                        Config_tree.render_values indent_str name inter_vals
                in
                let inter_diff = annotate_rendered Unchanged inter_rendered in
                let value_diff = sub_diff ^ inter_diff ^ add_diff in
                let rev_diff = diff_str ^ value_diff in
                {res with config_diff = rev_diff; open_blocks = rev_blocks;}
end

module D = Diff(Diff_show)

(* call recursive diff on config_trees with config_diff as the diff_func *)
let diff_show rt path left right =
    (* raises:
        [Incommensurable]
        [Empty_comparison]
     *)
    if (Vytree.name_of_node left) <> (Vytree.name_of_node right) then
        raise Incommensurable
    else
        let (left, right) =
            if not (Util.is_empty path) then
            let with_node =
            match Reference_tree.get_path_type rt path with
            | `Leaf -> true
            | _ -> false
            in
            (Config_tree.get_subtree ~with_node left path,
            Config_tree.get_subtree ~with_node right path)
            else (left, right)
        in
        let init = Diff_show.make_init left right rt path in
        let ret = D.diff init left right in
        (* close final braces *)
        let res = Diff_show.diff_func ~descent:false [] ret Unchanged in
        res.config_diff
