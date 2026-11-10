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

let render_level_open rend_comment indent node path =
    (* alert exn Config_tree.is_tag, Config_tree.is_tag_value:
        [Vytree.Empty_path] called in branch path non-empty
        [Vytree.Nonexistent_path] function diff never calls diff_func on nonexistent path
     *)
    if Util.is_empty path || (Config_tree.is_tag[@alert "-exn"]) node path then
        ""
    else
    let level = get_level_at_path node path in
    let indent_str = (Config_render.make_indent indent level) in
    let annot_indent_str = " " ^ indent_str in
    let data = (Vytree.get_data[@alert "-exn"]) node path in
    let comment = rend_comment indent_str data.comment in
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
        Printf.sprintf "%s%s%s %s {\n" comment annot_indent_str tag_node tag_value
    else
        let name =
            match Util.get_last path with
            | None -> (* not possible as path non-empty *) "none"
            | Some v -> v
        in
        Printf.sprintf "%s%s%s {\n" comment annot_indent_str name

let render_level_close indent node path =
    (* alert exn Config_tree.is_tag:
        [Vytree.Empty_path] called in branch path non-empty
        [Vytree.Nonexistent_path] function diff never calls diff_func on nonexistent path
     *)
    if Util.is_empty path || (Config_tree.is_tag[@alert "-exn"]) node path then
        ""
    else
    let level = get_level_at_path node path in
    let indent_str = Config_render.make_indent indent level in
    Printf.sprintf "%s}\n" indent_str


let render_node (module F : Config_render.R) ?(ord_val=false) root path indent level =
    F.render_relative_node ~ord_val:ord_val root path indent level

let render_annotated_node rend_values rend_comment root path indent level =
    let f = Config_render.make rend_values rend_comment in
    render_node f root path indent level


module Diff_show = struct
    type t = { left: Config_tree.t;
               right: Config_tree.t;
               rt: Reference_tree.t;
               base_path: string list;
               open_blocks: string list list;
               config_diff: string;
               diff_comments: bool;
             }

    let make_init ?(diff_comments=false) l r rt path =
        { left = l;
          right = r;
          rt = rt;
          base_path = path;
          open_blocks = [];
          config_diff = "";
          diff_comments = diff_comments;
        }

    let diff_comments res = res.diff_comments

    let diff_func ?(descent=true) (path : string list) res (chg : change) =
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
        match chg with
        | Added ->
            let level = get_level_at_path res.right path in
            let rendered =
                Config_render.render_relative_node res.right path indent level
            in
            let rev_diff = diff_str ^ annotate_rendered chg rendered in
            {res with config_diff = rev_diff; open_blocks = rev_blocks;}
        | Subtracted ->
            let level = get_level_at_path res.left path in
            let rendered =
                Config_render.render_relative_node res.left path indent level
            in
            let rev_diff = diff_str ^ annotate_rendered chg rendered in
            {res with config_diff = rev_diff; open_blocks = rev_blocks;}
        | Unchanged ->
            begin
            match descent with
            | false ->
                let rend_comment indent_str c =
                    annotate_rendered Unchanged (Config_render.render_comment indent_str c)
                in
                let rendered =
                    render_level_open rend_comment indent res.left path
                in
                let rev_diff = diff_str ^ rendered in
                {res with config_diff = rev_diff; open_blocks = path::rev_blocks}
            | true ->
                match path with
                | [] -> (* case left = right *)
                    let rendered =
                        Config_render.render_config res.left
                    in
                    let rev_diff = diff_str ^ annotate_rendered chg rendered in
                    {res with config_diff = rev_diff; open_blocks = rev_blocks;}
                | _ ->
                    let level = get_level_at_path res.left path in
                    let rendered =
                        Config_render.render_relative_node res.left path indent level
                    in
                    let rev_diff = diff_str ^ annotate_rendered chg rendered in
                    {res with config_diff = rev_diff; open_blocks = rev_blocks;}
            end
        | Updated (cmp, ldata, rdata) ->
            let rend_comment indent_str c =
                match cmp with
                | Values ->
                    annotate_rendered Unchanged (Config_render.render_comment indent_str c)
                | Values_and_comments ->
                    if ldata.comment = rdata.comment then
                        annotate_rendered Unchanged (Config_render.render_comment indent_str c)
                    else
                        match ldata.comment, rdata.comment with
                        | None, Some d ->
                            annotate_rendered Added (Config_render.render_comment indent_str (Some d))
                        | Some c, None ->
                            annotate_rendered Subtracted (Config_render.render_comment indent_str (Some c))
                        | Some _, Some d ->
                            annotate_rendered chg (Config_render.render_comment indent_str (Some d))
                        | None, None -> (* not reached *)
                            annotate_rendered Unchanged (Config_render.render_comment indent_str None)
            in
            let level = get_level_at_path res.left path in
            if not ldata.leaf then
                let rendered =
                    render_level_open rend_comment indent res.left path
                in
                let rev_diff = diff_str ^ rendered in
                {res with config_diff = rev_diff; open_blocks = path::rev_blocks;}
            else
            let refp =
                (Reference_tree.refpath[@alert "-exn"]) res.rt (res.base_path @ path) in
            let multi = (Reference_tree.is_multi[@alert "-exn"]) res.rt refp in
            match multi with
            | false ->
                let rend_values ?ord_val:_ indent_str name values =
                    let rendered =
                        Config_render.render_values indent_str name values
                    in
                    if ldata.values = rdata.values then
                        annotate_rendered Unchanged rendered
                    else
                        annotate_rendered chg rendered
                in
                let rev_diff =
                    diff_str ^ (render_annotated_node rend_values rend_comment res.right path indent level)
                in
                {res with config_diff = rev_diff; open_blocks = rev_blocks;}
            | true ->
                let ov = ldata.values in
                let v = rdata.values in
                let ov_set = ValueS.of_list ov in
                let v_set = ValueS.of_list v in
                let sub_vals = ValueS.elements (ValueS.diff ov_set v_set) in
                let add_vals = ValueS.elements (ValueS.diff v_set ov_set) in
                let inter_vals = ValueS.elements (ValueS.inter ov_set v_set) in
                let rend_values ?ord_val:_ indent_str name _values =
                    let sub_rendered =
                        match sub_vals with
                        | [] -> ""
                        | _ ->
                            Config_render.render_values indent_str name sub_vals
                    in
                    let sub_diff = annotate_rendered Subtracted sub_rendered in
                    let add_rendered =
                        match add_vals with
                        | [] -> ""
                        | _ ->
                            Config_render.render_values indent_str name add_vals
                    in
                    let add_diff = annotate_rendered Added add_rendered in
                    let inter_rendered =
                        match inter_vals with
                        | [] -> ""
                        | _ ->
                            Config_render.render_values indent_str name inter_vals
                    in
                    let inter_diff = annotate_rendered Unchanged inter_rendered in
                    sub_diff ^ inter_diff ^ add_diff
                in
                let rev_diff =
                    diff_str ^ (render_annotated_node rend_values rend_comment res.left path indent level)
                in
                {res with config_diff = rev_diff; open_blocks = rev_blocks;}
end

module D = Diff(Diff_show)

(* call recursive diff on config_trees with config_diff as the diff_func *)
let diff_show ?(diff_comments=false) rt path left right =
    (* raises:
        [Incommensurable]
        [Empty_comparison]
     *)
    if (Vytree.name_of_node left) <> (Vytree.name_of_node right) then
        raise Incommensurable
    else
        let (left, right, path') =
            if not (Util.is_empty path) then
            let with_node, trim_path =
                match Reference_tree.get_path_type rt path with
                | `Leaf | `Multi | `Tag -> true, (Util.drop_last path)
                | _ -> false, path
                in
                (Config_tree.get_subtree ~with_node left path,
                 Config_tree.get_subtree ~with_node right path,
                 trim_path)
            else (left, right, path)
        in
        let init = Diff_show.make_init ~diff_comments left right rt path' in
        let ret = D.diff init left right in
        (* close final braces *)
        let res = Diff_show.diff_func ~descent:false [] ret Unchanged in
        res.config_diff
