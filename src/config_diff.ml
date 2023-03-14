type change = Unchanged | Added | Subtracted | Updated of string list

type diff_func = ?recurse:bool -> string list -> change -> unit

type diff_trees = {
    left: Config_tree.t;
    right: Config_tree.t;
    add: Config_tree.t ref;
    sub: Config_tree.t ref;
    inter: Config_tree.t ref;
}

exception Incommensurable
exception Empty_comparison
exception Nonexistent_child

let make_diff_trees l r = { left = l; right = r;
                           add = ref (Config_tree.make "");
                           sub = ref (Config_tree.make "");
                           inter = ref (Config_tree.make "");
}

let name_of n = Vytree.name_of_node n
let data_of n = Vytree.data_of_node n
let children_of n = Vytree.children_of_node n
let make data name children = Vytree.make_full data name children

module ValueOrd = struct
    type t = string
    let compare a b =
        Util.lexical_numeric_compare a b
end
module ValueS = Set.Make(ValueOrd)

module TreeOrd = struct
    type t = Config_tree.t
    let compare a b =
        Util.lexical_numeric_compare (name_of a) (name_of b)
end
module ChildrenS = Set.Make(TreeOrd)

let (^~) (node : Config_tree.t) (node' : Config_tree.t) =
  name_of node = name_of node' &&
  (data_of node).values <> (data_of node').values

let left_opt_pairs n m =
    (children_of n) |> List.map (fun x ->
        let maybe_node =
            (children_of m) |> List.find_opt (fun y ->
                name_of y = name_of x) in
        (Some x, maybe_node))

let right_opt_pairs n m =
    (children_of m) |> List.map (fun y ->
        let maybe_node =
            (children_of n) |> List.find_opt (fun x ->
                name_of x = name_of y) in
        (maybe_node, Some y))

(* this is module option 'compare', but with Some _ preceding None, which is
   useful for maintaing left-right -> top-down order for show_diff
 *)
let opt_cmp o0 o1 =
    match o0, o1 with
    | Some v0, Some v1 -> compare v0 v1
    | None, None -> 0
    | None, Some _ -> 1
    | Some _, None -> -1

let tuple_cmp t1 t2 =
    match t1, t2 with
    | (x1, y1), (x2, y2) ->
            let first = opt_cmp x1 x2 in
            if first <> 0 then first else opt_cmp y1 y2

let opt_zip n m =
    left_opt_pairs n m @ right_opt_pairs n m |> List.sort_uniq tuple_cmp

let get_opt_name left_opt right_opt =
    match left_opt, right_opt with
    | Some left_node, None -> name_of left_node
    | None, Some right_node -> name_of right_node
    | Some left_node, Some _ -> name_of left_node
    | None, None -> raise Empty_comparison

let update_path path left_opt right_opt =
    let name = get_opt_name left_opt right_opt in
    if name = "" then path
    else path @ [name]

(* tree diff algorithm: walk the tree pair, calling a function of type
   diff_func for each comparison.
   The idea of matching on pairs of (node opt) is from
   https://github.com/LukeBurgessYeo/tree-diff
 *)
let rec diff (path : string list) (f : diff_func) (l : (Config_tree.t option * Config_tree.t option) list) =
    match l with
    | [] -> ()
    | (left_node_opt, right_node_opt) :: ls ->
        (let path = update_path path left_node_opt right_node_opt in
        match left_node_opt, right_node_opt with
        | Some _, None -> f path Subtracted
        | None, Some _ -> f path Added
        | Some left_node, Some right_node when left_node = right_node ->
                f path Unchanged
        | Some left_node, Some right_node when left_node ^~ right_node ->
                let values = (data_of right_node).values in
                f path (Updated values)
        | Some left_node, Some right_node ->
                (f ~recurse:false path Unchanged;
                diff path f (opt_zip left_node right_node))
        | None, None -> raise Empty_comparison)
        ; diff path f ls

(* copy node paths between trees *)
let rec clone_path ?(recurse=true) ?(set_values=None) old_root new_root path_done path_remaining =
    match path_remaining with
    | [] | [_] ->
        let path_total = path_done @ path_remaining in
        let old_node = Vytree.get old_root path_total in
        let data =
            match set_values with
            | Some v -> { (data_of old_node) with Config_tree.values = v }
            | None -> data_of old_node
        in
        if recurse then
            Vytree.insert ~position:Lexical ~children:(children_of old_node) new_root path_total data
        else
            Vytree.insert ~position:Lexical new_root path_total data
    | name :: names ->
        let path_done = path_done @ [name] in
        let old_node = Vytree.get old_root path_done in
        let new_root = Vytree.insert ~position:Lexical new_root path_done (data_of old_node) in
        clone_path ~recurse:recurse ~set_values:set_values old_root new_root path_done names

let clone ?(recurse=true) ?(set_values=None) old_root new_root path =
    match path with
    | [] -> if recurse then old_root else new_root
    | _ ->
            let path_existing = Vytree.get_existent_path new_root path in
            let path_remaining = Vylist.complement path path_existing in
            clone_path ~recurse:recurse ~set_values:set_values old_root new_root path_existing path_remaining

let is_empty l = (l = [])

(* define the diff_func; in this instance, we imperatively build the difference trees *)
let decorate_trees (trees : diff_trees) ?(recurse=true) (path : string list) (m : change) =
    match m with
    | Added -> trees.add := clone trees.right !(trees.add) path
    | Subtracted -> trees.sub := clone trees.left !(trees.sub) path
    | Unchanged -> trees.inter := clone ~recurse:recurse trees.left !(trees.inter) path
    | Updated v ->
            (* if in this case, node at path is guaranteed to exist *)
            let ov = Config_tree.get_values trees.left path in
            match ov, v with
            | [_], [_] -> trees.sub := clone trees.left !(trees.sub) path;
                          trees.add := clone trees.right !(trees.add) path
            | _, _ -> let ov_set = ValueS.of_list ov in
                      let v_set = ValueS.of_list v in
                      let sub_vals = ValueS.elements (ValueS.diff ov_set v_set) in
                      let add_vals = ValueS.elements (ValueS.diff v_set ov_set) in
                      let inter_vals = ValueS.elements (ValueS.inter ov_set v_set) in
                      if not (is_empty sub_vals) then
                          trees.sub := clone ~set_values:(Some sub_vals) trees.left !(trees.sub) path;
                      if not (is_empty add_vals) then
                          trees.add := clone ~set_values:(Some add_vals) trees.right !(trees.add) path;
                      if not (is_empty inter_vals) then
                          trees.inter := clone ~set_values:(Some inter_vals) trees.left !(trees.inter) path

(* define the 'trim' diff_func:

   One can use the diff algorithm with this function to produce 'delete'
   commands from the sub(-tract) tree. The subtract tree contains full paths
   not present in the right hand side of the original comparison; the delete
   tree is the subtract tree with paths ending at the first subtracted node.

   Initial application of diff algorithm with function 'diff_trees':
       left, right -> added, subtracted, intersection
   Second application of diff algorithm with function 'trim_trees':
       subtracted, right -> _, delete, _

   One needs to keep the distinction of sub and delete trees: the delete
   tree is used to produce correct 'delete' commands; the sub tree contains
   complete information of the difference, used, for example, in recursively
   detecting changes at a node between the effective/session configs.

   The two trees could be produced in one pass of the diff function, but is
   an overloaded use and would gain little in optimization: the trim-ing
   walk will be on a smaller tree, only involve diff_func calls on the
   subtracted nodes, and will end at the first node not present in the
   comparison.
 *)
let trim_trees (trees : diff_trees) ?(recurse=false) (path : string list) (m : change) =
    match m with
    | Added -> ()
    | Subtracted -> trees.sub := clone ~recurse:recurse ~set_values:(Some []) trees.left !(trees.sub) path
    | Unchanged -> ()
    | Updated v ->
            (* if in this case, node at path is guaranteed to exist *)
            let ov = Config_tree.get_values trees.left path in
            match ov, v with
            | [_], [_] -> trees.sub := clone trees.left !(trees.sub) path;
            | _, _ -> let ov_set = ValueS.of_list ov in
                      let v_set = ValueS.of_list v in
                      let sub_vals = ValueS.elements (ValueS.diff ov_set v_set) in
                      let add_vals = ValueS.elements (ValueS.diff v_set ov_set) in
                      (* in practice, the above sets will be disjoint *)
                      let inter_vals = ValueS.elements (ValueS.inter ov_set v_set) in
                      if not (is_empty sub_vals) then
                          if (is_empty add_vals) && (is_empty inter_vals) then
                              (* delete whole node, not just values *)
                              trees.sub := clone ~set_values:(Some []) trees.left !(trees.sub) path
                          else
                              trees.sub := clone ~set_values:(Some sub_vals) trees.left !(trees.sub) path

(* get sub trees for path-relative comparison *)
let tree_at_path path node =
    try
        let node = Vytree.get node path in
        make Config_tree.default_data "" [node]
    with Vytree.Nonexistent_path -> raise Empty_comparison

(* call recursive diff on config_trees with decorate_trees as the diff_func *)
let compare path left right =
    if (name_of left) <> (name_of right) then
        raise Incommensurable
    else
        let (left, right) = if not (path = []) then
            (tree_at_path path left, tree_at_path path right) else (left, right) in
        let trees = make_diff_trees left right in
        diff [] (decorate_trees trees) [(Option.some left, Option.some right)];
        trees

(* wrapper to return diff trees *)
let diff_tree path left right =
    let trees = compare path left right in
    let add_node = make Config_tree.default_data "add" (children_of !(trees.add)) in
    let sub_node = make Config_tree.default_data "sub" (children_of !(trees.sub)) in
    let int_node = make Config_tree.default_data "inter" (children_of !(trees.inter)) in
    let ret = make Config_tree.default_data "" [add_node; sub_node; int_node] in
    ret

(* wrapper to return trimmed tree for 'delete' commands *)
let trim_tree left right =
    let trees = make_diff_trees left right in
    diff [] (trim_trees trees) [(Option.some left, Option.some right)];
    !(trees.sub)

(* the following builds a diff_func to return a unified diff string of
   configs or config commands
 *)
let list_but_last l =
    let len = List.length l in
    List.filteri (fun i _ -> i < len - 1) l

let ppath_to_string_if_new (c: string list ref) (path: string list) =
    let p = list_but_last path in
    if (!c <> p) then
        (c := p; Printf.sprintf "[%s]\n" (String.concat " " !c)) else ""

let marked_render mark node =
    let lines = Config_tree.render_config node in
    let l = String.split_on_char '\n' lines in
    let m =
        List.map (fun s -> if (String.length s) > 0 then mark ^ s else s) l in
    String.concat "\n" m

let added_lines ?(cmds=false) node path =
    if not cmds then marked_render "+ " (tree_at_path path node)
    else
        let skel = Config_tree.make "" in
        let snode = clone node skel path in
        (Config_tree.render_commands ~op:Set snode []) ^ "\n"

let removed_lines ?(cmds=false) node path =
    if not cmds then marked_render "- " (tree_at_path path node)
    else
        let skel = Config_tree.make "" in
        let snode = clone node skel path in
        (Config_tree.render_commands ~op:Delete snode []) ^ "\n"

let order_commands (strl: string ref) =
    let l = String.split_on_char '\n' !strl in
    let del = List.filter (fun s -> (s <> "") && (s.[0] = 'd')) l in
    let set = List.filter (fun s -> (s <> "") && (s.[0] = 's')) l in
    strl := (String.concat "\n" del) ^ "\n" ^ (String.concat "\n" set) ^ "\n"

let ppath = ref [""]

let unified_diff ?(cmds=false) (str_diff: string ref) (trees : diff_trees) ?recurse:_ (path : string list) (m : change) =
    match m with
    | Added ->
            if not cmds then str_diff := !str_diff ^ (ppath_to_string_if_new ppath path);
            str_diff := !str_diff ^ (added_lines ~cmds:cmds trees.right path)
    | Subtracted ->
            if not cmds then str_diff := !str_diff ^ (ppath_to_string_if_new ppath path);
            str_diff := !str_diff ^ (removed_lines ~cmds:cmds trees.left path)
    | Unchanged -> ()
    | Updated v ->
            if not cmds then str_diff := !str_diff ^ (ppath_to_string_if_new ppath path);
            let ov = Config_tree.get_values trees.left path in
            match ov, v with
            | [_], [_] ->
                    str_diff := !str_diff ^ (removed_lines ~cmds:cmds trees.left path);
                    str_diff := !str_diff ^ (added_lines ~cmds:cmds trees.right path)
            | _, _ -> let ov_set = ValueS.of_list ov in
                      let v_set = ValueS.of_list v in
                      let sub_vals = ValueS.elements (ValueS.diff ov_set v_set) in
                      let add_vals = ValueS.elements (ValueS.diff v_set ov_set) in
                      if not (is_empty sub_vals) then
                          (trees.sub := clone ~set_values:(Some sub_vals) trees.left !(trees.sub) path;
                           str_diff := !str_diff ^ (removed_lines ~cmds:cmds !(trees.sub) path));
                      if not (is_empty add_vals) then
                          (trees.add := clone ~set_values:(Some add_vals) trees.right !(trees.add) path;
                           str_diff := !str_diff ^ (added_lines ~cmds:cmds !(trees.add) path))

let add_empty_path src_node dest_node path =
    clone ~recurse:false ~set_values:(Some []) src_node dest_node path

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

let show_diff ?(cmds=false) path left right =
    if (name_of left) <> (name_of right) then
        raise Incommensurable
    else
        let (left, right) =
            if (path <> []) then
                compare_at_path_maybe_empty left right path
            else (left, right) in
        let trees = make_diff_trees left right in
        let udiff = ref "" in
        ppath := [""];
        diff [] (unified_diff ~cmds:cmds udiff trees) [(Option.some left, Option.some right)];
        if cmds then order_commands udiff;
        !udiff

let union_of_values (n : Config_tree.t) (m : Config_tree.t) =
    let set_n = ValueS.of_list (data_of n).values in
    let set_m = ValueS.of_list (data_of m).values in
    ValueS.elements (ValueS.union set_n set_m)

let union_of_children n m =
    let set_n = ChildrenS.of_list (children_of n) in
    let set_m = ChildrenS.of_list (children_of m) in
    ChildrenS.elements (ChildrenS.union set_n set_m)

(* tree_union is currently used only for unit tests, so only values of data
   are considered. Should there be a reason to expose it in the future,
   consistency check and union of remaining data will need to be added.
 *)
let rec tree_union s t =
    let child_of_union s t c =
        let s_c = Vytree.find s (name_of c) in
        let t_c = Vytree.find t (name_of c) in
        match s_c, t_c with
        | Some child, None -> clone s t [(name_of child)]
        | None, Some _ -> t
        | Some u, Some v ->
                if u ^~ v then
                    let values = union_of_values u v in
                    let data = {(data_of v) with Config_tree.values = values} in
                    Vytree.replace t (Vytree.make data (name_of v))
                else
                    Vytree.replace t (tree_union u v)
        | None, None -> raise Nonexistent_child
    in
    List.fold_left (fun x c -> child_of_union s x c) t (union_of_children s t)
