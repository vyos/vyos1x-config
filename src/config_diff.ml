type change = Unchanged | Added | Deleted | Updated of string list

type diff_func = ?with_children:bool -> string list -> change -> unit

type diff_trees = {
    left: Config_tree.t;
    right: Config_tree.t;
    add: Config_tree.t ref;
    del: Config_tree.t ref;
    inter: Config_tree.t ref;
}

exception Incommensurable
exception Empty_comparison

module ValueS = Set.Make(struct type t = string let compare = compare end)

let make_diff_trees l r = { left = l; right = r;
                           add = ref (Config_tree.make "root");
                           del = ref (Config_tree.make "root");
                           inter = ref (Config_tree.make "root");
}

let name_of n = Vytree.name_of_node n
let data_of n = Vytree.data_of_node n
let children_of n = Vytree.children_of_node n
let make data name children = Vytree.make_full data name children

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

let opt_zip n m =
    left_opt_pairs n m @ right_opt_pairs n m |> List.sort_uniq compare

let get_opt_name left_opt right_opt =
    match left_opt, right_opt with
    | Some left_node, None -> name_of left_node
    | None, Some right_node -> name_of right_node
    | Some left_node, Some _ -> name_of left_node
    | None, None -> raise Empty_comparison

let update_path path left_opt right_opt =
    let name = get_opt_name left_opt right_opt in
    if name = "root" then path
    else path @ [name]

(* tree diff algorithm: walk the tree pair, calling a function of type
   diff_func at bifurcation points; f acts as a form of continuation.
   The idea of matching on pairs of (node opt) is from
   https://github.com/LukeBurgessYeo/tree-diff
*)
let rec diff (path : string list) (f : diff_func) (l : (Config_tree.t option * Config_tree.t option) list) =
    match l with
    | [] -> ()
    | (left_node_opt, right_node_opt) :: ls ->
        (let path = update_path path left_node_opt right_node_opt in
        match left_node_opt, right_node_opt with
        | Some _, None -> f path Deleted
        | None, Some _ -> f path Added
        | Some left_node, Some right_node when left_node = right_node ->
                f path Unchanged
        | Some left_node, Some right_node when left_node ^~ right_node ->
                let values = (data_of right_node).values in
                f path (Updated values)
        | Some left_node, Some right_node ->
                (f ~with_children:false path Unchanged;
                diff path f (opt_zip left_node right_node))
        | None, None -> raise Empty_comparison)
        ; diff path f ls

(* copy node paths between trees *)
let rec clone_path ?(with_children=true) ?(set_values=[]) old_root new_root path_done path_remaining =
    match path_remaining with
    | [] | [_] ->
        let path_total = path_done @ path_remaining in
        let old_node = Vytree.get old_root path_total in
        let data = if not (set_values = []) then
            { (data_of old_node) with Config_tree.values = set_values } else (data_of old_node) in
        if with_children then
            Vytree.insert ~children:(children_of old_node) new_root path_total data
        else
            Vytree.insert new_root path_total data
    | name :: names ->
        let path_done = path_done @ [name] in
        let old_node = Vytree.get old_root path_done in
        let new_root = Vytree.insert new_root path_done (data_of old_node) in
        clone_path ~with_children:with_children ~set_values:set_values old_root new_root path_done names

let clone ?(with_children=true) ?(set_values=[]) old_root new_root path =
    match path with
    | [] -> if with_children then old_root else new_root
    | _ ->
            let path_existing = Vytree.get_existent_path new_root path in
            let path_remaining = Vylist.complement path path_existing in
            clone_path ~with_children:with_children ~set_values:set_values old_root new_root path_existing path_remaining

let rec graft_children children stock path =
    match children with
    | [] -> stock
    | x::xs ->
            let stock = Vytree.insert ~children:(children_of x) stock (path @ [name_of x]) (data_of x)
            in graft_children xs stock path

let graft_tree stem stock path =
    graft_children (children_of stem) stock path

(* define the diff_func; in this instance, we imperatively build the difference trees *)
let decorate_trees (trees : diff_trees) ?(with_children=true) (path : string list) (m : change) =
    match m with
    | Added -> trees.add := clone trees.right !(trees.add) path
    | Deleted -> trees.del := clone ~with_children:false trees.left !(trees.del) path
    | Unchanged -> trees.inter := clone ~with_children:with_children trees.left !(trees.inter) path
    | Updated v ->
            (* if in this case, node at path is guaranteed to exist *)
            let ov = Config_tree.get_values trees.left path in
            match ov, v with
            | [_], [_] -> trees.del := clone trees.left !(trees.del) path;
                          trees.add := clone trees.right !(trees.add) path
            | _, _ -> let ovs = ValueS.of_list ov in
                      let vs = ValueS.of_list v in
                      let set_ov = ValueS.elements (ValueS.diff ovs vs) in
                      let set_v = ValueS.elements (ValueS.diff vs ovs) in
                      let set_inter = ValueS.elements (ValueS.inter ovs vs) in
                      if not (set_ov = []) then
                          trees.del := clone ~set_values:set_ov trees.left !(trees.del) path;
                      if not (set_v = []) then
                          trees.add := clone ~set_values:set_v trees.right !(trees.add) path;
                      if not (set_inter = []) then
                          trees.inter := clone ~set_values:set_inter trees.left !(trees.inter) path

(* get sub trees for path-relative comparison *)
let tree_at_path path node =
    try
        let node = Vytree.get node path in
        make Config_tree.default_data "root" [node]
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
    let add_node = Config_tree.make "add" in
    let del_node = Config_tree.make "delete" in
    let int_node = Config_tree.make "inter" in
    let ret = make Config_tree.default_data "root" [add_node; del_node; int_node] in
    let ret = graft_tree !(trees.add) ret ["add"] in
    let ret = graft_tree !(trees.del) ret ["delete"] in
    let ret = graft_tree !(trees.inter) ret ["inter"] in
    ret
