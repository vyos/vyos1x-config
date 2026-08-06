open Diff

module ValueS = Config_tree.ValueS

module Diff_tree = struct
    type t = { left: Config_tree.t;
               right: Config_tree.t;
               add: Config_tree.t;
               sub: Config_tree.t;
               del: Config_tree.t;
               inter: Config_tree.t;
             }

    let make_init l r = { left = l;
                            right = r;
                            add = Config_tree.default;
                            sub = Config_tree.default;
                            del = Config_tree.default;
                            inter = Config_tree.default;
                          }

    let diff_func ?(recurse=true) (path : string list) res (m : change) =
        (* raises no exception:
            clone will always be called on extant path of left or right
           alert exn Vytree.get_values:
            [Vytree.Empty_path] not possible as pattern Updated implies non-empty path
            [Vytree.Nonexistent_path] not possible as pattern Updated implies path exists
         *)
        match m with
        | Added -> {res with add = (Config_tree.clone[@alert "-exn"]) res.right res.add path; }
        | Subtracted ->
            {res with sub = (Config_tree.clone[@alert "-exn"]) res.left res.sub path;
             del = (Config_tree.clone[@alert "-exn"]) ~recurse:false ~set_values:(Some []) res.left res.del path; }
        | Unchanged ->
            {res with inter = (Config_tree.clone[@alert "-exn"]) ~recurse:recurse res.left res.inter path; }
        | Updated v ->
                (* if in this case, node at path is guaranteed to exist *)
                let ov = (Config_tree.get_values[@alert "-exn"]) res.left path in
                match ov, v with
                | [_], [_] -> {res with sub = (Config_tree.clone[@alert "-exn"]) res.left res.sub path;
                               del = (Config_tree.clone[@alert "-exn"]) res.left res.del path;
                               add = (Config_tree.clone[@alert "-exn"]) res.right res.add path; }
                | _, _ -> let ov_set = ValueS.of_list ov in
                          let v_set = ValueS.of_list v in
                          let sub_vals = ValueS.elements (ValueS.diff ov_set v_set) in
                          let add_vals = ValueS.elements (ValueS.diff v_set ov_set) in
                          let inter_vals = ValueS.elements (ValueS.inter ov_set v_set) in
                          let sub_tree =
                              if not (Util.is_empty sub_vals) then
                                  (Config_tree.clone[@alert "-exn"]) ~set_values:(Some sub_vals) res.left res.sub path
                              else
                                  res.sub
                          in
                          let del_tree =
                              if not (Util.is_empty sub_vals) then
                                  if (Util.is_empty add_vals) && (Util.is_empty inter_vals) then
                                      (* delete whole node, not just values *)
                                      (Config_tree.clone[@alert "-exn"]) ~set_values:(Some []) res.left res.del path
                                  else
                                      (Config_tree.clone[@alert "-exn"]) ~set_values:(Some sub_vals) res.left res.del path
                              else
                                  res.del
                          in
                          let add_tree =
                              if not (Util.is_empty add_vals) then
                                  (Config_tree.clone[@alert "-exn"]) ~set_values:(Some add_vals) res.right res.add path
                              else
                                  res.add
                          in
                          let inter_tree =
                              if not (Util.is_empty inter_vals) then
                                  (Config_tree.clone[@alert "-exn"]) ~set_values:(Some inter_vals) res.left res.inter path
                              else
                                  res.inter
                          in { res with add = add_tree;
                               sub = sub_tree;
                               del = del_tree;
                               inter = inter_tree; }
end

module D = Diff(Diff_tree)

(* get sub trees for path-relative comparison *)

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

(* call recursive diff on Diff_tree.t with Diff_tree.diff_func *)

let diff_trees path left right =
    (* raises:
        [Empty_comparison] from tree_at_path
        [Incommensurable]
     *)
    if (Vytree.name_of_node left) <> (Vytree.name_of_node right) then
        raise Incommensurable
    else
        let (left, right) = if not (path = []) then
            (tree_at_path path left, tree_at_path path right) else (left, right) in
        let trees = Diff_tree.make_init left right in
        D.diff trees left right

(* wrapper to return single tree with diff trees as subtrees *)

let diff_tree path left right =
    (* raises:
        [Incommensurable],
        [Empty_comparison] from compare
     *)
    let trees = diff_trees path left right in
    let add_node =
        Vytree.make_full Config_tree.default_data "add" (Vytree.children_of_node (trees.add)) in
    let sub_node =
        Vytree.make_full Config_tree.default_data "sub" (Vytree.children_of_node (trees.sub)) in
    let del_node =
        Vytree.make_full Config_tree.default_data "del" (Vytree.children_of_node (trees.del)) in
    let int_node =
        Vytree.make_full Config_tree.default_data "inter" (Vytree.children_of_node (trees.inter)) in
    Vytree.make_full Config_tree.default_data "" [add_node; sub_node; del_node; int_node]

(* convenience function needed for commit algorithm:
    we need a hybrid tree between the 'del' tree and the 'sub' tree, namely:
    in case the del tree has a terminal tag node (== all tag values have
    been removed) add tag node values for proper removal in commit execution
 *)

let get_tagged_delete_tree dt =
    (* alert exn Config_tree.is_tag:
        [Vytree.Empty_path] not possible in pattern non-empty path
        [Vytree.Nonexistent_path] not possible in fold_tree_with_path
       alert exn Vytree.is_terminal_path:
        [Vytree.Empty_path] not possible in pattern non-empty path
       alert exn Vytree.children_of_path:
        [Vytree.Empty_path] not possible in pattern non-empty path
        [Vytree.Nonexistent_path] not possible in super-tree of fold_tree_with_path arg
       alert exn Vytree.insert:
        [Vytree.Empty_path]: not possible since called on pattern path non-empty
        [Not_found]: not possible for postion=Lexical
        [Vytree.Duplicate_child]: not possible by condition is_terminal_path
        [Vytree.Insert_error]: not possible since constructed iteratively from existing path
     *)
    let del_tree = Config_tree.get_subtree dt ["del"] in
    let sub_tree = Config_tree.get_subtree dt ["sub"] in
    let f (p, a) _t =
        let q = List.rev p in
        match q with
        | [] -> (p, a)
        | _ ->
        if (Config_tree.is_tag[@alert "-exn"]) a q && (Vytree.is_terminal_path[@alert "-exn"]) a q then
            let children = (Vytree.children_of_path[@alert "-exn"]) sub_tree q in
            let insert_child path node name =
                (Vytree.insert[@alert "-exn"]) ~position:Lexical node (path @ [name]) Config_tree.default_data
            in
            let a' = List.fold_left (insert_child q) a children in
            (p, a')
        else
            (p, a)
    in
    Vytree.fold_tree_with_path f ([], del_tree) del_tree
