exception Incompatible_union
exception Nonexistent_child

module type Data = sig
    type t
end

module type Tree = sig
    module D: Data
    type t = D.t Vytree.t
end

module Tree_impl (D: Data): Tree with module D = D = struct
    module D = D
    type t = D.t Vytree.t
end

module Alg (D: Data) (T: Tree with module D = D) = struct
    module TreeOrd = struct
        type t = T.t
        let compare a b =
            Util.lexical_numeric_compare (Vytree.name_of_node a) (Vytree.name_of_node b)
    end
    module SetT = Set.Make(TreeOrd)

    let union_of_children n m =
        let set_n = SetT.of_list (Vytree.children_of_node n) in
        let set_m = SetT.of_list (Vytree.children_of_node m) in
        SetT.elements (SetT.union set_n set_m)

    let find_child n c = Vytree.find n (Vytree.name_of_node c)

    let insert_child n c =
        (* alert exn Vytree.insert:
            [Vytree.Empty_path] not possible as called on name_of_node for existing child
            [Not_found] not possible for postion=Lexical
            [Vytree.Duplicate_child] not possible as find_child is None
            [Vytree.Insert_error] not possible as no intermediary nodes
         *)
        (Vytree.insert[@alert "-exn"]) ~position:Vytree.Lexical ~children:(Vytree.children_of_node c) n [(Vytree.name_of_node c)] (Vytree.data_of_node c)

    let replace_child n c =
        (* alert exn Vytree.replace:
            [Not_found] not possible as child name already present
         *)
        (Vytree.replace[@alert "-exn"]) n c

    let rec tree_union s t f =
        (* raises:
            [Incompatible_union]
            [Nonexistent_child]
         *)
        if (Vytree.name_of_node s) <> (Vytree.name_of_node t) then
            raise Incompatible_union
        else
        let child_of_union s t c =
            let s_c = find_child s c in
            let t_c = find_child t c in
            match s_c, t_c with
            | Some child, None ->
                    insert_child t child
            | None, Some _ -> t
            | Some u, Some v ->
                    if (Vytree.data_of_node u <> Vytree.data_of_node v) then
                        replace_child t (tree_union u (f u v) f)
                    else
                        replace_child t (tree_union u v f)
            | None, None ->
                    (* Not possible in fold over union_of_children *)
                    raise Nonexistent_child
        in
        List.fold_left (fun x c -> child_of_union s x c) t (union_of_children s t)
end

module ConfigData: Data with type t = Config_tree.config_node_data = struct
    type t = Config_tree.config_node_data
end

module RefData: Data with type t = Reference_tree.ref_node_data = struct
    type t = Reference_tree.ref_node_data
end

module ConfigAlg = Alg(ConfigData)(Tree_impl(ConfigData))
module RefAlg = Alg(RefData)(Tree_impl(RefData))
