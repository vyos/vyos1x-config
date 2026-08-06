open Tree_alg

module ValueS = Config_tree.ValueS

let union_of_values (n : Config_tree.t) (m : Config_tree.t) =
    let set_n = ValueS.of_list (Vytree.data_of_node n).values in
    let set_m = ValueS.of_list (Vytree.data_of_node m).values in
    ValueS.elements (ValueS.union set_n set_m)

let tree_union s t =
    (* raises:
        [Tree_alg.Incompatible_union]
        [Tree_alg.Nonexistent_child] should not be reachable
       alert exn Tree_alg.ConfigAlg.tree_union:
        [Tree_alg.Incompatible_union] allow raise
        [Tree_alg.Nonexistent_child] allow raise; should not be reachable
     *)
    let f u v =
        let values = union_of_values u v in
        let data = {(Vytree.data_of_node v) with Config_tree.values = values} in
        Vytree.make_full data (Vytree.name_of_node v) (Vytree.children_of_node v)
    in
    (ConfigAlg.tree_union[@alert "-exn"]) s t f

let tree_merge ?(destructive=false) s t =
    (* raises:
        [Tree_alg.Incompatible_union]
        [Tree_alg.Nonexistent_child] should not be reachable
       alert exn Tree_alg.ConfigAlg.tree_union:
        [Tree_alg.Incompatible_union] allow raise
        [Tree_alg.Nonexistent_child] allow raise; should not be reachable
     *)
    let f u v =
        let data =
            match destructive with
            | false -> Vytree.data_of_node u
            | true  -> Vytree.data_of_node v
        in Vytree.make_full data (Vytree.name_of_node v) (Vytree.children_of_node v)
    in
    (ConfigAlg.tree_union[@alert "-exn"]) s t f

