type change = Unchanged | Added | Subtracted | Updated of string list

exception Incommensurable
exception Empty_comparison

let name_of n = Vytree.name_of_node n
let data_of n = Vytree.data_of_node n
let children_of n = Vytree.children_of_node n

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

let opt_tuple_cmp t1 t2 =
    let opt_tuple_val t =
        match t with
        | None, None -> ""
        | Some x, None -> name_of x
        | None, Some y -> name_of y
        | Some x, Some _ -> name_of x
    in Util.lexical_numeric_compare (opt_tuple_val t1) (opt_tuple_val t2)

let opt_zip n m =
    left_opt_pairs n m @ right_opt_pairs n m |> List.sort_uniq opt_tuple_cmp

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

module type Place = sig
    type t
    val diff_func : ?recurse:bool -> string list -> t -> change -> t
end

module Diff (P: Place) = struct
    let rec diff_calc (path : string list) (res: P.t) ((left_node_opt, right_node_opt) : Config_tree.t option * Config_tree.t option) =
        let path = update_path path left_node_opt right_node_opt in
        match left_node_opt, right_node_opt with
        | None, None -> raise Empty_comparison
        | Some _, None -> P.diff_func path res Subtracted
        | None, Some _ -> P.diff_func path res Added
        | Some left_node, Some right_node when left_node = right_node ->
            P.diff_func ~recurse:true path res Unchanged
        | Some left_node, Some right_node when left_node ^~ right_node ->
            let values = (data_of right_node).values in
            P.diff_func path res (Updated values)
        | Some left_node, Some right_node ->
            let ret = P.diff_func ~recurse:false path res Unchanged in
            List.fold_left (diff_calc path) ret (opt_zip left_node right_node)

    let diff (init: P.t) l r = diff_calc [] init (Option.some l, Option.some r)
end
