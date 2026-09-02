
open Reference_tree


let node_is_tag node =
    let data = Vytree.data_of_node node in
    match data.node_type with
    | `Tag -> true
    | _ -> false

let node_is_multi node =
    let data = Vytree.data_of_node node in
    data.multi

let get_nodes ?(tag_value_placeholder="") (condition: t -> bool)  rt =
    let stored l =
        match l with
        | [] -> []
        | x :: _ -> x
    in
    let func ((p, l), acc) rt =
        match p with
        | [] -> ((p, l), acc)
        | _ ->
        let v = stored l in
        let v = (Vytree.name_of_node rt) :: v in
        let acc =
            if condition rt then
                List.rev v :: acc
            else
                acc
        in
        let v =
            if node_is_tag rt then
                match tag_value_placeholder with
                | "" -> v
                | _ as s -> s :: v
            else
                v
        in ((p, v::l), acc)
    in
    let ret = Vytree.fold_tree_with_path_and_stack func (([], []), []) rt in
    List.rev ret

let get_multi_nodes ?(tag_value_placeholder="") rt =
    get_nodes ~tag_value_placeholder node_is_multi rt

let get_multi_nodes_yojson ?(tag_value_placeholder="") rt =
    let ret = get_multi_nodes ~tag_value_placeholder rt in
    [%to_yojson: string list list] ret |> Yojson.Safe.to_string

let get_tag_nodes ?(tag_value_placeholder="") rt =
    get_nodes ~tag_value_placeholder node_is_tag rt

let get_tag_nodes_yojson ?(tag_value_placeholder="") rt =
    let ret = get_tag_nodes ~tag_value_placeholder rt in
    [%to_yojson: string list list] ret |> Yojson.Safe.to_string

let node_is_of_kind k rt =
    let data = Vytree.data_of_node rt in
    List.mem k data.kind

let get_nodes_of_kind ?(tag_value_placeholder="") rt k =
    get_nodes ~tag_value_placeholder (node_is_of_kind k) rt

let get_nodes_of_kind_yojson ?(tag_value_placeholder="") rt k =
    let ret = get_nodes_of_kind ~tag_value_placeholder rt k in
    [%to_yojson: string list list] ret |> Yojson.Safe.to_string

let node_dependency_of_kind k rt =
    let data = Vytree.data_of_node rt in
    match data.dependency with
    | Some c when c.kind = k -> true
    | _ -> false

let get_rdeps_of_kind ?(tag_value_placeholder="") rt k =
    get_nodes ~tag_value_placeholder (node_dependency_of_kind k) rt

let get_rdeps_of_kind_yojson ?(tag_value_placeholder="") rt k =
    let ret = get_rdeps_of_kind ~tag_value_placeholder rt k in
    [%to_yojson: string list list] ret |> Yojson.Safe.to_string


let get_nodes_data ?(tag_value_placeholder="") (data_opt: t -> string option)  rt =
    let stored l =
        match l with
        | [] -> []
        | x :: _ -> x
    in
    let func ((p, l), acc) rt =
        match p with
        | [] -> ((p, l), acc)
        | _ ->
        let v = stored l in
        let v = (Vytree.name_of_node rt) :: v in
        let acc =
            match data_opt rt with
            | Some o -> (List.rev v, o) :: acc
            | _ -> acc
        in
        let v =
            if node_is_tag rt then
                match tag_value_placeholder with
                | "" -> v
                | _ as s -> s :: v
            else
                v
        in ((p, v::l), acc)
    in
    let ret = Vytree.fold_tree_with_path_and_stack func (([], []), []) rt in
    List.rev ret

let node_dependency_of_kind_opt k rt =
    let data = Vytree.data_of_node rt in
    match data.dependency with
    | Some c when c.kind = k -> Some c.alert
    | _ -> None

let get_rdeps_of_kind_data ?(tag_value_placeholder="") rt k =
    get_nodes_data ~tag_value_placeholder (node_dependency_of_kind_opt k) rt

let get_rdeps_of_kind_data_yojson ?(tag_value_placeholder="") rt k =
    let ret = get_rdeps_of_kind_data ~tag_value_placeholder rt k in
    [%to_yojson: (string list * string) list] ret |> Yojson.Safe.to_string

let get_path_owner reftree path =
    if Util.is_empty path then None
    else
    if not ((Vytree.exists[@alert "-exn"]) reftree path) then None
    else
    let func data = data.owner in
    (get_ceil_data[@alert "-exn"]) func reftree path

