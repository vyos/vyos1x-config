type value_behaviour = AddValue | ReplaceValue [@@deriving yojson]
type command = Set | Delete

exception Duplicate_value
exception Node_has_no_value
exception No_such_value
exception Useless_set

type config_node_data = {
    values: string list;
    comment: string option;
    tag: bool;
    leaf: bool;
} [@@deriving yojson]

type t = config_node_data Vytree.t [@@deriving yojson]

let default_data = {
    values = [];
    comment = None;
    tag = false;
    leaf = false;
}

let default = Vytree.make default_data ""

let make name = Vytree.make default_data name

let op_to_string op =
    match op with
    | Set -> "set"
    | Delete -> "delete"

let replace_value node path value =
    (* alert exn Vytree.update:
        [Vytree.Empty_path] allow raise
        [Vytree.Nonexistent_path] allow raise
     *)
    let data = {default_data with values=[value]; leaf=true} in
    (Vytree.update[@alert "-exn"]) node path data

let add_value node path value =
    (* alert exn Vytree.get; Vytree.update:
        [Vytree.Empty_path] allow raise
        [Vytree.Nonexistent_path] allow raise
     *)
    let node' = (Vytree.get[@alert "-exn"]) node path in
    let data = Vytree.data_of_node node' in
    let values = data.values in
    match (Vylist.find (fun x -> x = value) values) with
    | Some _ -> raise Duplicate_value
    | None ->
    let values = values @ [value] in
    (Vytree.update[@alert "-exn"]) node path ({data with values=values; leaf=true})

let delete_value node path value =
    (* alert exn Vytree.update:
        [Vytree.Empty_path] allow raise
        [Vytree.Nonexistent_path] allow raise
     *)
    let data = Vytree.data_of_node @@ (Vytree.get[@alert "-exn"]) node path in
    let values = Vylist.remove (fun x -> x = value) data.values in
    (Vytree.update[@alert "-exn"]) node path {data with values=values; leaf=true}

let set_value node path value behaviour =
    (* raises
        [Config_tree.Duplicate_value] from add_value
        [Vytree.Empty_path],
        [Vytree.Nonexistent_path] from add_value; replace_value
     *)
    match behaviour with
    | AddValue -> add_value node path value
    | ReplaceValue -> replace_value node path value

let create_node node path =
    (* raises
        [Vytree.Empty_path]
        [Useless_set]
       alert exn Vytree.exists:
        [Vytree.Empty_path] check and raise
       alert exn Vytree.insert_multi_level
        [Vytree.Empty_path] check and raise
        [Not_found] not possible since position=Default
        [Vytree.Duplicate_child] not possible since path_remaining is complement
        [Vytree.Insert_error] not possible since path_existing exists
     *)
    if Util.is_empty path then raise Vytree.Empty_path
    else
        if ((Vytree.exists[@alert "-exn"]) node path) then raise Useless_set
    else
        let path_existing = Vytree.get_existent_path node path in
        let path_remaining = Vylist.complement path path_existing in
        (Vytree.insert_multi_level[@alert "-exn"]) default_data node path_existing path_remaining default_data

let set node path value behaviour =
    (* raises:
        [Config_tree.Duplicate_value],
        [Vytree.Empty_path]
        [Useless_set]
       avoids:
        [Vytree.Nonexistent_path] from set_value, since called if Vytree.exists
       alert exn Vytree.exists:
        [Vytree.Empty_path] check and raise
       alert exn Vytree.insert_muilt_level:
        [Vytree.Empty_path] check and raise
        [Not_found] not possible since position=Default
        [Vytree.Duplicate_child] not possible since path_remaining is complement
        [Vytree.Insert_error] not possible since path_existing exists
     *)
    if Util.is_empty path then raise Vytree.Empty_path
    else
        if ((Vytree.exists[@alert "-exn"]) node path) then
        (match value with
         | None -> raise Useless_set
         | Some v -> set_value node path v behaviour)
    else
        let path_existing = Vytree.get_existent_path node path in
        let path_remaining = Vylist.complement path path_existing in
        let values = match value with None -> [] | Some v -> [v] in
        let end_data = {default_data with values=values; leaf=true} in
        (* alert exn Vytree.insert_muilt_level:
            [Vytree.Empty_path] allow raise of Vytree.Empty_path
            [Not_found] not possible since position=Default
            [Vytree.Duplicate_child] not possible since path_remaining is complement
            [Vytree.Insert_error] not possible since path_existing exists
         *)
        (Vytree.insert_multi_level[@alert "-exn"]) ~position:Lexical default_data node path_existing path_remaining end_data

let get_values node path =
    (* alert exn Vytree.get:
        [Vytree.Empty_path] allow raise
        [Vytree.Nonexistent_path] allow raise
     *)
    let node' = (Vytree.get[@alert "-exn"]) node path in
    let data = Vytree.data_of_node node' in
    data.values

let get_value node path =
    (* raises
        [Vytree.Empty_path],
        [Vytree.Nonexistent_path] from get_values
        [Node_has_no_value]
     *)
    let values = get_values node path in
    match values with
    | [] -> raise Node_has_no_value
    | x :: _ -> x

let value_exists node path value =
    (* alert exn Vytree.get:
        [Vytree.Empty_path] allow raise
        [Vytree.Nonexistent_path] checked
     *)
    if not ((Vytree.exists[@alert "-exn"]) node path) then false
    else let node' = (Vytree.get[@alert "-exn"]) node path in
    let data = Vytree.data_of_node node' in
    Vylist.in_list data.values value

let delete node path value =
    (* raises:
        [Vytree.Nonexistent_path] from get_values; delete_value
        [Vytree.Empty_path]
        [No_such_value]

       alert exn Vytree.delete
        [Vytree.Empty_path] check and raise
        [Vytree.Nonexistent_path] allow raise
     *)
    if Util.is_empty path then raise Vytree.Empty_path
    else
    match value with
    | Some v ->
        (let values = get_values node path in
        if Vylist.in_list values v then
        (match values with
        | [_] -> (Vytree.delete[@alert "-exn"]) node path
        | _ -> delete_value node path v)
        else raise No_such_value)
    | None ->
            (Vytree.delete[@alert "-exn"]) node path

let set_comment node path comment =
    (* alert exn Vytree.get_data; Vytree.update:
        [Vytree.Empty_path] allow raise
        [Vytree.Nonexistent_path] allow raise
     *)
    let data = (Vytree.get_data[@alert "-exn"]) node path in
    (Vytree.update[@alert "-exn"]) node path {data with comment=comment}

let get_comment node path =
    (* alert exn Vytree.get_data:
        [Vytree.Empty_path] allow raise
        [Vytree.Nonexistent_path] allow raise
     *)
    let data = (Vytree.get_data[@alert "-exn"]) node path in
    data.comment

let set_tag node path tag =
    (* alert exn Vytree.get_data; Vytree.update:
        [Vytree.Empty_path] allow raise
        [Vytree.Nonexistent_path] allow raise
     *)
    let data = (Vytree.get_data[@alert "-exn"]) node path in
    (Vytree.update[@alert "-exn"]) node path {data with tag=tag}

let is_tag node path =
    (* alert exn Vytree.get_data:
        [Vytree.Empty_path] allow raise
        [Vytree.Nonexistent_path] allow raise
     *)
    let data = (Vytree.get_data[@alert "-exn"]) node path in
    data.tag

let is_tag_value node path =
    (* raises
        [Vytree.Empty_path],
        [Vytree.Nonexistent_path] from is_tag
     *)
    match path with
    | [] | [_] -> false
    | _ -> is_tag node (Util.drop_last path)

let set_leaf node path leaf =
    (* alert exn Vytree.get_data; Vytree.update:
        [Vytree.Empty_path] allow raise
        [Vytree.Nonexistent_path] allow raise
     *)
    let data = (Vytree.get_data[@alert "-exn"]) node path in
    (Vytree.update[@alert "-exn"]) node path {data with leaf=leaf}

let is_leaf node path =
    (* alert exn Vytree.get_data:
        [Vytree.Empty_path] allow raise
        [Vytree.Nonexistent_path] allow raise
     *)
    let data = (Vytree.get_data[@alert "-exn"]) node path in
    data.leaf

let get_subtree ?(with_node=false) node path =
    (* alert exn Vytree.get:
        [Vytree.Empty_path] checked
        [Vytree.Nonexistent_path] caught
     *)
    match path with
    | [] -> node
    | _ ->
    try
        let n = (Vytree.get[@alert "-exn"]) node path in
        if with_node then
            Vytree.make_full default_data "" [n]
        else
            Vytree.make_full default_data "" (Vytree.children_of_node n)
    with Vytree.Nonexistent_path -> make ""

let value_paths_of_tree node =
    (* alert exn Vytree.is_terminal_path:
        [Vytree.Empty_path] not possible in pattern path non-empty
     *)
    let func ct (p, a) _t =
        match p with
        | [] -> (p, a)
        | _ ->
            let q = List.rev p in
            if not ((Vytree.is_terminal_path[@alert "-exn"]) ct q) then
                (p, a)
            else
                let vals = get_values ct q in
                match vals with
                | [] -> (p, q::a)
                | _ as vs ->
                    let a' =
                        let f acc v =
                            let q' = q @ [v] in
                            q'::acc
                        in List.fold_left f a vs
                    in (p, a')
    in List.rev (snd (Vytree.fold_tree_with_path (func node) ([], []) node))

let prune_delete node path =
    (* raises:
        [Vytree.Nonexistent_path] from is_tag_value; delete
        [Vytree.Empty_path]
       alert exn Vytree.is_terminal_path:
        [Vytree.Empty_path] check and raise
     *)
    if Util.is_empty path then raise Vytree.Empty_path
    else
    if is_tag_value node path then
        let tag_path = Util.drop_last path in
        let terminal = (Vytree.is_terminal_path[@alert "-exn"]) node tag_path in
        match terminal with
        | true -> delete node tag_path None
        | false -> node
    else node


module Renderer =
struct
    (* Rendering configs as set commands *)
    let render_set_path ?(op=Set) path value =
        let v = Printf.sprintf "\'%s\'" value in
        List.append path [v] |> String.concat " " |> Printf.sprintf "%s %s" (op_to_string op)

    let rec render_commands ?(op=Set) path ct =
        (* alert exn Vytree.get:
            [Vytree.Empty_path] not possible as called on non-empty children
            [Vytree.Nonexistent_path] not possible as called on non-empty children
         *)
        let new_path = List.append path [Vytree.name_of_node ct] in
        let new_path_str = String.concat " " new_path in
        let data = Vytree.data_of_node ct in
        (* Get the node comment, if any *)
        let comment =
            match op with
            | Set -> Util.default "" data.comment
            | Delete -> ""
        in
        let comment_cmd = (if comment = "" then "" else Printf.sprintf "comment %s \'%s\'" new_path_str comment) in
        let child_names = Vytree.list_children ct in
        (* Now handle the different cases for nodes with and without children *)
        match child_names with
        | [] ->
             let values = List.map Util.escape_string data.values in
             let cmds =
                 begin
                 match values with
                 | [] ->
                      (* Valueless leaf node or a non-leaf node *)
                      String.concat " " new_path |> Printf.sprintf "%s %s" (op_to_string op)
                 | [v] ->
                      (* Single value, just one command *)
                      render_set_path ~op:op new_path v
                 | vs ->
                      (* A leaf node with multiple values *)
                      List.map (render_set_path ~op:op new_path) vs |> String.concat "\n"
                  end
              in
              if comment_cmd = "" then cmds else Printf.sprintf "%s\n%s" cmds comment_cmd
        | _ :: _ ->
            let children =
                List.map (fun n -> (Vytree.get[@alert "-exn"]) ct [n]) child_names
            in
            let rendered_children = List.map (render_commands ~op:op new_path) children in
            let cmds = String.concat "\n" rendered_children in
            if comment_cmd = "" then cmds else Printf.sprintf "%s\n%s" cmds comment_cmd

  (* Rendering config as a VyOS/EdgeOS config file *)
  let make_indent indent level = String.make (level * indent) ' '

  let render_values ?(ord_val=false) indent_str name values =
    match values with
    | [] -> Printf.sprintf "%s%s\n" indent_str name
    | [v] -> Printf.sprintf "%s%s \"%s\"\n" indent_str name (Util.escape_string v)
    | _  -> 
      let values =
          if ord_val then
              List.sort Util.lexical_numeric_compare values
          else values
      in
      let rendered = List.map (fun s -> Printf.sprintf "%s%s \"%s\"" indent_str name (Util.escape_string s)) values in
      let rendered = String.concat "\n" rendered in
      Printf.sprintf "%s\n" rendered

  let render_comment indent c =
    match c with
    | None -> ""
    | Some c ->  Printf.sprintf "%s/* %s */\n" indent c

  let rec render_node ?(ord_val=false) indent level node =
    let indent_str = make_indent indent level in
    let name = Vytree.name_of_node node in
    let data = Vytree.data_of_node node in
    let is_tag = data.tag in 
    let comment = render_comment indent_str data.comment in
    let children = Vytree.children_of_node node in
    match children with
    | [] ->
      if data.leaf then
        let values = render_values ~ord_val:ord_val indent_str name data.values in
        Printf.sprintf "%s%s" comment values
      else
        Printf.sprintf "%s%s%s {\n%s}\n" comment indent_str name indent_str
    | _ :: _ ->
      if is_tag then 
        begin
          let inner = List.map (render_tag_node_child ~ord_val:ord_val indent level name) children in
          String.concat "" inner
        end
      else
        begin
          let inner = List.map (render_node ~ord_val:ord_val indent (level + 1)) children in
          let inner = String.concat "" inner in
          Printf.sprintf "%s%s%s {\n%s%s}\n" comment indent_str name inner indent_str
        end
  and render_tag_node_child ?(ord_val=false) indent level parent node =
    let indent_str = make_indent indent level in
    let name = Vytree.name_of_node node in
    let data = Vytree.data_of_node node in
    let comment = render_comment indent_str data.comment in
    let children = Vytree.children_of_node node in
    let inner = List.map (render_node ~ord_val:ord_val indent (level + 1)) children in
    let inner = String.concat "" inner in
    Printf.sprintf "%s%s%s %s {\n%s%s}\n" comment indent_str parent name inner indent_str

  let render_config ?(ord_val=false) node =
    let children = Vytree.children_of_node node in
    let child_configs = List.map (render_node ~ord_val:ord_val 4 0) children in
    String.concat "" child_configs

end (* Renderer *)

module JSONRenderer = struct
    let render_values values =
        match values with
        | [] -> Printf.sprintf "{}"
        | [v] -> Printf.sprintf "\"%s\"" (Util.escape_string v)
        | _  ->
            let rendered = List.map (fun s -> Printf.sprintf "\"%s\"" (Util.escape_string s)) values in
            let rendered = String.concat "," rendered in
            Printf.sprintf "[%s]" rendered

    let rec render_node node =
        let name = Vytree.name_of_node node in
        let children = Vytree.children_of_node node in
        let data = Vytree.data_of_node node in
        match children, data.values with
        | [], [] ->
            (* Empty node.
               In JSON, we don't differentiate between leaf and non-leaf nodes in this case. *)
            Printf.sprintf "\"%s\": {}" name
        | _, [] ->
            (* Non-empty, non-leaf node. *)
            let children_strs = List.map render_node children in
            let children_str = String.concat "," children_strs in
            Printf.sprintf "\"%s\": {%s}" name children_str
        | [], _ ->
            (* Leaf node with children. *)
            Printf.sprintf "\"%s\": %s" name (render_values data.values)
        | _, _ ->
            (* Shouldn't happen *)
            failwith "Internal error: non-leaf node with values"

    let render_json node =
        let children = Vytree.children_of_node node in
        let child_configs = List.map render_node children in
        let child_configs = String.concat "," child_configs in
        Printf.sprintf "{%s}" child_configs
end (* JSONRenderer *)

let render_commands ?(op=Set) node path =
    (* raises:
        [Vytree.Nonexistent_path]
       alert exn Vytree.get:
        [Vytree.Empty_path] not possible as called on pattern non-empty path
        [Vytree.Nonexistent_path] allow raise
     *)
    let node =
	match path with
        | [] -> node
        | _ -> (Vytree.get[@alert "-exn"]) node path
    in
    let children = Vytree.children_of_node node in
    let commands = List.map (Renderer.render_commands ~op:op path) children in
    String.concat "\n" commands

let render_config ?(ord_val=false) = Renderer.render_config ~ord_val:ord_val

let render_at_level node path =
    (* alert exn Vytree.get:
        [Vytree.Empty_path] not possible as called on pattern non-empty path
        [Vytree.Nonexistent_path] allow raise
     *)
    let  node =
        match path with
        | [] -> node
        | _ -> (Vytree.get[@alert "-exn"]) node path
    in
    render_config node

let render_json = JSONRenderer.render_json

let render_json_ast c = to_yojson c |> Yojson.Safe.to_string
