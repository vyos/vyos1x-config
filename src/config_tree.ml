type value_behaviour = AddValue | ReplaceValue

exception Duplicate_value
exception Node_has_no_value
exception No_such_value
exception Useless_set

type config_node_data = {
    values: string list;
    comment: string option;
    tag: bool;
} [@@deriving yojson]

type t = config_node_data Vytree.t [@@deriving yojson]

let default_data = {
    values = [];
    comment = None;
    tag = false;
}

let make name = Vytree.make default_data name

let replace_value node path value =
  let data = {default_data with values=[value]} in
  Vytree.update node path data

let add_value node path value =
  let node' = Vytree.get node path in
  let data = Vytree.data_of_node node' in
  let values = data.values in
  match (Vylist.find (fun x -> x = value) values) with
  | Some _ -> raise Duplicate_value
  | None ->
    let values = values @ [value] in
    Vytree.update node path ({data with values=values})

let delete_value node path value =
    let data = Vytree.data_of_node @@ Vytree.get node path in
    let values = Vylist.remove (fun x -> x = value) data.values in
    Vytree.update node path {data with values=values}

let set_value node path value behaviour =
    match behaviour with
    | AddValue -> add_value node path value
    | ReplaceValue -> replace_value node path value

let set node path value behaviour =
    if (Vytree.exists node path) then
        (match value with
         | None -> raise Useless_set
         | Some v -> set_value node path v behaviour)
    else
        let path_existing = Vytree.get_existent_path node path in
        let path_remaining = Vylist.complement path path_existing in
        let values = match value with None -> [] | Some v -> [v] in
        Vytree.insert_multi_level default_data node path_existing path_remaining {default_data with values=values}

let get_values node path =
    let node' = Vytree.get node path in
    let data = Vytree.data_of_node node' in
    data.values

let get_value node path =
    let values = get_values node path in
    match values with
    | [] -> raise Node_has_no_value
    | x :: _ -> x

let delete node path value =
    match value with
    | Some v ->
        (let values = get_values node path in
        if Vylist.in_list values v then
        (match values with
        | [_] -> Vytree.delete node path
        | _ -> delete_value node path v)
        else raise No_such_value)
    | None ->
	Vytree.delete node path

let set_comment node path comment =
    let data = Vytree.get_data node path in
    Vytree.update node path {data with comment=comment}

let get_comment node path =
    let data = Vytree.get_data node path in
    data.comment

let set_tag node path tag =
    let data = Vytree.get_data node path in
    Vytree.update node path {data with tag=tag}

let is_tag node path =
    let data = Vytree.get_data node path in
    data.tag

module Renderer =
struct
    (* Rendering configs as set commands *)
    let render_set_path path value =
        let v = Printf.sprintf "\'%s\'" value in
        List.append path [v] |> String.concat " " |> Printf.sprintf "set %s"

    let rec render_commands path ct =
        let new_path = List.append path [Vytree.name_of_node ct] in
        let new_path_str = String.concat " " new_path in
        let data = Vytree.data_of_node ct in
        (* Get the node comment, if any *)
        let comment = Util.default "" data.comment in 
        let comment_cmd = (if comment = "" then "" else Printf.sprintf "comment %s \'%s\'" new_path_str comment) in
        let child_names = Vytree.list_children ct in
        (* Now handle the different cases for nodes with and without children *)
        match child_names with
        | [] ->
             (* This is a leaf node *)
             let values = List.map String.escaped data.values in
             let cmds =
                 begin
                 match values with
                 | [] ->
                      (* Valueless leaf node *)
                      String.concat " " new_path |> Printf.sprintf "set %s"
                 | [v] ->
                      (* Single value, just one command *)
                      render_set_path new_path v
                 | vs ->
                      (* A leaf node with multiple values *)
                      List.map (render_set_path new_path) vs |> String.concat "\n"
                  end
              in
              if comment_cmd = "" then cmds else Printf.sprintf "%s\n%s" cmds comment_cmd
        | _ :: _ ->
            (* A node with children *)
            let children = List.map (fun n -> Vytree.get ct [n]) child_names in
            let rendered_children = List.map (render_commands new_path) children in
            let cmds = String.concat "\n" rendered_children in
            if comment_cmd = "" then cmds else Printf.sprintf "%s\n%s" cmds comment_cmd

  (* Rendering config as a VyOS/EdgeOS config file *)
  let make_indent indent level = String.make (level * indent) ' '

  let render_values indent_str name values =
    match values with
    | [] -> Printf.sprintf "%s%s { }\n" indent_str name
    | [v] -> Printf.sprintf "%s%s \"%s\"\n" indent_str name (String.escaped v)
    | _  -> 
      let rendered = List.map (fun s -> Printf.sprintf "%s%s \"%s\"" indent_str name (String.escaped s)) values in
      let rendered = String.concat "\n" rendered in
      Printf.sprintf "%s\n" rendered

  let render_comment indent c =
    match c with
    | None -> ""
    | Some c ->  Printf.sprintf "%s/* %s */\n" indent c

  let rec render_node indent level node =
    let indent_str = make_indent indent level in
    let name = Vytree.name_of_node node in
    let data = Vytree.data_of_node node in
    let is_tag = data.tag in 
    let comment = render_comment indent_str data.comment in
    let values = render_values indent_str name data.values in
    let children = Vytree.children_of_node node in
    match children with
    | [] -> Printf.sprintf "%s%s" comment values
    | _ :: _ ->
      if is_tag then 
        begin
          let inner = List.map (render_tag_node_child indent level name) children in
          String.concat "" inner
        end
      else
        begin
          let inner = List.map (render_node indent (level + 1)) children in
          let inner = String.concat "" inner in
          Printf.sprintf "%s%s%s {\n%s%s}\n" comment indent_str name inner indent_str
        end
  and render_tag_node_child indent level parent node =
    let indent_str = make_indent indent level in
    let name = Vytree.name_of_node node in
    let data = Vytree.data_of_node node in
    let comment = render_comment indent_str data.comment in
    let values = render_values indent_str name data.values in
    let children = Vytree.children_of_node node in
    match children with
    (* This produces too much whitespace due to indent_str from values,
       but the issue is cosmetic *)
    | [] -> Printf.sprintf "%s%s%s %s" comment indent_str parent values
    | _ ->
        (* Exploiting the fact that immediate children of tag nodes are
           never themselves tag nodes *)
        let inner = List.map (render_node indent (level + 1)) children in
        let inner = String.concat "" inner in
        Printf.sprintf "%s%s%s %s {\n%s%s}\n" comment indent_str parent name inner indent_str

  let render_config node =
    let children = Vytree.children_of_node node in
    let child_configs = List.map (render_node 4 0) children in
    String.concat "" child_configs

end (* Renderer *)

let render_commands node path =
    let node =
	match path with
        | [] -> node
        | _ -> Vytree.get node path
    in
    let children = Vytree.children_of_node node in
    let commands = List.map (Renderer.render_commands path) children in
    String.concat "\n" commands

let render_config = Renderer.render_config
