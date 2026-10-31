
let make_indent indent level = String.make (level * indent) ' '

module type C = sig
    val render_values : ?ord_val:bool -> string -> string -> string list -> string
    val render_comment : string -> string option -> string
end

module type R = sig
    val render_node : ?ord_val:bool -> int -> int -> Config_tree.t -> string
    val render_relative_node : ?ord_val:bool -> Config_tree.t -> string list -> int -> int -> string
    val render_config : ?ord_val: bool -> Config_tree.t -> string
end

module Make (X : C) : R = struct
    let rec render_node ?(ord_val=false) indent level node =
      let indent_str = make_indent indent level in
      let name = Vytree.name_of_node node in
      let (data: Config_tree.config_node_data) = Vytree.data_of_node node in
      let is_tag = data.tag in
      let comment = X.render_comment indent_str data.comment in
      let children = Vytree.children_of_node node in
      match children with
      | [] ->
        if data.leaf then
          let values = X.render_values ~ord_val:ord_val indent_str name data.values in
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
      let comment = X.render_comment indent_str data.comment in
      let children = Vytree.children_of_node node in
      let inner = List.map (render_node ~ord_val:ord_val indent (level + 1)) children in
      let inner = String.concat "" inner in
      Printf.sprintf "%s%s%s %s {\n%s%s}\n" comment indent_str parent name inner indent_str

    let render_relative_node ?(ord_val=false) root path indent level =
        if not (Util.is_empty path) && not ((Vytree.exists[@alert "-exn"]) root path) then ""
        else
        match path with
        | [] -> render_node ~ord_val:ord_val indent level root
        | [_] ->
            let node = (Vytree.get[@alert "-exn"]) root path in
            render_node ~ord_val:ord_val indent level node
        | _ ->
            let node = (Vytree.get[@alert "-exn"]) root path in
            if (Config_tree.is_tag_value[@alert "-exn"]) root path then
                let tag_node =
                    match Util.get_last_n path 1 with
                    | None -> (* not possible as len path > 1 *) "none"
                    | Some n -> n
                in
                render_tag_node_child ~ord_val:ord_val indent level tag_node node
            else
                render_node ~ord_val:ord_val indent level node

    let render_config ?(ord_val=false) node =
      let children = Vytree.children_of_node node in
      let child_configs = List.map (render_node ~ord_val:ord_val 4 0) children in
      String.concat "" child_configs
end

module Default : C = struct
    let render_values ?(ord_val=false) indent_str name values =
        match values with
        | [] -> Printf.sprintf "%s%s\n" indent_str name
        | [v] -> Printf.sprintf "%s%s \"%s\"\n" indent_str name (Util.escape_string v)
        | _  ->
        let values =
            if ord_val then List.sort Util.lexical_numeric_compare values
            else values
        in
        let rendered =
            List.map (fun s -> Printf.sprintf "%s%s \"%s\"" indent_str name (Util.escape_string s)) values
        in
        let rendered = String.concat "\n" rendered in
        Printf.sprintf "%s\n" rendered

    let render_comment indent_str c =
        match c with
        | None -> ""
        | Some c ->  Printf.sprintf "%s/* %s */\n" indent_str c
end

module Renderer = Make (Default)

let render_relative_node ?(ord_val=false) node path indent level =
    Renderer.render_relative_node ~ord_val:ord_val node path indent level

let render_config ?(ord_val=false) =
    Renderer.render_config ~ord_val:ord_val

let render_values ?(ord_val=false) indent_str name values =
    Default.render_values ~ord_val:ord_val indent_str name values

let render_comment indent_str c =
    Default.render_comment indent_str c

(* first-class module *)
let make func_values func_comment : (module R) =
    (module Make (struct let render_values = func_values let render_comment = func_comment end))
