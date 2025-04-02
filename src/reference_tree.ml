type node_type =
    | Leaf
    | Tag
    | Other

let node_type_to_yojson = function
    | Leaf -> `String "leaf"
    | Tag -> `String "tag"
    | Other -> `String "other"

let node_type_of_yojson = function
    | `String "leaf" -> Ok Leaf
    | `String "tag" -> Ok Tag
    | `String "other" -> Ok Other
    | json -> Error (Yojson.Safe.to_string json)

type completion_help_type =
    | List of string [@name "list"]
    | Path of string [@name "path"]
    | Script of string [@name "script"]
    [@@deriving yojson]

type doc_hints = {
    text: string;
    hint_type: string;
} [@@deriving yojson]

type docs = {
    headline: string;
    text: string;
    usageExample: string;
    hints: doc_hints list;
} [@@deriving yojson]

type ref_node_data = {
    node_type: node_type;
    constraints: Value_checker.value_constraint list;
    constraint_group: Value_checker.value_constraint list;
    constraint_error_message: string;
    completion_help: completion_help_type list;
    help: string;
    value_help: (string * string) list;
    multi: bool;
    valueless: bool;
    owner: string option;
    priority: string option;
    default_value: string option;
    hidden: bool;
    secret: bool;
    docs: docs;
} [@@deriving yojson]

type t = ref_node_data Vytree.t [@@deriving yojson]

exception Bad_interface_definition of string

exception Validation_error of string

let default_data = {
    node_type = Other;
    constraints = [];
    constraint_group = [];
    constraint_error_message = "Invalid value";
    completion_help = [];
    help = "No help available";
    value_help = [];
    multi = false;
    valueless = false;
    owner = None;
    priority = None;
    default_value = None;
    hidden = false;
    secret = false;
    docs = {
        headline = "";
        text = "";
        usageExample = "";
        hints = [];
    };
}

let default = Vytree.make default_data ""

(* Loading from XML *)

let node_type_of_string s =
    match s with
    | "node" -> Other
    | "tagNode" -> Tag
    | "leafNode" -> Leaf
    | _ -> raise (Bad_interface_definition
                  (Printf.sprintf "node, tagNode, or leafNode expected, %s found" s))

let completion_help_type_of_string v s =
    match v with
    | "list" -> List s
    | "path" -> Path s
    | "script" -> Script s
    | _ -> raise (Bad_interface_definition
                  (Printf.sprintf "list, path, or script expected, %s found" s))

(** Find a child node in xml-light *)
let find_xml_child name xml =
    let find_aux e =
        match e with
        | Xml.Element (name', _, _) when name' = name -> true
        | _ -> false
    in
    match xml with
    | Xml.Element (_, _, children) -> Vylist.find find_aux children
    | Xml.PCData _ -> None

(* handle possible empty elements *)
let try_pcdata x =
    match x with
    | [] -> ""
    | _ ->
    try
        Xml.pcdata (List.hd x)
    with Xml.Not_pcdata _ -> ""

let get_pcdata_child name xml =
    let c = find_xml_child name xml in
    match c with
    | Some Xml.Element(_, _, x_data) -> try_pcdata x_data
    | _ -> raise (Bad_interface_definition (Printf.sprintf "No child named %s" name))

let load_value_help_from_xml d x =
    let fmt = get_pcdata_child "format" x in
    let descr = get_pcdata_child "description" x in
    let vhs = d.value_help in
    let vhs' = (fmt, descr) :: vhs in
    {d with value_help=vhs'}

let load_completion_help_from_xml d c =
    let res =
    let aux l c =
        match c with
        | Xml.Element (_, _, [Xml.PCData s]) ->
                l @ [completion_help_type_of_string (Xml.tag c) s]
        | _ -> raise (Bad_interface_definition ("Malformed completion help :" ^ Xml.to_string c))
    in Xml.fold aux [] c in
    let l = d.completion_help in
    let l' = l @ res in
    {d with completion_help=l'}

let load_constraint_from_xml d c =
    let aux d c =
        match c with
        | Xml.Element ("regex", _, [Xml.PCData s]) ->
            let cs = (Value_checker.Regex s) :: d.constraints in
            {d with constraints=cs}
        | Xml.Element ("validator", [("name", n); ("argument", a)], _) ->
            let cs = (Value_checker.External (n, Some a)) :: d.constraints in
            {d with constraints=cs}
        | Xml.Element ("validator", [("name", n)], _) ->
            let cs = (Value_checker.External (n, None)) :: d.constraints in
            {d with constraints=cs}
        | _ -> raise (Bad_interface_definition ("Malformed constraint: " ^ Xml.to_string c))
    in Xml.fold aux d c

let load_constraint_group_from_xml d c =
    let aux d c =
        match c with
        | Xml.Element ("regex", _, [Xml.PCData s]) ->
            let cs = (Value_checker.Regex s) :: d.constraint_group in
            {d with constraint_group=cs}
        | Xml.Element ("validator", [("name", n); ("argument", a)], _) ->
            let cs = (Value_checker.External (n, Some a)) :: d.constraint_group in
            {d with constraint_group=cs}
        | Xml.Element ("validator", [("name", n)], _) ->
            let cs = (Value_checker.External (n, None)) :: d.constraint_group in
            {d with constraint_group=cs}
        | _ -> raise (Bad_interface_definition ("Malformed constraint: " ^ Xml.to_string c))
    in Xml.fold aux d c

let load_docs_hints d c =
    let aux d c =
        match c with
        | Xml.Element ("hints", attrs, [Xml.PCData s]) ->
            let hint_type = List.assoc "type" attrs in
            let hint = { text = s; hint_type = hint_type } in
            let new_docs = { d.docs with hints = hint :: d.docs.hints } in
            { d with docs = new_docs }
        | _ -> raise (Bad_interface_definition ("Malformed hint: " ^ Xml.to_string c))
    in aux d c

let load_docs_from_xml d x =
    let aux d x =
        match x with
        | Xml.Element ("headline", _, [Xml.PCData s]) ->
            let new_docs = {d.docs with headline = s} in
            {d with docs = new_docs}
        | Xml.Element ("text", _, [Xml.PCData s]) ->
            let new_docs = {d.docs with text = s} in
            {d with docs = new_docs}
        | Xml.Element ("hints", _, _) ->
            load_docs_hints d x
        | Xml.Element ("usageExample", _, [Xml.PCData s]) ->
            let new_docs = {d.docs with usageExample = s} in
            {d with docs = new_docs}
        | _ -> d  (* Ignore unknown elements instead of raising an error *)
    in Xml.fold aux d x

let data_from_xml d x =
    let aux d x =
        match x with
        | Xml.Element ("help", _, [Xml.PCData s]) -> {d with help=s}
        | Xml.Element ("valueHelp", _, _) -> load_value_help_from_xml d x
        | Xml.Element ("completionHelp", _, _) ->
            load_completion_help_from_xml d x
        | Xml.Element ("multi", _, _) -> {d with multi=true}
        | Xml.Element ("valueless", _, _) -> {d with valueless=true}
        | Xml.Element ("constraintErrorMessage", _, [Xml.PCData s]) ->
            {d with constraint_error_message=s}
        | Xml.Element ("constraint", _, _) -> load_constraint_from_xml d x
        | Xml.Element ("constraintGroup", _, _) -> load_constraint_group_from_xml d x
        | Xml.Element ("priority", _, [Xml.PCData i]) ->
            {d with priority=Some i}
        | Xml.Element ("hidden", _, _) -> {d with hidden=true}
        | Xml.Element ("secret", _, _) -> {d with secret=true}
        | Xml.Element ("docs", _, _) -> load_docs_from_xml d x
        | _ -> raise (Bad_interface_definition ("Malformed property tag: " ^ Xml.to_string x))
    in Xml.fold aux d x

let rec insert_from_xml basepath reftree xml =
    match xml with
    | Xml.Element ("syntaxVersion", _, _) -> reftree
    | Xml.Element (_, _,  _) ->
        let props = find_xml_child "properties" xml in
        let data =
            (match props with
            | None -> default_data
            | Some p -> data_from_xml default_data p)
        in
        let node_type = node_type_of_string (Xml.tag xml) in
        let node_owner = try let o = Xml.attrib xml "owner" in Some o
                         with _ -> None
        in
        let default_value_elem = find_xml_child "defaultValue" xml in
        let default_value =
            (match default_value_elem with
            | Some (Xml.Element (_, _, [Xml.PCData s])) -> Some s
            | _ -> None)
        in
        let data = {data with node_type=node_type; owner=node_owner; default_value=default_value} in
        let name = Xml.attrib xml "name" in
        let path = basepath @ [name] in
        let new_tree =
            if data <> default_data then
                Vytree.insert_or_update reftree path data
            else
                Vytree.insert_maybe reftree path data
        in
        (match node_type with
        | Leaf -> new_tree
        | _ ->
            let children = find_xml_child "children" xml in
            (match children with
             | None -> raise (Bad_interface_definition (Printf.sprintf "Node %s has no children" name))
             | Some c ->  List.fold_left (insert_from_xml path) new_tree (Xml.children c)))
    | _ -> raise (Bad_interface_definition "PCData not allowed here")

let load_from_xml reftree file =
    let xml_to_reftree xml reftree =
        match xml with
        | Xml.Element ("interfaceDefinition", _, children) ->
            List.fold_left (insert_from_xml []) reftree children
        | _ -> raise (Bad_interface_definition "File should begin with <interfaceDefinition>")
    in
    try
        let xml = Xml.parse_file file in
        xml_to_reftree xml reftree
    with
    | Xml_light_errors.File_not_found msg -> raise (Bad_interface_definition msg)
    | Xml_light_errors.Xml_error err ->
        let (msg, pos) = err in
        let s = Printf.sprintf ": line %d in file %s" pos.eline file in
        raise (Bad_interface_definition ((Xml.error_msg msg)^s))

(* Validation function *)

let has_illegal_characters name =
    (** Checks if string name has illegal characters in it.
        All whitespace, curly braces, square brackets, and quotes
        are disallowed due to their special significance to the curly config
        format parser *)
    try Some (Pcre.get_substring (Pcre.exec ~pat:"[\\s\\{\\}\\[\\]\"\'#]" name) 0)
    with Not_found -> None

let format_out l =
    let fl = List.filter (fun s -> (String.length s) > 0) l in
    String.concat "\n\n" fl

(** Takes a list of string that represents a configuration path that may have
    node value at the end, validates it, and splits it into path and value parts.

   A list of strings is a valid path that can be created in the config tree unless:
     1. It's a tag node without a child
     2. It's a tag node with an invalid tag value
     3. It's a non-valueless leaf node without a value
     4. It's a valueless leaf node with a value
     5. It's a non-valueless leaf node with an invalid value
     6. It's a node that is neither leaf nor tag value with a name that
        doesn't exist in the reference tree
 *)
let validate_path validators_dir node path =
    let show_path p = Printf.sprintf "[%s]" @@ Util.string_of_list (List.rev p) in
    let rec aux node path acc =
        let data = Vytree.data_of_node node in
        match data.node_type with
        | Leaf ->
            begin
            match path with
            | [] ->
                if data.valueless then ()
                else
                let msg =
                    Printf.sprintf "Configuration path %s requires a value" (show_path acc)
                in raise (Validation_error msg)
            | [p] ->
                 if not data.valueless then
                     let res =
                         try Value_checker.validate_any validators_dir data.constraints p
                         with Value_checker.Bad_validator msg -> raise (Validation_error msg)
                     in
                     match res with
                     | None -> ()
                     | Some out ->
                        let ret = format_out [out; data.constraint_error_message]
                        in raise (Validation_error ret)
                 else
                     let msg = Printf.sprintf "Node %s cannot have a value" (show_path acc)
                     in raise (Validation_error msg)
            | _ ->
                let msg = Printf.sprintf "Path %s is too long" (show_path acc)
                in raise (Validation_error msg)
            end
        | Tag ->
            begin
            match path with
            | p :: p' :: ps ->
                begin
                match (has_illegal_characters p) with
                | Some c ->
                    let msg =
                        Printf.sprintf "Illegal character \"%s\" in node name \"%s\"" c p
                    in raise (Validation_error msg)
                | None ->
                    let res =
                        try Value_checker.validate_any validators_dir data.constraints p
                        with Value_checker.Bad_validator msg -> raise (Validation_error msg)
                    in
                    begin
                    match res with
                    | None ->
                        let child = Vytree.find node p' in
                        begin
                        match child with
                        | Some c -> aux c ps (p' :: p :: acc)
                        | None ->
                            let msg =
                                Printf.sprintf "Node %s has no child %s" (show_path acc) p'
                            in raise (Validation_error msg)
                        end
                    | Some out ->
                        let msg =
                            Printf.sprintf "%s is not a valid child name for node %s" p (show_path acc)
                        in
                        let ret = format_out [out; data.constraint_error_message; msg]
                        in raise (Validation_error ret)
                    end
                end
            | [p] ->
                begin
                match (has_illegal_characters p) with
                | Some c ->
                    let msg =
                        Printf.sprintf "Illegal character \"%s\" in node name \"%s\"" c p
                    in raise (Validation_error msg)
                | None ->
                    let res =
                        try Value_checker.validate_any validators_dir data.constraints p
                        with Value_checker.Bad_validator msg -> raise (Validation_error msg)
                    in
                    begin
                    match res with
                    | None -> ()
                    | Some out ->
                        let msg =
                            Printf.sprintf "%s is not a valid child name for node %s" p (show_path acc)
                        in
                        let ret = format_out [out; data.constraint_error_message; msg]
                        in raise (Validation_error ret)
                    end
                end
            | _ ->
                let msg =
                    Printf.sprintf "Configuration path %s requires a value" (show_path acc)
                in raise (Validation_error msg)
            end
        | Other ->
            begin
            match path with
            | [] -> ()
            | p :: ps ->
                let child = Vytree.find node p in
                match child with
                | Some c -> aux c ps (p :: acc)
                | None ->
                    let msg = Printf.sprintf "Path %s is incomplete" (show_path acc)
                    in raise (Validation_error msg)
            end
    in aux node path []

(* This is only to be used after the path has been validated *)
let split_path node path =
    let rec aux node path acc =
        let data = Vytree.data_of_node node in
        match data.node_type with
        | Leaf ->
            begin
            match path with
            | [] -> (List.rev acc, None)
            | [p] -> (List.rev acc, Some p)
            | _ -> (List.rev acc, None)
            end
        | Tag ->
            begin
            match path with
            | p :: p' :: ps ->
                (let child = Vytree.find node p' in
                match child with
                | Some c -> aux c ps (p' :: p :: acc)
                | None -> (List.rev acc, None))
            | [_] -> (List.rev acc, None)
            | _ -> (List.rev acc, None)
            end
        | Other ->
            begin
            match path with
            | [] -> (List.rev acc, None)
            | p :: ps ->
                let child = Vytree.find node p in
                match child with
                | Some c -> aux c ps (p :: acc)
                | None -> (List.rev acc, None)
            end
    in aux node path []

let is_multi reftree path =
    let data = Vytree.get_data reftree path in
    data.multi

let is_hidden reftree path =
    let data = Vytree.get_data reftree path in
    data.hidden

let is_secret reftree path =
    let data = Vytree.get_data reftree path in
    data.secret

let is_tag reftree path =
    let data = Vytree.get_data reftree path in
    match data.node_type with
    | Tag -> true
    | _ -> false

let is_leaf reftree path =
    let data = Vytree.get_data reftree path in
    match data.node_type with
    | Leaf -> true
    | _ -> false

let is_valueless reftree path =
    let data = Vytree.get_data reftree path in
    data.valueless

let get_owner reftree path =
    let data = Vytree.get_data reftree path in
    data.owner

let get_priority reftree path =
    let data = Vytree.get_data reftree path in
    data.priority

let get_help_string reftree path =
    let data = Vytree.get_data reftree path in
    data.help

let get_value_help reftree path =
    let data = Vytree.get_data reftree path in
    data.value_help

let get_completion_data reftree path =
    let aux node =
        let data = Vytree.data_of_node node in
        (data.node_type, data.multi, data.help)
    in List.map aux (Vytree.children_of_node @@ Vytree.get reftree path)

(* Convert from config path to reference tree path *)
let refpath reftree path =
    let rec aux acc p =
    match acc, p with
    | [], h :: tl -> aux (acc @ [h]) tl
    | _, [h] -> if is_tag reftree acc then acc else acc @ [h]
    | _, h :: h' :: tl -> if is_tag reftree acc then aux (acc @ [h']) tl
                          else aux (acc @ [h]) ([h'] @ tl)
    | _, [] -> acc
    in aux [] path

let flag path =
    let len = List.length path in
    let aux p i =
        Util.drop_last_n p (len - i - 1)
    in
    List.mapi (fun k _ -> aux path k) path

let set_tag_data rtree ctree path =
    let ext = Vytree.exists ctree path in
    match ext with
    | false -> ctree
    | true ->
        let set_tag rt ct p =
            let refp = refpath rt p in
            if is_tag rt refp && not (Config_tree.is_tag_value ct p)
            then Config_tree.set_tag ct p true
            else ct
        in
        List.fold_left (set_tag rtree) ctree (flag path)

let set_leaf_data rtree ctree path =
    let ext = Vytree.exists ctree path in
    match ext with
    | false -> ctree
    | true ->
        let refp = refpath rtree path in
        if is_leaf rtree refp then Config_tree.set_leaf ctree path true
        else ctree

let get_ceil_data f reftree path =
    let data_of_path d p =
        let data = Vytree.get_data reftree p in
        match (f data) with
        | Some d' -> Some d'
        | None -> d
    in
    let rec aux d acc p =
        match acc, p with
        | _, h :: tl ->
                let acc' = acc @ [h] in
                aux (data_of_path d (refpath reftree acc')) acc' tl
        | _, [] -> d
    in aux None [] path

module JSONRenderer =
struct
    let render_data data =
        ref_node_data_to_yojson data |> Yojson.Safe.to_string

    let rec render_node node =
        let name = Vytree.name_of_node node in
        let children = Vytree.children_of_node node in
        let data = Vytree.data_of_node node in
        let data_str = render_data data in
        let children_strs = List.map render_node children in
        let children_str = String.concat "," children_strs in
        if children_str <> "" then
            Printf.sprintf "\"%s\": {\"node_data\": %s, %s}" name data_str children_str
        else
            Printf.sprintf "\"%s\": {\"node_data\": %s}" name data_str

    let render_json node =
        let data = Vytree.data_of_node node in
        let data_str = render_data data in
        let children = Vytree.children_of_node node in
        let child_configs = List.map render_node children in
        let child_config = String.concat "," child_configs in
        Printf.sprintf "{\"node_data\": %s, %s}" data_str child_config
end (* JSONRenderer *)

let render_json = JSONRenderer.render_json
