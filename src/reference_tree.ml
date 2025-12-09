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
    (* raises:
        [Bad_interface_definition]
       alert exn Vytree.insert_or_update; Vytree.insert_maybe:
        [Vytree.Empty_path] not possible as all nodes have nodeNameAttr by schema
        [Not_found] not possible for position=Default
        [Vytree.Insert_error] not possible for recursive fold over children
     *)
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
                (Vytree.insert_or_update[@alert "-exn"]) reftree path data
            else
                (Vytree.insert_maybe[@alert "-exn"]) reftree path data
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
    (* raises:
        [Bad_interface_definition] from insert_from_xml and explicit
     *)
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
    try Some (Pcre2.get_substring (Pcre2.exec ~pat:"[\\s\\{\\}\\[\\]\"\'#]" name) 0)
    with Not_found -> None

let format_out l =
    let fl = List.filter (fun s -> (String.length s) > 0) l in
    Printf.sprintf "%s\n\n" (String.concat "\n" fl)


(** Take a list of string that represents a configuration path that may have
    node value at the end and validates it.

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
    (* raises:
        [Validation_error]
     *)
    let show_path p =
        Printf.sprintf "[%s]" @@ Util.string_of_list (List.rev p)
    in
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
                         let ret = format_out [show_path (p::acc); out; data.constraint_error_message]
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
            | [p] -> (List.rev (p :: acc), None)
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
    (* raises:
        [Vytree.Empty_path]
        [Vytree.Nonexistent_path]
       alert exn Vytree.get_data:
        [Vytree.Empty_path] allow raise
        [Vytree.Nonexistent_path] allow raise
     *)
    let data = (Vytree.get_data[@alert "-exn"]) reftree path in
    data.multi

let is_hidden reftree path =
    (* raises:
        [Vytree.Empty_path]
        [Vytree.Nonexistent_path]
       alert exn Vytree.get_data:
        [Vytree.Empty_path] allow raise
        [Vytree.Nonexistent_path] allow raise
     *)
    let data = (Vytree.get_data[@alert "-exn"]) reftree path in
    data.hidden

let is_secret reftree path =
    (* raises:
        [Vytree.Empty_path]
        [Vytree.Nonexistent_path]
       alert exn Vytree.get_data:
        [Vytree.Empty_path] allow raise
        [Vytree.Nonexistent_path] allow raise
     *)
    let data = (Vytree.get_data[@alert "-exn"]) reftree path in
    data.secret

let is_tag reftree path =
    (* raises:
        [Vytree.Empty_path]
        [Vytree.Nonexistent_path]
       alert exn Vytree.get_data:
        [Vytree.Empty_path] allow raise
        [Vytree.Nonexistent_path] allow raise
     *)
    let data = (Vytree.get_data[@alert "-exn"]) reftree path in
    match data.node_type with
    | Tag -> true
    | _ -> false

let is_leaf reftree path =
    (* raises:
        [Vytree.Empty_path]
        [Vytree.Nonexistent_path]
       alert exn Vytree.get_data:
        [Vytree.Empty_path] allow raise
        [Vytree.Nonexistent_path] allow raise
     *)
    let data = (Vytree.get_data[@alert "-exn"]) reftree path in
    match data.node_type with
    | Leaf -> true
    | _ -> false

let is_valueless reftree path =
    (* raises:
        [Vytree.Empty_path]
        [Vytree.Nonexistent_path]
       alert exn Vytree.get_data:
        [Vytree.Empty_path] allow raise
        [Vytree.Nonexistent_path] allow raise
     *)
    let data = (Vytree.get_data[@alert "-exn"]) reftree path in
    data.valueless

let get_owner reftree path =
    (* raises:
        [Vytree.Empty_path]
        [Vytree.Nonexistent_path]
       alert exn Vytree.get_data:
        [Vytree.Empty_path] allow raise
        [Vytree.Nonexistent_path] allow raise
     *)
    let data = (Vytree.get_data[@alert "-exn"]) reftree path in
    data.owner

let get_priority reftree path =
    (* raises:
        [Vytree.Empty_path]
        [Vytree.Nonexistent_path]
       alert exn Vytree.get_data:
        [Vytree.Empty_path] allow raise
        [Vytree.Nonexistent_path] allow raise
     *)
    let data = (Vytree.get_data[@alert "-exn"]) reftree path in
    data.priority

let get_help_string reftree path =
    (* raises:
        [Vytree.Empty_path]
        [Vytree.Nonexistent_path]
       alert exn Vytree.get_data:
        [Vytree.Empty_path] allow raise
        [Vytree.Nonexistent_path] allow raise
     *)
    let data = (Vytree.get_data[@alert "-exn"]) reftree path in
    data.help

let get_value_help reftree path =
    (* raises:
        [Vytree.Empty_path]
        [Vytree.Nonexistent_path]
       alert exn Vytree.get_data:
        [Vytree.Empty_path] allow raise
        [Vytree.Nonexistent_path] allow raise
     *)
    let data = (Vytree.get_data[@alert "-exn"]) reftree path in
    data.value_help

let get_completion_data reftree path =
    (* raises:
        [Vytree.Empty_path]
        [Vytree.Nonexistent_path]
       alert exn Vytree.get:
        [Vytree.Empty_path] allow raise
        [Vytree.Nonexistent_path] allow raise
     *)
    let aux node =
        let data = Vytree.data_of_node node in
        (data.node_type, data.multi, data.help)
    in
    List.map aux (Vytree.children_of_node @@ (Vytree.get[@alert "-exn"]) reftree path)

(* Convert from config path to reference tree path *)
let refpath reftree path =
    (* raises:
        [Vytree.Nonexistent_path] from is_tag
     *)
    let rec aux acc p =
    match acc, p with
    | [], h :: tl -> aux (acc @ [h]) tl
    | _, [h] -> if is_tag reftree acc then acc else acc @ [h]
    | _, h :: h' :: tl -> if is_tag reftree acc then aux (acc @ [h']) tl
                          else aux (acc @ [h]) ([h'] @ tl)
    | _, [] -> acc
    in aux [] path

let set_tag_data rtree ctree path =
    (* raises:
        [Vytree.Empty_path],
        [Vytree.Nonexistent_path] from refpath; is_tag; and
       alert exn Vytree.exists:
        [Vytree.Empty_path] allow raise
       alert exn Config_tree.is_tag_value; Config_tree.set_tag:
        [Vytree.Empty_path] allow raise
        [Vytree.Nonexistent_path] allow raise
     *)
    let ext = (Vytree.exists[@alert "-exn"]) ctree path in
    match ext with
    | false -> ctree
    | true ->
        let set_tag rt ct p =
            let refp = refpath rt p in
            if is_tag rt refp && not ((Config_tree.is_tag_value[@alert "-exn"]) ct p)
            then (Config_tree.set_tag[@alert "-exn"]) ct p true
            else ct
        in
        List.fold_left (set_tag rtree) ctree (Util.flag path)

let set_leaf_data rtree ctree path =
    (* raises:
        [Vytree.Empty_path],
        [Vytree.Nonexistent_path] from refpath; is_leaf; and
       alert exn Vytree.exists:
        [Vytree.Empty_path] allow raise
       alert exn Config_tree.set_leaf:
        [Vytree.Empty_path] allow raise
        [Vytree.Nonexistent_path] allow raise
     *)
    let ext = (Vytree.exists[@alert "-exn"]) ctree path in
    match ext with
    | false -> ctree
    | true ->
        let refp = refpath rtree path in
        if is_leaf rtree refp then (Config_tree.set_leaf[@alert "-exn"]) ctree path true
        else ctree

let potential_tag_value rtree cpath =
    (* check given path against reftree for potential to be tag value
     *)
    (* raises:
        [Vytree.Nonexistent_path] from refpath; is_tag
     *)
    match cpath with
    | [] | [_] -> false
    | _ ->
    let refp = refpath rtree cpath in
    let ref_drop_last = refpath rtree (Util.drop_last cpath) in
    match ref_drop_last with
    | [] -> false
    | _ as c when c = refp -> is_tag rtree refp
    | _ -> false

let potential_leaf_value rtree cpath =
    (* check given path against reftree for potential to be leaf value
     *)
    (* raises:
        [Vytree.Nonexistent_path] from refpath; is_leaf
     *)
    match cpath with
    | [] | [_] -> false
    | _ ->
    let ref_drop_last = refpath rtree (Util.drop_last cpath) in
    match ref_drop_last with
    | [] -> false
    | _ -> is_leaf rtree ref_drop_last


(* The 'edit' command can descend along a not-as-yet configured path,
   assuming that it is
   (1) a valid path of the reference tree
   (2) neither a tag nor leaf node
   To confirm (2) in the case of a tag node, one has to allow for a
   'potential' tag value as final element of the path.
 *)
let allowed_edit_level rtree path =
    try
        let refp = refpath rtree path
        in
        if Util.is_empty refp then
            Error {|The "edit" command cannot be issued at an empty path|}
        else
        if is_tag rtree refp && not (potential_tag_value rtree path)
        then
            Error {|The "edit" command cannot be issued at the level of tag node|}
        else
        if potential_leaf_value rtree path
        then
            Error {|The "edit" command cannot be issued at the level of leaf value|}
        else
        if is_leaf rtree refp
        then
            Error {|The "edit" command cannot be issued at the level of leaf node|}
        else Ok ()
    with Vytree.Nonexistent_path ->
        Error {|The "edit" command cannot be issued at a non-existent path of the reference tree|}

let get_ceil_data f reftree path =
    (* raises:
        [Vytree.Empty_path]
        [Vytree.Nonexistent_path]
       alert exn Vytree.get_data:
        [Vytree.Empty_path] allow raise
        [Vytree.Nonexistent_path] allow raise
     *)
    let data_of_path d p =
        let data = (Vytree.get_data[@alert "-exn"]) reftree p in
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

(** Add alternative validation function to be called on an existing tree.
    This allows folding over tree for full validation.
    Numbered comments list constraints as described above validate_path.
 *)
let validate_tree_at_path validators_dir rt ct path value =
    (* raises:
        [Validation_error]
       alert exn Vytree.exists:
        [Vytree.Empty_path] ruled out in branch
       alert exn Vytree.get:
        [Vytree.Empty_path] ruled out in branch
        [Vytree.Nonexistent_path] catch and raise Validation_error
       alert exn Config_tree.is_tag;
                 Config_tree.is_tag_value;
                 Vytree.get_data;
                 Config_tree.is_leaf:
        [Vytree.Empty_path] ruled out
        [Vytree.Nonexistent_path] ruled out
     *)
    if Util.is_empty path then ()
    else
    let show_path p =
        Printf.sprintf "[%s]" @@ Util.string_of_list p
    in
    let refp = refpath rt path in
    (* 6. It's a node that is neither leaf nor tag value with a name that
          doesn't exist in the reference tree
     *)
    if not ((Vytree.exists[@alert "-exn"]) rt refp) then
        let msg = Printf.sprintf "Path %s is not in reference tree\n" (show_path path)
        in raise (Validation_error msg)
    else
    let node =
        try
            (Vytree.get[@alert "-exn"]) ct path
        with Vytree.Nonexistent_path ->
            let msg = Printf.sprintf "Path %s is not in config file\n" (show_path path)
            in raise (Validation_error msg)
    in
    let children = Vytree.children_of_node node in
    let childless = Util.is_empty children in
    let ct_data = Vytree.data_of_node node in
    let values = ct_data.Config_tree.values in
    let values_empty = Util.is_empty values in
    if (Config_tree.is_tag[@alert "-exn"]) ct path then
        (* 1. It's a tag node without a child *)
        if childless then
            let msg =
                Printf.sprintf "Configuration path %s requires a tag value\n" (show_path path)
            in raise (Validation_error msg)
        else ()
    else
    if ((Config_tree.is_tag_value[@alert "-exn"]) ct path) then
        let rt_data = (Vytree.get_data[@alert "-exn"]) rt (refpath rt (Util.drop_last path)) in
        let tag_value =
            match (Util.get_last path) with
            | Some v -> v
            | None -> raise (Validation_error "Internal error\n")
        in
        (* 2. It's a tag node with an invalid tag value *)
        let res =
            try
                Value_checker.validate_any validators_dir rt_data.constraints tag_value
            with Value_checker.Bad_validator msg -> raise (Validation_error msg)
        in
        match res with
        | None -> ()
        | Some out ->
            let ret = format_out [show_path path; out; rt_data.constraint_error_message]
            in raise (Validation_error ret)
    else
    if (Config_tree.is_leaf[@alert "-exn"]) ct path then
        let rt_data = (Vytree.get_data[@alert "-exn"]) rt (refpath rt path) in
        (* 4. It's a valueless leaf node with a value *)
        if is_valueless rt refp then
            if not values_empty then
                let msg =
                    Printf.sprintf "Path at valueless leaf %s has values\n" (show_path path)
                in raise (Validation_error msg)
            else ()
        else
        (* 3. It's a non-valueless leaf node without a value *)
        if values_empty then
            let msg =
                Printf.sprintf "Configuration path %s requires a value\n" (show_path path)
            in raise (Validation_error msg)
        else
        (* It's a non-multi node with multiple values *)
        if not (is_multi rt refp) then
        match values with
        | [_] -> ()
        | _ ->
            let msg =
                Printf.sprintf "Multiple values for non-multi node %s\n" (show_path path)
            in raise (Validation_error msg)
        else
        match value with
        | None -> ()
        | Some v ->
            (* 5. It's a non-valueless leaf node with an invalid value *)
            let res =
                try
                    Value_checker.validate_any validators_dir rt_data.constraints v
                with Value_checker.Bad_validator msg -> raise (Validation_error msg)
            in
            match res with
            | None -> ()
            | Some out ->
                let ret = format_out [show_path (path @ [v]); out; rt_data.constraint_error_message]
                in raise (Validation_error ret)
    else ()

let validate_tree_filter dir rt ct =
    (* validate and filter invalid paths *)
    (* catches:
        [Validation_error] from validate_tree_at_path
       alert exn Config_tree.delete; Config_tree.prune_delete:
        [Vytree.Empty_path] not possible as validate_tree_at_path ignores
        [Vytree.Nonexistent_path] not possible as extant in tree
       alert exn Vytree.exists:
        [Vytree.Empty_path] not possible as non-empty in branch
       alert exn Vytree.get_data:
        [Vytree.Empty_path] not possible as non-empty in branch
        [Vytree.Nonexistent_path] not possible in fold_tree_with_path
     *)
    let try_validate (p, (ctree, out)) value =
        let q = List.rev p in
        try
            validate_tree_at_path dir rt ctree q value;
            (p, (ctree, out))
        with Validation_error x ->
            let ct' =
                (Config_tree.delete[@alert "-exn"]) ctree q value |>
                (fun c -> (Config_tree.prune_delete[@alert "-exn"]) c q)
            in
            (p, (ct', out ^ x))
    in
    let validate_path_filter (p, (ctree, out)) _node =
        if Util.is_empty p then
            (p, (ctree, out))
        else
        let q = List.rev p in
        (* the path may have been removed in previous iteration *)
    if not ((Vytree.exists[@alert "-exn"]) ctree q) then
            (p, (ctree, out))
        else
            let data = (Vytree.get_data[@alert "-exn"]) ct q in
        let values  = data.Config_tree.values in
        match values with
        | [] ->
            try_validate (p, (ctree, out)) None
        | _ as l ->
            let l' = List.map Option.some l in
            List.fold_left try_validate (p, (ctree, out)) l'
    in
    let tree, out =
        snd (Vytree.fold_tree_with_path validate_path_filter ([], (ct, "")) ct)
    in
    tree, out

let validate_tree dir rt ct =
    (* raises:
        [Validation_error] from validate_tree_at_path
     *)
    let _, out = validate_tree_filter dir rt ct in
    out


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
