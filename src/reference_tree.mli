type node_type = [ `Leaf | `Tag | `Other ]

type path_type = [ node_type | `Tag_value | `Leaf_value | `Multi | `Invalid ]

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
} [@@deriving to_yojson]

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

val default_data : ref_node_data

val default : t

val load_from_xml : t -> string -> t
[@@alert exn "Reference_tree.Bad_interface_definition"]

val find_xml_child : string -> Xml_light_types.xml -> Xml_light_types.xml option

val validate_path : string -> t -> string list -> unit
[@@alert exn "Reference_tree.Validation_error"]

val validate_tree_filter : string -> t -> Config_tree.t -> Config_tree.t * string

val validate_tree : string -> t -> Config_tree.t -> string
[@@alert exn "Reference_tree.Validation_error"]

val split_path : t -> string list -> string list * string option

val is_multi : t -> string list -> bool
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val is_hidden : t -> string list -> bool
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val is_secret : t -> string list -> bool
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val is_tag : t -> string list -> bool
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val is_leaf : t -> string list -> bool
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val is_valueless : t -> string list -> bool
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val get_owner : t -> string list -> string option
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val get_priority : t -> string list -> string option
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val get_help_string : t -> string list -> string
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val get_value_help : t -> string list -> (string * string) list
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val get_completion_data : t -> string list -> (node_type * bool * string) list
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val refpath : t -> string list -> string list

val set_tag_data : t -> Config_tree.t -> string list -> Config_tree.t
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val set_leaf_data : t -> Config_tree.t -> string list -> Config_tree.t
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val reference_path_exists : t -> string list -> bool

val get_path_type : t -> string list -> path_type

val get_path_type_str : ?legacy_format:bool -> t -> string list -> string

val allowed_edit_level : t -> string list -> (unit, string) result

val get_ceil_data : (ref_node_data -> string option) -> t -> string list -> string option
[@@alert exn "Vytree.Empty_path"]
[@@alert exn "Vytree.Nonexistent_path"]

val render_json : t -> string
