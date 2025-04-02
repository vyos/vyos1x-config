type node_type =
    | Leaf
    | Tag
    | Other

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

val find_xml_child : string -> Xml_light_types.xml -> Xml_light_types.xml option

val validate_path : string -> t -> string list -> unit

val split_path : t -> string list -> string list * string option

val is_multi : t -> string list -> bool

val is_hidden : t -> string list -> bool

val is_secret : t -> string list -> bool

val is_tag : t -> string list -> bool

val is_leaf : t -> string list -> bool

val is_valueless : t -> string list -> bool

val get_owner : t -> string list -> string option

val get_priority : t -> string list -> string option

val get_help_string : t -> string list -> string

val get_value_help : t -> string list -> (string * string) list

val get_completion_data : t -> string list -> (node_type * bool * string) list

val refpath : t -> string list -> string list

val get_ceil_data : (ref_node_data -> string option) -> t -> string list -> string option

val set_tag_data : t -> Config_tree.t -> string list -> Config_tree.t

val set_leaf_data : t -> Config_tree.t -> string list -> Config_tree.t

val render_json : t -> string
