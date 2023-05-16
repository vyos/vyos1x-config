type node_type =
    | Leaf
    | Tag
    | Other

type value_constraint =
    | Regex of string [@name "regex"]
    | External of string * string option [@name "exec"]
    [@@deriving yojson]

type completion_help_type =
    | List of string [@name "list"]
    | Path of string [@name "path"]
    | Script of string [@name "script"]
    [@@deriving to_yojson]

type ref_node_data = {
    node_type: node_type;
    constraints: value_constraint list;
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
} [@@deriving to_yojson]

type t = ref_node_data Vytree.t [@@deriving to_yojson]

exception Bad_interface_definition of string

exception Validation_error of string

val default_data : ref_node_data

val default : t

val load_from_xml : t -> string -> t

val is_multi : t -> string list -> bool

val is_hidden : t -> string list -> bool

val is_secret : t -> string list -> bool

val is_tag : t -> string list -> bool

val is_leaf : t -> string list -> bool

val is_valueless : t -> string list -> bool

val get_owner : t -> string list -> string option

val get_help_string : t -> string list -> string

val get_value_help : t -> string list -> (string * string) list

val get_completion_data : t -> string list -> (node_type * bool * string) list

val render_json : t -> string
