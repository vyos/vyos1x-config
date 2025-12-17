type completion_env = {
    name: string;
    path_typ: Reference_tree.path_type;
    values: string list;
    completion_help: Reference_tree.completion_help_type list;
    help: string;
    value_help: (string * string) list;
    multi: bool;
} [@@deriving yojson]

type completion_env_list = completion_env list [@@deriving yojson]

val get_completion_data : Reference_tree.t -> completion_env

val get_completion_env : Reference_tree.t -> Config_tree.t -> string -> string list -> (completion_env_list, string) result

val get_completion_env_str : ?legacy_format:bool -> Reference_tree.t -> Config_tree.t -> string -> string list -> (string, string) result

