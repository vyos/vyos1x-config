(*type value_constraint = Regex of string | External of string * string option*)

type value_constraint =
    | Regex of string [@name "regex"]
    | External of string * string option [@name "exec"]
    [@@deriving yojson]

exception Bad_validator of string

val validate_value : string -> Buffer.t -> value_constraint -> string -> bool
[@@alert exn "Value_checker.Bad_validator"]

val validate_any : string -> value_constraint list -> string -> string option

val validate_all : string -> value_constraint list -> string -> string option
