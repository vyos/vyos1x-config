
val load_config : string -> (Config_tree.t, string) result

val save_config : Config_tree.t -> string -> (unit, string) result
