let default default_value opt =
  match opt with
  | None -> default_value
  | Some value -> value
