module F = Filename

(*type value_constraint = Regex of string | External of string * string option*)
type value_constraint =
    | Regex of string [@name "regex"]
    | External of string * string option [@name "exec"]
    [@@deriving yojson]

exception Bad_validator of string

let validate_value dir buf value_constraint value =
    (* raises:
        [Bad_validator]
     *)
    match value_constraint with
    | Regex s ->
      (try
          let _ = Pcre2.exec ~pat:(Printf.sprintf "^%s$" s) value in true
       with Not_found -> false)
    | External (v, c) ->
      (* XXX: Unix.open_process_in is "shelling out", which is a bad idea on multiple levels,
         especially when the input comes directly from the user...
         We should do something about it.
       *)
        let validator = F.concat dir v in
        let cmd =
            match c with
            | Some arg ->
                let safe_arg = Printf.sprintf "%s" (Pcre2.qreplace ~pat:"\"" ~templ:"\\\"" arg) in
                Printf.sprintf "%s %s \'%s\' 2>&1" validator safe_arg value
            | None ->
                Printf.sprintf "%s \'%s\' 2>&1" validator value
        in
        let () = Unix.putenv "vyos_libexec_dir" "/usr/libexec/vyos" in
        let () = Unix.putenv "vyos_validators_dir" "/usr/libexec/vyos/validators" in
        let chan = Unix.open_process_in cmd in
        let out = try CCIO.read_all chan with _ -> "" in
        let result = Unix.close_process_in chan in
        match result with
        | Unix.WEXITED 0 -> true
        | Unix.WEXITED 127 ->
            raise (Bad_validator (Printf.sprintf "Could not execute validator %s" validator))
        | _ ->
            let () = Buffer.add_string buf out in
            false

(* If no constraints given, consider it valid.
   Otherwise consider it valid if it satisfies at least one constraint *)
let validate_any validators constraints value =
    (* raises no error; catches [Bad_validator] from validate_value
     *)
    let buf = Buffer.create 4096 in
    let validate_exists validators constraints value =
        match constraints with
        | [] -> true
        | _ ->
            try
                List.exists (fun c -> validate_value validators buf c value) constraints
            with Bad_validator e -> let () = Buffer.add_string buf e in false
    in
    match validate_exists validators constraints value with
    | true ->
        let () = Buffer.clear buf in
        None
    | false ->
        let out = Buffer.contents buf in
        let () = Buffer.clear buf in
        Some out

(* If no constraints given, consider it valid.
   Otherwise consider it valid if it satisfies all constraints *)
let validate_all validators constraints value =
    (* raises no error; catches [Bad_validator] from validate_value
     *)
    let buf = Buffer.create 4096 in
    let validate_forall validators constraints value =
        match constraints with
        | [] -> true
        | _ ->
            try
                List.for_all (fun c -> validate_value validators buf c value) constraints
            with Bad_validator e -> let () = Buffer.add_string buf e in false
    in
    match validate_forall validators constraints value with
    | true ->
        let () = Buffer.clear buf in
        None
    | false ->
        let out = Buffer.contents buf in
        let () = Buffer.clear buf in
        Some out
