(*
 * A functor for 'internal' representation of Config_tree and Reference_tree.
 * This is useful for avoiding re-parsing *_trees on cache and load.
 *)

exception Read_error of string
exception Write_error of string

module type T =
    sig
        type t
        val to_yojson : t -> Yojson.Safe.t
        val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
        val default : t
    end

module type FI = functor (M: T) ->
    sig
        val write_internal : M.t -> string -> unit
        val write_internal_atomic : M.t -> string -> unit
        val read_internal : string -> M.t
        val replace_internal : string -> string -> unit
    end

module Make : FI = functor (M: T) -> struct
    let write_internal x file_name =
        let yt = M.to_yojson x in
        let ys = Yojson.Safe.to_string yt in
        let fd = Unix.openfile file_name [Unix.O_CREAT;Unix.O_WRONLY] 0o664 in
        let oc = Unix.out_channel_of_descr fd in
        let () = Unix.ftruncate fd 0 in
        let () = Printf.fprintf oc "%s" ys in
        let () = Unix.fsync fd in
        let () = Unix.chmod file_name 0o664 in
        close_out_noerr oc

    let read_internal file_name =
        let fd =
            try
                Unix.openfile file_name [Unix.O_RDONLY] 0o664
            with Unix.Unix_error (e,f,p) ->
                let out =
                    Printf.sprintf "%s %s: %s" (Unix.error_message e) f p
                in raise (Read_error out)
        in
        let ic = Unix.in_channel_of_descr fd in
        let ys = really_input_string ic (in_channel_length ic) in
        let yt = Yojson.Safe.from_string ys in
        let ct_res = M.of_yojson yt in
        let ct = Result.value ct_res ~default:M.default in
        close_in_noerr ic; ct

    let write_internal_atomic x file_name =
        let file_name_tmp = file_name ^ ".tmp" in
        let yt = M.to_yojson x in
        let ys = Yojson.Safe.to_string yt in
        let fd =
            try
                Unix.openfile file_name_tmp [Unix.O_CREAT;Unix.O_WRONLY] 0o664
            with Unix.Unix_error (e,f,p) ->
                let out =
                    Printf.sprintf "%s %s: %s" (Unix.error_message e) f p
                in raise (Write_error out)
        in
        let oc = Unix.out_channel_of_descr fd in
        let () = Unix.ftruncate fd 0 in
        let () = Printf.fprintf oc "%s" ys in
        let () = Unix.fsync fd in
        let () = Unix.chmod file_name_tmp 0o664 in
        let () = Unix.rename file_name_tmp file_name in
        close_out_noerr oc

    let replace_internal dst src =
        let tmp = src ^ ".tmp" in
        try
            let () = FileUtil.cp ~force:Force [src] tmp in
            let () = FileUtil.rm ~force:Force [dst] in
            FileUtil.mv ~force:Force tmp dst
        with _ -> raise (Write_error "replace error")
end
