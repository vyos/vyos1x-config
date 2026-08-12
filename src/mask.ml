open Diff

module Mask_inclusive = struct
    type t = { left: Config_tree.t;
               right: Config_tree.t;
             }

    let make_init l r = { left = l;
                          right = r;
                        }

    (* mask function; mask applied on right *)
    let diff_func ?descent:_ (path : string list) res (m : change) =
        (* alert exn Vytree.delete:
            [Vytree.Empty_path] not possible since Unchanged pattern is only empty path
            [Vytree.Nonexistent_path] not possible as called on existing config paths (res.left)
           alert exn Vytree.is_terminal_path:
            [Vytree.Empty_path] not possible since Unchanged pattern is only empty path
         *)
        match m with
        | Added -> res
        | Subtracted ->
            begin
                match path with
                | [_] ->
                    {res with left = (Vytree.delete[@alert "-exn"]) res.left path}
                |  _  ->
                    if not ((Vytree.is_terminal_path[@alert "-exn"]) res.right (Util.drop_last path)) then
                        {res with left = (Vytree.delete[@alert "-exn"]) res.left path}
                    else res
            end
        | Unchanged -> res
        | Updated _ -> res
end

module Mask_exclusive = struct
    type t = { left: Config_tree.t;
               right: Config_tree.t;
             }

    let make_init l r = { left = l;
                          right = r;
                        }

    (* mask function; mask applied on right *)
    let diff_func ?(descent=true) (path : string list) res (m : change) =
        (* alert exn Vytree.delete:
            [Vytree.Empty_path] not possible in pattern match case
            [Vytree.Nonexistent_path] not possible as called on existing config paths (res.left)
           alert exn Vytree.is_terminal_path:
            [Vytree.Empty_path] not possible in pattern match case
         *)
        match m with
        | Added -> res
        | Subtracted -> res
        | Unchanged | Updated _ ->
            begin
                match path with
                | [] ->
                    if descent then
                        (* in the diff function, descent = true on an empty path means that the
                           trees are equal, hence exclude all: return default (empty) tree *)
                        {res with left = Config_tree.default}
                    else res
                | _ ->
                    if descent || ((Vytree.is_terminal_path[@alert "-exn"]) res.right path) then
                        let tmp = (Vytree.delete[@alert "-exn"]) res.left path in
                        let left' = (Config_tree.prune_delete[@alert "-exn"]) tmp path in
                        {res with left = left'}
                    else res
            end
end

module MI = Diff(Mask_inclusive)

module ME = Diff(Mask_exclusive)

let mask_inclusive left right =
    (* raises:
        [Empty_comparison] from diff
        [Incommensurable]
     *)
    if (Vytree.name_of_node left) <> (Vytree.name_of_node right) then
        raise Incommensurable
    else
    let init = Mask_inclusive.make_init left right in
    let res = MI.diff init left right in
    res.left

let mask_exclusive left right =
    (* raises:
        [Empty_comparison] from diff
        [Incommensurable]
     *)
    if (Vytree.name_of_node left) <> (Vytree.name_of_node right) then
        raise Incommensurable
    else
    let init = Mask_exclusive.make_init left right in
    let res = ME.diff init left right in
    res.left
