%{
    open Config_tree

    exception Duplicate_child of (string * string)

    (* Used for checking if after merging immediate children,
       any of them have duplicate children inside,
       e.g. "interfaces { ethernet eth0 {...} ethernet eth0 {...} }" *)
    let find_duplicate_children n =
        let rec aux xs =
            let xs = List.sort compare xs in
            match xs with
            | [] | [_] -> ()
            | x :: x' :: xs ->
                if x = x' then raise (Duplicate_child (Vytree.name_of_node n, x))
                else aux (x' :: xs)
        in
        aux @@ Vytree.list_children n

    (* When merging nodes with values, append values of subsequent nodes to the
       first one *)
    let merge_data l r = {l with values=(List.append l.values r.values)}
%}

%token <string> IDENTIFIER
%token <string> STRING
%token <string> COMMENT
%token LEFT_BRACE
%token RIGHT_BRACE
%token NEWLINE
%token EOF

%start <Config_tree.t> config
%%

(* If there are multiple comments before a node, consider the last one its real comment *)
comments: 
    cs = list(COMMENT) { match cs with [] -> None | _ -> Some (List.rev cs |> List.hd |> String.trim) }

value:
  | v = STRING
    { v }
  | v = IDENTIFIER
    { v }
;


leaf_node_body:
  | comment = comments;
    name = IDENTIFIER; value = value;
    { Vytree.make_full {default_data with values=[value]; comment=comment} name []}
  | comment = comments;
    name = IDENTIFIER; (* valueless node *)
    { Vytree.make_full {default_data with comment=comment} name [] }
;

leaf_node:
  | n = leaf_node_body; NEWLINE;
    { n }
  | n = leaf_node_body; EOF;
    { n }

node:
  | comment = comments;
    name = IDENTIFIER; LEFT_BRACE; children = list(node_content); RIGHT_BRACE;
    {
        let node =
            Vytree.make_full {default_data with comment=comment} name [] in
        let node = List.fold_left Vytree.adopt node (List.rev children) |> Vytree.merge_children merge_data in
        try
            List.iter find_duplicate_children (Vytree.children_of_node node);
            node
        with
        | Duplicate_child (child, dup) ->
            failwith (Printf.sprintf "Node \"%s %s\" has two children named \"%s\"" name child dup)
    }
;

(* XXX: for the config to be loadable with the old CStore backend, what was formatted as a tag node
   in the original config, must remain formatted that way.
   This is why the tag field is set.     
 *)
tag_node:
  | comment = comments;
    name = IDENTIFIER; tag = value; LEFT_BRACE; children = list(node_content); RIGHT_BRACE
  {
      let outer_node = Vytree.make_full {default_data with tag=true} name [] in
      let inner_node =
          Vytree.make_full {default_data with comment=comment} tag [] in
      let inner_node = List.fold_left Vytree.adopt inner_node (List.rev children) |> Vytree.merge_children merge_data in
      let node = Vytree.adopt outer_node inner_node in
      try
          List.iter find_duplicate_children (Vytree.children_of_node inner_node);
          node
      with
      | Duplicate_child (child, dup) ->
          failwith (Printf.sprintf "Node \"%s %s %s\" has two children named \"%s\"" name tag child dup)
  }

node_content: n = node { n } | n = leaf_node { n } | n = tag_node { n };


%public config:
 | ns = list(node_content); EOF
 {
    let root = make "root" in
    let root = List.fold_left Vytree.adopt root (List.rev ns) |> Vytree.merge_children merge_data in
        try
            List.iter find_duplicate_children (Vytree.children_of_node root);
            root
        with
        | Duplicate_child (child, dup) ->
            failwith (Printf.sprintf "Node \"%s\" has two children named \"%s\"" child dup)
  }
;
