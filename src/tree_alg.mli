exception Incompatible_union
exception Nonexistent_child

module type Data = sig type t end

module type Tree = sig module D : Data type t = D.t Vytree.t end

module Tree_impl :
  functor (D : Data) ->
    sig module D : sig type t = D.t end type t = D.t Vytree.t end

module Alg :
  functor (D : Data)
    (T : sig module D : sig type t = D.t end type t = D.t Vytree.t end) ->
    sig
      module TreeOrd :
        sig
          type t = T.t
          val compare : 'a Vytree.t -> 'b Vytree.t -> int
        end
      module SetT :
        sig
          type elt = TreeOrd.t
          type t = Set.Make(TreeOrd).t
          val empty : t
          val is_empty : t -> bool
          val mem : elt -> t -> bool
          val add : elt -> t -> t
          val singleton : elt -> t
          val remove : elt -> t -> t
          val union : t -> t -> t
          val inter : t -> t -> t
          val disjoint : t -> t -> bool
          val diff : t -> t -> t
          val compare : t -> t -> int
          val equal : t -> t -> bool
          val subset : t -> t -> bool
          val iter : (elt -> unit) -> t -> unit
          val map : (elt -> elt) -> t -> t
          val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
          val for_all : (elt -> bool) -> t -> bool
          val exists : (elt -> bool) -> t -> bool
          val filter : (elt -> bool) -> t -> t
          val filter_map : (elt -> elt option) -> t -> t
          val partition : (elt -> bool) -> t -> t * t
          val cardinal : t -> int
          val elements : t -> elt list
          val min_elt : t -> elt
          val min_elt_opt : t -> elt option
          val max_elt : t -> elt
          val max_elt_opt : t -> elt option
          val choose : t -> elt
          val choose_opt : t -> elt option
          val split : elt -> t -> t * bool * t
          val find : elt -> t -> elt
          val find_opt : elt -> t -> elt option
          val find_first : (elt -> bool) -> t -> elt
          val find_first_opt : (elt -> bool) -> t -> elt option
          val find_last : (elt -> bool) -> t -> elt
          val find_last_opt : (elt -> bool) -> t -> elt option
          val of_list : elt list -> t
          val to_seq_from : elt -> t -> elt Seq.t
          val to_seq : t -> elt Seq.t
          val to_rev_seq : t -> elt Seq.t
          val add_seq : elt Seq.t -> t -> t
          val of_seq : elt Seq.t -> t
        end
      val union_of_children : D.t Vytree.t -> D.t Vytree.t -> SetT.elt list
      val find_child : 'a Vytree.t -> 'b Vytree.t -> 'a Vytree.t option
      val insert_child : 'a Vytree.t -> 'a Vytree.t -> 'a Vytree.t
      val replace_child : 'a Vytree.t -> 'a Vytree.t -> 'a Vytree.t
      val tree_union :
        D.t Vytree.t ->
        D.t Vytree.t ->
        (D.t Vytree.t -> D.t Vytree.t -> D.t Vytree.t) -> D.t Vytree.t
    end

module ConfigData : sig type t = Config_tree.config_node_data end

module RefData : sig type t = Reference_tree.ref_node_data end

module ConfigAlg :
  sig
    module TreeOrd :
      sig
        type t = Tree_impl(ConfigData).t
        val compare : 'a Vytree.t -> 'b Vytree.t -> int
      end
    module SetT :
      sig
        type elt = TreeOrd.t
        type t = Set.Make(TreeOrd).t
        val empty : t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val compare : t -> t -> int
        val equal : t -> t -> bool
        val subset : t -> t -> bool
        val iter : (elt -> unit) -> t -> unit
        val map : (elt -> elt) -> t -> t
        val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val split : elt -> t -> t * bool * t
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
      end
    val union_of_children :
      ConfigData.t Vytree.t -> ConfigData.t Vytree.t -> TreeOrd.t list
    val find_child : 'a Vytree.t -> 'b Vytree.t -> 'a Vytree.t option
    val insert_child : 'a Vytree.t -> 'a Vytree.t -> 'a Vytree.t
    val replace_child : 'a Vytree.t -> 'a Vytree.t -> 'a Vytree.t
    val tree_union :
      ConfigData.t Vytree.t ->
      ConfigData.t Vytree.t ->
      (ConfigData.t Vytree.t ->
       ConfigData.t Vytree.t -> ConfigData.t Vytree.t) ->
      ConfigData.t Vytree.t
    [@@alert exn "Tree_alg.Incompatible_union"]
    [@@alert exn "Tree_alg.Nonexistent_child"]
  end

module RefAlg :
  sig
    module TreeOrd :
      sig
        type t = Tree_impl(RefData).t
        val compare : 'a Vytree.t -> 'b Vytree.t -> int
      end
    module SetT :
      sig
        type elt = TreeOrd.t
        type t = Set.Make(TreeOrd).t
        val empty : t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val compare : t -> t -> int
        val equal : t -> t -> bool
        val subset : t -> t -> bool
        val iter : (elt -> unit) -> t -> unit
        val map : (elt -> elt) -> t -> t
        val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val split : elt -> t -> t * bool * t
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
      end
    val union_of_children :
      RefData.t Vytree.t -> RefData.t Vytree.t -> TreeOrd.t list
    val find_child : 'a Vytree.t -> 'b Vytree.t -> 'a Vytree.t option
    val insert_child : 'a Vytree.t -> 'a Vytree.t -> 'a Vytree.t
    val replace_child : 'a Vytree.t -> 'a Vytree.t -> 'a Vytree.t
    val tree_union :
      RefData.t Vytree.t ->
      RefData.t Vytree.t ->
      (RefData.t Vytree.t -> RefData.t Vytree.t -> RefData.t Vytree.t) ->
      RefData.t Vytree.t
    [@@alert exn "Tree_alg.Incompatible_union"]
    [@@alert exn "Tree_alg.Nonexistent_child"]
  end
