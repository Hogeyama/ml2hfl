open Util

(** Ordered environment *)
module Env = struct
  module type KEY = sig
    type t
    val compare : t -> t -> int
    val print : Format.formatter -> t -> unit
  end

  module type VALUE = sig
    type t
    val print : Format.formatter -> t -> unit
  end

  module type ENV = sig
    type t
    type key
    type value
    val empty : t
    val add : key -> value -> t -> t
    val create : key list -> (key -> value) -> t

    val of_list : (key * value) list -> t
    val to_list : t -> (key * value) list
    val dom : t -> key list
    val cod : t -> value list
    val codom : t -> value list
    val range : t -> value list

    val find : (key * value -> bool) -> t -> key * value
    val assoc : key -> t -> value
    val mem_assoc : key -> t -> bool
    val assoc_option : key -> t -> value option
    val assoc_all : key -> t -> value list

    val map : (key * value -> key * value) -> t -> t
    val map_key : (key -> key) -> t -> t
    val map_value : (value -> value) -> t -> t
    val filter : (key * value -> bool) -> t -> t
    val filter_key : (key -> bool) -> t -> t
    val filter_value : (value -> bool) -> t -> t
    val filter_out : (key * value -> bool) -> t -> t
    val filter_key_out : (key -> bool) -> t -> t
    val filter_value_out : (value -> bool) -> t -> t
    val merge : t -> t -> t

    val print : Format.formatter -> t -> unit
  end

  module Make (Key : KEY) (Value : VALUE) : ENV with type key := Key.t with type value := Value.t
  =
  struct
    type t = (Key.t * Value.t) list

    let eq_key k1 k2 = Key.compare k1 k2 = 0

    let empty = []
    let add x y env = (x,y)::env
    let create keys f = List.fold_right (fun x env -> add x (f x) env) keys empty

    let of_list env = env
    let to_list env = env
    let dom env = List.map fst env
    let cod env = List.map snd env
    let codom = cod
    let range = cod

    let find f env = List.find f env
    let assoc k env = List.assoc ~cmp:eq_key k env
    let mem_assoc k env = List.mem_assoc ~cmp:eq_key k env
    let assoc_option k env = List.assoc_option ~cmp:eq_key k env
    let assoc_all k env = List.assoc_all ~cmp:eq_key k env

    let map f env = List.map f env
    let map_key f env = List.map (Pair.map_fst f) env
    let map_value f env = List.map (Pair.map_snd f) env
    let filter f env = List.filter f env
    let filter_key f env = filter (fst |- f) env
    let filter_value f env = filter (snd |- f) env
    let filter_out f env = List.filter_out f env
    let filter_key_out f env = filter_out (fst |- f) env
    let filter_value_out f env = filter_out (snd |- f) env
    let merge env1 env2 = env1 @ env2

    let print fm env =
      let pr fm (k,x) = Format.fprintf fm "%a |-> %a" Key.print k Value.print x in
      Format.fprintf fm "%a" (List.print pr) env
  end
end