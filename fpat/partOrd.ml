open Util
open Combinator

(** Partial orders *)

let is_initial ord p = List.for_all (fun (p1, p2) -> p1 <> p || p1 = p2) ord

let preds ord p =
  List.filter_map
    (fun (p1, p2) -> if p2 = p && p1 <> p2 then Some(p1) else None)
    ord
let succs ord p =
  List.filter_map
    (fun (p1, p2) -> if p1 = p && p1 <> p2 then Some(p1) else None)
    ord

let reflexive_closure_of brel =
  brel
  |> List.concat_map (fun (e1, e2) -> [e1, e1; e1, e2; e2, e2])
  |> List.unique

(** Warshall-Floyd algorithm @todo optimize *)
let transitive_closure_of brel =
  let vs = List.map fst brel @ List.map snd brel |> List.unique in
  List.fold_left
    (fun brel v ->
       let brel1 = List.filter (snd >> (=) v) brel in
       let brel2 = List.filter (fst >> (=) v) brel in
       Vector.multiply (fun (v1, _) (_, v2) -> v1, v2) brel1 brel2
       @ brel
       |> List.unique)
    brel vs

let reflexive_transitive_closure_of brel =
  brel |> transitive_closure_of |> reflexive_closure_of
