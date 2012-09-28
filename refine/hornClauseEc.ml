open ExtList
open ExtString
open HornClause

(** Equivalence classes of Horn clauses *)

let fvs_of_ec ec = List.unique (Util.concat_map (function `L(p) -> Atom.fvs p | `R(t) -> Term.fvs t) ec)
let pids_of_ec ec = Util.concat_map (function `L(pid, _) -> [pid] | `R(_) -> []) ec
let preds_of_ec ec = List.filter_map (function `L(p) -> Some(p) | `R(_) -> None) ec
let terms_of_ec ec = List.filter_map (function `L(_) -> None | `R(t) -> Some(t)) ec
let embed_preds atms = List.map (fun p -> `L(p)) atms
let embed_terms ts = List.map (fun t -> `R(t)) ts

let rec rel bvs xs1 xs2 =
  match xs1, xs2 with
    `L(p1), `L(p2) ->
      Util.intersects
        (Util.diff (Atom.fvs p1) bvs)
        (Util.diff (Atom.fvs p2) bvs)
  | `L(p1), `R(t2) ->
      Util.intersects
        (Util.diff (Atom.fvs p1) bvs)
        (Util.diff (Term.fvs t2) bvs)
  | `R(t1), `L(p2) ->
      rel bvs (`L(p2)) (`R(t1))
  | `R(t1), `R(t2) ->
      Util.intersects
        (Util.diff (Term.fvs t1) bvs)
        (Util.diff (Term.fvs t2) bvs)
