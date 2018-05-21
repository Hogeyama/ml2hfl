open Util
open Combinator
open HCCSSolver

(** An HCCS solver, which splits the given HCCS into (1) an HCCS over
    booleans, and (2) an HCCS over non-booleans *)

(* @todo incomplete? *)
let solve solver_bool solver hcs =
  let hcs_bool, hcs_non_bool =
    hcs
    |> HCCS.fresh (* free variables may not be fresh *)
    |> HCCS.split_bool_non_bool
  in
  try solver_bool hcs_bool
  with
  | HCCSSolver.NoSolution -> solver hcs_non_bool
  | HCCSSolver.Unknown -> assert false

let solve_labeled solver lhcs =
  let lhcs_bool, lhcs_non_bool =
    lhcs
    |> HCCS.fresh_label (* free variables may not be fresh *)
    |> HCCS.split_bool_non_bool_label
  in
  try BoolHCCSSolver.solve (List.map snd lhcs_bool) with
  | HCCSSolver.NoSolution -> solver lhcs_non_bool
  | HCCSSolver.Unknown -> assert false
