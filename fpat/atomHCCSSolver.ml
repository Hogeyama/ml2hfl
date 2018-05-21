open Util
open Combinator
open HCCSSolver

(** An HCCS solver for finding atomic solutions using Farkas' lemma *)

let use_pepm_for_bool = ref false

(* @todo theory level splitting seems preferable *)
let solve =
  SplitBoolHCCSSolver.solve
    (if !use_pepm_for_bool
     then GenHCCSSolver.solve (CHGenInterpProver.interpolate true)
     else BoolHCCSSolver.solve)
    TemplateBasedHCCSSolver.solve
let solve = solve |> CheckHCCSSolver.solve
let solve =
  Logger.log_block1
    "AtomHCCSSolver.solve"
    ~before:(Logger.printf "input: %a@," HCCS.pr)
    solve

(* @todo theory level splitting seems preferable *)
let solve_labeled =
  SplitBoolHCCSSolver.solve_labeled TemplateBasedHCCSSolver.solve_labeled
let solve_labeled =
  Logger.log_block1 "AtomHCCSSolver.solve_labeled" solve_labeled
