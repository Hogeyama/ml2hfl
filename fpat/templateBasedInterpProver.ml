open Util
open Combinator
open InterpProver

(** A template beased interpolating prover (deprecated) *)

(** template increase strategy @todo adhoc *)
let strategy = [1, 1; 2, 1; 1, 2]

(** @raise Not_found on failure *)
let interpolate_exc lb nub =
  let xs = Set_.inter (Formula.fvs lb) (Formula.fvs nub) |> List.unique in

  if false && List.length xs > 4 then raise Not_found
  else
    let tenv, [lb; nub] =
      SimTypInfer.infer_formulas [] [lb; nub]
      |> Pair.map_fst (flip TypEnv.restrict xs)
    in
    strategy
    |> List.find_map
      (fun (num_of_conjuncts, num_of_disjuncts) ->
         try
           let interp =
             tenv
             |> Template.make num_of_conjuncts num_of_disjuncts 1
             |> Logger.pprintf "template: %a@," Formula.pr
           in
           (* find substitution s for coefficients such that:
              lb => s(interp) /\ s(interp) => not nub *)
           Formula.bor [Formula.band [lb; Formula.bnot interp];
                        Formula.band [interp; nub]]
           |> Logger.pprintf "constr: %a@," Formula.pr
           |> (PolyConstrSolver.gen_coeff_constr
                 ~pos:false ~linear:!Template.linear_farkas)
           |> (PolyConstrSolver.solve_coeffs_opt
                 PolyConstrSolver.solve_dyn true !Template.linear_farkas)
           |> TermSubst.complement ~real:false (Formula.coeffs interp)
           |> Logger.pprintf "solutions:@,  %a@," PolyConstrSolver.pr
           |> flip Formula.subst interp
           |> FormulaSimplifier.simplify
           |> Logger.pprintf "interpolant: %a@," Formula.pr
           |> Option.some
         with
         | PolyConstrSolver.Unknown
         | PolyConstrSolver.NoSolution -> None)

(** @raises whatever InterpProver.interpolate_csisat0_dyn may raise *)
let interpolate lb nub =
  try interpolate_exc lb nub
  with Not_found -> InterpProver.interpolate_csisat0_dyn lb nub
(* resort to CSIsat interpolating prover *)
let interpolate =
  Logger.log_block2
    "TemplateBasedInterpProver.interpolate"
    ~after:(Logger.printf "output:@,  %a" Formula.pr)
    interpolate

let interpolate qelim p =
  interpolate
  |> UnitBoolInterpProver.interpolate
  |> interpolate_quick
  |> interpolate_simplify ~qelim:qelim
  |> CheckInterpProver.interpolate
  |> interpolate_log
  |> interpolate_fresh p
