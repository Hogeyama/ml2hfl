open Util
open Combinator
open HCCSSolver

(** An HCCS solver based on template based synthesis *)

let solve psub phi_opt hcs =
  let psub = hcs |> HCCS.tenv |> Template.mk_psub ~psub in
  try
    let tsub =
      hcs
      |> HCCS.subst psub
      |> FwHCCSSolver.formula_of_forward
      |> FormulaSimplifier.simplify
      |> DNF.of_formula |> DNF.formula_of
      |> Logger.pprintf "DNF transformed: %a@," Formula.pr
      |> (PolyConstrSolver.gen_coeff_constr
            ~pos:false ~linear:!Template.linear_farkas)
      |> Formula.mk_and (Option.fold Formula.mk_true id phi_opt)
      |> Formula.mk_and (Formula.band !Template.nonzero_const)
      |> Logger.pprintf "constraints on the coefficients: %a@," Formula.pr
      |> (PolyConstrSolver.solve_coeffs_opt
            PolyConstrSolver.solve_dyn true !Template.linear_farkas)
      |> TermSubst.complement (PredSubst.coeffs psub)
    in
    psub
    |> List.map @@ Pair.map_snd @@ Pair.map_snd
      (Formula.subst tsub >> FormulaSimplifier.simplify)
  with
  | PolyConstrSolver.NoSolution ->
    (* meaning that there is no solution of the specified template *)
    (match phi_opt with
     | None -> GenHCCSSolver.solve (CHGenInterpProver.interpolate true) hcs
     | Some(_) -> raise HCCSSolver.NoSolution(*unknown?*))
  | PolyConstrSolver.Unknown ->
    (match phi_opt with
     | None -> GenHCCSSolver.solve (CHGenInterpProver.interpolate true) hcs
     | Some(_) -> raise HCCSSolver.NoSolution)
let solve psub phi_opt =
  match phi_opt with
  | None -> solve psub phi_opt |> CheckHCCSSolver.solve
  | Some(_) -> solve psub phi_opt
let solve ?(psub=[]) ?(phi_opt=None) =
  Logger.log_block1
    "TemplateBasedHCCSSolver.solve"
    ~before:(Logger.printf "input: %a@," HCCS.pr)
    ~after:(Logger.printf "output: %a" PredSubst.pr)
    (solve psub phi_opt)

let solve_labeled lhcs =
  let psub = lhcs |> List.map snd |> HCCS.tenv |> Template.mk_psub in
  let tsub =
    try
      let _(* returned solution is not used *) =
        lhcs
        |> List.map @@ Pair.map_snd
          (HornClause.subst psub
           >> HornClause.fold
             (function
               | [] ->
                 FormulaSimplifier.simplify
                 >> DNF.of_formula >> DNF.formula_of
                 (*>> sef (Format.printf "before: %a@." Formula.pr)*)
                 >> PolyConstrSolver.gen_coeff_constr ~pos:false ~linear:true
                 (*>> sef (Format.printf "after: %a@." Formula.pr)*)
               | _ -> assert false)
             (fun _ _ _ -> assert false))
        |> SMTProver.solve_labeled_dyn
        |> TermSubst.complement (PredSubst.coeffs psub)
      in
      lhcs
      |> List.map snd
      |> HCCS.subst psub
      |> FwHCCSSolver.formula_of_forward
      |> FormulaSimplifier.simplify
      |> DNF.of_formula |> DNF.formula_of
      |> Logger.pprintf "DNF transformed: %a@," Formula.pr
      |> (PolyConstrSolver.gen_coeff_constr
            ~pos:false ~linear:!Template.linear_farkas(*@todo false*))
      |> Logger.pprintf "constraints on the coefficients: %a@," Formula.pr
      |> (PolyConstrSolver.solve_coeffs_opt
            PolyConstrSolver.solve_dyn true !Template.linear_farkas
          (*@todo false*))
      |> TermSubst.complement (PredSubst.coeffs psub)
    with
    | SMTProver.Unsat -> assert false
    | SMTProver.UnsatCore(labels) -> raise (HCCSSolver.UnsolvableCore labels)
    | SMTProver.Unknown -> raise HCCSSolver.Unknown
  in
  psub
  |> List.map @@ Pair.map_snd @@ Pair.map_snd
    (Formula.subst tsub >> FormulaSimplifier.simplify)
let solve_labeled =
  Logger.log_block1
    "TemplateBasedHCCSSolver.solve_labeled"
    ~before:(List.map snd >> Logger.printf "input: %a@," HCCS.pr)
    ~after:(Logger.printf "output: %a" PredSubst.pr)
    solve_labeled
