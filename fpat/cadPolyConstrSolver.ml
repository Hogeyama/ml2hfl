open Util
open Combinator

(** A polynomial constraint solver based on QECAD
    (see [Colon+ CAV 2003] for details)
    @todo make it more efficient *)

(** [solve phi] returns a map from each variable and coefficient in [phi] to an integer
    @require each variable and coefficient in [phi] has the integer type
    @raise PolyConstrSolver.Unknown if [solve] fails to sove [phi] *)
let solve phi =
  phi
  |> DNF.of_formula
  |> DNF.disjunction_of
  |> (List.map
        (fun phi ->
           try phi |> CunFormula.linearize |> FormulaSimplifier.simplify
           with Invalid_argument _ -> phi))
  |> (List.map
        (Logger.pprintf
           "(possibly) linearized constraint on coefficients:@,  %a@,"
           Formula.pr))
  |> (List.map
        (fun phi ->
           phi
           |> Formula.exists
             (SimTypJudge.env_of (Formula.term_of phi) Type.mk_bool)
           |> (if_ CunFormula.is_linear
                 (* apply Fourier-Motzkin? *)
                 Qelim.integer_qelim_dyn
                 (* apply QECAD? *)
                 (* @todo check whether the result is correct also for
                    the integer domain *)
                 Qelim.real_qelim_dyn)
           |> FormulaSimplifier.simplify))
  |> (List.map
        (Logger.pprintf
           "quantifier eliminated constraint on coefficients:@,  %a@,"
           Formula.pr))
  |> Formula.band
  |> (fun phi ->
      try PolyConstrSolver.solve_dyn phi
      with SMTProver.Unknown -> raise PolyConstrSolver.Unknown)

let solve = Logger.log_block1 "CadPolyConstrSolver.solve" solve
