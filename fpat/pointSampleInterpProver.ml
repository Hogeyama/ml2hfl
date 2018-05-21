open Util
open Combinator
open InterpProver

(** Point sampling based interpolating prover (deprecated) *)

let complement cs tsub =
  List.map
    (fun c -> c, try List.assoc c tsub with Not_found -> RealTerm.zero)
    cs

let compute_candidate tenv ps ns =
  try
    let interp =
      Template.mk_atom tenv 1 |> Logger.pprintf "template: %a@," Formula.pr
    in
    (ps, ns)
    |> Pair.lift (List.map (flip Formula.subst interp))
    |> Pair.map Formula.band (List.map Formula.bnot >> Formula.band)
    |> Pair.fold Formula.mk_and
    |> Logger.pprintf "generated constraints:@, %a@," Formula.pr
    |> PolyConstrSolver.solve_coeffs_opt
      PolyConstrSolver.solve_dyn false false
    |> TermSubst.complement (Formula.coeffs interp)
    |> Logger.pprintf "solutions:@,  %a@," PolyConstrSolver.pr
    |> flip Formula.subst interp
    |> FormulaSimplifier.simplify
    |> Logger.pprintf "candidate interpolant: %a@," Formula.pr
    |> Option.some
  with PolyConstrSolver.Unknown | PolyConstrSolver.NoSolution -> None

let rec interpolate ex ps ns lb nub =
  let Some(cand) =
    let xs = Set_.inter (Formula.fvs lb) (Formula.fvs nub) in
    let tenv, [lb; nub] =
      SimTypInfer.infer_formulas [] [lb; nub]
      |> Pair.map_fst (flip TypEnv.restrict xs)
    in
    compute_candidate tenv ps ns
  in
  Format.printf "candidate interpolant:@.  %a@." Formula.pr cand;
  if SMTProver.implies_dyn [lb] [cand] then
    if SMTProver.implies_dyn [cand] [Formula.bnot nub] then begin
      Format.printf "genuine!@.@.";
      cand
    end else begin
      let cex =
        if ex then
          match Polyhedron.find_extremal nub (Formula.bnot cand) with
          | Polyhedron.Ray(_) -> assert false
          | Polyhedron.Point(m) | Polyhedron.ExPoint(m, _) -> m
        else SMTProver.solve_dyn (Formula.mk_and nub cand)
      in
      if ex then
        Format.printf
          "a negative extremal counterexample found:@.  @[<v>%a@]@.@."
          TermSubst.pr cex
      else
        Format.printf
          "a negative counterexample found:@.  @[<v>%a@]@.@."
          TermSubst.pr cex;
      interpolate ex ps (cex :: ns) lb nub
    end
  else begin
    let ecex =
      if ex then
        match Polyhedron.find_extremal lb cand with
        | Polyhedron.Ray(_) -> assert false
        | Polyhedron.Point(m) | Polyhedron.ExPoint(m, _) -> m
      else SMTProver.solve_dyn (Formula.mk_and lb (Formula.bnot cand))
    in
    if ex then
      Format.printf
        "a positive extremal counterexample found:@.  @[<v>%a@]@.@."
        TermSubst.pr ecex
    else
      Format.printf
        "a positive counterexample found:@.  @[<v>%a@]@.@."
        TermSubst.pr ecex;
    interpolate ex (ecex :: ps) ns lb nub
  end

let interpolate ex = interpolate ex [] []
let interpolate ex =
  Logger.log_block2
    "PointSampleInterpProver.interpolate"
    ~after:(Logger.printf "output:@,  %a" Formula.pr)
    (interpolate ex)

let interpolate ?(ex=false) qelim p =
  (interpolate ex)
  (* |> interpolate_ub *)
  (* |> interpolate_quick *)
  (* |> interpolate_simplify ~qelim:qelim *)
  (* |> interpolate_check *)
  (* |> interpolate_log *)
  (* |> interpolate_fresh p *)
