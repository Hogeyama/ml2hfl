open Util
open Combinator
open InterpProver

(** interpolating prover based on HCCS solving *)

let interpolate solver _(*@todo*) lb nub =
  let xs = Set_.inter (Formula.fvs lb) (Formula.fvs nub) in
  let tenv, [lb; nub] =
    SimTypInfer.infer_formulas [] [lb; nub]
    |> Pair.map_fst (flip TypEnv.restrict xs)
  in

  let pid = Idnt.make "interp" in
  let atm = Pva.make pid (List.map (Pair.map_fst Term.mk_var) tenv) in
  let hccs =
    [HornClause.mk_def (PredVar.make pid tenv) [] lb;
     HornClause.mk_goal [atm] nub]
  in
  Format.printf "HCCS for interpolation:@.  %a@." HCCS.pr hccs;
  let sol = solver hccs in
  PredSubst.lookup sol atm
