open Util
open Combinator

(** A dag HCCS solver based on top-down interative quantifier elimination *)

(** @todo incomplete for forall-exists HCCSs *)

let solve_rhs_left_to_right = ref true

(** [solve_pv pv phi = sol]
    @ensure [sol(pv) /\ phi => bot]
    @ensure [sol(pv) subseteq fvs pv] *)
let rec solve_pv pv phi =
  phi
  |> Formula.exists
    (Map_.diff
       (phi |> Formula.fvs |> List.map (flip Pair.make Type.mk_int(*@todo*)))
       (PredVar.args_of pv |> List.map fst))
  |> Qelim.integer_qelim_dyn(*real_qelim_dyn*)
  |> Formula.bnot
  |> FormulaSimplifier.simplify
  |> PredSubst.elem_of_pvar pv
let solve_pv =
  Logger.log_block2
    "BwQEHCCSSolver.solve_pv"
    ~before:(fun pv phi ->
        Logger.printf "pv: %a@," PredVar.pr pv;
        Logger.printf "phi: %a@," Formula.pr phi)
    ~after:(Logger.printf "solution: %a" PredSubst.pr_elem)
    solve_pv

let rec solve_pvas lbs pvas phi =
  match pvas with
  | [] ->
    if not (SMTProver.is_sat_dyn phi) then []
    else
      begin
        Format.printf "sat: %a@," Formula.pr phi;
        raise HCCSSolver.Unknown
      end
  (*| [pva] -> @todo optimization *)
  | pva :: pvas' ->
    let pv, phi_eq = Pva.pvar_of pva in
    let phi' =
      try
        pvas'
        |> List.map (PredSubst.lookup_fresh lbs)
        |> List.cons phi_eq
        |> List.cons phi
        |> Formula.band
      with Not_found -> assert false
    in
    let sol = solve_pv pv phi' in
    sol :: (solve_pvas lbs pvas'
              (Formula.band [PredSubst.lookup_fresh [sol] pva; phi]
               |> FormulaSimplifier.simplify))
let solve_pvas lbs pvas =
  solve_pvas lbs (if !solve_rhs_left_to_right then pvas else List.rev pvas)
let solve_pvas =
  Logger.log_block3
    "BwQEHCCSSolver.solve_pvas"
    ~before:(fun lbs pvas phi ->
        let pvs = List.map Pva.idnt_of pvas in
        Logger.printf
          "@[<v>input lbs:@,  %a]@,"
          PredSubst.pr
          (List.filter (fun (p, _) -> List.mem p pvs) lbs);
        Logger.printf
          "@[<v>input hc:@,  %a]@,"
          HornClause.pr
          (HornClause.mk_goal pvas phi))
    solve_pvas

let solve_hc lbs part_sol hc =
  let phi' =
    HornClause.hpv_of hc
    |> PredSubst.lookup_node part_sol
    |> flip List.cons [hc |> HornClause.hphi_of]
    |> Formula.band
    |> Formula.exists (hc |> HornClause.htenv_of)
    |> Qelim.integer_qelim_dyn(*real_qelim_dyn*)
    |> Formula.bnot
    |> Formula.mk_and (HornClause.bphi_of hc)
    |> (if !InterpProver.interp_simplify then FormulaSimplifier.simplify else id)
  in
  (* begin optimization *)
  if not (SMTProver.is_sat_dyn phi') then
    HornClause.bpvas_of hc
    |> List.map (fun pva -> Pva.idnt_of pva, Pred.mk_top (Pva.type_of pva))
  else if HornClause.bpvas_of hc = [] then raise HCCSSolver.NoSolution
  else
    (* end optimization *)
    solve_pvas lbs (HornClause.bpvas_of hc) phi'
let solve_hc =
  Logger.log_block3
    "BwQEHCCSSolver.solve_hc"
    ~before:(fun _ _ -> Logger.printf "input: %a@," HornClause.pr)
    ~after:(Logger.printf "output: %a" PredSubst.pr)
    solve_hc

let rec solve lbs sol hcs =
  let not_used = HCCS.not_used hcs in
  let hcs1, hcs2 =
    List.partition
      ((* ready to solve? *)
        HornClause.fold
          (fun _ _ -> true)
          (fun pv _ _ -> PredVar.idnt_of pv |> not_used))
      hcs
  in
  if hcs1 = [] then
    begin
      Logger.debug_assert
        ~on_failure:(fun () ->
            Logger.printf0 "error: recursive HCCS not supported@,")
        (fun () -> hcs2 = []);
      sol
    end
  else
    let sol' =
      sol @ List.concat_map (solve_hc lbs sol) hcs1
      |> PredSubst.merge_and
    in
    solve lbs sol' hcs2
let solve hcs =
  let lbs =
    (if !InterpProver.interp_simplify then FwHCCSSolver.solve hcs
     else FwHCCSSolver.solve ~simplify_hc:id hcs)
    |> Logger.pprintf "lower bounds:@,  %a@," PredSubst.pr
  in
  solve lbs [] hcs
let solve = solve |> EncBoolHCCSSolver.solve false
let solve =
  Logger.log_block1
    "BwQEHCCSSolver.solve"
    ~before:(Logger.printf "HCCS: %a@," HCCS.pr)
    ~after:(Logger.printf "solution: %a@," PredSubst.pr)
    solve
