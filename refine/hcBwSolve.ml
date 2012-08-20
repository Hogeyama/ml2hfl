open ExtList
open ExtString
open HornClause
open HornClauseEc
open HcSolve

(** Constraint solving for bon-recursive Horn clauses *)

(** {6 Functions for solving Horn clauses} *)

(** why verification of file.ml gets too slow? *)
let generalize_interpolate pid p t1 t2 =
  let xns, ts2 =
    Util.partition_map
      (fun t ->
        try
          match LinArith.aif_of t with
            (Const.EqInt, [1, x], n) ->
              `L(x, -n)
          | (Const.EqInt, [-1, x], n) ->
              `L(x, n)
          | aif ->
              `R(LinArith.term_of_aif aif)
        with Invalid_argument _ ->
          `R(t))
      (Formula.conjuncts t1)
  in
  match xns with
    [] -> CsisatInterface.interpolate_bvs p t1 t2
  | _ ->
      let (x, n) :: xns =
        try
          let xns1, (x, n), xns2 = Util.pick (fun (x, _) -> pid = x) xns in
          (x, n) :: xns1 @ xns2
        with Not_found ->
          List.sort ~cmp:(fun (_, n1) (_, n2) -> n1 - n2) xns
      in
      let t = Formula.eqInt (Term.make_var x) (Term.tint n) in
      let ts1 =
        List.map
          (fun (x', n') ->
            Formula.eqInt (Term.make_var x') (Term.add (Term.make_var x) (Term.tint (n' - n))))
          xns
      in
      let t1 = Formula.band (t :: ts1 @ ts2) in
      try
        if Cvc3Interface.is_valid (Formula.bnot t1) then
          Formula.tfalse (*???*)
        else
          CsisatInterface.interpolate_bvs p (Formula.band (ts1 @ ts2)) t2
      with CsisatInterface.NoInterpolant | CsisatInterface.Unknown ->
        CsisatInterface.interpolate_bvs p t1 t2


(* @todo merge with HcSolve.compute_ubs_hc_aux *)
let solve_hc_aux lbs ps t =
  let _ = Global.log_begin "solve_hc_aux" in
  let _ =
    Global.log (fun () ->
      Format.printf "horn clause:@,  @[<v>%a@]@,"
        (*(Util.pr_list pr_elem "@,") (List.map2 (fun lb p -> Hc(Some(p), [], Fes.make [] [lb])) lbs ps)*)
        pr_elem (Hc(None, ps, t)))
  in
  (*if List.length ps = 1 then
    let [pid, ttys] = ps in
    ToDo: optimization
  else*)
    let rec aux ps t =
      match ps with
        [] -> []
      | (pid, ttys)::ps ->
          let sub = List.map (fun (t, ty) -> Var.new_var (), t, ty) ttys in
          let xs = List.map Util.fst3 sub in
          let _ =
            Global.log
              (fun () ->
                Format.printf "finding a solution to %a@,"
                  Atom.pr (pid, List.map (fun (x, _, ty) -> Term.make_var x, ty) sub))
          in
          let interp =
            let simplify t =
              let sub, t = Formula.extract_from [pid] (fun x -> List.mem x xs || Var.is_coeff x) t in
              let t = Term.subst sub t in
              let [], t = subst_formula (fun x -> List.mem x xs || Var.is_coeff x) [] t in
              t
            in
            let t1 =
              try
                simplify
                  (TypPredSubst.lookup_map
                    (pid, List.map (fun (x, _, ty) -> Term.make_var x, ty) sub)
                    lbs)
              with Not_found -> assert false
            in
            let t2 =
              try
                simplify
                  (Formula.band
                    (t ::
                    List.map (fun (pid, ts) -> TypPredSubst.lookup_map (pid, ts) lbs) ps @
                    List.map Formula.of_subst_elem sub))
              with Not_found -> assert false
            in
            let t =
              try
                if !Global.generalize_predicates_simple then
                  generalize_interpolate pid (fun x -> List.mem x xs || Var.is_coeff x) t1 t2
                else
                  CsisatInterface.interpolate_bvs (fun x -> List.mem x xs || Var.is_coeff x) t1 t2
              with CsisatInterface.NoInterpolant ->
                raise NoSolution
              | CsisatInterface.Unknown ->
                assert false
            in
            t
          in
          let sol = pid, (List.map (fun (x, _, ty) -> x, ty) sub, interp) in
          let _ = Global.log (fun () -> Format.printf "solution:@,  @[<v>%a@]@," TypPredSubst.pr_elem sol) in
          sol :: aux ps (Formula.band (t :: interp :: List.map Formula.of_subst_elem sub))
    in
    let sol = aux (if !Global.find_preds_forward then ps else List.rev ps) t in
    let _ = Global.log_end "solve_hc_aux" in
    sol

(* @todo merge with HcSolve.compute_ubs_hc *)
let solve_hc lbs sol (Hc(popt, ps, t)) =
  let t, ps' =
    match popt with
      None ->
        t, []
    | Some(pid, xtys) ->
        if List.mem_assoc pid sol then
          let tpid = TypPredSubst.lookup (pid, List.map (fun (x, ty) -> Term.make_var x, ty) xtys) sol in
          Formula.band [t; Formula.bnot tpid], []
        else
          Formula.tfalse, [pid, (xtys, Formula.ttrue)]
  in
  (* begin optimization *)
  if Cvc3Interface.is_valid (Formula.simplify (Formula.bnot t)) then
    ps' @
    List.map
      (fun (pid, ttys) ->
        pid,
        (List.map (fun (_, ty) -> Var.new_var (), ty) ttys, Formula.ttrue))
      ps
  else if ps = [] then
    raise NoSolution
  else
  (* end optimization *)
    solve_hc_aux lbs ps t

(** @require is_non_recursive hc && is_well_defined hcs
    @ensure Util.is_map ret && Util.set_equiv (Util.dom ret) (pids hcs)
    @todo merge with HcSolve.compute_ubs *)
let solve hcs =
  let _ = Global.log_begin "solving Horn clauses" in
  let lbs =
    let lbs = compute_lbs hcs in
    let _ = Global.log (fun () -> Format.printf "lower bounds:@,  %a@," TypPredSubst.pr lbs) in
    lbs
  in
  let sol =
    let sol =
      let rec aux hcs sol =
        (** if is_im_sol hc then we can solve hc immediately *)
        let is_im_sol =
          let lhs_pids = lhs_pids hcs in
          function
            (Hc(None, _, _)) ->
              true
          | (Hc(Some(pid, _), _, _)) ->
              not (List.mem pid lhs_pids)
        in
        let hcs1, hcs2 = List.partition is_im_sol hcs in
        if hcs1 = [] && hcs2 = [] then
          TypPredSubst.merge sol
        else if hcs1 = [] && hcs2 <> [] then
          assert false
        else
          let sol' = sol @ (Util.concat_map (solve_hc lbs sol) hcs1) in
          aux hcs2 sol'
      in
      aux hcs []
    in
    let _ = if !Global.debug then TypPredSubst.check sol hcs in
    let _ = Global.log (fun () -> Format.printf "solution:@,  @[<v>%a@]" TypPredSubst.pr sol) in
    sol
  in
  let _ = Global.log_end "solving Horn clauses" in
  sol
