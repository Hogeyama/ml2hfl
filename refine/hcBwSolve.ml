open ExtList
open ExtString
open HornClause
open HcSolve

(** A backward constraint solver for non-recursive Horn clauses *)

(** {6 Functions for solving Horn clauses} *)

let solve_preds lbs atms t =
  let _ = Global.log_begin "solve_preds" in
  let _ =
    Global.log (fun () ->
      Format.printf "input:@,  @[<v>%a@,%a@]@,"
        TypPredSubst.pr
        (List.filter (fun (pid, _) -> List.mem_assoc pid atms) lbs)
        pr_elem (Hc(None, atms, t)))
  in
  (*
  if List.length atms = 1 then
    let [pid, ttys] = atms in
    @todo optimization
  else
  *)
    let rec aux atms t =
      match atms with
        [] -> []
      | (pid, ttys) :: atms ->
          let sub = List.map (fun (t, ty) -> Var.new_var (), t, ty) ttys in
          let xs = List.map Util.fst3 sub in
          let ttys = List.map (fun (x, _, ty) -> Term.make_var x, ty) sub in
          let _ =
            Global.log
              (fun () ->
                Format.printf "finding a solution to %a@,"
                  Atom.pr (pid, ttys))
          in
          let interp =
            let t1 =
              try
                HornClauseUtil.simplify_formula [pid] xs
                  (TypPredSubst.lookup_map (pid, ttys) lbs)
              with Not_found -> assert false
            in
            let t2 =
              try
                HornClauseUtil.simplify_formula [pid] xs
                  (Formula.band
                    (t ::
                     List.map
                       (fun (pid, ts) ->
                         TypPredSubst.lookup_map (pid, ts) lbs)
                       atms @
                     List.map Formula.of_subst_elem sub))
              with Not_found -> assert false
            in
            let t =
              try
                if !Global.enable_syntactic_predicate_generalization then
                  CsisatInterface.generalize_interpolate pid
                    (fun x -> List.mem x xs || Var.is_coeff x) t1 t2
                else
                  CsisatInterface.interpolate
                    (fun x -> List.mem x xs || Var.is_coeff x) t1 t2
              with CsisatInterface.NoInterpolant ->
                raise NoSolution
              | CsisatInterface.Unknown ->
                assert false
            in
            t
          in
          let sol = pid, (List.map (fun (x, _, ty) -> x, ty) sub, interp) in
          let _ =
            Global.log
              (fun () ->
                Format.printf
                  "solution:@,  @[<v>%a@]@,"
                  TypPredSubst.pr_elem sol)
          in
          sol ::
          aux atms (Formula.band (t :: interp :: List.map Formula.of_subst_elem sub))
    in
    let sol = aux (if !Global.solve_preds_left_to_right then atms else List.rev atms) t in
    let _ = Global.log_end "solve_preds" in
    sol

let solve_hc lbs sol (Hc(popt, atms, t)) =
  let t, sol' =
    match popt with
      None ->
        t, []
    | Some(pid, xtys) ->
        if List.mem_assoc pid sol then
          let tpid =
            TypPredSubst.lookup
              (pid, List.map (fun (x, ty) -> Term.make_var x, ty) xtys)
              sol
          in
          Formula.band [t; Formula.bnot tpid],
          []
        else
          (* Formula.bor [Formula.tfalse; t] is equivalent to Formula.tfalse *)
          Formula.tfalse,
          [pid, (xtys, Formula.ttrue)]
  in
  sol' @
  (* begin optimization *)
  if Cvc3Interface.is_valid (FormulaUtil.simplify (Formula.bnot t)) then
    List.map
      (fun (pid, ttys) ->
        pid,
        (List.map (fun (_, ty) -> Var.new_var (), ty) ttys, Formula.ttrue))
      atms
  else if atms = [] then
    raise NoSolution
  else
  (* end optimization *)
    solve_preds lbs atms t

(** @require is_non_recursive hc && is_well_defined hcs
    @ensure Util.is_map ret && Util.set_equiv (Util.dom ret) (pids hcs) *)
let solve hcs =
  let _ = Global.log_begin "solving Horn clauses" in
  let lbs =
    let lbs = compute_lbs hcs in
    let _ =
      Global.log
        (fun () ->
          Format.printf "lower bounds:@,  %a@," TypPredSubst.pr lbs)
    in
    lbs
  in
  let sol =
    let rec aux hcs sol =
      let hcs1, hcs2 =
        (** if ready hc then we are ready to solve hc *)
        let ready =
          let lhs_pids = lhs_pids hcs in
          function
            (Hc(None, _, _)) ->
              true
          | (Hc(Some(pid, _), _, _)) ->
              not (List.mem pid lhs_pids)
        in
        List.partition ready hcs
      in
      if hcs1 = [] && hcs2 = [] then
        TypPredSubst.merge sol
      else if hcs1 = [] && hcs2 <> [] then
        assert false
      else
        aux hcs2 (sol @ (Util.concat_map (solve_hc lbs sol) hcs1))
    in
    let sol = aux hcs [] in
    let _ = if !Global.debug then TypPredSubst.check_validity sol hcs in
    let _ =
      Global.log
        (fun () ->
          Format.printf "solution:@,  @[<v>%a@]" TypPredSubst.pr sol)
    in
    sol
  in
  let _ = Global.log_end "solving Horn clauses" in
  sol
