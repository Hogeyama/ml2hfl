open ExtList
open ExtString
open Zipper
open CallTree
open HornClause
open HornClauseEc

(** Constraint solving for bon-recursive Horn clauses *)

exception NoSolution

(** {6 Functions for computing lower bounds} *)

let compute_lb lbs (Hc(Some(pid, xtys), _, _) as hc) =
  let Hc(_, [], t) = TypPredSubst.subst_lbs lbs hc in
  pid, (xtys, t)

(** @assume is_non_recursive hcs && is_non_disjunctive hcs && is_well_defined hcs
    @ensure TypPredSubst.is_function ret *)
let compute_lbs hcs =
  let _ = Global.log_begin "compute_lbs" in
  let hcs = List.filter (fun hc -> not (is_root hc)) hcs in
  let rec aux hcs lbs =
    let hcs1, hcs2 =
      List.partition
       (function (Hc(Some(_), ps, _)) ->
         List.for_all (fun (pid, _) -> List.mem_assoc pid lbs) ps
       | (Hc(None, _, _)) -> assert false)
       hcs
    in
    if hcs1 = [] then
      let _ =
        if !Global.debug then
          assert (hcs2 = [])
          (* should we relax the assumption is_well_defined hcs? *)
      in
      lbs
    else
      let lbs' =
        List.map
          (fun hc ->
            let lb = compute_lb lbs hc in
            let _ = Global.log (fun () -> Format.printf "%a@," TypPredSubst.pr_elem lb) in
            lb)
          hcs1
      in
      aux hcs2 (lbs @ lbs' (* no need to merge thanks to the assumption *))
  in
  let res = aux hcs [] in
  let _ = Global.log_end "compute_lbs" in
  res


(** @assume is_non_recursive hcs && is_non_disjunctive hcs
    @ensure ret is a function *)
let compute_extlbs hcs =
  let _ = Global.log_begin "compute_extlbs" in
  let hcs = List.filter (fun hc -> not (is_root hc)) hcs in
  let pids1 = rhs_pids hcs in
  let rec aux hcs lbs =
    let hcs1, hcs2 =
      let pids2 = rhs_pids lbs in
      List.partition
       (function (Hc(Some(_), ps, _)) ->
         List.for_all (fun (pid, _) -> List.mem pid pids2 || not (List.mem pid pids1)) ps
       | (Hc(None, _, _)) -> assert false)
       hcs
    in
    if hcs1 = [] then
      lbs @ hcs2
    else
      let lbs' =
        List.map
          (fun hc ->
            let lb = subst_hcs lbs hc in
            let _ = Global.log (fun () -> Format.printf "inlined horn clause:@,  @[<v>%a@]@," pr lb) in
            lb)
        hcs1
      in
      aux hcs2 (lbs @ lbs'(* no need to merge thanks to the assumption *))
  in
  let res = aux hcs [] in
  let _ = Global.log_end "compute_extlbs" in
  res


(** {6 Functions for computing an equivalent FOL formula} *)

(** @assume is_non_recursive hcs *)
let formula_of_forward hcs =
  let lbs = compute_lbs hcs in
  let _ = Global.log (fun () -> Format.printf "lower bounds:@,  @[<v>%a@]@," TypPredSubst.pr lbs) in
  Formula.simplify
    (Formula.bor
      (List.map
        (fun hc ->
          let Hc(None, [], t) = TypPredSubst.subst_lbs lbs hc in
          t)
        (List.filter is_root hcs)))

(** @assume is_non_recursive hcs *)
let formula_of_forward_ext hcs =
  let hcs1, hcs = List.partition is_root hcs in
  let hcs2, hcs3 = List.partition is_coeff hcs in
  let lbs = compute_extlbs hcs3 in
  let _ = Global.log (fun () -> Format.printf "extended lower bounds:@,  @[<v>%a@]@," (Util.pr_list pr "@,") lbs) in
  Formula.simplify
    (Formula.bor
      (List.map
        (fun hc ->
          let Hc(None, [], t) = subst_hcs hcs2 (subst_hcs lbs hc) in
          t)
        hcs1))

(** @assume is_non_recursive hcs *)
let formula_of_backward hcs =
  let hcs1, hcs2 = List.partition (fun hc -> is_root hc || is_coeff hc) hcs in
  let hcs = List.map (subst_hcs_fixed hcs2) hcs1 in
  let hcs1, hcs2 = List.partition is_root hcs in
  Formula.simplify
    (Formula.bor
      (List.map
        (fun (Hc(None, ps, t)) ->
          if ps = [] then
            t
          else
            let _ = Format.printf "%a@." pr (Hc(None, ps, t)) in
            assert false)
        (List.map (subst_hcs hcs2) hcs1)))

(** {6 Functions for inlining Horn clauses} *)

(** @assume is_non_recursive hcs *)
let inline_forward fs hcs =
  let hcs1, hcs2 =
    List.partition
      (function Hc(Some(pid, _), _, _) ->
        not (Var.is_coeff pid) &&
        List.exists
          (fun f ->
            let Var.V(id), _ = CallId.tlfc_of pid in
            Idnt.string_of id = f)
          fs
      | _ -> false)
      hcs
  in
  let lbs = compute_extlbs hcs1 in
  let hcs = List.map (subst_hcs lbs) hcs2 in
  hcs

(** @assume is_non_recursive hcs *)
let inline_backward fs hcs =
  let hcs1, hcs2 =
    List.partition
      (function Hc(Some(pid, _), _, _) ->
        not (Var.is_coeff pid) &&
        List.exists
          (fun f ->
            let Var.V(id), _ = CallId.tlfc_of pid in
            Idnt.string_of id = f)
          fs
      | _ -> false)
      hcs
  in
  List.map (subst_hcs_fixed hcs1) hcs2




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
          let xns1, (x, n), xns2 = Util.find_split (fun (x, _) -> pid = x) xns in
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
      with CsisatInterface.No_interpolant ->
        CsisatInterface.interpolate_bvs p t1 t2


let solve_hc_aux lbs ps t =
  let _ = Global.log_begin "solve_hc_aux" in
  let _ =
    Global.log (fun () ->
      Format.printf "horn clause:@,  @[<v>%a@]@,"
        (*(Util.pr_list pr "@,") (List.map2 (fun lb p -> Hc(Some(p), [], Fes.make [] [lb])) lbs ps)*)
        pr (Hc(None, ps, t)))
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
                  Pred.pr (pid, List.map (fun (x, _, ty) -> Term.make_var x, ty) sub))
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
                  (TypPredSubst.lookup_lbs
                    (pid, List.map (fun (x, _, ty) -> Term.make_var x, ty) sub)
                    lbs)
              with Not_found -> assert false
            in
            let t2 =
              try
                simplify
                  (Formula.band
                    (t ::
                    List.map (fun (pid, ts) -> TypPredSubst.lookup_lbs (pid, ts) lbs) ps @
                    List.map Formula.of_subst_elem sub))
              with Not_found -> assert false
            in
            let t =
              try
                if !Global.generalize_predicates_simple then
                  generalize_interpolate pid (fun x -> List.mem x xs || Var.is_coeff x) t1 t2
                else
                  CsisatInterface.interpolate_bvs (fun x -> List.mem x xs || Var.is_coeff x) t1 t2
              with CsisatInterface.No_interpolant ->
                raise NoSolution
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

let solve_hc lbs sol (Hc(popt, ps, t)) =
  let t, ps' =
    match popt with
      None ->
        t, []
    | Some(pid, xtys) ->
        try
          let tpid = TypPredSubst.lookup (pid, List.map (fun (x, ty) -> Term.make_var x, ty) xtys) sol in
          Formula.band [t; Formula.bnot tpid], []
        with Not_found ->
          Formula.tfalse, [pid, (xtys, Formula.ttrue)]
  in
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
    solve_hc_aux lbs ps t

(** @assume is_non_recursive hcs
    @ensure TypPredSubst.is_function ret
            dom ret = fpv hcs *)
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
        (** if is_im_sol hc then we can solve hc immediately? *)
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
