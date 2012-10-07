open ExtList
open ExtString
open HornClause
open HornClauseUtil

(** Utility functions for solving non-recursive Horn clauses *)

exception NoSolution

(** {6 Functions for computing the greatest lower bounds} *)

let compute_lb lbs (Hc(Some(pid, xtys), _, _) as hc) =
  let Hc(_, [], t) = TypPredSubst.subst_lhs lbs hc in
  pid, (xtys, t)

(** @require is_non_recursive hcs && is_non_disjunctive hcs && is_well_defined hcs
    @ensure Util.is_map ret && Util.set_equiv (Util.dom ret) (HornClause.pids hcs) *)
let compute_lbs hcs =
  let _ = Global.log_begin "compute_lbs" in
  let hcs = List.filter (fun hc -> not (is_root hc)) hcs in
  let rec aux hcs lbs =
    let hcs1, hcs2 =
      let ready_to_compute_lb lbs =
        function
          (Hc(Some(_), atms, _)) ->
             List.for_all (fun (pid, _) -> List.mem_assoc pid lbs) atms
        | (Hc(None, _, _)) ->
             assert false
      in
      List.partition (ready_to_compute_lb lbs) hcs
    in
    if hcs1 = [] then
      (* should we relax the assumption "is_well_defined hcs"? *)
      let _ = if !Global.debug then assert (hcs2 = []) in
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
      aux hcs2 (lbs @ lbs' (* no need to merge here due to the assumption "is_non_disjunctive hcs"*))
  in
  let res = aux hcs [] in
  let _ = Global.log (fun () -> Format.printf "output:@,  @[<v>%a@]" TypPredSubst.pr res) in
  let _ = Global.log_end "compute_lbs" in
  res

(** @require is_non_recursive hcs && is_non_disjunctive hcs
    @ensure Util.is_map ret && (is_well_defined hcs => Util.set_equiv (Util.dom ret) (HornClause.pids hcs))
    a predicate not in the domain of ret should have the solution false *)
let compute_extlbs hcs =
  let _ = Global.log_begin "compute_extlbs" in
  let hcs = List.filter (fun hc -> not (is_root hc)) hcs in
  let not_defined =
    let pids = rhs_pids hcs in
    fun pid -> not (List.mem pid pids)
  in
  let rec aux hcs extlbs =
    let hcs1, hcs2 =
      let ready =
        let pids = rhs_pids extlbs in
        fun pid -> List.mem pid pids
      in
      List.partition
       (function (Hc(Some(_), atms, _)) ->
         List.for_all (fun (pid, _) -> ready pid || not_defined pid) atms
       | (Hc(None, _, _)) -> assert false)
       hcs
    in
    if hcs1 = [] then
      let _ = if !Global.debug then assert (hcs2 = []) in
      extlbs (*@ hcs2*)
    else
      let extlbs' =
        List.map
          (fun hc ->
            let extlb = subst_hcs extlbs hc in
            let _ = Global.log (fun () -> Format.printf "inlined horn clause:@,  @[<v>%a@]@," pr_elem extlb) in
            extlb)
        hcs1
      in
      aux hcs2 (extlbs @ extlbs'(* no need to merge here due to the assumption "is_non_disjunctive hcs" *))
  in
  let res = aux hcs [] in
  let _ = Global.log (fun () -> Format.printf "output:@,  @[<v>%a@]" pr res) in
  let _ = Global.log_end "compute_extlbs" in
  res


(** {6 Functions for computing a FOL formula equivalent to a given Horn clauses} *)

(** @require is_non_recursive hcs
    @ensure hcs is solvable if and only if res => bot *)
let formula_of_forward lbs hcs =
  let _ = Global.log_begin "formula_of_forward" in
  let res =
    FormulaUtil.simplify
      (Formula.bor
        (List.map
          (fun hc ->
            let Hc(None, [], t) = TypPredSubst.subst_lhs lbs hc in
            (*let _ =
              if false && Cvc3Interface.is_satisfiable t then
                let _ = Format.printf "%a is satisfiable@," Term.pr t in
                assert false
            in*)
            t)
          (List.filter is_root hcs)))
  in
  let _ = Global.log (fun () -> Format.printf "output:@,  @[<v>%a@]" Term.pr res) in
  let _ = Global.log_end "formula_of_forward" in
  res

(** @require is_non_recursive hcs
    @ensure hcs is solvable if and only if res => bot *)
let formula_of_forward_ext hcs =
  let _ = Global.log_begin "formula_of_forward_ext" in
  let hcs1, hcs = List.partition is_root hcs in
  let hcs2, hcs3 = List.partition is_coeff hcs in
  let lbs = compute_extlbs hcs3 in
  let _ =
    Global.log
      (fun () ->
        Format.printf
          "extended greatest lower bounds:@,  @[<v>%a@]@,"
          (Util.pr_list pr_elem "@,") lbs)
  in
  let res =
    FormulaUtil.simplify
      (Formula.bor
        (List.map
          (fun hc ->
            let Hc(None, [], t) = subst_hcs hcs2 (subst_hcs lbs hc) in
            t)
          hcs1))
  in
  let _ = Global.log_end "formula_of_forward_ext" in
  res

(** @require is_non_recursive hcs
    @ensure hcs is solvable if and only if res => bot *)
let formula_of_backward hcs =
  let _ = Global.log_begin "formula_of_backward" in
  let hcs1, hcs2 = List.partition (fun hc -> is_root hc || is_coeff hc) hcs in
  let hcs = List.map (subst_hcs_fixed hcs2) hcs1 in
  let hcs1, hcs2 = List.partition is_root hcs in
  let res =
    FormulaUtil.simplify
      (Formula.bor
        (List.map
          (fun (Hc(None, atms, t)) ->
            if atms = [] then
              t
            else
              let _ = Format.printf "%a@," pr_elem (Hc(None, atms, t)) in
              assert false)
          (List.map (subst_hcs hcs2) hcs1)))
  in
  let _ = Global.log_end "formula_of_backward" in
  res


(** {6 Functions for inlining Horn clauses} *)

let inline_other_than pid hcs =
  let _ = Global.log_begin "inline_other_than" in
  let hcs0 = backward_depend [pid] hcs in
  let hcs1, hcs2 =
    List.partition
      (function Hc(Some(pid', _), _, _) ->
        pid' <> pid
      | _ -> false)
      hcs
  in
  let lbs = compute_extlbs (Util.inter hcs0 hcs1) in
  let res =
    List.filter
      (function Hc(None, [], t) ->
        if Cvc3Interface.is_satisfiable t then
          true (*???*)
        else
          false
      | _ ->
          true)
      (List.map (subst_hcs lbs) hcs2)
  in
  let _ = Global.log (fun () -> Format.printf "output:@,  @[<v>%a@]" pr res) in
  let _ = Global.log_end "inline_other_than" in
  res

(** inline predicates that satisfy p
    @require is_non_recursive hcs *)
let inline_forward p hcs =
  let _ = Global.log_begin "inline_forward" in
  let hcs1, hcs2 =
    List.partition
      (function Hc(Some(pid, _), _, _) ->
        p pid
      | _ -> false)
      hcs
  in
  let lbs = compute_extlbs hcs1 in
  let res =
    List.filter
      (function Hc(None, [], t) ->
        if Cvc3Interface.is_satisfiable t then
          true (* assert false if hcs is assumed to be satisfiable *)
        else
          false
      | _ ->
          true)
      (List.map (subst_hcs lbs) hcs2)
  in
  let _ = Global.log (fun () -> Format.printf "output:@,  @[<v>%a@]" pr res) in
  let _ = Global.log_end "inline_forward" in
  res

(** inline predicates that satisfy p
    @require is_non_recursive hcs *)
let inline_backward p hcs =
  let _ = Global.log_begin "inline_backward" in
  let hcs1, hcs2 =
    List.partition
      (function Hc(Some(pid, _), _, _) ->
        p pid
      | _ -> false)
      hcs
  in
  let res = List.map (subst_hcs_fixed hcs1) hcs2 in
  let _ = Global.log_end "inline_backward" in
  res


(** {6 Functions for computing the least upper bounds} *)

(** @deprecated use compute_ubs instead *)
let compute_ubs_incorrect_hc lbs ubs (Hc(popt, atms, t) as hc) =
  let _ = Global.log_begin "compute_ubs_hc" in
  let _ = Global.log (fun () -> Format.printf "input: %a@," HornClause.pr_elem hc) in
  let t, ubs' =
    match popt with
      None ->
        t, []
    | Some(pid, xtys) ->
        if List.mem_assoc pid ubs then
          let tpid = TypPredSubst.lookup (pid, List.map (fun (x, ty) -> Term.make_var x, ty) xtys) ubs in
          let _ = if !Global.debug then assert (Util.diff (Term.fvs tpid) (List.map fst xtys) = []) in
          Formula.band [t; Formula.bnot tpid], []
        else
          Formula.tfalse, [pid, (xtys, Formula.ttrue)]
  in
  let ubs =
    ubs' @
    Util.maplr
      (fun atms1 (pid, ttys) atms2 ->
        let sub = List.map (fun (t, ty) -> Var.new_var (), t, ty) ttys in
        let Hc(None, [], t) =
          TypPredSubst.subst_lhs
            ~bvs:(List.map Util.fst3 sub)
            lbs
            (Hc(None, atms1 @ atms2, Formula.band [t; Formula.of_subst sub]))
        in
        pid,
        (List.map (fun (x, _, ty) -> x, ty) sub, Formula.bnot t))
      atms
  in
  let _ = Global.log (fun () -> Format.printf "output:@,  @[<v>%a@]" TypPredSubst.pr ubs) in
  let _ = Global.log_end "compute_ubs_hc" in
  ubs


(** @deprecated use compute_ubs instead
    @require is_non_recursive hcs && is_well_defined hcs
    @ensure Util.is_map ret && Util.set_equiv (Util.dom ret) (HornClause.pids hcs)
    incorrect in general*)
let compute_ubs_incorrect lbs hcs =
  let _ = Global.log_begin "compute_ubs" in
  let ubs =
    let rec aux hcs ubs =
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
        TypPredSubst.merge ubs
      else if hcs1 = [] && hcs2 <> [] then
        assert false
      else
        let ubs' = ubs @ (Util.concat_map (compute_ubs_incorrect_hc lbs ubs) hcs1) in
        aux hcs2 ubs'
    in
    aux hcs []
  in
  let _ = Global.log_end "compute_ubs" in
  ubs

(** compute the least upper bound for pid
    @require hcs is satifiable (see (1))
    @raise Not_found if the function fail to compute the least upper bound *)
let ubs_of_pid hcs pid =
  let hcs =
    if false then
      inline_other_than pid hcs
    else
      inline_forward (fun pid' -> pid' <> pid) hcs
  in
  let hcs =
    List.filter
      (function Hc(None, [], t) ->
        let _ =
          if !Global.debug then
            let _ = Format.printf "%a@," Term.pr t in
            assert (Cvc3Interface.is_valid (Formula.bnot t)) (* (1) *)
        in
        false
      | _ -> true)
      hcs
  in
  if List.exists (function Hc(Some(_, _), [], _) | Hc(None, [_], _) -> false | _ -> true) hcs then
    raise Not_found
  else
    let lhcs, rhcs =
      List.partition
        (function Hc(Some(pid', _), [], _) ->
          let _ = if !Global.debug then assert (pid' = pid) in
          true
        | Hc(None, [_], _) ->
          false
        | _ ->
          assert false)
        hcs
    in
    if rhcs = [] then
      let Hc(Some(pid, xtys), _, _) = List.hd lhcs in
      pid, (xtys, Formula.ttrue)
    else
      Util.elem_of_singleton
        (TypPredSubst.merge
          (List.map
            (fun (Hc(None, atms, t) as hc) ->
              match atms with
                [pid', ttys] ->
                  let _ = if !Global.debug then assert (pid' = pid) in
                  let sub = List.map (fun (t, ty) -> Var.new_var (), t, ty) ttys in
                  let Hc(None, [], t) =
                    TypPredSubst.subst_lhs
                      ~bvs:(List.map Util.fst3 sub)
                      []
                      (Hc(None, [], Formula.band [t; Formula.of_subst sub]))
                  in
                  pid, (List.map (fun (x, _, ty) -> x, ty) sub, Formula.bnot t)
              | _ ->
                let _ = Format.printf "%a@," HornClause.pr_elem hc in
                assert false)
            rhcs))

(** @require is_non_recursive hcs && is_non_disjunctive hcs
    @ensure Util.is_map ret && Util.set_equiv (Util.dom ret) pids
    upper bounds for some predicate variables may not be computed if
    a subset of hcs is equivalent to a constraint like "P(x) and P(y) => phi" *)
let compute_ubs_pids pids hcs =
  let _ = Global.log_begin "compute_ubs_pids" in
  let ubs, ndpids =
    Util.partition_map
      (fun pid ->
        try
          `L(ubs_of_pid hcs pid)
        with Not_found ->
          `R(pid))
      pids
  in
  let _ = Global.log (fun () -> Format.printf "output:@,  @[<v>%a@]" TypPredSubst.pr ubs) in
  let _ = Global.log_end "compute_ubs_pids" in
  ubs, ndpids

let compute_ubs hcs =
  let _ = Global.log_begin "compute_ubs" in
  let pids = List.unique (HornClause.pids hcs) in
  let ubs, ndpids = compute_ubs_pids pids hcs in
  let _ = Global.log_end "compute_ubs" in
  ubs, ndpids
