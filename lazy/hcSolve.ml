open ExtList
open ExtString
open HornClause
open HornClauseEc

(** Solving non-recursive Horn clauses *)

exception NoSolution

(** {6 Functions for computing lower bounds} *)

let compute_lb lbs (Hc(Some(pid, xtys), _, _) as hc) =
  let Hc(_, [], t) = TypPredSubst.subst_lhs lbs hc in
  pid, (xtys, t)

(** @require is_non_recursive hcs && is_non_disjunctive hcs && is_well_defined hcs
    @ensure Util.is_map ret && Util.set_equiv (Util.dom ret) (pids hcs) *)
let compute_lbs hcs =
  let _ = Global.log_begin "compute_lbs" in
  let hcs = List.filter (fun hc -> not (is_root hc)) hcs in
  let rec aux hcs lbs =
    let hcs1, hcs2 =
						let ready_to_compute_lb lbs =
						  function
						    (Hc(Some(_), afs, _)) ->
						       List.for_all (fun (pid, _) -> List.mem_assoc pid lbs) afs
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
      aux hcs2 (lbs @ lbs' (* no need to merge thanks to the assumption "is_non_disjunctive hcs"*))
  in
  let res = aux hcs [] in
  let _ = Global.log_end "compute_lbs" in
  res

(** @require is_non_recursive hcs && is_non_disjunctive hcs
    @ensure Util.is_map ret && (is_well_defined hcs => Util.set_equiv (Util.dom ret) (pids hcs))
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
       (function (Hc(Some(_), afs, _)) ->
         List.for_all (fun (pid, _) -> ready pid || not_defined pid) afs
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
            let _ = Global.log (fun () -> Format.printf "inlined horn clause:@,  @[<v>%a@]@," pr extlb) in
            extlb)
        hcs1
      in
      aux hcs2 (extlbs @ extlbs'(* no need to merge thanks to the assumption "is_non_disjunctive hcs" *))
  in
  let res = aux hcs [] in
  let _ = Global.log_end "compute_extlbs" in
  res


(** {6 Functions for computing upper bounds} *)

(** @require is_non_recursive hcs && is_non_disjunctive hcs && is_well_defined hcs
    @ensure Util.is_map ret && Util.set_equiv (Util.dom ret) (pids hcs) *)
let compute_ubs lbs hcs =
  List.map
    (fun ((pid, (xtys, t)) :: rs) ->
      pid, (xtys, Formula.simplify (Formula.band (t :: List.map (fun (_, (xtys', t')) -> Term.rename xtys' xtys t') rs))))
		  (Util.classify
		    (fun (pid1, _) (pid2, _) -> pid1 = pid2)
				  (Util.concat_map
				    (fun (Hc(popt, afs, t)) ->
          Util.map_left_right
            (fun afs1 (pid, ttys) afs2 ->
              let sub = List.map (fun (t, ty) -> Var.new_var (), t, ty) ttys in
              let Hc(None, [], t) =
                TypPredSubst.subst
                  ~bvs:(List.map Util.fst3 sub)
                  lbs
                  (Hc(popt, afs1 @ afs2, Formula.band [t; Formula.of_subst sub]))
              in
              pid,
              (List.map (fun (x, _, ty) -> x, ty) sub, Formula.bnot t))
            afs)
				    hcs))

(** {6 Functions for computing a FOL formula equivalent to a given Horn clauses} *)

(** @require is_non_recursive hcs *)
let formula_of_forward hcs =
  let lbs = compute_lbs hcs in
  let _ = Global.log (fun () -> Format.printf "lower bounds:@,  @[<v>%a@]@," TypPredSubst.pr lbs) in
  Formula.simplify
    (Formula.bor
      (List.map
        (fun hc ->
          let Hc(None, [], t) = TypPredSubst.subst_lhs lbs hc in
          t)
        (List.filter is_root hcs)))

(** @require is_non_recursive hcs *)
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

(** @require is_non_recursive hcs *)
let formula_of_backward hcs =
  let hcs1, hcs2 = List.partition (fun hc -> is_root hc || is_coeff hc) hcs in
  let hcs = List.map (subst_hcs_fixed hcs2) hcs1 in
  let hcs1, hcs2 = List.partition is_root hcs in
  Formula.simplify
    (Formula.bor
      (List.map
        (fun (Hc(None, afs, t)) ->
          if afs = [] then
            t
          else
            let _ = Format.printf "%a@." pr (Hc(None, afs, t)) in
            assert false)
        (List.map (subst_hcs hcs2) hcs1)))

(** {6 Functions for inlining Horn clauses} *)

(** @require is_non_recursive hcs *)
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

(** @require is_non_recursive hcs *)
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
