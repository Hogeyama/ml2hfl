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
            let _ = Global.log (fun () -> Format.printf "inlined horn clause:@,  @[<v>%a@]@," pr_elem extlb) in
            extlb)
        hcs1
      in
      aux hcs2 (extlbs @ extlbs'(* no need to merge thanks to the assumption "is_non_disjunctive hcs" *))
  in
  let res = aux hcs [] in
  let _ = Global.log_end "compute_extlbs" in
  res


(** {6 Functions for computing upper bounds} *)

let compute_ubs_hc_aux lbs afs t =
		Util.map_left_right
		  (fun afs1 (pid, ttys) afs2 ->
		    let sub = List.map (fun (t, ty) -> Var.new_var (), t, ty) ttys in
		    let Hc(None, [], t) =
		      TypPredSubst.subst_lhs
		        ~bvs:(List.map Util.fst3 sub)
		        lbs
		        (Hc(None, afs1 @ afs2, Formula.band [t; Formula.of_subst sub]))
		    in
		    pid,
		    (List.map (fun (x, _, ty) -> x, ty) sub, Formula.bnot t))
		  afs

let compute_ubs_hc lbs ubs (Hc(popt, ps, t)) =
  let t, ps' =
    match popt with
      None ->
        t, []
    | Some(pid, xtys) ->
        if List.mem_assoc pid ubs then
          let tpid = TypPredSubst.lookup (pid, List.map (fun (x, ty) -> Term.make_var x, ty) xtys) ubs in
          Formula.band [t; Formula.bnot tpid], []
        else
          Formula.tfalse, [pid, (xtys, Formula.ttrue)]
  in
  ps' @ compute_ubs_hc_aux lbs ps t

(** @require is_non_recursive hcs && is_well_defined hcs
    @ensure Util.is_map ret && Util.set_equiv (Util.dom ret) (pids hcs) *)
let compute_ubs lbs hcs =
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
		      let ubs' = ubs @ (Util.concat_map (compute_ubs_hc lbs ubs) hcs1) in
		      aux hcs2 ubs'
		  in
		  aux hcs []
		in
  let _ = Global.log_end "compute_ubs" in
		ubs

(** {6 Functions for computing a FOL formula equivalent to a given Horn clauses} *)

(** @require is_non_recursive hcs
    @ensure hcs is solvable if and only if res => bot *)
let formula_of_forward lbs hcs =
  let _ = Global.log_begin "formula_of_forward" in
  let res =
		  Formula.simplify
		    (Formula.bor
		      (List.map
		        (fun hc ->
		          let Hc(None, [], t) = TypPredSubst.subst_lhs lbs hc in
            (*let _ =
												  if false && Cvc3Interface.satisfiable t then
														  let _ = Format.printf "%a is satisfiable@," Term.pr t in
														  assert false
            in*)
		          t)
		        (List.filter is_root hcs)))
  in
  let _ = Global.log_end "formula_of_forward" in
  res

(** @require is_non_recursive hcs
    @ensure hcs is solvable if and only if res => bot *)
let formula_of_forward_ext hcs =
  let _ = Global.log_begin "formula_of_forward_ext" in
  let hcs1, hcs = List.partition is_root hcs in
  let hcs2, hcs3 = List.partition is_coeff hcs in
  let lbs = compute_extlbs hcs3 in
  let _ = Global.log (fun () -> Format.printf "extended lower bounds:@,  @[<v>%a@]@," (Util.pr_list pr_elem "@,") lbs) in
  let res =
		  Formula.simplify
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
		  Formula.simplify
		    (Formula.bor
		      (List.map
		        (fun (Hc(None, afs, t)) ->
		          if afs = [] then
		            t
		          else
		            let _ = Format.printf "%a@." pr_elem (Hc(None, afs, t)) in
		            assert false)
		        (List.map (subst_hcs hcs2) hcs1)))
  in
  let _ = Global.log_begin "formula_of_backward" in
  res


(** {6 Functions for inlining Horn clauses} *)

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
  let res = List.map (subst_hcs lbs) hcs2 in
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
