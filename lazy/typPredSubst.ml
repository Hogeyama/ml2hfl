open ExtList
open HornClause
open HornClauseEc

(** Typed predicate substitutions*)

(** {6 Functions on predicate substitutions} *)

let pr_elem ppf (pid, (xtys, t)) =
  Format.fprintf ppf
    "@[<hov>%a =@ %a@]"
    Pred.pr (pid, List.map (fun (x, ty) -> Term.make_var x, ty) xtys)
    Term.pr t

let pr ppf sol =
  Format.fprintf ppf "@[<v>%a@]" (Util.pr_list pr_elem "@,") sol

let lookup (pid, ttys) sol =
  Formula.simplify
    (Formula.band
		    (List.map
		      (fun (_, (xtys, t)) ->
		        let sub = List.combine (List.map fst xtys) (List.map fst ttys) in
		        Term.subst (fun x -> List.assoc x sub) t)
		      (List.filter (fun (pid', _) -> pid = pid') sol)))

let merge sol =
  List.map
    (fun (pid, xtys) ->
      pid, (xtys, lookup (pid, List.map (fun (x, ty) -> Term.make_var x, ty) xtys) sol))
    (List.map
      (fun ((pid, (xtys, _))::_) -> pid, xtys)
      (Util.classify (fun (pid1, _) (pid2, _) -> pid1 = pid2) sol))

let lookup_lbs (pid, ttys) lbs =
		let xtys, t = List.assoc pid lbs in

		let fvs = Util.diff (List.unique (Term.fvs t)) (List.map fst xtys) in
		let sub = List.map (fun x -> x, Term.make_var (Var.new_var ())) fvs in
		let t = Term.subst (fun x -> List.assoc x sub) t in

		let sub = List.combine (List.map fst xtys) (List.map fst ttys) in
		Term.subst (fun x -> List.assoc x sub) t

let subst_lbs lbs (Hc(popt, ps, t)) =
  let _ = Global.log_begin "subst_lbs" in
  let t, ps =
    let ts, ps =
      Util.partition_map (fun (pid, ttys) -> try `L(lookup_lbs (pid, ttys) lbs) with Not_found -> `R(pid, ttys)) ps
				in
    Formula.band (t :: ts), ps
  in
  let hc = simplify (Hc(popt, ps, t)) in
  let _ = Global.log_end "subst_lbs" in
  hc

let check sol hcs =
  List.iter
    (fun hc ->
      let Hc(popt, [], t) =
        try
          subst_lbs sol hc
        with Not_found ->
		        let _ = Format.printf "%a@," HornClause.pr hc in
		        assert false
      in
      let t' =
        try
          match popt with
										  None ->
												  Formula.tfalse
										| Some(pid, xtys) ->
										    let t = lookup_lbs (pid, List.map (fun (x, ty) -> Term.make_var x, ty) xtys) sol in
              Formula.exists
                (List.filter
                  (fun (x, _) -> not (List.mem_assoc x xtys))
  																(Term.tyfvs_ty t SimType.Bool))
                t
        with Not_found ->
		        let _ = Format.printf "%a@," HornClause.pr hc in
          assert false
      in
      if not (Cvc3Interface.implies [t] [t']) then
        let _ = Format.printf "%a@,%a => %a@," HornClause.pr hc Term.pr t Term.pr t' in
        assert false)
    hcs
