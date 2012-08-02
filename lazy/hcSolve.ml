open ExtList
open ExtString
open Zipper
open CallTree
open HornClause
open HornClauseEc

(** Horn clause solving *)

exception NoSolution

(** {6 Functions for solving Horn clauses} *)

let compute_lb lbs (Hc(Some(pid, xtys), ps, t)) =
  let Hc(_, [], t) = subst_lbs lbs (Hc(Some(pid, xtys), ps, t)) in
  pid, (xtys, t)

let compute_lbs hcs =
  let _ = Global.log_begin "compute_lbs" in
  let rec aux hcs lbs =
    let hcs1, hcs2 =
      List.partition
       (function (Hc(Some(_), ps, _)) ->
         List.for_all (fun (pid, _) -> List.mem_assoc pid lbs) ps
       | (Hc(None, _, _)) -> false)
       hcs
    in
    if hcs1 = [] then
      let _ = assert (List.filter (function Hc(None, _, _) -> false | _ -> true) hcs2 = []) in
      lbs(* hcs2 are all false *)
    else
      let lbs' =
        List.map
          (fun hc ->
            let lb = compute_lb lbs hc in
            let _ = Global.log (fun () -> Format.printf "%a@," TypPredSubst.pr_elem lb) in
            lb)
        hcs1
      in
      aux hcs2 (lbs @ (* need to merge? *)lbs')
  in
  let res = aux hcs [] in
  let _ = Global.log_end "compute_lbs" in
  res


let compute_lbs_ext hcs =
  let hcs = List.filter (function Hc(None, _, _) -> false | _ -> true) hcs in
  let pids1 = List.map (function Hc(Some(pid, _), _, _) -> pid | _ -> assert false) hcs in
  let _ = Global.log_begin "compute_lbs_ext" in
  let rec aux hcs lbs =
    let hcs1, hcs2 =
      let pids2 = List.map (function Hc(Some(pid, _), _, _) -> pid | _ -> assert false) lbs in
      List.partition
       (function (Hc(Some(_), ps, _)) ->
         List.for_all (fun (pid, _) -> List.mem pid pids2 || not (List.mem pid pids1)) ps
       | (Hc(None, _, _)) -> false)
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
      aux hcs2 (lbs @ (* need to merge? *)lbs')
  in
  let res = aux hcs [] in
  let _ = Global.log_end "compute_lbs_ext" in
  res




let formula_of_forward hcs =
  let lbs = compute_lbs hcs in
  let _ = Global.log (fun () -> Format.printf "lower bounds:@,  @[<v>%a@]@," TypPredSubst.pr lbs) in
  let hcs = List.filter (function (Hc(None, _, _)) -> true | _ -> false) hcs in
  Formula.simplify
    (Formula.bor
				  (List.map
				    (fun hc ->
				      let Hc(None, [], t) = subst_lbs lbs hc in
				      t)
				    hcs))

let formula_of_forward_ext hcs =
  let hcs1, hcs = List.partition (function (Hc(None, _, _)) -> true | _ -> false) hcs in
  let hcs2, hcs3 = List.partition (function (Hc(Some(pid, _), _, _)) -> Var.is_coeff pid | _ -> false) hcs in
  let lbs = compute_lbs_ext hcs3 in
  let _ = Global.log (fun () -> Format.printf "lower bounds:@,  @[<v>%a@]@," (Util.pr_list pr "@,") lbs) in
  Formula.simplify
    (Formula.bor
				  (List.map
				    (fun hc ->
				      let Hc(None, [], t) = subst_hcs hcs2 (subst_hcs lbs hc) in
				      t)
				    hcs1))

let formula_of_backward hcs =
  let hcs1, hcs2 = List.partition (function Hc(None, _, _) -> true | Hc(Some(pid, _), _, _) -> Var.is_coeff pid) hcs in
  let hcs = List.map (subst_hcs_fixed hcs2) hcs1 in

  let hcs1, hcs2 = List.partition (function Hc(None, _, _) -> true | _ -> false) hcs in
  let hcs = List.map (subst_hcs hcs2) hcs1 in

  Formula.simplify
    (Formula.bor
      (List.map
        (fun (Hc(None, ps, t)) ->
          if ps = [] then
            t
          else
            let _ = Format.printf "%a@." pr (Hc(None, ps, t)) in
            assert false)
        hcs))


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
  let lbs = compute_lbs_ext hcs1 in
  let hcs = List.map (subst_hcs lbs) hcs2 in
  hcs

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





(** makes verification of file.ml too slow... why? *)
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
				    List.map (fun (x', n') -> Formula.eqInt (Term.make_var x') (Term.add (Term.make_var x) (Term.tint (n' - n)))) xns
		    in
      let t1 = Formula.band (t :: ts1 @ ts2) in
						try
						  if Cvc3Interface.is_valid (Formula.bnot t1) then
						    Formula.tfalse (*???*)
        else
										CsisatInterface.interpolate_bvs p (Formula.band (ts1 @ ts2)) t2
		    with CsisatInterface.No_interpolant ->
		      CsisatInterface.interpolate_bvs p t1 t2


let solve_hc_aux (*prog*) lbs ps t =
  let _ = Global.log_begin "solve_hc_aux" in
		let _ =
		  Global.log (fun () ->
		    Format.printf "horn clause:@,  @[<v>%a@]@,"
		      (*(Util.pr_list pr "@,") (List.map2 (fun lb p -> Hc(Some(p), [], Fes.make [] [lb])) lbs ps)*)
		      pr (Hc(None, ps, t)))
		in
  if false && List.length ps = 1 then
    (* ToDo: optimization *)
    assert false
  else
		  let rec aux ps t =
						match ps with
						  [] -> []
						| (pid, ttys)::ps ->
          (*let tys = try List.map (Prog.type_of prog) (RefType.visible_vars (Prog.type_of prog) pid) with Not_found -> List.map (fun _ -> SimType.Int) xs in*)
		        let sub = List.map (fun (t, ty) -> Var.new_var (), t, ty) ttys in
          let xs = List.map Util.fst3 sub in
				      let _ = Global.log (fun () -> Format.printf "finding a solution to %a@," Pred.pr (pid, List.map (fun (x, _, ty) -> Term.make_var x, ty) sub)) in
						    let interp =
						      let t1 =
						        try
                let t = lookup_lbs (pid, List.map (fun (x, _, ty) -> Term.make_var x, ty) sub) lbs in
						          let sub, t = TypSubst.extract_from [pid] (fun x -> List.mem x xs || Var.is_coeff x) t in
						          let t = Term.subst sub t in
                let [], t = subst_formula (fun x -> List.mem x xs || Var.is_coeff x) [] t in
                t
						        with Not_found -> assert false
						      in
						      let t2 =
              try
                let t =
										        Formula.band
                    (t :: List.map (fun (pid, ts) -> lookup_lbs (pid, ts) lbs) ps @ List.map TypSubst.formula_of_elem sub)
                in
						          let sub, t = TypSubst.extract_from [pid] (fun x -> List.mem x xs || Var.is_coeff x) t in
						          let t = Term.subst sub t in
                let [], t = subst_formula (fun x -> List.mem x xs || Var.is_coeff x) [] t in
                t
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
						    let sol = pid, (List.map (fun (x, _, ty) -> x, ty) sub, interp)	in
						    let _ = Global.log (fun () -> Format.printf "solution:@,  @[<v>%a@]@," TypPredSubst.pr_elem sol) in
						    sol :: aux ps (Formula.band (t :: interp :: List.map TypSubst.formula_of_elem sub))
		  in
		  let sol = aux (if !Global.find_preds_forward then ps else List.rev ps) t in
				let _ = Global.log_end "solve_hc_aux" in
		  sol

let solve_hc (*prog*) lbs sol (Hc(popt, ps, t)) =
  let t, ps' =
    match popt with
      None -> t, []
    | Some(pid, xtys) ->
        try
          Formula.band [t; Formula.bnot (TypPredSubst.lookup (pid, List.map (fun (x, ty) -> Term.make_var x, ty) xtys) sol)],
          [pid, (xtys, Formula.ttrue)]
        with Not_found ->
          Formula.tfalse, []
  in
  if Cvc3Interface.is_valid (Formula.simplify (Formula.bnot t)) then
    ps' @ List.map (fun (pid, ttys) -> pid, (List.map (fun (_, ty) -> Var.new_var (), ty) ttys, Formula.ttrue)) ps(*[]*)
  else if ps = [] then
    raise NoSolution
  else
    solve_hc_aux (*prog*) lbs ps t

let solve_aux (*prog*) lbs hcs =
  let rec aux hcs sol =
    let lhs_pids = get_lhs_pids hcs in
    (* hcs1: immediately solvable Horn clauses *)
    let hcs1, hcs2 =
      List.partition
        (function
          (Hc(None, _, _)) ->
            true
        | (Hc(Some(pid, _), _, _)) ->
            not (List.mem pid lhs_pids))
        hcs
    in
    if hcs1 = [] && hcs2 = [] then
      TypPredSubst.merge sol
    else if hcs1 = [] && hcs2 <> [] then
      assert false
    else
      aux hcs2 (sol @ (Util.concat_map (solve_hc (*prog*) lbs sol) hcs1))
  in
  aux hcs []

(** @returns sol
    each predicate not in sol must be true *)
let solve (*prog*) ctrs hcs ohcs =
  let _ = Global.log_begin "solving Horn clauses" in
  let lbs = compute_lbs hcs in
  let _ = Global.log (fun () -> Format.printf "lower bounds:@,  %a@," TypPredSubst.pr lbs) in
  let sol = solve_aux (*prog*) lbs hcs in
  let _ = if !Global.debug then TypPredSubst.check sol hcs in
  let _ = Global.log (fun () -> Format.printf "solution:@,  @[<v>%a@]" TypPredSubst.pr sol) in
  let _ =
    if !Global.debug && false then
      let ohcs = List.map simplify (List.map (subst_lbs sol) ohcs) in
      let lbs = compute_lbs ohcs in
      let ohcs = List.map simplify (List.map (subst_lbs lbs) ohcs) in
      let _ = Global.log (fun () -> Format.printf "solved horn clauses:@,  @[<v>%a@]@," (Util.pr_list HornClause.pr "@,@,") ohcs) in
      let sol = sol @ List.filter_map (function Hc(Some(pid, xtys), [], t) -> Some(pid, (xtys, t)) | _ -> None ) ohcs in
      TypPredSubst.check sol ohcs
  in
  let _ = Global.log_end "solving Horn clauses" in
  sol
