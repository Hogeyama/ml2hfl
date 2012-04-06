open ExtList
open ExtString
open Zipper
open CallTree
open HornClause

(** Horn clause solving *)

exception NoSolution

let pr_sol_elem ppf (pid, (xs, t)) =
  Format.fprintf ppf "@[<hov>%a =@ %a@]" pr_pred (pid, xs) Term.pr t

let rec solve_hc lbs ub fes ps =
  match ps with
    [] -> []
  | (pid, xs)::ps ->
      let interp =
        let t1 =
          try
            let ys, fes = lookup_lbs pid lbs in
												let sub = List.combine ys xs in
            let fes = Formula.subst_fes (fun x -> Term.make_var (List.assoc x sub)) fes in
            let fes =
				          let fes = Formula.make_fes [] (Formula.conjuncts (Formula.formula_of_fes fes)) in
										    let fes = Formula.equantify_fes (fun x -> List.mem x xs) fes in
										    let fes = Formula.eqelim_fes (fun x -> List.mem x xs) fes in
              fes
            in
            Formula.simplify (Formula.formula_of_fes fes)
          with Not_found ->
            assert false
        in
        let t2 =
          let fes =
            Formula.band_fes
		            (Formula.make_fes [] [Formula.bnot ub]::
              fes::
						        List.map
						          (fun (pid, xs) ->
								          let ys, fes = lookup_lbs pid lbs in
																		let sub = List.combine ys xs in
														    Formula.subst_fes (fun x -> Term.make_var (List.assoc x sub)) fes)
						          ps)
          in
          let fes =
		          let fes = Formula.make_fes [] (Formula.conjuncts (Formula.formula_of_fes fes)) in
								    let fes = Formula.equantify_fes (fun x -> List.mem x xs) fes in
								    let fes = Formula.eqelim_fes (fun x -> List.mem x xs) fes in
            fes
          in
          Formula.simplify (Formula.formula_of_fes fes)
        in
						  if true (* true makes verification of file.ml too slow... why? *) then
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
          try
								    (match xns with
								      [] -> raise Not_found
								    | _ ->
                let xns1, (x, n), xns2 = Util.find_split (fun (x, _) -> pid = x) xns in
                let t = Formula.eqInt (Term.make_var x) (Term.tint n) in
                let ts1 =
										        List.map (fun (x', n') -> Formula.eqInt (Term.make_var x') (Term.add (Term.make_var x) (Term.tint (n' - n)))) (xns1 @ xns2)
                in
		      						  try
        						    CsisatInterface.interpolate_bvs (fun x -> List.mem x xs) (Formula.band (ts1 @ ts2)) t2
                with CsisatInterface.No_interpolant ->
                  CsisatInterface.interpolate_bvs (fun x -> List.mem x xs) (Formula.band (t :: ts1 @ ts2)) t2)
          with Not_found ->
            if false then
              CsisatInterface.interpolate_bvs (fun x -> List.mem x xs) t1 t2
            else
		            (*List.map (fun (x, n) -> Formula.eqInt (Term.make_var x) (Term.tint n)) xns*)
										    (let xns = List.sort ~cmp:(fun (_, n1) (_, n2) -> n1 - n2) xns in
										    match xns with
										      [] -> CsisatInterface.interpolate_bvs (fun x -> List.mem x xs) t1 t2
										    | (x, n)::xns ->
		                let ts1 =
												        (Formula.eqInt (Term.make_var x) (Term.tint n))::
												        List.map (fun (x', n') -> Formula.eqInt (Term.make_var x') (Term.add (Term.make_var x) (Term.tint (n' - n)))) xns
		                in
		                CsisatInterface.interpolate_bvs (fun x -> List.mem x xs) (Formula.band (ts1 @ ts2)) t2)
						  else
						    CsisatInterface.interpolate_bvs (fun x -> List.mem x xs) t1 t2
      in
      let sol = pid, (xs, interp)	in
      let _ = Format.printf "%a@." pr_sol_elem sol in
						let fes = Formula.band_fes [Formula.make_fes [] [interp]; fes] in
      sol :: solve_hc lbs ub fes ps

let pr_sol ppf sol =
  Format.printf "@[<v>%a@]" (Util.pr_list pr_sol_elem "@,") sol

let merge_solution sol =
  List.map
    (fun sol ->
      let _ = if !Global.debug > 0 then assert (List.length (List.unique (List.map (fun (_, (xs, _)) -> xs) sol)) = 1) in
      let pid = fst (List.hd sol) in
      let xs = fst (snd (List.hd sol)) in
      let ts =
        List.map
          (fun (_, (ys, t)) ->
            let sub = List.combine ys xs in
            let sub x = Term.make_var (List.assoc x sub) in
            Term.subst sub t)
          sol
      in
      pid, (xs, Formula.band ts))
    (Util.classify (fun (pid1, _) (pid2, _) -> pid1 = pid2) sol)

let solve_aux lbs hcs =
  let get_lh_pids hcs =
    Util.concat_map
      (fun (Hc(_, ps, _)) ->
        List.map fst ps)
      hcs
  in
  let rec aux hcs sol =
    let lh_pids = get_lh_pids hcs in
    let hcs1, hcs2 =
      List.partition
        (function
          (Hc(None, _, _)) ->
            true
        | (Hc(Some(pid, _), _, _)) ->
            (*List.mem_assoc pid sol (*remark1*) &&*) not (List.mem pid lh_pids))
        hcs
    in
    if hcs1 = [] then
      if hcs2 <> [] then
        assert false (* call filter_constr before solve_constr *)
      else
        merge_solution sol
    else
      let sol' =
        sol @
        (Util.concat_map
          (fun (Hc(popt, ps, fes)) ->
            let ub =
              match popt with
                None -> Formula.tfalse
              | Some(pid, xs) ->
                  Formula.band
                    (List.map
                      (fun (_, (ys, t)) ->
                        let sub = List.combine ys xs in
                        let sub x = Term.make_var (List.assoc x sub) in
                        Term.subst sub t)
                      (List.filter (fun (pid', _) -> pid = pid') sol))
            in
            (if List.length ps = 1 && (let Formula.FES(xttys, ts) = fes in xttys = [] && ts = []) then
              (* ToDo: optimization *)
              ());
            if Cvc3Interface.is_valid (Formula.imply (Formula.formula_of_fes fes) ub) then
              [](*remark1*)
            else if ps = [] then
              raise NoSolution
            else
              solve_hc lbs ub fes ps)
          hcs1)
      in
      aux hcs2 sol'
  in
  aux hcs []

let solve ctrs hcs =
  let lbs = compute_lbs hcs in
  let _ = Format.printf "lower bounds:@.  %a@." pr_lbs lbs in
  let sol = solve_aux lbs hcs in
  let _ = Format.printf "solution:@.  @[<v>%a@]@." pr_sol sol in
  List.map (fun (pred, (xs, t)) -> pred, (xs, Formula.simplify t)) sol

let infer_env prog sums fcs =
  let env = RefType.of_summaries prog sums fcs in
  let env' =
    List.map
      (fun (f, sty) ->
        f, RefType.of_simple_type sty)
      (List.find_all
        (fun (f, sty) -> not (List.mem_assoc f env))
        (List.map
          (fun (f, sty) -> Var.make f, sty)
          prog.Prog.types))
  in
  env @ env'

