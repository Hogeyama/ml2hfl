open ExtList
open ExtString
open Zipper
open CallTree
open HornClause

(** Horn clause solving *)

exception NoSolution

let lookup pid lbs =
		let xs, ts = List.assoc pid lbs in
  xs,
		let fvs = List.filter (fun x -> not (List.mem x xs)) (List.unique (Util.concat_map Term.fvs ts)) in
		let sub = List.map (fun x -> x, Term.make_var (Var.new_var ())) fvs in
  List.map (Term.subst (fun x -> List.assoc x sub)) ts

let compute_lb lbs (Hc(Some(pid, xs), ps, ts)) =
  let ts =
    List.unique
		    (ts @
		    Util.concat_map
		      (fun (pid, xs) ->
		        let ys, ts = lookup pid lbs in
		        let sub = List.combine ys xs in
		        let sub x = Term.make_var (List.assoc x sub) in
		        List.map (Term.subst sub) ts)
		      ps)
  in
(*
  Format.printf "%a@." (Util.pr_list Term.pr ",") ts;
*)
  let ts = Formula.eqelim (fun x -> List.mem x xs) ts in
  pid, (xs, ts)

let compute_lbs hcs =
  let rec aux hcs lbs =
    let hcs1, hcs2 =
      List.partition
       (function (Hc(Some(_), ps, _)) ->
         List.for_all (fun (pid, _) -> List.mem_assoc pid lbs) ps
       | (Hc(None, _, _)) -> false)
       hcs
    in
    if hcs1 = [] then
      lbs (* hcs2 are all false *)
    else
      aux hcs2 (lbs @ (* need to merge? *)(List.map (compute_lb lbs) hcs1))
  in
  aux hcs []

let rec solve_hc lbs ub ts ps =
  match ps with
    [] -> []
  | (pid, xs)::ps ->
      let interp =
        let t1 =
          try
            let ys, ts = lookup pid lbs in
												let sub = List.combine ys xs in
												let sub x = Term.make_var (List.assoc x sub) in
            Formula.band (List.map (Term.subst sub) ts)
          with Not_found ->
            assert false
        in
        let t2 = Formula.band
          (Formula.bnot ub :: ts @
          Util.concat_map
            (fun (pid, xs) ->
		            let ys, ts = lookup pid lbs in
														let sub = List.combine ys xs in
														let sub x = Term.make_var (List.assoc x sub) in
								      List.map (Term.subst sub) ts)
            ps)
        in
        CsisatInterface.interpolate_bvs (fun x -> List.mem x xs) t1 t2
      in
      (pid, (xs, interp))::
      solve_hc lbs ub (interp::ts) ps

let pr_sol ppf sol =
  let pr_aux ppf (pid, (xs, t)) =
    Format.fprintf ppf "@[<hov>%a =@ %a@]" pr_pred (pid, xs) Term.pr t
  in
  Format.printf "@[<v>%a@]" (Util.pr_list pr_aux "@,") sol

let merge_solution sol =
  List.map
    (fun sol ->
      let _ = if !Flags.debug then assert (List.length (List.unique (List.map (fun (_, (xs, _)) -> xs) sol)) = 1) in
      fst (List.hd sol), (fst (snd (List.hd sol)), Formula.band (List.map (fun (_, (_, t)) -> t) sol)))
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
						    (fun (Hc(popt, ps, ts)) ->
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
						      (if List.length ps = 1 && ts = [] then
						        (* ToDo: optimization *)
						        ());
						      if Cvc3Interface.is_valid (Formula.imply (Formula.band ts) ub) then
						        [](*remark1*)
						      else if ps = [] then
						        raise NoSolution
						      else
						        solve_hc lbs ub ts ps)
						    hcs1)
    		in
		    aux hcs2 sol'
  in
  aux hcs []

let solve ctrs hcs =
  let lbs = compute_lbs hcs in
  let _ =
    let pr_lb ppf (pid, (xs, ts)) =
      Format.fprintf ppf "@[<hov>%a =@ %a@]" pr_pred (pid, xs) Term.pr (Formula.band ts)
    in
    Format.printf "lower bounds:@.  @[<v>%a@]@." (Util.pr_list pr_lb "@,") lbs
  in
  let sol = solve_aux lbs hcs in
  let _ = Format.printf "solution:@.  @[<v>%a@]@." pr_sol sol in
  List.map (fun (pred, (xs, t)) -> pred, (xs, Formula.simplify t)) sol
