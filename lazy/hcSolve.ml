open ExtList
open ExtString
open Zipper
open CallTree
open HornClause

(** Horn clause solving *)

exception NoSolution

(** {6 Functions on solutions} *)

let pr_sol_elem ppf (pid, (xs, t)) =
  Format.fprintf ppf "@[<hov>%a =@ %a@]" pr_pred (pid, xs) Term.pr t

let pr_sol ppf sol =
  Format.printf "@[<v>%a@]" (Util.pr_list pr_sol_elem "@,") sol

let lookup_sol (pid, xs) sol =
  Formula.simplify
    (Formula.band
		    (List.map
		      (fun (_, (ys, t)) ->
		        let sub = List.map2 (fun y x -> y, Term.make_var x) ys xs in
		        Term.subst (fun x -> List.assoc x sub) t)
		      (List.filter (fun (pid', _) -> pid = pid') sol)))

let merge_sol sol =
  List.map
    (fun (pid, xs) ->
      pid, (xs, lookup_sol (pid, xs) sol))
    (List.map (fun ((pid, (xs, _))::_) -> pid, xs)
      (Util.classify (fun (pid1, _) (pid2, _) -> pid1 = pid2) sol))

(** {6 Functions for solving Horn clauses} *)

(** makes verification of file.ml too slow... why? *)
let general_interpolate pid p t1 t2 =
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
          let t1 = Formula.band (ts1 @ ts2) in
								  CsisatInterface.interpolate_bvs p t1 t2
		      with CsisatInterface.No_interpolant ->
          let t1 = Formula.band (t :: ts1 @ ts2) in
		        CsisatInterface.interpolate_bvs p t1 t2)
		with Not_found ->
		  if false then
		    CsisatInterface.interpolate_bvs p t1 t2
		  else
		    (*List.map (fun (x, n) -> Formula.eqInt (Term.make_var x) (Term.tint n)) xns*)
				  (let xns = List.sort ~cmp:(fun (_, n1) (_, n2) -> n1 - n2) xns in
				  match xns with
				    [] -> CsisatInterface.interpolate_bvs p t1 t2
				  | (x, n)::xns ->
		        let ts1 =
						      (Formula.eqInt (Term.make_var x) (Term.tint n))::
						      List.map (fun (x', n') -> Formula.eqInt (Term.make_var x') (Term.add (Term.make_var x) (Term.tint (n' - n)))) xns
		        in
          let t1 = Formula.band (ts1 @ ts2) in
		        CsisatInterface.interpolate_bvs p t1 t2)


let solve_hc_aux lbs ps fes =
  let _ = Global.log_begin "solve_hc_aux" in
		let _ =
		  Global.log (fun () ->
		    Format.printf "horn clause:@,  @[<v>%a@]@,"
		      (*(Util.pr_list pr "@,") (List.map2 (fun lb p -> Hc(Some(p), [], Fes.make [] [lb])) lbs ps)*)
		      pr (Hc(None, ps, fes)))
		in
  if false && List.length ps = 1 then
    (* ToDo: optimization *)
    assert false
  else
		  let rec aux fes ps =
						match ps with
						  [] -> []
						| (pid, xs)::ps ->
		        let ys = List.map (fun _ -> Var.new_var ()) xs in
          let mfes = Fes.make (List.map2 (fun x y -> x, Term.make_var y, SimType.Int(*???*)) xs ys) [] in
				      let _ = Global.log (fun () -> Format.printf "finding a solution to P[%a](%a)@," Var.pr pid (Util.pr_list Var.pr ",") ys) in
						    let interp =
						      let t1 =
						        try
						          let fes = Fes.eqelim_conjuncts pid (fun x -> List.mem x ys) (lookup_lbs (pid, ys) lbs) in
						          Formula.simplify (Fes.formula_of fes)
						        with Not_found -> assert false
						      in
						      let t2 =
              try
								        let fes =
										        Fes.eqelim_conjuncts pid (fun x -> List.mem x ys)
										          (Fes.band
												          (fes :: mfes :: List.map (fun (pid, xs) -> lookup_lbs (pid, xs) lbs) ps))
								        in
								        Formula.simplify (Fes.formula_of fes)
              with Not_found -> assert false
						      in
												if true then
              general_interpolate pid (fun x -> List.mem x ys) t1 t2
												else
												  CsisatInterface.interpolate_bvs (fun x -> List.mem x ys) t1 t2
						    in
						    let sol = pid, (ys, interp)	in
						    let _ = Global.log (fun () -> Format.printf "solution:@,  @[<v>%a@]@," pr_sol_elem sol) in
						    sol :: aux (Fes.band [fes; mfes; Fes.make [] [interp]]) ps
		  in
		  let sol = aux fes ps in
				let _ = Global.log_end "solve_hc_aux" in
		  sol

let solve_hc lbs sol (Hc(popt, ps, fes)) =
  let fes =
    match popt with
      None -> fes
    | Some(pid, xs) ->
        try
          Fes.band [fes; Fes.make [] [Formula.bnot (lookup_sol (pid, xs) sol)]]
        with Not_found ->
          Fes.make [] [Formula.tfalse]
  in
  if Cvc3Interface.is_valid (Formula.bnot (Fes.formula_of fes)) then
    []
  else if ps = [] then
    raise NoSolution
  else
    solve_hc_aux lbs ps fes

let solve_aux lbs hcs =
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
      merge_sol sol
    else if hcs1 = [] && hcs2 <> [] then
      assert false
    else
      aux hcs2 (sol @ (Util.concat_map (solve_hc lbs sol) hcs1))
  in
  aux hcs []

(** @returns sol
    each predicate not in sol must be true *)
let solve ctrs hcs =
  let _ = Global.log_begin "solving Horn clauses" in
  let lbs = compute_lbs hcs in
  let _ = Global.log (fun () -> Format.printf "lower bounds:@,  %a@," pr_lbs lbs) in
  let sol = solve_aux lbs hcs in
  let _ = Global.log (fun () -> Format.printf "solution:@,  @[<v>%a@]" pr_sol sol) in
  let _ = Global.log_end "solving Horn clauses" in
  sol
