open ExtList
open ExtString
open HornClause
open HcSolve

let rec solve hcs0 =
  let _ = Global.log_begin "HcGenSolve.solve" in
  (*let _ = if false then
    let t = HcSolve.formula_of_forward (compute_lbs hcs0) hcs0 in
    if not (Cvc3Interface.is_valid (Formula.bnot t)) then
      let _ = Format.printf "not solvable:@,  %a@," HornClause.pr hcs0 in
      assert false
  in*)
  let pids = List.unique (pids hcs0) in
  (*let npid = List.length pids in let _ = Format.printf "#pids: %d@," npid in*)
  let pidss = Util.sort_by_dec List.length (Util.classify Var.cong pids) in
  let sol =
		  match pidss with
		    [] ->
		      []
		  | pids :: _ ->
		      let hcs = (*inline_forward (fun pid -> not (List.mem pid pids))*) hcs0 in
		      let lb_ub_of =
		        let lbs, ubs =
		          let _ = Global.log (fun () -> Format.printf "Horn clauses:@,  %a@," HornClause.pr hcs) in
		          let lbs = compute_lbs hcs in
		          let _ = Global.log (fun () -> Format.printf "lower bounds:@,  %a@," TypPredSubst.pr lbs) in
		          let ubs = compute_ubs hcs in
		          let _ = Global.log (fun () -> Format.printf "upper bounds:@,  %a@," TypPredSubst.pr ubs) in
		          lbs, ubs
		        in
		        fun pid ->
		          try
		            let xtys = TypPredSubst.args_of pid lbs in
		            let af = Atom.of_pred (pid, xtys) in
		            xtys,
		            TypPredSubst.lookup_map af lbs,
		            TypPredSubst.lookup_map af ubs
		          with Not_found ->
		            let _ = Format.printf "%a is not found@," Var.pr pid in
		            assert false
		      in
		      let lbs_ubs_of pids =
		        let _ = assert (pids <> []) in
		        let (xtys, lb, ub) :: rs = List.map lb_ub_of pids in
		        let lbs, ubs = List.split (List.map (fun (xtys', lb', ub') -> Term.rename xtys' xtys lb', Term.rename xtys' xtys ub') rs) in
		        xtys, lb :: lbs, ub :: ubs
		      in
		      (*let _ =
		        if false then
		          List.iter
		            (fun pid ->
		              let _, lb, ub = lb_ub_of pid in
		              if not (Cvc3Interface.implies [lb] [ub]) then
		                let _ = Format.printf "pid: %a@,not |= %a => %a@," Var.pr pid Term.pr lb Term.pr ub in
		                assert false)
		            pids
		      in*)
		      let pidss =
		        let check pid pids =
		          let _, lbs, ubs = lbs_ubs_of (pid :: pids) in
		          Cvc3Interface.implies [Formula.bor lbs] [Formula.band ubs]
		        in
		        let rec add pid pidss =
		          match pidss with
		            [] -> [[pid]]
		          | pids :: pidss ->
		              if check pid pids then
		                (pid :: pids) :: pidss
		              else
		                pids :: (add pid pidss)
		        in
		        let rec classify pids pidss =
		          match pids with
		            [] -> pidss
		          | pid :: pids ->
		              classify pids (add pid pidss)
		        in
		        Util.sort_by_dec List.length (classify pids [])
		      in
		      let _ =
								  Global.log (fun () ->
				        let _ = Format.printf "classified pids:@,  @[<v>" in
				        let _ =
				          let pr_aux ppf pids =
				            Format.fprintf ppf "[%a]" (Util.pr_list Var.pr ";") pids
				          in
				          Format.printf "%a" (Util.pr_list pr_aux ",") pidss
				        in
				        Format.printf "@]@,")
		      in
		      let pids = List.hd pidss in
		      let sol =
		        if List.length pids = 1 then
		          let xtys, [lb], [ub] = lbs_ubs_of pids in
		          let nub = Formula.bnot ub in
		          let _ = Global.log (fun () -> Format.printf "lb:%a@,nub:%a@," Term.pr lb Term.pr nub) in
		          let interp =
		            try
		              CsisatInterface.interpolate_bvs (fun x -> List.mem_assoc x xtys || Var.is_coeff x) lb nub
		            with CsisatInterface.NoInterpolant ->
		              raise NoSolution
		            | CsisatInterface.Unknown ->
		              raise (Util.NotImplemented "integer interpolation")
		          in
		          [List.hd pids, (xtys, interp)]
		        else
		          (*let hcs = inline_forward (fun pid -> not (List.mem pid pids)) hcs in*)
		          let xtys, lbs, ubs = lbs_ubs_of pids in
		          let lb = Formula.bor lbs in
		          let nub = Formula.bor (List.map Formula.bnot ubs) in
		          let _ = Global.log (fun () -> Format.printf "lb:%a@,nub:%a@," Term.pr lb Term.pr nub) in
		          let lb' = ApronInterface.convex_hull lb in
		          let nub'= ApronInterface.convex_hull nub in
		          let _ = Global.log (fun () -> Format.printf "lb':%a@,nub':%a@," Term.pr lb' Term.pr nub') in
		          try
		            let interp = CsisatInterface.interpolate_bvs (fun x -> List.mem_assoc x xtys || Var.is_coeff x) lb' nub' in
		            List.map (fun pid -> pid, (xtys, interp)) pids
		          with CsisatInterface.NoInterpolant | CsisatInterface.Unknown ->
		            Util.map3
		              (fun pid lb ub ->
		                let interp =
		                  let nub = Formula.bnot ub in
		                  try
		                    CsisatInterface.interpolate_bvs (fun x -> List.mem_assoc x xtys || Var.is_coeff x) lb' nub
		                  with CsisatInterface.NoInterpolant | CsisatInterface.Unknown ->
		                    try
		                      CsisatInterface.interpolate_bvs (fun x -> List.mem_assoc x xtys || Var.is_coeff x) lb nub
		                    with CsisatInterface.NoInterpolant ->
		                      raise NoSolution
		                    | CsisatInterface.Unknown ->
		                      raise (Util.NotImplemented "integer interpolation")
		                in
		                pid, (xtys, interp))
		              pids lbs ubs
		      in
		      let sol, hcs =
		        if List.length pids = 1 then
		          sol, List.map (TypPredSubst.subst sol) hcs
		        else
										  if true then
												  let sol0 =
														  (** @todo implement topological sort of sol with resepect to the parent-child relation of the tree of Horn clauses *)
														  List.sort
				              ~cmp:(fun (pid1, _) (pid2,_) -> if pid1 = pid2 then 0 else if Var.lt pid1 pid2 then 1 else -1)
				              sol
														in
														let hcs, sol =
																List.fold_left
																  (fun (hcs, sol) s ->
								            let hcs' = List.map (TypPredSubst.subst [s]) hcs in
								            let t = HcSolve.formula_of_forward (compute_lbs hcs') hcs' in
								            if Cvc3Interface.is_valid (Formula.bnot t) then
								              hcs', s::sol
								            else
								              hcs, sol)
																  (hcs0, [])
																		sol0
														in
														if sol = [] then
																let _ = Format.printf "Horn clauses:@,  @[<v>%a@]@," HornClause.pr hcs0 in
																let _ = Format.printf "solution:@,  @[<v>%a@]@," TypPredSubst.pr sol0 in
																assert false
														else
		  												sol, hcs
												else (** the following code is faster but incorrect? *)
												  let sol0 =
														  (** @todo implement topological sort of sol with resepect to the parent-child relation of the tree of Horn clauses *)
				            List.sort
				              ~cmp:(fun (pid1, _) (pid2,_) -> if pid1 = pid2 then 0 else if Var.lt pid1 pid2 then -1 else 1)
				              sol
														in
				          let rec valid_sol sol hcs =
														  match sol with
				              [] ->
																				let _ = Format.printf "Horn clauses:@,  @[<v>%a@]@," HornClause.pr hcs0 in
																				let _ = Format.printf "solution:@,  @[<v>%a@]@," TypPredSubst.pr sol0 in
																				assert false (*raise (Util.NotImplemented "HcGenSolve.solve")*)
				            | _ ->
								            let hcs' = List.map (TypPredSubst.subst sol) hcs in
								            let t = HcSolve.formula_of_forward (compute_lbs hcs') hcs' in
								            if Cvc3Interface.is_valid (Formula.bnot t) then
								              sol, hcs'
								            else
								              valid_sol (List.tl sol) hcs
				          in
				          valid_sol sol0 hcs0
		      in
		      sol @ solve hcs
  in
  let _ = Global.log_end "HcGenSolve.solve" in
  sol

let solve hcs =
  let _ = Global.log_begin "solving Horn clauses" in
  let sol = solve hcs in
  let _ = if !Global.debug then TypPredSubst.check sol hcs in
  let _ = Global.log (fun () -> Format.printf "solution:@,  @[<v>%a@]" TypPredSubst.pr sol) in
  let _ = Global.log_end "solving Horn clauses" in
  sol
