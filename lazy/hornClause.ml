open ExtList
open ExtString

(** Horn clauses *)

(** Horn clause
    @assume an expression containing a coefficient variables never occurs in an argument of a predicate*)
type t = Hc of (Var.t * (Var.t * SimType.t) list) option * Pred.t list * Term.t

let pr ppf (Hc(popt, ps, t)) =
		let _ = Format.fprintf ppf "@[<hov>" in
		let _ =
    if ps <> [] then
      Format.fprintf ppf "%a,@ " (Util.pr_list Pred.pr ",@ ") ps
  in
		let _ = Format.fprintf ppf "%a@ " Term.pr t in
  match popt with
    None ->
      Format.fprintf ppf "|- bot@]"
  | Some(pid, xtys) ->
      Format.fprintf ppf "|- %a@]"
        Pred.pr
        (Pred.make pid (List.map (fun (x, ty) -> Term.make_var x, ty) xtys))

(** require: variables that popt depend do not occur in the domain of sub *)
let subst sub (Hc(popt, ps, t)) =
  Hc(popt, List.map (Pred.subst sub) ps, Term.subst sub t)

let fvs (Hc(popt, ps, t)) =
  Util.diff
    (List.unique (Util.concat_map Pred.fvs ps @ Term.fvs t))
    (match popt with None -> [] | Some(_, xtys) -> List.map fst xtys)

let coeffs (Hc(popt, ps, t)) =
  List.unique (Util.concat_map Pred.coeffs ps @ Term.coeffs t)

let get_lhs_pids hcs =
  Util.concat_map
    (fun (Hc(_, ps, _)) ->
      List.map fst ps)
    hcs

let alpha (Hc(popt, ps, t) as hc) =
  let fvs = fvs hc in
  let sub = List.map (fun x -> x, Term.make_var (Var.new_var ())) fvs in
  Hc(popt,
    List.map (Pred.subst (fun x -> List.assoc x sub)) ps,
    Term.subst (fun x -> List.assoc x sub) t)

let lookup_hcs (pid, ttys) hcs =
  let hcs' =
    List.find_all
      (function Hc(Some(pid', _), _, _) -> pid = pid' | _ -> false)
						hcs
  in
		match hcs' with
    [Hc(Some(_, xtys), _, _) as hc] ->
						let Hc(_, ps, t) = alpha hc in
						let sub = List.combine (List.map fst xtys) (List.map fst ttys) in
						List.map (Pred.subst (fun x -> List.assoc x sub)) ps,
						Term.subst (fun x -> List.assoc x sub) t
  | [] -> raise Not_found
  | _ -> assert false

(** unsound for non-linear expressions? *)
let rec subst_formula p ps t =
  (*if Term.coeffs t <> [] then
    ps, t
  else*)
		(*Format.printf "input: %a@," Term.pr t;*)
		let ts = Formula.conjuncts t in
		let xttys, t = Tsubst.extract_from2 (Util.concat_map Pred.fvs ps) p ts in
		(*Format.printf "xttys: %a@,t: %a@," Tsubst.pr xttys Term.pr t;*)
		let ps, t =
				if xttys = [] then
				  ps, t
				else
				  let sub = Tsubst.fun_of xttys in
				  subst_formula p
						  (List.map (Pred.subst_fixed sub) ps)
								(Term.subst_fixed sub t)
		in
		(*Format.printf "output: %a@," Term.pr t;*)
		ps, t

let matches env xs ttys1 ttys2 =
  let xttys =
				(*try*)
						Util.concat_map2
						  (fun (t1, ty1) (t2, ty2) ->
								  let _ = if !Global.debug then assert (ty1 = ty2) in
						    if t1 = t2 then
										  []
										else if Util.inter (Term.fvs t1) xs = [] then
										  if Cvc3Interface.implies env [Formula.eq_ty ty1 t1 t2] then
				  						  []
												else
												  let _ = Format.printf "t1: %a@,t2: %a@," Term.pr t1 Term.pr t2 in
												  assert false
										else
												match t1 with
												  Term.Var(_, x) when List.mem x xs ->
												    [x, t2, ty1]
												| _ ->
																(try
																  let nxs, n' = LinArith.of_term t1 in
																  match nxs with
																    [n, x] when n = 1 && List.mem x xs ->
						                [x, LinArith.simplify (Term.sub t2 (Term.tint n')), ty1]
																  | _ ->
																      raise (Invalid_argument "")
																with Invalid_argument _ ->
																  let _ = Format.printf "??t1: %a@,??t2: %a@," Term.pr t1 Term.pr t2 in
																		[](*raise Not_found*)(*assert false*)))
								ttys1 ttys2
						(*with Not_found ->
						  []*)
		in
		let _ =
				if !Global.debug then
		  		let xttys = List.unique xttys in
						assert
								(List.for_all
								   (fun xttys ->
													match xttys with
															[] -> assert false
													| (_, t, ty)::xttys ->
															  List.for_all (fun (_, t', _) -> Cvc3Interface.implies env [Formula.eq_ty ty t t']) xttys)
								   (Util.classify (fun (x, _, _) (y, _, _) -> x = y) xttys))
		in
		xttys


let xttyss_of env q ps1 ps2 =
  try
		  let ttys_tss_s =
				  List.map
						  (fun p1 ->
								  snd p1,
										let ps =
												List.filter_map
												  (fun p2 ->
														  if Pred.matches q env p2 p1 then
																  Some(List.map fst (snd p2))
															 else
																  None)
														ps2
										in
										if ps = [] then raise Not_found else ps)
								ps1
				in
				let xttyss =
						Util.multiply_list_list
						  (fun xttys1 xttys2 -> xttys1 @ xttys2)
						  (List.map
						    (fun (ttys, tss) ->
						      List.map
						        (fun ts ->
														  Util.concat_map2
																  (fun (t1, ty) t2 ->
						              if t1 = t2 then
																				  []
																				else
																						match t1 with
																						  Term.Var(_, x) when q x ->
																						    [x, t2, ty]
																						| _ ->
																										(try
																										  let nxs, n' = LinArith.of_term t1 in
																										  match nxs with
																										    [n, x] when n = 1 && q x ->
																                [x, LinArith.simplify (Term.sub t2 (Term.tint n')), ty]
																										  | _ ->
																										      raise (Invalid_argument "")
																										with Invalid_argument _ ->
																										  []))
																		ttys ts)
										  tss)
								  ttys_tss_s)
				in
				let xttyss =
				  List.filter
						  (fun xttys ->
								  let xttys = List.unique xttys in
										if List.for_all
										     (fun xttys ->
																	match xttys with
																	  [] -> assert false
																	| (_, t, ty)::xttys ->
																	    List.for_all (fun (_, t', _) -> Cvc3Interface.implies env [Formula.eq_ty ty t t']) xttys)
										     (Util.classify (fun (x, _, _) (y, _, _) -> x = y) xttys) then
										  true
										else
								    (*let _ = Format.printf "duplicate: %a@," Tsubst.pr xttys in*)
										  false)
						  xttyss
				in
				xttyss
  with Not_found ->
		  []

let dup_num ps =
		let pss = Util.classify (fun (pid1, _) (pid2, _) -> pid1 = pid2) ps in
  List.fold_left (+) 0 (List.map (fun ps -> List.length ps - 1) pss)

let ignored_vars bvs ps =
  let tss =
    let pss = Util.classify (fun (pid1, _) (pid2, _) -> pid1 = pid2) ps in
    Util.concat_map
      (fun ps -> Util.transpose (List.map (fun p -> List.map fst (snd p)) ps))
      pss
  in
		let xs =
				Util.diff
		    (List.unique
		      (Util.concat_map
		        (fun ts ->
										  Util.redundant (Util.concat_map (fun t -> List.unique (Term.fvs t)) ts)
												(*Util.concat_map Term.fvs (Util.redundant ts)*))
		        tss))
		    bvs
  in
		let _ = Format.printf "xs: %a@," (Util.pr_list Var.pr ",") xs in
		let ys =
		  List.unique
				  (Util.concat_map
						  (fun (_, ttys) ->
								  List.flatten
										  (Util.map_left_right
												  (fun ttys1 (t, _) ttys2 ->
														  let zs = List.unique (Term.fvs t) in
																if List.length zs > 1 then
																  Util.diff zs (xs @ (Util.concat_map (fun (t, _) -> Term.fvs t) (*???*)(ttys1 @ ttys2)))
																else
																  [])
														ttys))
						  ps)
		in
		let _ = Format.printf "ys: %a@," (Util.pr_list Var.pr ",") ys in
		xs @ ys

let changing_vars bvs ps =
  let tss =
    let pss = Util.classify (fun (pid1, _) (pid2, _) -> pid1 = pid2) ps in
    Util.concat_map
      (fun ps -> Util.transpose (List.map (fun p -> List.map fst (snd p)) ps))
      pss
  in
		Util.diff
    (List.unique
      (Util.concat_map
        (fun ts -> let ts = List.unique ts in match ts with [_] -> [] | _ -> Util.concat_map Term.fvs ts)
        tss))
    bvs

let fvs_of_ec ec = List.unique (Util.concat_map (function `L(p) -> Pred.fvs p | `R(t) -> Term.fvs t) ec)
let pids_of_ec ec = Util.concat_map (function `L(pid, _) -> [pid] | `R(_) -> []) ec
let preds_of_ec ec = List.filter_map (function `L(p) -> Some(p) | `R(_) -> None) ec
let terms_of_ec ec = List.filter_map (function `L(_) -> None | `R(t) -> Some(t)) ec
let embed_preds ps = List.map (fun p -> `L(p)) ps
let embed_terms ts = List.map (fun t -> `R(t)) ts

let share_predicates bvs0 _ ps t =
  let debug = true in
		let t = Formula.simplify t in
		if Term.coeffs t <> [] || dup_num ps = 0 then
		  ps, t
  else
		  let share_predicates_aux cvs bvs ps t =
						let ts = Formula.conjuncts t in
		    let rec rel xs1 xs2 =
		      match xs1, xs2 with
						    `L((pid1, ttys1) as p1), `L((pid2, ttys2) as p2) ->
				        let fvs1 = Util.diff (Pred.fvs p1) bvs in
				        let fvs2 = Util.diff (Pred.fvs p2) bvs in
				        List.exists (fun x -> List.mem x fvs2) fvs1
						  | `L(p1), `R(t2) ->
				        let fvs1 = Util.diff (Pred.fvs p1) bvs in
				        let fvs2 = Util.diff (Term.fvs t2) bvs in
				        List.exists (fun x -> List.mem x fvs2) fvs1
						  | `R(t1), `L(p2) ->
				        rel (`L(p2)) (`R(t1))
						  | `R(t1), `R(t2) ->
				        let fvs1 = Util.diff (Term.fvs t1) bvs in
				        let fvs2 = Util.diff (Term.fvs t2) bvs in
				        List.exists (fun x -> List.mem x fvs2) fvs1
		    in
		    let ecs, env, zs =
				    let env, ts1 = List.partition (fun t -> Util.subset (Term.fvs t) bvs) ts in
				    let ps0, ps1 = List.partition (fun p -> Util.subset (Pred.fvs p) bvs) ps in
				    let ps0 =
				      Util.concat_map
				        (Util.representatives (Pred.equiv env))
				        (Util.classify (fun (pid1, _) (pid2, _) -> pid1 = pid2) ps0)
				    in
								let ecs = Util.equiv_classes rel (embed_preds ps1(* redundant *) @ embed_terms ts1) in
								let zs =
  								let zs = Util.diff (List.unique (Util.concat_map Pred.fvs ps0)) bvs0 in
										List.filter
										  (fun z ->
      								let ecs' = List.filter (fun ec -> List.mem z (fvs_of_ec ec)) ecs in
														match ecs' with
														  [_] -> true
														| _ -> false)
												zs
								in
        (if ps0 = [] then [] else [embed_preds ps0]) @ ecs,
        env,
								zs
      in
						let ecs =
						  List.map
								  (fun ec ->
										  if preds_of_ec ec = [] then
														(try
														  let t = Formula.band (terms_of_ec ec) in
																let xs = List.unique (Util.diff (Term.fvs_ty SimType.Int t SimType.Bool) bvs) in
														  let ts = Formula.conjuncts (AtpInterface.integer_qelim (Formula.exists (List.map (fun x -> x, SimType.Int) xs) t)) in
																List.map (fun t -> `R(t)) ts
														with Util.NotImplemented _ -> ec)
												else
												  ec)
										ecs
						in
		    let _ =
		      Format.printf "bvs: %a@," (Util.pr_list Var.pr ",") bvs;
		      Format.printf "env: %a@," Term.pr (Formula.band env);
		      List.iter
		        (fun ec ->
		          let ps, ts = Util.partition_map (fun x -> x) ec in
		          Format.printf "ec: %a@," pr (Hc(None, ps, Formula.band ts)))
		        ecs
		    in
						let is_covered ec1 ec2 =
								let ts0 = env @ terms_of_ec ec2 in
						  let rec aux pxs ts =
										let pxs = List.filter (fun (_, ttys, _, ttyss) -> List.for_all (fun ttys' -> ttys' <> ttys) ttyss) pxs in
										let ts = Util.diff ts ts0 in
										match List.filter (fun (_, _, xs, _) -> xs <> []) pxs with
										  [] ->
														let b =
										      Cvc3Interface.implies ts0 ts &&
												    List.for_all
										        (fun (pid, ttys, _, ttyss) ->
																				List.exists (fun ttys' -> Pred.equiv env (pid, ttys) (pid, ttys')) ttyss)
										        pxs
														in
														let _ =
																if b then
														    Format.printf "succeeded@,"
																else
																  let _ = Format.printf "ts: %a@," (Util.pr_list Term.pr ",") ts in
																  let _ = List.iter (fun (pid, ttys, _, _) -> Format.printf "ttys: %a@," (Util.pr_list Term.pr ",") (List.map fst ttys)) pxs in
														    Format.printf "failed:@,"
														in
														b
														(*let pxs' =
										      List.filter
								          (fun (pid, ttys, _, ttyss) ->
																		  List.for_all (fun ttys' -> not (Pred.equiv env (pid, ttys) (pid, ttys'))) ttyss)
								          pxs
														in
														(match pxs' with
														  [] ->
																  true
														| [pid, ttys, _, ttyss] ->
														    let xs = Util.diff (List.unique (Util.concat_map (fun (t, _) -> Term.fvs t) ttys)) bvs0 in
																		let _ = Format.printf "osii: %a@," (Util.pr_list Var.pr ",") xs in
														    false
														| _ -> false)*)
										| (pid, ttys, xs, ttyss)::_ ->
														let xttyss =
														  List.filter_map
																  (fun ttys' ->
																		  match matches env xs ttys ttys' with
																				  [] -> None
																				| xttys -> Some(xttys)
																		  (*match xttyss_of env (fun x -> List.mem x xs) [pid, ttys] [pid, ttys'] with
																				  [] -> None
																				| [xttys] -> Some(xttys)
																				| _ -> assert false*))
																		ttyss
														in
														List.exists
														  (fun xttys ->
																		let _ = Format.printf "xttys: %a@," Tsubst.pr xttys in
																  let ys = List.map Util.fst3 xttys in
																		let pxs =
																				List.sort
																						~cmp:(fun (_, _, _, ttyss1) (_, _, _, ttyss2) -> List.length ttyss1 - List.length ttyss2)
																				  (List.map
																						  (fun (pid, ttys, xs0, ttyss) ->
																										let xs = Util.diff xs0 ys in
																								  if xs <> xs0 then
																										  let _ = Format.printf "pid: %a@," Var.pr pid in
																										  let pid, ttys = Pred.simplify (Pred.subst (Tsubst.fun_of xttys) (pid, ttys)) in
																												let ttyss =
				  																						  List.filter (fun ttys' -> Pred.matches (fun x -> List.mem x xs) env (pid, ttys') (pid, ttys)) ttyss
																												in
																												pid, ttys, xs, ttyss
																										else
																										  pid, ttys, xs0, ttyss)
																								pxs)
																		in
																		let ts = List.map (fun t -> Formula.simplify (Term.subst (Tsubst.fun_of xttys) t)) ts in
																  let b = aux pxs ts in
																		let _ = if not b then Format.printf "backtracked@," in
																		b)
																xttyss
								in
								let ps = preds_of_ec ec2 in
								aux
								  (List.sort
												~cmp:(fun (_, _, _, ttyss1) (_, _, _, ttyss2) -> List.length ttyss1 - List.length ttyss2)
												(List.map
												  (fun ((pid, ttys) as p1) ->
														  let xs = Util.diff (Pred.fvs (pid, ttys)) bvs in
																let ttyss =
																		List.filter_map
																				(fun ((_, ttys') as p2) ->
																						if Pred.matches (fun x -> List.mem x xs) env p2 p1 then Some(ttys') else None)
																				ps
																in
														  pid, ttys, xs, ttyss)
														(preds_of_ec ec1)))
										(terms_of_ec ec1)
						in
		    (*let reduce ec1 ec2 =
        ec1 = [] ||
		      let xs = List.sort (Util.diff (fvs_of_ec ec1) bvs) in
        if xs = [] then
          false
        else
				    		let _ = if debug then Global.log (fun () -> Format.printf "xs: %a@," (Util.pr_list Var.pr ",") xs) in
						    let xttyss =
				        let ps1 = preds_of_ec ec1 in
				        let ps2 = preds_of_ec ec2 in
  										let xttyss = xttyss_of env (fun x -> not (List.mem x bvs)) ps1 ps2 in
										  List.filter
												  (fun xttys ->
														  if Util.subset xs (List.map Util.fst3 xttys) then
																  true
																else
																  (*let _ = Format.printf "non-covered: %a@," Tsubst.pr xttys in*)
																		false(*assert false*))
														xttyss
						    in
				      if List.exists
				           (fun xttys ->
				             let b =
																			let ec1' =
						               List.map
						                 (function
						                   `L(p) -> `L(Pred.simplify (Pred.subst (Tsubst.fun_of xttys) p))
						                 | `R(t) -> `R(Formula.simplify (Term.subst (Tsubst.fun_of xttys) t)))
						                 ec1
						             in
		  		             let ps1, ts1 = Util.partition_map (fun x -> x) (Util.diff ec1' ec2) in
		                 let ps2, ts2 = Util.partition_map (fun x -> x) ec2 in
				               let _ = if debug then Format.printf "hc1: %a@,hc2: %a@," pr (Hc(None, ps1, Formula.band ts1)) pr (Hc(None, ps2, Formula.band ts2)) in
				               Cvc3Interface.implies (env @ ts2) ts1 &&
				               List.for_all
		                   (fun p1 -> List.exists (fun p2 -> Pred.equiv env(*@ ts2 not necessary?*) p1 p2) ps2)
		                   ps1
				             in
				             let _ = if b then Format.printf "xttys: %a@," Tsubst.pr xttys in
				             b)
				           xttyss then
				        true
				      else
				        false
		    in*)
		    let rec aux ecs1 ecs2 =
		      match ecs1 with
		        [] -> ecs2
		      | ec::ecs1 ->
            (*let covers ec1 ec2 =
				          Util.subset
                (Util.concat_map (function `L(p) -> [fst p] | `R(_) -> []) ec1)
		              (Util.concat_map (function `L(p) -> [fst p] | `R(_) -> []) ec2)
            in*)
            (*let min_coverings nonms ec ecs =
              let pids0 = pids_of_ec ec in
              let ec_pids_s =
                let ec_pids_s = List.map (fun ec -> List.sort ec, pids_of_ec ec) ecs in
		  												List.filter (fun (_, pids) -> List.exists (fun pid -> List.mem pid pids) pids0) ec_pids_s
														in
              let ec_pids_s1, ec_pids_s2 = List.partition (fun (ec, pids) -> (if nonms then Util.subset else Util.subset_ms) pids0 pids) ec_pids_s in
              let rec aux ec_pids_b_s =
                let ec_pids_b_s' =
																  List.unique ~cmp:(fun (ec1, _, _) (ec2, _, _) -> ec1 = ec2)
		                  (Util.concat_map
		                    (fun (ec, pids, b) ->
		                      if b then
		                        [ec, pids, b]
		                      else
		                        let insf_pids = (if nonms then Util.diff else Util.diff_ms) pids0 pids in
		                        List.map
		                          (fun (ec', pids') ->
																												  if nonms then
		                              List.sort (ec @ ec'), List.unique (pids @ pids'), Util.subset insf_pids pids'
																														else
		                              List.sort (ec @ ec'), pids @ pids', Util.subset_ms insf_pids pids')
		                          (List.filter
		                            (fun (ec', pids') ->
		                              Util.intersects insf_pids pids' && not (Util.subset ec' ec))
		                            ec_pids_s2))
																						ec_pids_b_s)
																in
                if List.length ec_pids_b_s = List.length ec_pids_b_s' then
                  let ecs = List.filter_map (fun (ec, _, b) -> if b then Some(ec) else None) ec_pids_b_s' in
                  (if nonms then
																		  []
																		else
																		  List.filter_map
																				  (fun (ec, pids, b) ->
																						  if not b && Util.subset pids0 pids && List.for_all (fun ec' -> Util.diff ec ec' <> []) ecs then
																								  Some(ec)
																								else
																								  None)
																						ec_pids_b_s') @
																		ecs
                else
                  aux ec_pids_b_s'
              in
              List.map fst ec_pids_s1 @
														aux (List.map (fun (ec, pids) -> ec, pids, false) ec_pids_s2)
            in*)
												let b =
												  match cvs with
														  Some(xs) when not (Util.intersects xs (fvs_of_ec ec)) ->
																  false
														| _ ->
		    												let _ = Format.printf "checking: %a@," (Util.pr_list Pred.pr ",") (preds_of_ec ec) in
																  (*if true then*)
																		  is_covered ec (List.flatten (ecs1 @ ecs2))
																		(*else
								            let ecs = min_coverings (!Global.disable_pred_sharing1 || not (Util.is_dup (pids_of_ec ec))) ec (ecs1 @ ecs2) in
																				let _ = List.iter (fun ec -> Format.printf "%a@," (Util.pr_list Pred.pr ",") (preds_of_ec ec)) ecs in
																				List.exists (fun ec' -> reduce ec ec') ecs*)
												in
		          aux ecs1 (if b then ecs2 else ec :: ecs2)
		    in
		    let ecs = aux (List.sort ecs) [] in
		    let ps, ts = Util.partition_map (fun x -> x) (List.flatten ecs) in
		    ps, Formula.band (ts @ env), zs
		  in
    let rec loop cvs bvs' ps t =
		    let _ = if bvs' <> [] then Format.printf "bvs': %a@," (Util.pr_list Var.pr ",") bvs' in
      let ps', t', zs = share_predicates_aux cvs (bvs0 @ bvs') ps t in
		    if List.length ps <> List.length ps' && dup_num ps' > 0 then
						  let bvs'' = ignored_vars bvs0 ps' in
								let cvs' = Util.diff bvs' bvs'' in
		      if cvs' = [](*Util.set_equiv bvs' bvs''*) then
		        bvs'', ps', t', zs
		      else
		        loop (Some(cvs')) bvs'' ps' t'
		    else
		      bvs', ps', t', zs
    in
    let bvs', ps, t, zs = loop None (ignored_vars bvs0 ps) ps t in
    if not !Global.enable_pred_sharing2 then
						if false then
  						ps, t
						else (*a-maxÇ™rsn 0Ç≈Ç»Ç¢Ç∆ê¨å˜ÇµÇ»Ç≠Ç»ÇÈ intro3ÇÕrsn0Ç≈OKÇ…Ç»ÇÈ*)
				    try
  		      let _ = if zs <> [] then Format.printf "zs: %a@," (Util.pr_list Var.pr ",") zs in
				      Util.find_map
				        (fun xs ->
												  let bvs1 = Util.diff bvs' xs in
														if List.length bvs1 = List.length bvs' then
														  raise Not_found
														else
						          let ps', t', _ = share_predicates_aux (Some(xs)) (bvs0 @ bvs1) ps t in
						          if List.length ps <> List.length ps' then
						            ps', t'
						          else
						            raise Not_found)
				        (Util.pick 1 zs)
				    with Not_found ->
				      ps, t
				else
		    try
		      let zs = Util.inter bvs' (changing_vars bvs0 ps) in
		      let _ = if zs <> [] then Format.printf "zs: %a@," (Util.pr_list Var.pr ",") zs in
		      Util.find_map
		        (fun xs ->
										  let bvs1 = Util.diff bvs' xs in
												if List.length bvs1 = List.length bvs' then
														raise Not_found
												else
				          let ps', t', _ = share_predicates_aux (Some(xs)) (bvs0 @ bvs1) ps t in
				          if List.length ps <> List.length ps' then
				            ps', t'
				          else
				            raise Not_found)
		        (Util.pick 1 zs @ Util.pick 2 zs)
		    with Not_found ->
		      ps, t

let simplify_aux bs (Hc(popt, ps, t)) =
  let _ = Global.log_begin "HornClause.simplify" in
  let _ = Global.log (fun () -> Format.printf "input:@,  @[<v>%a@]@," pr (Hc(popt, ps, t))) in
  let shared = ref (List.length ps) in
  let bvs = (match popt with None -> [] | Some(_, xtys) -> List.map fst xtys) in
  let bs, ps, t =
    (*let _ = Global.log_begin "simplifying formula" in
    let _ = Global.log (fun () -> Format.printf "input:@,  @[<v>%a@]@," Term.pr t) in*)
    let ps, t =
		    let sub, t =
		      Tsubst.extract_from
		        (match popt with None -> [] | Some(pid, _) -> [pid])
		        (fun x -> List.mem x bvs || Var.is_coeff x) t
		    in
      List.map (Pred.subst sub) ps, Term.subst sub t
    in
    (*let _ = Global.log (fun () -> Format.printf "a:@,  @[<v>%a@]@," Term.pr t) in*)
    let t =
      let xs = List.unique (bvs @ Util.concat_map Pred.fvs ps) in
      AtpInterface.simplify2 (fun x -> List.mem x xs || Var.is_coeff x) t
      (*
      let t = Term.simplify (AtpInterface.qelim_fes (diff bvs (fvs ps)) t) in
      *)
    in
    (*let _ = Global.log (fun () -> Format.printf "b:@,  @[<v>%a@]@," Term.pr t) in*)
    let ps, t =
				  (*let rec aux ts1 ts2 =
						  match ts1 with
						    [] -> Formula.band ts2
						  | t::ts' ->
				        if Cvc3Interface.implies (ts' @ ts2) [t] then
				          aux ts' ts2
				        else
				          aux ts' (t::ts2)
				  in
				  if true then aux (Formula.conjuncts t) [] else t*)
      let sub = List.filter_map (fun t -> try Some(Formula.xtty_of (fun x -> not (List.mem x bvs)) [] t) with Not_found -> None) (Formula.conjuncts t) in
      let sub = List.filter (fun (_, t, ty) -> ty = SimType.Bool && (t = Formula.ttrue || t = Formula.tfalse) (*|| ty = SimType.Int && Term.is_int_const t*)) sub in
      let t0 =
        Formula.simplify
          (Formula.band
            (Term.subst (Tsubst.fun_of sub) t ::
            List.map
              (fun (x, t, _) ->
														  if t = Formula.ttrue then
																  Term.make_var x
																else if t = Formula.tfalse then
																  Formula.bnot (Term.make_var x)
																else
																  (*if Term.is_int_const t then Formula.eqInt (Term.make_var x) t else*) assert false)
              sub))
      in
      let ps0 = ps(*List.map (Pred.subst (Tsubst.fun_of sub)) ps*) in
      (*let _ = Format.printf "a:%a@," Term.pr t0 in*)
      ps0,
      if Term.fvs_ty SimType.Bool t0 SimType.Bool = [] then
        let t' = Formula.elim_eq_neq_boolean t0 in
        (*let _ = Format.printf "b:%a@," Term.pr t' in*)
        let t' = Formula.elim_imply_iff t' in
        (*let _ = Format.printf "c:%a@," Term.pr t' in*)
        if t0 <> t' then
          let t' = Formula.formula_of_dnf (Formula.dnf t') in
          (*let _ = Format.printf "d:%a@," Term.pr t' in*)
          let t' = Formula.simplify t' in
          let _ = Format.printf "boolean equalities eliminated:@,  @[<v>before: %a@,after: %a@]@," Term.pr t Term.pr t' in
          t'
        else
          t0
      else
        t0
    in
    (*let _ = Global.log (fun () -> Format.printf "c:@,  @[<v>%a@]@," Term.pr t) in*)
    let ps, t = subst_formula (fun x -> List.mem x bvs || Var.is_coeff x) ps t in
    (*let _ = Global.log (fun () -> Format.printf "output:@,  @[<v>%a@]" Term.pr t) in
    let _ = Global.log_end "simplifying formula" in*)
    let ps = List.map Pred.simplify ps in
    let rec unique bs ps =
      match bs, ps with
         [], [] ->
           [], []
      |  b::bs, p::ps ->
           if List.mem p ps then
             let bs, ps = List.split (Util.filter_map2 (fun b p' -> if p <> p' then Some(b, p') else None) bs ps) in
             let bs, ps = unique bs ps in
             false(*??*) :: bs, p :: ps
           else
             let bs, ps = unique bs ps in
             b :: bs, p :: ps
    in
    let bs, ps = unique bs ps in
    bs, ps, t
  in
  let ps, t = share_predicates bvs bs ps t in
  let res = Hc(popt, ps, t) in
  let _ =
    let _ = shared := !shared - List.length ps in
    Global.log (fun () -> if !shared <> 0 then Format.printf "# of shared predicate variables: %d@," !shared)
  in
  let _ =
    let n = dup_num ps in
    Global.log (fun () -> if n <> 0 then Format.printf "# of duplicate predicate variables: %d@," n)
  in
  let _ = Global.log (fun () -> Format.printf "output:@,  @[<v>%a@]" pr res) in
  let _ = Global.log_end "HornClause.simplify" in
  res

let simplify (Hc(_, ps, _) as hc) = simplify_aux (List.map (fun _ -> false(*???*)) ps) hc

let subst_hcs hcs (Hc(popt, ps, t) as hc) =
  let _ = Global.log_begin "subst_hcs" in
  let _ = Global.log (fun () -> Format.printf "input:@,  @[<v>%a@]@," pr hc) in
  let hc =
		  if !Global.subst_hcs_inc then
      let rec aux ps t =
        try
		        let lps, (ps, t'), rps =
				        Util.find_split_map
				          (fun (pid, ttys) ->
																let res = lookup_hcs (pid, ttys) hcs in
												    let _ = Global.log (fun () -> Format.printf "%a is substituted@," Var.pr pid) in
				            res)
				          ps
		        in
		        let Hc(_, ps, t) =
            let bs =
              List.map (fun _ -> false) lps @
              List.map (fun _ -> true) ps @
              List.map (fun _ -> false) rps
            in
            simplify_aux bs (Hc(popt, lps @ ps @ rps, Formula.band [t; t']))
          in
		        aux ps t
        with Not_found ->
          Hc(popt, ps, t)
      in
      aux ps t
		  else
				  let bs, ps, t =
				    let bss, pss, ts =
						    Util.unzip3
										  (List.map
										    (fun (pid, ttys) ->
						          try
														    let res = lookup_hcs (pid, ttys) hcs in
						            let _ = Global.log (fun () -> Format.printf "%a is being substituted@," Var.pr pid) in
						            List.map (fun _ -> true) (fst res), fst res, snd res
						          with Not_found ->
						            [false], [pid, ttys], Formula.ttrue)
										    ps)
				    in
				    List.flatten bss, List.flatten pss, Formula.band (t::ts)
				  in
		    simplify_aux bs (Hc(popt, ps, t))
  in
  let _ = Global.log (fun () -> Format.printf "output:@,  @[<v>%a@]" pr hc) in
  let _ = Global.log_end "subst_hcs" in
  hc

let subst_hcs_fixed hcs hc =
		Util.fixed_point
				(fun hc ->
		    (*Format.printf "%a@." pr hc;*)
		    subst_hcs hcs hc)
				(fun hc1 hc2 ->
		    match hc1, hc2 with
		      Hc(_, ps1, _), Hc(_, ps2, _) -> Util.set_equiv ps1 ps2)
		  hc
(*
let rec fixpoint hcs =
  match hcs with
    [] -> []
  | hc::hcs' ->
      let hc' = subst_hcs_fixed hcs' hc in
      hc' :: fixpoint (List.map (subst_hcs [hc']) hcs')
  
let subst_hcs_fixed hcs hc =
  subst_hcs (fixpoint hcs) hc
*)
