open ExtList
open ExtString
open Cgen

(* generate a set of constraints from an error trace *)
let cgen etr =
  let rec aux (Loc(tr, p) as loc) etr0 =
    match etr0 with
      [] ->
        assert false
    | s::etr ->
        (match s with
          CompTree.Call(y, g) ->
            if Var.is_top (fst y) then
              aux (insert_down loc (make y true [g] [])) etr
            else if Var.is_pos (fst y) then (* changed *)
              let _ = assert (g = Term.ttrue) in
              aux (insert_down loc (make y true [g] [])) etr
            else if Var.is_neg (fst y) then (* changed *)
(*              if Flags.use_ret then*)
		              let nd = get tr in
		              let nd' = { nd with ret = Some(y) } in
				            aux (up (Loc(set tr nd', p))) etr
(*              else
		              let _ = assert (g = Term.ttrue) in
		              aux (insert_down loc (make y true [g] [])) etr*)
            else assert false
        | CompTree.Arg(xttys) ->
            let xttys = List.filter (fun (_, _, ty) -> SimType.is_base ty) xttys in
            let nd = get tr in
            aux (Loc(set tr { nd with constr = nd.constr @ [Term.ttrue]; subst = nd.subst @ [xttys] }, p)) etr
        | CompTree.Ret(x, t, ty) ->
            let xttys = List.filter (fun (_, _, ty) -> SimType.is_base ty) [x, t, ty] in
            let nd = get tr in
            let nd' = { nd with constr = nd.constr @ [Term.ttrue]; subst = nd.subst @ [xttys] } in
            let Var.T(f, _, _) = x in
            if Var.is_pos f then
              aux (up (Loc(set tr nd', p))) etr
            else if Var.is_neg f then (* changed *)
(*              if Flags.use_ret then*)
                aux (insert_down (Loc(set tr nd', p)) (make (Var.fc_ref_of f) true [Term.ttrue] [])) etr
(*              else
                aux (up (Loc(set tr nd', p))) etr*)
            else assert false
        | CompTree.Nop ->
            aux loc etr
        | CompTree.Error ->
            let _ = assert (etr = []) in
            let nd = get tr in
            root (Loc(set tr { nd with closed = false }, path_set_open p)))
  in
  match etr with
    CompTree.Call(x, g)::etr ->
      let tr = aux (zipper (make x true [g] [])) etr in
      let tr' = (*get_unsat_prefix*) tr in
(*
      let _ = Format.printf "%a@.%a@." pr tr pr tr' in
*)
      tr'
  | _ -> assert false

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

(* assume that x is a structured variable *)
let rec visible x y =
  match x with
    Var.V(_) ->
      false
  | Var.T(z, uid, arg) ->
      (match y with
        Var.V(_) -> false
      | Var.T(z', uid', arg') ->
				      (z = z' && uid = uid' && arg' <= arg) ||
				      (visible z y))

(*
let rec enum_visible x =
  match x with
    Var.V(_) ->
      [x]
  | Var.T(x, uid, arg) ->
      enum_visible x @ List.init (arg + 1) (fun i -> Var.T(x, uid, i))
*)

let args_of_tree env tr =
  (* ToDo: not enought for higher-order functions *)
  let x, uid = (get tr).name in
  let n = SimType.arity (env x) in
  List.init n (fun i -> Var.T(x, uid, i))

let ret_of_tree env tr =
  (* ToDo: not enought for higher-order functions *)
  let x, uid = (get tr).name in
		match (get tr).ret with
		  None ->
		    Var.T(x, uid, SimType.arity (env x))
		| Some(x, uid) ->
						SimType.find_last_base env (x, uid)

let arg_of env nd =
		SimType.find_last_base env nd.name

let ret_of env nd =
  let x, uid = nd.name in
		match nd.ret with
				None ->
				  Var.T(x, uid, SimType.arity (env x))
		| Some(x, uid) ->
						SimType.find_last_base env (x, uid)

(* assume that all the substituted nodes are closed *)
let subst_interps env tr ret_interp_list =
  let rec aux (Node(nd, trs) as tr) =
    if trs = [] then
      try
		      `L(List.assoc (ret_of env nd) ret_interp_list)
      with Not_found ->
        `R(tr)
    else
      let rec aux2 trs ts xttyss =
        match trs, ts, xttyss with
          [], _, _ ->
            [], ts, xttyss
        | tr::trs, t::ts, xttys::xttyss ->
            let trs, ts, xttyss = aux2 trs ts xttyss in
            (match tr with
              `L(t') -> (match ts, xttyss with t''::ts, xttys'::xttyss -> trs, Term.band [t; t'; t''] :: ts, (xttys @ xttys') :: xttyss | _ -> assert false)
            | `R(tr) -> tr::trs, t::ts, xttys::xttyss)
      in
      let trs, ts, xttyss = aux2 (List.map aux trs) (List.tl nd.constr) nd.subst in
      `R(Node({ nd with constr = List.hd nd.constr :: ts; subst = xttyss }, trs))
  in
  match aux tr with `R(tr) -> tr | _ -> assert false

let subst_interp closed p interp =
		if closed then
		  match p with
		    Top -> assert false
		  | Path(up, trs1, nd, trs2) ->
        let ts1, t::ts2 = Util.split_at nd.constr (List.length trs1 + 1) in
        let xttyss1, xttys::xttyss2 = Util.split_at nd.subst (List.length trs1) in
        (* must not apply xttys to interp *)
        let ts2 = match ts2 with t'::ts2' -> (Term.band [t; interp; t'])::ts2' | [] -> assert false in
        let xttyss2 = match xttyss2 with xttys'::xttyss2' -> (xttys @ xttys')::xttyss2' | [] -> assert false in
        Some(root (Loc(Node({ nd with constr = ts1 @ ts2;
                                      subst = xttyss1 @ xttyss2 }, trs1 @ trs2), up)))
		else
		  match p with
		    Top -> let _ = assert (interp = Term.ttrue) in None
		  | Path(up, trs1, nd, []) ->
				    if Term.equiv interp Term.ttrue then
          let _ = Format.printf "stop propagation@." in
				      None
				    else
		        let ts1, t::[] = Util.split_at nd.constr (List.length trs1 + 1) in
		        let xttyss1, xttys::[] = Util.split_at nd.subst (List.length trs1) in
          (* must not apply xttys to interp *)
		        let interp, xttys =
		          (* apply xttys to interp: unsound try hrec.ml
				        let xts = List.map (fun (x, t, _) -> x, t) xttys in
				        let sub x = List.assoc x xts in
		          Term.subst sub interp,
            []*)
            interp, xttys
		        in
		        Some(root (Loc(Node({ nd with (**)ret = None;
				                                    closed = false;(**)
				                                    constr = ts1 @ [Term.band [t; Term.bnot interp]];
		                                      subst = xttyss1 @ [xttys] }, trs1), (**)path_set_open(**) up)))

let related n1 n2 =
  let tmp = Var.tlfc_of (Var.T(fst n1, snd n1, (*dummy*)-1)) in
  Var.ancestor_of tmp n2
let related_locs loc =
  let Loc(tr, _) = loc in
		find_all
		  (fun nd ->
		    related (get tr).name nd.name)
		  (root loc)

let rec prune_tree pred (Node(nd, trs)) =
  if pred nd.name then
    None
  else
    let trs = List.map (prune_tree pred) trs in
    let [[t1]; ts1; ts2] = Util.split nd.constr [1; List.length trs] in
    let [xttyss1; xttyss2] = Util.split nd.subst [List.length trs] in
    let res, xttys =
      List.fold_left
       (fun (res, xttys1) (t, xttys2, tr) ->
         match tr with
           None -> res, xttys1 @ xttys2
         | Some(tr) -> res @ [t, xttys1 @ xttys2, tr], [])
       ([], [])
       (Util.zip3 ts1 xttyss1 trs)
    in
    let (ts1, xttyss1, trs) = Util.unzip3 res in
    Some
      (Node({ nd with constr = List.flatten [[t1]; ts1; ts2];
                      subst = List.flatten [xttyss1; if xttys = [] then xttyss2 else (xttys @ List.hd xttyss2) :: List.tl xttyss2] },
            trs))

(* assume that for any nd in the spine, not (pred nd.name) *)
let rec prune_path pred p =
  match p with
    Top -> Top
  | Path(up, trs1, nd, trs2) ->
      let trs1 = List.map (prune_tree pred) trs1 in
      let trs2 = List.map (prune_tree pred) trs2 in
      let [[t1]; ts1; [t2]; ts2; ts3] = Util.split nd.constr [1; List.length trs1; 1; List.length trs2] in
      let [xttyss1; [xttys1]; xttyss2; xttyss3] = Util.split nd.subst [List.length trs1; 1; List.length trs2] in
      let (ts1, xttyss1, trs1), xttys1' =
				    let res, xttys =
				      List.fold_left
				       (fun (res, xttys1) (t, xttys2, tr) ->
				         match tr with
				           None -> res, xttys1 @ xttys2
				         | Some(tr) -> res @ [t, xttys1 @ xttys2, tr], [])
				       ([], [])
				       (Util.zip3 ts1 xttyss1 trs1)
				    in
        Util.unzip3 res, xttys
(*
        Util.unzip3
          (List.filter_map (fun x -> x)
            (Util.map3 (fun t xttys -> function (Some(tr)) -> Some(t, xttys, tr) | None -> assert (t = Term.ttrue); None) ts1 xttyss1 trs1))
*)
      in
      let (ts2, xttyss2, trs2), xttys2' =
				    let res, xttys =
				      List.fold_left
				       (fun (res, xttys1) (t, xttys2, tr) ->
				         match tr with
				           None -> res, xttys1 @ xttys2
				         | Some(tr) -> res @ [t, xttys1 @ xttys2, tr], [])
				       ([], [])
				       (Util.zip3 ts2 xttyss2 trs2)
				    in
        Util.unzip3 res, xttys
      in
      Path(prune_path pred up, trs1,
        { nd with constr = List.flatten [[t1]; ts1; [t2]; ts2; ts3];
                  subst = List.flatten [xttyss1; [xttys1' @ xttys1]; xttyss2; if xttys2' = [] then xttyss3 else (xttys2' @ List.hd xttyss3) :: List.tl xttyss3] }, trs2)

(* assume that all the related locations of loc is a leaf *)
let summary_of env loc =
  let Loc(Node(nd, []), p) = loc in
		let _ = if not nd.closed then assert (nd.ret = None) in
		let locs = related_locs loc in
  try
    if List.length locs = 1 then
						let arg = if nd.closed then ret_of env nd else arg_of env nd in
						let _ = Format.printf "computing a condition of %a:@.  @[<v>" Var.pr arg in
		    let interp =
								let tt = Term.simplify (qelim (visible arg) (term_of_nodes [nd])) in
								let tp = Term.simplify (qelim (visible arg) (term_of_nodes (nodes_of_path p))) in
								let t1, t2 = if nd.closed then tt, tp else tp, tt in
				    interpolate_bvs (visible arg) t1 t2
		    in
		    let _ = Format.printf "@]@." in
						[`P(arg, interp)], Util.opt2list (subst_interp nd.closed p interp)
    else (* necessary try repeat.ml *)
      raise CsisatInterface.No_interpolant
  with CsisatInterface.No_interpolant ->
    let _ = Format.printf "**** quick inference failed ****@]@." in
    let arg_p_interp_list, ret_interp_list =
						let arg_p_t_list =
						  List.map
						    (fun (Loc(tr, p)) ->
		          let p = left_of_path p in
            let p = (* necessary: try linmax.ml *)prune_path (fun name -> related nd.name name) p in
            (*let _ = Format.printf "%a@." pr_path p in*)
		          let arg = arg_of env (get tr) in
            let t = Term.rename_fresh (visible arg) (Term.simplify (qelim (visible arg) (term_of_nodes (nodes_of_path p)))) in
						      (**)let _ = Format.printf "%a: %a@." Var.pr arg Term.pr t in(**)
		          arg, p, t)
		        locs
      in
      if nd.closed then
		      let ret_nds_t_list, _ =
								  Util.map_fold_left
								    (fun res nds0 (Loc(tr, _)) _ ->
		  		        let arg_p_t_list = List.take (List.length res + 1) arg_p_t_list in
				          let ret = ret_of env (get tr) in
		            let nds = nds0 @ nodes_of_tree tr in
														let ts, xttys = term_of_nodes nds in
								      let t = Term.rename_fresh (visible ret) (Term.simplify (qelim (visible ret) ((List.map Util.trd_triple arg_p_t_list) @ ts, xttys))) in
								      (**)let _ = Format.printf "%a: %a@." Var.pr ret Term.pr t in(**)
		            (ret, nds, t), nds)
								    [] locs
								in
								let Some(tr) = prune_tree (fun name -> related nd.name name) (root loc) in
(**)
        let _ = Format.printf "%a@." pr tr in
(**)
        let res =
								  Util.map_right
								    (fun ret_nds_t_list (ret, nds, t1) res ->
						        let interp =
										 					let _ = Format.printf "computing a condition of %a:@.  @[<v>" Var.pr ret in
						          let _, ret_interp_list = List.split res in
                let t2 =
                  let ts, xttys = term_of_nodes (nodes_of_tree tr) in
                  Term.simplify (qelim (visible ret)
                    ((**)List.map Util.trd_triple arg_p_t_list @(**)
                     List.map Util.trd_triple ret_nds_t_list @
                     List.map snd ret_interp_list @ ts,
                     xttys))
                in
																let interp = interpolate_bvs (visible ret) t1 t2 in
						  						  let _ = Format.printf "@]@." in
						          interp
						        in
														let arg_p_interp_list =
																Util.map_left
																		(fun arg_p_interp_list (arg, p, t) arg_p_t_list ->
																				let _ = Format.printf "computing a condition of %a:@.  @[<v>" Var.pr arg in
																				let interp =
																				  let t1 = t in
																				  let t2 =
																					  	let ts, xttys = term_of_nodes nds in
																								Term.simplify (qelim (visible arg)
                          (List.map Util.trd_triple arg_p_interp_list @
                           List.map Util.trd_triple arg_p_t_list @
                           Term.bnot interp :: ts,
                           xttys))
																				  in
																				  interpolate_bvs (visible arg) t1 t2
																		  in
																		  let _ = Format.printf "@]@." in
																				(arg, p, interp))
																		(List.take (List.length ret_nds_t_list + 1) arg_p_t_list)
														in
														arg_p_interp_list,
		            (ret, interp))
												ret_nds_t_list
        in
				    let arg_p_interp_list_list, ret_interp_list = List.split res in
								let arg_p_interp_list =
										Util.map_left
												(fun arg_p_interp_list (arg, p, t) arg_p_t_list ->
														let _ = Format.printf "computing a condition of %a:@.  @[<v>" Var.pr arg in
														let interp =
														  let t1 = t in
														  let t2 =
                  let ts, xttys = term_of_nodes (nodes_of_tree tr) in
  																Term.simplify
                    (qelim (visible arg)
                      (List.map Util.trd_triple arg_p_interp_list @
                      List.map Util.trd_triple arg_p_t_list @
                      List.map snd ret_interp_list @
                      ts, xttys))
														  in
														  interpolate_bvs (visible arg) t1 t2
												  in
												  let _ = Format.printf "@]@." in
														(arg, p, interp))
												arg_p_t_list
								in
								List.concat arg_p_interp_list_list @ arg_p_interp_list,
        ret_interp_list
      else
		      let nds =
								  Util.concat_map
								    (fun (Loc(tr, _)) ->
		            nodes_of_tree tr)
								    locs
								in
								let arg_p_interp_list =
										Util.map_left
												(fun arg_p_interp_list (arg, p, t) arg_p_t_list ->
														let _ = Format.printf "computing a condition of %a:@.  @[<v>" Var.pr arg in
														let interp =
														  let t1 = t in
														  let t2 =
															  	let ts, xttys = term_of_nodes nds in
																		Term.simplify (qelim (visible arg)
                    (List.map Util.trd_triple arg_p_interp_list @
                     List.map Util.trd_triple arg_p_t_list @
                     ts,
                     xttys))
														  in
														  interpolate_bvs (visible arg) t1 t2
												  in
												  let _ = Format.printf "@]@." in
														(arg, p, interp))
												arg_p_t_list
								in
								arg_p_interp_list,
        []
    in
    List.map (fun (ret, interp) -> `P(ret, interp)) ret_interp_list @
    List.map (fun (arg, _, interp) -> `P(arg, interp)) arg_p_interp_list,
    (if nd.closed then [subst_interps env (root loc) ret_interp_list] else []) @
    List.filter_map (fun (_, p, interp) -> subst_interp false p interp) arg_p_interp_list

let summary_of_widen env (Loc(Node(nd, []), p) as loc) = assert false
(*
  let arg = arg_of env nd in
  let _ = Format.printf "computing a condition of %a:@.  @[<v>" Var.pr arg in
  let parginterps, interp =
    try
      let interp =
								let (tts, tps), xss =
	  							let trs, ps = (*reverseしなくてよいのか？*)rec_calls_of (fst nd.name) loc in
	         List.split
												(List.map2
													 (fun tr p ->
			             let arg = arg_of env (get tr) in
(*
			             let _ = Format.printf "%a@." Var.pr arg in
*)
													   Term.rename_fresh (visible arg) (Term.simplify (qelim (visible arg) (term_of_nodes (nodes_of_tree tr)))),
													   Term.rename_fresh (visible arg) (Term.simplify (qelim (visible arg) (term_of_nodes (nodes_of_path p)))))
													 trs ps),
			  		   List.map (fun tr -> args_of_tree env tr @ [ret_of_tree env tr]) trs
								in
								let tt = List.hd tts in
								let tp = List.hd tps in
								let ttw = widen xss tts in
								let tpw = widen xss tps in
								let t1, t2, tw1, tw2 =
										if nd.closed then
											 tt, tp, ttw, tpw
										else
											 tp, tt, tpw, ttw
								in
			     interpolate_widen_bvs (visible arg) nd.closed t1 t2 tw1 tw2
      in
      let _ = Format.printf "@]@." in
						[], interp
    with CsisatInterface.No_interpolant ->
      let _ = Format.printf "*******@ " in
		    let argps, nds =
				    let locs = related_locs loc in
								List.map
										(fun (Loc(tr, p)) ->
												SimType.find_last_base env (get tr).name, left_of_path p)
										locs,
								Util.concat_map
										(fun (Loc(tr, _)) -> nodes_of_tree tr)
										locs
      in
				  let ts0 =
		      List.map
		        (fun (arg, p) ->
		          let t = Term.rename_fresh (visible arg) (Term.simplify (qelim (visible arg) (term_of_nodes (nodes_of_path p)))) in
		          (*let _ = Format.printf "%a: %a@." Var.pr arg Term.pr t in*)
		          t)
		        argps
				  in
		    let interp =
								let (tts, tps), xss =
    						let trs, ps = (*reverseしなくてよいのか？*)rec_calls_of (fst nd.name) loc in
          List.split
												(List.map2
												  (fun tr p ->
		              let arg = arg_of env (get tr) in
(*
		              let _ = Format.printf "%a@." Var.pr arg in
*)
(* ts0 や ndsもこの中で求めないと全く意味が無いのでは？ *)
(*
                if nd.closed then
														    Term.rename_fresh (visible arg) (Term.simplify (qelim (visible arg) (term_of_nodes (nodes_of_tree tr)))),
  												    let ts, xttys = term_of_nodes (nds @ nodes_of_path p) in
														    Term.rename_fresh (visible arg) (Term.simplify (qelim (visible arg) (ts0 @ ts, xttys)))
                else
*)
  												    let ts, xttys = term_of_nodes (nds @ nodes_of_tree tr) in
														    Term.rename_fresh (visible arg) (Term.simplify (qelim (visible arg) (ts0 @ ts, xttys))),
														    Term.rename_fresh (visible arg) (Term.simplify (qelim (visible arg) (term_of_nodes (nodes_of_path p)))))
												  trs ps),
				      List.map (fun tr -> args_of_tree env tr @ [ret_of_tree env tr]) trs
								in
								let tt = List.hd tts in
								let tp = List.hd tps in
								let ttw = widen xss tts in
								let tpw = widen xss tps in
								let t1, t2, tw1, tw2 =
										if nd.closed then
										  tt, tp, ttw, tpw
										else
										  tp, tt, tpw, ttw
								in
		      interpolate_widen_bvs (visible arg) nd.closed t1 t2 tw1 tw2
		    in
		    let _ = Format.printf "@]@." in
		    let _, parginterps =
  		    let tr = Node(nd, []) in
				    List.fold_left
				      (fun (t0::ts0, parginterps) (arg, p) ->
												let _ = Format.printf "computing a condition of %a:@.  @[<v>" Var.pr arg in
												let interp =
				          let t1 = t0 in
				          let t2 =
												    let ts, xttys = term_of_nodes (nds @ nodes_of_tree tr) in
									  					Term.simplify (qelim (visible arg) ((if nd.closed then Term.bnot interp else interp)::ts0 @ ts, xttys))
				          in
				          interpolate_bvs (visible arg) t1 t2
		          in
		          let _ = Format.printf "@]@." in
				        ts0 @ [interp], (p, arg, interp)::parginterps)
				      (ts0, []) argps
		    in
		    parginterps, interp
  in
		if nd.closed then
    `P(arg, interp)::(List.map (fun (_, arg, interp) -> `P(arg, interp)) parginterps),
		  (match p with
		    Top -> assert false
		  | Path(up, trs1, nd, trs2) ->
        let ts1, t::ts2 = Util.split_at nd.constr (List.length trs1 + 1) in
        let xttyss1, xttys::xttyss2 = Util.split_at nd.subst (List.length trs1) in
(*
        let xts = List.map (fun (x, t, _) -> x, t) xttys in
        let sub x = List.assoc x xts in
*)
        let ts2 = match ts2 with t'::ts2' -> (Term.band [t; (*Term.subst sub*) interp; t'])::ts2' | [] -> assert false in
(**)
        let xttyss2 = match xttyss2 with xttys'::xttyss2' -> (xttys @ xttys')::xttyss2' | [] -> assert false in
(**)
        (root (Loc(Node({ nd with constr = ts1 @ ts2;
                                  subst = xttyss1 @ xttyss2 }, trs1 @ trs2), up))))::
    Util.concat_map
      (fun (Path(up, trs1, nd, []), _, interp) ->
				    if Term.equiv interp Term.ttrue then
				      []
				    else
		        let ts1, t::[] = Util.split_at nd.constr (List.length trs1 + 1) in
		        let xttyss1, xttys::[] = Util.split_at nd.subst (List.length trs1) in
(**)
		        let xts = List.map (fun (x, t, _) -> x, t) xttys in
		        let sub x = List.assoc x xts in
(**)
				      [root (Loc(Node({ nd with ret = None;
				                                closed = false;
				                                constr = ts1 @ [Term.band [t; Term.bnot ((*Term.subst sub*) interp)]];
				                                subst = xttyss1 @ [(*[]*)(**)xttys(**)] }, trs1), path_set_open up))])
      parginterps
		else
		  let _ = assert (nd.ret = None) in
    [`P(arg, interp)],
		  match p with
		    Top -> let _ = assert (interp = Term.ttrue) in []
		  | Path(up, trs1, nd, []) ->
        let ts1, t::[] = Util.split_at nd.constr (List.length trs1 + 1) in
        let xttyss1, xttys::[] = Util.split_at nd.subst (List.length trs1) in
(**)
        let xts = List.map (fun (x, t, _) -> x, t) xttys in
        let sub x = List.assoc x xts in
(**)
        [root (Loc(Node({ nd with constr = ts1 @ [Term.band [t; Term.bnot ((**)Term.subst sub(**) interp)]];
                                  subst = xttyss1 @ [[](*xttys*)] }, trs1), up))]
*)


let summaries_of env constrss0 =
  let rec summaries_of_aux sums constrss =
    match constrss with
      [] -> sums
   | constrs::constrss' ->
	    			(**)
				    let _ = Format.printf "constraints:@.  %a@." pr constrs in
				    (**)
				    let sums', constrss'' =
				      try
            let loc =
              let locs = List.rev (find_leaves constrs) in
              let locs = List.filter (fun loc -> Util.diff (related_locs loc) locs = []) locs in
              let _ = assert (locs <> []) in
              if Flags.enable_widening then
                let locs' = List.filter (fun (Loc(Node(nd, []), p) as loc) -> is_recursive nd.name loc) locs in
                (match locs' with
                  [] -> List.hd locs
                | loc::_ -> loc)
              else
                List.hd locs(*find_leaf constrs*)
            in
				        if Flags.enable_widening then summary_of_widen env loc else summary_of env loc
				      with CsisatInterface.No_interpolant ->
				        raise (FeasibleErrorTrace(constrs(*???*)))
				    in
				    match constrss with
				      [] -> sums' @ sums
				    | _ ->
				        summaries_of_aux (sums' @ sums) (constrss'' @ constrss')
(*
      Format.printf "@.";
*)
  in
  summaries_of_aux [] constrss0
