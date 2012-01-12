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
    CompTree.Call(x, g)::etr -> get_unsat_prefix (aux (zipper (make x true [g] [])) etr)
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
  let x, uid = nd.name in
		if nd.closed then
				match nd.ret with
				  None ->
				    Var.T(x, uid, SimType.arity (env x))
				| Some(x, uid) ->
								SimType.find_last_base env (x, uid)
		else
				SimType.find_last_base env (x, uid)

let summary_of env (Loc(Node(nd, []), p) as loc) =  
  let arg = arg_of env nd in
  let _ = Format.printf "computing a condition of %a:@.  @[<v>" Var.pr arg in
  let parginterps, interp =
    try
      let interp =
		      if Flags.enable_widening then
										let trs, ps = rec_calls_of (fst nd.name) loc in
										let tts, tps = List.split
										  (List.map2
										    (fun tr p ->
                let arg = arg_of env (get tr) in
(*
                let _ = Format.printf "%a@." Var.pr arg in
*)
										      Term.rename_fresh (visible arg) (Term.simplify (qelim (visible arg) (term_of_nodes (nodes_of_tree tr)))),
										      Term.rename_fresh (visible arg) (Term.simplify (qelim (visible arg) (term_of_nodes (nodes_of_path p)))))
										    trs ps)
										in
										let tt = List.hd tts in
										let tp = List.hd tps in
				      let xss = List.map (fun tr -> args_of_tree env tr @ [ret_of_tree env tr]) trs in
										let ttw = widen xss tts in
										let tpw = widen xss tps in
										let t1, t2, tw1, tw2 =
										  if nd.closed then
										    tt, tp, ttw, tpw
										  else
										    tp, tt, tpw, ttw
										in
		        interpolate_widen_bvs (visible arg) nd.closed t1 t2 tw1 tw2
		      else
										let tt = Term.simplify (qelim (visible arg) (term_of_nodes [nd])) in
										let tp = Term.simplify (qelim (visible arg) (term_of_nodes (nodes_of_path p))) in
										let t1, t2 = if nd.closed then tt, tp else tp, tt in
		        interpolate_bvs (visible arg) t1 t2
      in
      let _ = Format.printf "@]@." in
						[], interp
    with CsisatInterface.No_interpolant ->
      let _ = Format.printf "*******@ " in
      if Flags.enable_widening then
								let trs, ps = rec_calls_of (fst nd.name) loc in
		      let argps =
  		      let tr = Node(nd, []) in
				      let locs =
            find_all
              (fun nd ->
                SimType.find_last_base env nd.name <> arg &&
                let tmp = Var.tlfc_of (Var.T(fst (get tr).name, snd (get tr).name, (*dummy*)-1)) in
                Var.ancestor_of tmp nd.name)
              (root loc) in
								  List.map
								    (fun (Loc(tr, p)) ->
														SimType.find_last_base env (get tr).name,
								      left_of_path p)
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
        let ndss =
          List.map
            (fun tr ->
								      let locs = find_all (fun nd -> Var.ancestor_of (Var.tlfc_of (Var.T(fst (get tr).name, snd (get tr).name, (*dummy*)-1))) nd.name) (root loc) in
								      Util.concat_map (fun (Loc(tr, _)) -> nodes_of_tree tr) locs)
            trs
		      in
		      let interp =
										let tts, tps = List.split
										  (Util.map3
										    (fun tr nds p ->
                let arg = arg_of env (get tr) in
(*
                let _ = Format.printf "%a@." Var.pr arg in
*)
										      let ts, xttys = term_of_nodes nds in
										      Term.rename_fresh (visible arg) (Term.simplify (qelim (visible arg) (ts0 @ ts, xttys))),
										      Term.rename_fresh (visible arg) (Term.simplify (qelim (visible arg) (term_of_nodes (nodes_of_path p)))))
										    trs ndss ps)
										in
										let tt = List.hd tts in
										let tp = List.hd tps in
				      let xss = List.map (fun tr -> args_of_tree env tr @ [ret_of_tree env tr]) trs in
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
        let nds = List.hd ndss in
		      let _, parginterps =
				      List.fold_left
				        (fun (t0::ts0, parginterps) (arg, p) ->
												  let _ = Format.printf "computing a condition of %a:@.  @[<v>" Var.pr arg in
														let interp =
				            let t1 = t0 in
				            let t2 =
					  			        let ts, xttys = term_of_nodes nds in
									  					  Term.simplify (qelim (visible arg) ((if nd.closed then Term.bnot interp else interp)::ts0 @ ts, xttys))
				            in
				            interpolate_bvs (visible arg) t1 t2
		            in
		            let _ = Format.printf "@]@." in
				          ts0 @ [interp], (p, arg, interp)::parginterps)
				        (ts0, []) argps
		      in
		      parginterps, interp
      else
		      let argps =
  		      let tr = Node(nd, []) in
		        let locs = find_all (fun nd -> Var.ancestor_of (Var.tlfc_of (Var.T(fst (get tr).name, snd (get tr).name, (*dummy*)-1))) nd.name) (root loc) in
		        List.map
		          (fun (Loc(tr, p)) ->
														SimType.find_last_base env (get tr).name,
		            left_of_path p)
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
        let nds =
  		      let tr = Node(nd, []) in
		        let locs = find_all (fun nd -> Var.ancestor_of (Var.tlfc_of (Var.T(fst (get tr).name, snd (get tr).name, (*dummy*)-1))) nd.name) (root loc) in
		        Util.concat_map (fun (Loc(tr, _)) -> nodes_of_tree tr) locs
        in
		      let interp =
		        let tt =
						      let ts, xttys = term_of_nodes nds in
												Term.simplify (qelim (visible arg) (ts0 @ ts, xttys))
		        in
		        let tp = Term.simplify (qelim (visible arg) (term_of_nodes (nodes_of_path p))) in
		        let t1, t2 = if nd.closed then tt, tp else tp, tt in
										interpolate_bvs (visible arg) t1 t2
		      in
		      let _ = Format.printf "@]@." in
		      let _, parginterps =
				      List.fold_left
				        (fun (t0::ts0, parginterps) (arg, p) ->
												  let _ = Format.printf "computing a condition of %a:@.  @[<v>" Var.pr arg in
														let interp =
				            let t1 = t0 in
				            let t2 =
					  			        let ts, xttys = term_of_nodes nds in
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
              if Flags.enable_widening then
                let locs = List.rev (find_leaves constrs) in
                let locs' = List.filter (fun (Loc(Node(nd, []), p) as loc) -> is_recursive nd.name loc) locs in
                (match locs' with
                  [] -> List.hd locs
                | loc::_ -> loc)
              else
                find_leaf constrs
            in
				        summary_of env loc
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
