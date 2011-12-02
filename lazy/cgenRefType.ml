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
    CompTree.Call(x, g)::etr -> aux (zipper (make x true [g] [])) etr
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

let summary_of env (Loc(Node(nd, []), p) as loc) =  
  let x, uid = fst nd.name, snd nd.name in
  let arg =
		  let i = SimType.find_last_base (env x) in
		  let _ = if i = -1 then ()(* condition must be Term.ttrue *) in
    Var.T(x, uid, i)
  in
		let ret =
		  match nd.ret with
		    None ->
		      Var.T(x, uid, SimType.arity (env x))
		  | Some(x, uid) ->
		      let i = SimType.find_last_base (env x) in
		      let _ = if i = -1 then ()(* condition must be Term.ttrue *) in
		      Var.T(x, uid, i)
		in
  let _ =
		  if nd.closed then
				  Format.printf "computing conditions of %a and %a:@.  @[<v>" Var.pr arg Var.pr ret
		  else
				  Format.printf "computing a condition of %a:@.  @[<v>" Var.pr arg
  in
  let interp1, interp2 =
				let trs, ps = rec_calls_of (fst nd.name) loc in
				let sub (y, uid) x =
				  match x with
				    Var.V(_) ->
				      Term.make_var2 x
				  | Var.T(y', uid', arg) ->
				      (* ToDo: no need to be recursive??? *)
				      if y = y' && uid = uid' then
				        Term.make_var2 (Var.T(y', 0(*???*), arg))
				      else
				        Term.make_var2 x
				in
    let vis = visible (if nd.closed then ret else arg) in
				let tts, tps = List.split
				  (List.map2
				    (fun tr p ->
				      Term.simplify
            (Term.subst
              (sub (get tr).name)
              (qelim vis (term_of_nodes (nodes_of_tree tr)))),
				      Term.simplify
            (Term.subst
              (sub (get tr).name)
              (qelim vis (term_of_nodes (nodes_of_path p)))))
				    trs ps)
				in
				let sub_inv (y, uid) x =
				  match x with
				    Var.V(_) ->
				      Term.make_var2 x
				  | Var.T(y', uid', arg) ->
				      (* ToDo: no need to be recursive??? *)
				      if y = y' && uid' = 0 then
				        Term.make_var2 (Var.T(y', uid, arg))
				      else
				        Term.make_var2 x
				in
				let hoge ts =
						let _, tss = List.fold_left
						  (fun (ts, tss) t ->
						    t::ts, tss @ [t::ts])
						  ([], [])
						  ts
				  in
				  tss
				in
				let tt = Term.subst (sub_inv nd.name) (List.hd tts) in
				let tp = Term.subst (sub_inv nd.name) (List.hd tps) in
				let ttw =
      if Flags.enable_widening then
        Term.subst
          (sub_inv nd.name)
          (ApronInterface.widen (List.map Term.bor (hoge tts)))
      else
        Term.tunit(*dummy*)
    in
				let tpw =
      if Flags.enable_widening then
        Term.subst
          (sub_inv nd.name)
          (ApronInterface.widen (List.map Term.bor (hoge tps)))
      else
        Term.tunit(*dummy*)
    in
				let t1, t2, tw1, tw2 =
				  if nd.closed then
				    tt, tp, ttw, tpw
				  else
				    tp, tt, tpw, ttw
				in

    try
						let interp =
								try
								  (if Flags.enable_widening then
								    try
								      let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr tw1 Term.pr tw2 in
								      CsisatInterface.interpolate_bvs vis tw1 tw2
								    with CsisatInterface.No_interpolant ->
								      if nd.closed then
								        if Term.equiv t2 tw2 then
								          raise CsisatInterface.No_interpolant
								        else
								          let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr tw1 Term.pr t2 in
								          CsisatInterface.interpolate_bvs vis tw1 t2
								      else
								        if Term.equiv t1 tw1 then
								          raise CsisatInterface.No_interpolant
								        else
								          let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr t1 Term.pr tw2 in
								          CsisatInterface.interpolate_bvs vis t1 tw2
								  else
								    raise CsisatInterface.No_interpolant)
								with CsisatInterface.No_interpolant ->
								  (try
								    if Flags.enable_widening && Term.equiv t1 tw1 && Term.equiv t2 tw2 then
								      raise CsisatInterface.No_interpolant
								    else
								      let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr t1 Term.pr t2 in
								      CsisatInterface.interpolate_bvs vis t1 t2
								  with CsisatInterface.No_interpolant ->
								    raise CsisatInterface.No_interpolant)
						in
						let _ = Format.printf "interp_out: %a@]@." Term.pr interp in
						if nd.closed then Term.ttrue, interp else interp, Term.ttrue(*dummy*)
    with CsisatInterface.No_interpolant ->
      if nd.closed then
        let _ = Format.printf "*******@ " in
								let tt = Term.simplify (raw (term_of_nodes [nd])) in
				    let tp1, tp2 = terms_of_path p in
								let tp1 = Term.simplify tp1 in
								let tp2 = Term.simplify tp2 in
				    let t1 = Term.band [tp1; tt] in
				    let t2 = Term.band [tp1; tp2] in
				    let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr t1 Term.pr t2 in
								let interp2 = CsisatInterface.interpolate_bvs (visible ret) t1 t2 in
								let _ = Format.printf "interp_out: %a@ " Term.pr interp2 in
				    let t1 = tp1 in
				    let t2 = Term.band [tt; Term.bnot interp2] in
				    let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr t1 Term.pr t2 in
								let interp1 = CsisatInterface.interpolate_bvs (visible arg) t1 t2 in
								let _ = Format.printf "interp_out: %a@ " Term.pr interp1 in
        interp1, interp2
      else
        raise CsisatInterface.No_interpolant
  in
		if nd.closed then
    [`P(arg, interp1); `P(ret, interp2)],
		  match p with
		    Top -> assert false
		  | Path(up, trs1, nd, trs2) ->
        let ts1, t::ts2 = Util.split_at nd.constr (List.length trs1 + 1) in
        let xttyss1, xttys::xttyss2 = Util.split_at nd.subst (List.length trs1) in
        let ts2 = match ts2 with t'::ts2' -> (Term.band [t; interp2; t'])::ts2' | [] -> assert false in
        let xttyss2 = match xttyss2 with xttys'::xttyss2' -> (xttys @ xttys')::xttyss2' | [] -> assert false in
        (root (Loc(Node({ nd with constr = ts1 @ ts2;
                                  subst = xttyss1 @ xttyss2 }, trs1 @ trs2), up)))::
        if Term.equiv interp1 Term.ttrue then
          []
        else
		        [root (Loc(Node({ nd with ret = None;
                                    closed = false;
                                    constr = ts1 @ [Term.band [t; Term.bnot interp1]];
		                                  subst = xttyss1 @ [xttys] }, trs1), path_set_open (left_of_path up)))]
		else
		  let _ = assert (nd.ret = None) in
    [`P(arg, interp1)],
		  match p with
		    Top -> let _ = assert (interp1 = Term.ttrue) in []
		  | Path(up, trs1, nd, trs2) ->
        (* assert (trs2 = []) *)
        let ts1, t::[] = Util.split_at nd.constr (List.length trs1 + 1) in
        let xttyss1, xttys::[] = Util.split_at nd.subst (List.length trs1) in
        [root (Loc(Node({ nd with constr = ts1 @ [Term.band [t; Term.bnot interp1]];
                                  subst = xttyss1 @ [xttys] }, trs1 @ trs2), up))]

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
				        summary_of env (find_leaf constrs)
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
