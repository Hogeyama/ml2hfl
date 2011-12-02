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
            else if Var.is_pos (fst y) then
              let _ = assert (g = Term.ttrue) in
              aux (down loc (Var.fc_of (fst y))) etr
            else if Var.is_neg (fst y) then
              let _ = assert (g = Term.ttrue) in
		            aux (up loc) etr
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
            else if Var.is_neg f then
              aux (down (Loc(set tr nd', p)) (Var.fc_of f)) etr
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
  let env = IntType.of_summaries prog sums fcs in
  let env' =
    List.map
      (fun (f, sty) ->
        f, IntType.of_simple_type sty)
      (List.find_all
        (fun (f, sty) -> not (List.mem_assoc f env))
        (List.map
          (fun (f, sty) -> Var.make f, sty)
          prog.Prog.types))
  in
  env @ env'


let summary_of (Loc(Node(nd, []), p) as loc) =  
  let x, uid = fst nd.name, snd nd.name in
		let _ =
		  if nd.closed then
		    Format.printf "computing a postcondition of <%a:%d>:@.  @[<v>" Var.pr x uid
		  else
		    Format.printf "computing a precondition of <%a:%d>:@.  @[<v>" Var.pr x uid
		in
  let interp =
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
				(* require that (x, uid) is a top level function call *)
				let dependable (x, uid) =
				  function
				    Var.T(x', uid', _) as y ->
				      ancestor_of (x, uid) (x', uid')
						| Var.V(_) ->
				      assert false
		  in
				let tts, tps = List.split
				  (List.map2
				    (fun tr p ->
				      Term.simplify
            (Term.subst
              (sub (get tr).name)
              (qelim (dependable (get tr).name) (term_of_nodes (nodes_of_tree tr)))),
				      Term.simplify
            (Term.subst
              (sub (get tr).name)
              (qelim (dependable (get tr).name) (term_of_nodes (nodes_of_path p)))))
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

				let interp =
						try
						  (if Flags.enable_widening then
						    try
						      let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr tw1 Term.pr tw2 in
						      CsisatInterface.interpolate tw1 tw2
						    with CsisatInterface.No_interpolant ->
						      if nd.closed then
						        if Term.equiv t2 tw2 then
						          raise CsisatInterface.No_interpolant
						        else
						          let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr tw1 Term.pr t2 in
						          CsisatInterface.interpolate tw1 t2
						      else
						        if Term.equiv t1 tw1 then
						          raise CsisatInterface.No_interpolant
						        else
						          let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr t1 Term.pr tw2 in
						          CsisatInterface.interpolate t1 tw2
						  else
						    raise CsisatInterface.No_interpolant)
						with CsisatInterface.No_interpolant ->
						  (try
						    if Flags.enable_widening && Term.equiv t1 tw1 && Term.equiv t2 tw2 then
						      raise CsisatInterface.No_interpolant
						    else
						      let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr t1 Term.pr t2 in
						      CsisatInterface.interpolate t1 t2
						  with CsisatInterface.No_interpolant ->
						    raise CsisatInterface.No_interpolant)
				in
				let _ = Format.printf "interp_out: %a@]@." Term.pr interp in
    interp
  in
		if nd.closed then
		  (match nd.ret with
		    None -> [`Post((x, uid), interp)]
		  | Some(x, uid) -> assert false),
		  match p with
		    Top -> assert false
		  | Path(up, trs1, nd, trs2) ->
        let ts1, t::ts2 = Util.split_at nd.constr (List.length trs1 + 1) in
        let xttyss1, xttys::xttyss2 = Util.split_at nd.subst (List.length trs1) in
        let ts2 = match ts2 with t'::ts2' -> (Term.band [t; interp; t'])::ts2' | [] -> assert false in
        let xttyss2 = match xttyss2 with xttys'::xttyss2' -> (xttys @ xttys')::xttyss2' | [] -> assert false in
        Some(root (Loc(Node({ nd with constr = ts1 @ ts2;
                                      subst = xttyss1 @ xttyss2 }, trs1 @ trs2), up)))
		else
		  let _ = assert (nd.ret = None) in
		  [`Pre((x, uid), interp)],
		  match p with
		    Top -> let _ = assert (interp = Term.ttrue) in None
		  | Path(up, trs1, nd, trs2) ->
        (* assert (trs2 = []) *)
        let ts1, t::[] = Util.split_at nd.constr (List.length trs1 + 1) in
        let xttyss1, xttys::[] = Util.split_at nd.subst (List.length trs1) in
        let ts2 = [Term.band [t; Term.bnot interp]] in
        let xttyss2 = [xttys] in
        Some(root (Loc(Node({ nd with constr = ts1 @ ts2;
                                      subst = xttyss1 @ xttyss2 }, trs1 @ trs2), up)))

let summaries_of constrs0 =
  let rec summaries_of_aux sums constrs =
(**)
    let _ = Format.printf "constraints:@.  %a@." pr constrs in
(**)
    let sums', constrs_opt =
      try
        summary_of (find_leaf constrs)
      with CsisatInterface.No_interpolant ->
        raise (FeasibleErrorTrace(constrs0))
    in
    match constrs_opt with
      None -> sums' @ sums
    | Some(constrs) ->
        summaries_of_aux (sums' @ sums) constrs
  in
  summaries_of_aux [] constrs0
