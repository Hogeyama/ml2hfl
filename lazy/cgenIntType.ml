open ExtList
open ExtString
open Cgen

(** Constraint generation for interaction types *)

(** generate a set of constraints from an error trace *)
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
              let _ = assert (g = Formula.ttrue) in
              aux (down loc (Var.tlfc_of (fst y))) etr
            else if Var.is_neg (fst y) then
              let _ = assert (g = Formula.ttrue) in
		            aux (up loc) etr
            else assert false
        | CompTree.Arg(xttys) ->
            let xttys = List.filter (fun (_, _, ty) -> SimType.is_base ty) xttys in
            let nd = get tr in
            aux (Loc(set tr { nd with constr = nd.constr @ [Formula.ttrue]; subst = nd.subst @ [xttys] }, p)) etr
        | CompTree.Ret(x, t, ty) ->
            let xttys = List.filter (fun (_, _, ty) -> SimType.is_base ty) [x, t, ty] in
            let nd = get tr in
            let nd' = { nd with constr = nd.constr @ [Formula.ttrue]; subst = nd.subst @ [xttys] } in
            let Var.T(f, _, _) = x in
            if Var.is_pos f then
              aux (up (Loc(set tr nd', p))) etr
            else if Var.is_neg f then
              aux (down (Loc(set tr nd', p)) (Var.tlfc_of f)) etr
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


(** require: (x, uid) is a top level function call *)
let visible (x, uid) =
  function
    Var.T(x', uid', _) as y ->
      Var.ancestor_of (x, uid) (x', uid')
		| Var.V(_) ->
      assert false

(** ToDo: not enought for higher-order functions *)
let vars_of_tree env tr =
  let x, uid = (get tr).name in
  let n = SimType.arity (env x) in
  List.init (n + 1) (fun i -> Var.T(x, uid, i))

let summary_of env (Loc(Node(nd, []), p) as loc) =  
  let x, uid = fst nd.name, snd nd.name in
		let _ =
		  if nd.closed then
		    Format.printf "computing a postcondition of %a:@.  @[<v>" Var.pr_x_uid nd.name
		  else
		    Format.printf "computing a precondition of %a:@.  @[<v>" Var.pr_x_uid nd.name
		in
  let interp =
    if Flags.enable_widening then
				  let trs, ps = rec_calls_of (fst nd.name) loc in
						let tts, tps = List.split
						  (List.map2
						    (fun tr p ->
						      Term.rename_fresh (visible (get tr).name) (Formula.simplify (Formula.eqelim (visible (get tr).name) (term_of_nodes (nodes_of_tree tr)))),
						      Term.rename_fresh (visible (get tr).name) (Formula.simplify (Formula.eqelim (visible (get tr).name) (term_of_nodes (nodes_of_path p)))))
						    trs ps)
						in
						let tt = List.hd tts in
						let tp = List.hd tps in
      let xss = List.map (vars_of_tree env) trs in
						let ttw = widen xss tts in
						let tpw = widen xss tps in
						let t1, t2, tw1, tw2 =
						  if nd.closed then
						    tt, tp, ttw, tpw
						  else
						    tp, tt, tpw, ttw
						in
      interpolate_widen nd.closed t1 t2 tw1 tw2
    else
						let tt = Formula.simplify (Formula.eqelim (visible nd.name) (term_of_nodes [nd])) in
						let tp = Formula.simplify (Formula.eqelim (visible nd.name) (term_of_nodes (nodes_of_path p))) in
						let t1, t2 = if nd.closed then tt, tp else tp, tt in
      interpolate t1 t2
  in
  let _ = Format.printf "@]@." in
		if nd.closed then
		  (match nd.ret with
		    None -> [`Post((x, uid), interp)]
		  | Some(x, uid) -> assert false),
		  match p with
		    Top -> assert false
		  | Path(up, trs1, nd, trs2) ->
        let ts1, t::ts2 = List.split_nth (List.length trs1 + 1) nd.constr in
        let xttyss1, xttys::xttyss2 = List.split_nth (List.length trs1) nd.subst in
        let xts = List.map (fun (x, t, _) -> x, t) xttys in
        let sub x = List.assoc x xts in
        let ts2 = match ts2 with t'::ts2' -> (Formula.band [t; Term.subst sub interp; t'])::ts2' | [] -> assert false in
        Some(root (Loc(Node({ nd with constr = ts1 @ ts2;
                                      subst = xttyss1 @ xttyss2 }, trs1 @ trs2), up)))
		else
		  let _ = assert (nd.ret = None) in
		  [`Pre((x, uid), interp)],
		  match p with
		    Top -> let _ = assert (interp = Formula.ttrue) in None
		  | Path(up, trs1, nd, trs2) ->
        (* assert (trs2 = []) *)
        let ts1, t::[] = List.split_nth (List.length trs1 + 1) nd.constr in
        let xttyss1, xttys::[] = List.split_nth (List.length trs1) nd.subst in
        let xts = List.map (fun (x, t, _) -> x, t) xttys in
        let sub x = List.assoc x xts in
        Some(root (Loc(Node({ nd with constr = ts1 @ [Formula.band [t; Formula.bnot (Term.subst sub interp)]];
                                      subst = xttyss1 @ [[]]}, trs1 @ trs2), up)))

let summaries_of env constrs0 =
  let rec summaries_of_aux sums constrs =
(**)
    let _ = Format.printf "constraints:@.  %a@." pr constrs in
(**)
    let sums', constrs_opt =
      try
        summary_of env (find_leaf constrs)
      with CsisatInterface.No_interpolant ->
        raise (FeasibleErrorTrace(constrs0))
    in
    match constrs_opt with
      None -> sums' @ sums
    | Some(constrs) ->
        summaries_of_aux (sums' @ sums) constrs
  in
  summaries_of_aux [] constrs0
