open ExtList
open ExtString
open Zipper
open TraceConstr
open TcSolve

(** Trace constraint solving for interaction types *)

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
      CallId.ancestor_of (x, uid) (x', uid')
		| Var.V(_) ->
      assert false

(** ToDo: not enough for higher-order functions *)
let vars_of_tree env tr =
  let x, uid = (get tr).name in
  let n = SimType.arity (env x) in
  List.init (n + 1) (fun i -> Var.T(x, uid, i))

let summary_of env (Loc(Node(nd, []), p) as loc) =  
  let x, uid = fst nd.name, snd nd.name in
		let _ =
		  if nd.closed then
		    Format.printf "computing a postcondition of %a:@,  @[<v>" CallId.pr nd.name
		  else
		    Format.printf "computing a precondition of %a:@,  @[<v>" CallId.pr nd.name
		in
  let interp =
    match !Global.predicate_discovery with
      ConvexHull ->
						  let trs, ps = rec_calls_of (fst nd.name) loc in
								let tts, tps = List.split
								  (List.map2
								    (fun tr p ->
								      Term.rename_fresh (visible (get tr).name)
		              (Formula.simplify (Fes.formula_of (Fes.eqelim (visible (get tr).name) (fes_of_nodes (nodes_of_tree tr))))),
								      Term.rename_fresh (visible (get tr).name)
		              (Formula.simplify (Fes.formula_of (Fes.eqelim (visible (get tr).name) (fes_of_nodes (nodes_of_path p))))))
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
    | Backward ->
								let tt = Formula.simplify (Fes.formula_of (Fes.eqelim (visible nd.name) (fes_of_nodes [nd]))) in
								let tp = Formula.simplify (Fes.formula_of (Fes.eqelim (visible nd.name) (fes_of_nodes (nodes_of_path p)))) in
								let t1, t2 = if nd.closed then tt, tp else tp, tt in
		      CsisatInterface.interpolate t1 t2
  in
  let _ = Format.printf "@]@," in
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
    let _ = Format.printf "constraints:@,  %a@," pr constrs in
(**)
    let sums', constrs_opt =
      try
        summary_of env (find_leaf constrs)
      with CsisatInterface.NoInterpolant ->
        raise (FeasibleErrorTrace(constrs0))
    in
    match constrs_opt with
      None -> sums' @ sums
    | Some(constrs) ->
        summaries_of_aux (sums' @ sums) constrs
  in
  summaries_of_aux [] constrs0
