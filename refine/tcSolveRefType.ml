open ExtList
open ExtString
open Zipper
open TraceConstr
open TcSolve

(** {6 Options for deprecated old refinement type inference method} *)

let enable_quick_inference = false


(** Trace constraint solving for refinement types
    @deprecated use HcSolveRefType *)

(** ToDo: not enough for higher-order functions *)
let args_of_tree env tr =
  let x, uid = (get tr).name in
  let n = SimType.arity (env x) in
  List.init n (fun i -> Var.T(x, uid, i))

(** ToDo: not enough for higher-order functions *)
let ret_of_tree env tr =
  let x, uid = (get tr).name in
  match (get tr).ret with
    None ->
      Var.T(x, uid, SimType.arity (env x))
  | Some(x, uid) ->
      RefType.find_last_base env (x, uid)

let arg_of env nd =
  RefType.find_last_base env nd.name

let ret_of env nd =
  let x, uid = nd.name in
  match nd.ret with
    None ->
      Var.T(x, uid, SimType.arity (env x))
  | Some(x, uid) ->
      RefType.find_last_base env (x, uid)

(** require: all the substituted nodes are closed *)
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
              `L(t') ->
                (match ts, xttyss with
                  t''::ts, xttys'::xttyss ->
                    trs, Formula.band [t; t'; t''] :: ts, (xttys @ xttys') :: xttyss
                | _ -> assert false)
            | `R(tr) -> tr::trs, t::ts, xttys::xttyss)
      in
      let trs, ts, xttyss = aux2 (List.map aux trs) nd.constr nd.subst in
      `R(Node({ nd with constr = ts; subst = xttyss }, trs))
  in
  match aux tr with `R(tr) -> tr | _ -> assert false

let subst_interp closed p interp =
  if closed then
    match p with
      Top -> assert false
    | Path(up, trs1, nd, trs2) ->
        let ts1, t::ts2 = List.split_nth (List.length trs1) nd.constr in
        let xttyss1, xttys::xttyss2 = List.split_nth (List.length trs1) nd.subst in
        (** must not apply xttys to interp *)
        let ts2 =
          match ts2 with
            t'::ts2' -> (Formula.band [t; interp; t'])::ts2'
          | [] -> assert false
        in
        let xttyss2 =
          match xttyss2 with
            xttys'::xttyss2' -> (xttys @ xttys')::xttyss2'
          | [] -> assert false
        in
        Some(root (Loc(Node({ nd with constr = ts1 @ ts2;
                                      subst = xttyss1 @ xttyss2 }, trs1 @ trs2), up)))
  else
    match p with
      Top -> let _ = assert (interp = Formula.ttrue) in None
    | Path(up, trs1, nd, trs2) ->
        let _ = assert (trs2 = []) in
        if Term.equiv interp Formula.ttrue then
          let _ = Format.printf "stop propagation@," in
          None
        else
          let ts1, t::[] = List.split_nth (List.length trs1) nd.constr in
          let xttyss1, xttys::[] = List.split_nth (List.length trs1) nd.subst in
          (** must not apply xttys to interp *)
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
                                        constr = ts1 @ [Formula.band [t; Formula.bnot interp]];
                                        subst = xttyss1 @ [xttys] }, trs1), (**)path_set_open(**) up)))

let related n1 n2 =
  let tmp = CallId.tlfc_of (Var.T(fst n1, snd n1, (*dummy*)-1)) in
  CallId.ancestor_of tmp n2
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
    let [ts1; ts2] = Util.split_at [List.length trs] nd.constr in
    let [xttyss1; xttyss2] = Util.split_at [List.length trs] nd.subst in
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
      (Node({ nd with constr = List.flatten [ts1; ts2];
                      subst = List.flatten [xttyss1; if xttys = [] then xttyss2 else (xttys @ List.hd xttyss2) :: List.tl xttyss2] },
            trs))

(** require: for any nd in the spine, not (pred nd.name) *)
let rec prune_path pred p =
  match p with
    Top -> Top
  | Path(up, trs1, nd, trs2) ->
      let trs1 = List.map (prune_tree pred) trs1 in
      let trs2 = List.map (prune_tree pred) trs2 in
      let [ts1; [t2]; ts2; ts3] =
        Util.split_at [List.length trs1; 1; List.length trs2] nd.constr
      in
      let [xttyss1; [xttys1]; xttyss2; xttyss3] =
        Util.split_at [List.length trs1; 1; List.length trs2] nd.subst
      in
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
            (Util.map3 (fun t xttys -> function (Some(tr)) -> Some(t, xttys, tr) | None -> assert (t = Formula.ttrue); None) ts1 xttyss1 trs1))
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
        { nd with constr = List.flatten [ts1; [t2]; ts2; ts3];
                  subst = List.flatten [xttyss1; [xttys1' @ xttys1]; xttyss2; if xttys2' = [] then xttyss3 else (xttys2' @ List.hd xttyss3) :: List.tl xttyss3] }, trs2)

(** require: all the related locations of loc is a leaf *)
let summary_of env loc =
  let Loc(Node(nd, []), p) = loc in
  let _ = if not nd.closed then assert (nd.ret = None) in
  let locs = related_locs loc in
  let b = Global.enable_quick_inference && List.length locs = 1 in
  try
    if b then
      let arg = if nd.closed then ret_of env nd else arg_of env nd in
      let _ = Format.printf "computing a condition of %a:@,  @[<v>" Var.pr arg in
      let interp =
        let tt =
          FormulaUtil.simplify
            (Fes.formula_of
              (Fes.eqelim (RefType.visible arg) (fes_of_nodes [nd])))
        in
        let tp =
          FormulaUtil.simplify
            (Fes.formula_of
              (Fes.eqelim (RefType.visible arg) (fes_of_nodes (nodes_of_path p))))
        in
        let t1, t2 = if nd.closed then tt, tp else tp, tt in
        CsisatInterface.interpolate_bvs (RefType.visible arg) t1 t2
      in
      let _ = Format.printf "@]@," in
      [`P(arg, interp)], Util.list_of_opt (subst_interp nd.closed p interp)
    else (* necessary try repeat.ml *)
      raise CsisatInterface.NoInterpolant
  with CsisatInterface.NoInterpolant ->
    let _ = if b then Format.printf "**** quick inference failed ****@]@," in
    let arg_p_interp_list, ret_interp_list =
      let arg_p_t_list =
        List.map
          (fun (Loc(tr, p)) ->
            let p = left_of_path p in
            let p =
              if nd.closed then
                (* try linmax.ml *)
                prune_path (fun name -> related nd.name name) p
              else
                (* try neg.ml *)
                p
            in
            (*let _ = Format.printf "%a@," pr_path p in*)
            let arg = arg_of env (get tr) in
            let t =
              Term.fresh
                (RefType.visible arg)
                (FormulaUtil.simplify
                  (Fes.formula_of
                    (Fes.eqelim
                      (RefType.visible arg)
                      (fes_of_nodes (nodes_of_path p)))))
            in
            (*let t = Formula.of_dnf (Term.dnf t) in*)
            (**)let _ = Format.printf "%a: %a@," Var.pr arg Term.pr t in(**)
            arg, p, t)
          locs
      in
      let ret_nds_t_list, _ =
        Util.foldac_left
          (fun res nds0 (Loc(tr, _)) _ ->
            let arg_p_t_list = List.take (List.length res + 1) arg_p_t_list in
            let ret = ret_of env (get tr) in
            let nds = nds0 @ nodes_of_tree tr in
            let fes =
              Fes.band
                [Fes.make [] (List.map Util.trd3 arg_p_t_list);
                 fes_of_nodes nds]
            in
            let t = Term.fresh (RefType.visible ret) (FormulaUtil.simplify (Fes.formula_of (Fes.eqelim (RefType.visible ret) fes))) in
            (*let t = Formula.of_dnf (Term.dnf t) in*)
            (**)let _ = if (get tr).closed then Format.printf "%a: %a@," Var.pr ret Term.pr t in(**)
            (ret, nds, t), nds)
          [] locs
      in
      if nd.closed then
        let Some(tr) = prune_tree (fun name -> related nd.name name) (root loc) in
        (*let _ = Format.printf "context: %a@," pr tr in*)
        let res =
          Util.maprac
            (fun ret_nds_t_list (ret, nds, t1) res ->
              let interp =
                let _ = Format.printf "computing a condition of %a:@,  @[<v>" Var.pr ret in
                let _, ret_interp_list = List.split res in
                let t2 =
                  let fes =
                     Fes.band
                       [Fes.make
                         []
                         ((**)List.map Util.trd3 arg_p_t_list @(**)
                         List.map Util.trd3 ret_nds_t_list @
                         List.map snd ret_interp_list);
                        fes_of_nodes (nodes_of_tree tr)]
                  in
                  FormulaUtil.simplify (Fes.formula_of (Fes.eqelim (RefType.visible ret) fes))
                in
                let interp = CsisatInterface.interpolate_bvs (RefType.visible ret) t1 t2 in
                let _ = Format.printf "@]@," in
                interp
              in
              let arg_p_interp_list =
                Util.maplac
                  (fun arg_p_interp_list (arg, p, t) arg_p_t_list ->
                    let _ = Format.printf "computing a condition of %a:@,  @[<v>" Var.pr arg in
                    let interp =
                      let t1 = t in
                      let t2 =
                        let fes =
                          Fes.band
                            [Fes.make
                              []
                              (List.map Util.trd3 arg_p_interp_list @
                               List.map Util.trd3 arg_p_t_list @
                               [Formula.bnot interp]);
                             fes_of_nodes nds]
                        in
                        FormulaUtil.simplify (Fes.formula_of (Fes.eqelim (RefType.visible arg) fes))
                      in
                      CsisatInterface.interpolate_bvs (RefType.visible arg) t1 t2
                    in
                    let _ = Format.printf "@]@," in
                    (arg, p, interp))
                  (List.take (List.length ret_nds_t_list + 1) arg_p_t_list)
              in
              arg_p_interp_list,
              (ret, interp))
            ret_nds_t_list
        in
        let arg_p_interp_list_list, ret_interp_list = List.split res in
        let arg_p_interp_list =
          Util.maplac
            (fun arg_p_interp_list (arg, p, t) arg_p_t_list ->
              let _ = Format.printf "computing a condition of %a:@,  @[<v>" Var.pr arg in
              let interp =
                let t1 = t in
                let t2 =
                  let fes =
                    Fes.band
                      [Fes.make
                        []
                        (List.map Util.trd3 arg_p_interp_list @
                        List.map Util.trd3 arg_p_t_list @
                        List.map snd ret_interp_list);
                       fes_of_nodes (nodes_of_tree tr)]
                  in
                  FormulaUtil.simplify
                    (Fes.formula_of (Fes.eqelim (RefType.visible arg) fes))
                in
                CsisatInterface.interpolate_bvs (RefType.visible arg) t1 t2
              in
              let _ = Format.printf "@]@," in
              (arg, p, interp))
            arg_p_t_list
        in
        List.concat arg_p_interp_list_list @ arg_p_interp_list,
        ret_interp_list
      else
        let ret_nds_t_list, [_, nds0, _] =
          List.split_nth (List.length ret_nds_t_list - 1) ret_nds_t_list
        in
(*
        let res =
          Util.maprac
            (fun ret_nds_t_list (ret, nds, t1) res ->
              let interp =
                let _ = Format.printf "computing a condition of %a:@,  @[<v>" Var.pr ret in
                let _, ret_interp_list = List.split res in
                let t2 =
                  let fes =
                    Fes.band
                      [Fes.make
                        []
                        ((**)List.map Util.trd3 arg_p_t_list @(**)
                         List.map Util.trd3 ret_nds_t_list @
                         List.map snd ret_interp_list);
                       fes_of_nodes nds0]
                  in
                  FormulaUtil.simplify (Fes.formula_of (Fes.eqelim (RefType.visible ret) fes))
                in
                let interp = CsisatInterface.interpolate_bvs (RefType.visible ret) t1 t2 in
                let _ = Format.printf "@]@," in
                interp
              in
              let arg_p_interp_list =
                Util.maplac
                  (fun arg_p_interp_list (arg, p, t) arg_p_t_list ->
                    let _ = Format.printf "computing a condition of %a:@,  @[<v>" Var.pr arg in
                    let interp =
                      let t1 = t in
                      let t2 =
                        let fes =
                          Fes.band
                            [Fes.make
                              []
                              (List.map Util.trd3 arg_p_interp_list @
                               List.map Util.trd3 arg_p_t_list @
                               Formula.bnot interp;
                             fes_of_nodes nds]
                        in
                        FormulaUtil.simplify (Fes.formula_of (Fes.eqelim (RefType.visible arg) fes))
                      in
                      CsisatInterface.interpolate_bvs (RefType.visible arg) t1 t2
                    in
                    let _ = Format.printf "@]@," in
                    (arg, p, interp))
                  (List.take (List.length ret_nds_t_list + 1) arg_p_t_list)
              in
              arg_p_interp_list,
              (ret, interp))
            ret_nds_t_list
        in
*)
        let arg_p_interp_list_list, ret_interp_list = [], [](*List.split res*) in
        let arg_p_interp_list =
          Util.maplac
            (fun arg_p_interp_list (arg, p, t) arg_p_t_list ->
              let _ = Format.printf "computing a condition of %a:@,  @[<v>" Var.pr arg in
              let interp =
                let t1 = t in
                let t2 =
                  let fes =
                    Fes.band
                      [Fes.make
                        []
                        (List.map Util.trd3 arg_p_interp_list @
                         List.map Util.trd3 arg_p_t_list @
                         List.map snd ret_interp_list);
                       fes_of_nodes nds0]
                  in
                  FormulaUtil.simplify (Fes.formula_of (Fes.eqelim (RefType.visible arg) fes))
                in
                CsisatInterface.interpolate_bvs (RefType.visible arg) t1 t2
              in
              let _ = Format.printf "@]@," in
              (arg, p, interp))
            arg_p_t_list
        in
        List.concat arg_p_interp_list_list @ arg_p_interp_list,
        ret_interp_list
    in
    List.map (fun (ret, interp) -> `P(ret, interp)) ret_interp_list @
    List.map (fun (arg, _, interp) -> `P(arg, interp)) arg_p_interp_list,
    (if nd.closed then [subst_interps env (root loc) ret_interp_list] else []) @
    List.filter_map (fun (_, p, interp) -> subst_interp false p interp) arg_p_interp_list

let summary_of_widen env (Loc(Node(nd, []), p) as loc) = assert false
(*
  let arg = arg_of env nd in
  let _ = Format.printf "computing a condition of %a:@,  @[<v>" Var.pr arg in
  let parginterps, interp =
    try
      let interp =
        let (tts, tps), xss =
          let trs, ps = (* no need to reverse? *)rec_calls_of (fst nd.name) loc in
          List.split
            (List.map2
              (fun tr p ->
                let arg = arg_of env (get tr) in
(*
                let _ = Format.printf "%a@," Var.pr arg in
*)
                Term.fresh
                  (RefType.visible arg)
                  (FormulaUtil.simplify
                    (Fes.formula_of
                      (Fes.eqelim (RefType.visible arg) (fes_of_nodes (nodes_of_tree tr))))),
                Term.fresh
                  (RefType.visible arg)
                  (FormulaUtil.simplify
                    (Fes.formula_of
                      (Fes.eqelim (RefType.visible arg) (fes_of_nodes (nodes_of_path p))))))
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
        interpolate_widen_bvs (RefType.visible arg) nd.closed t1 t2 tw1 tw2
      in
      let _ = Format.printf "@]@," in
      [], interp
    with CsisatInterface.NoInterpolant ->
      let _ = Format.printf "*******@ " in
      let argps, nds =
        let locs = related_locs loc in
        List.map
          (fun (Loc(tr, p)) ->
            RefType.find_last_base env (get tr).name, left_of_path p)
          locs,
        Util.concat_map
          (fun (Loc(tr, _)) -> nodes_of_tree tr)
          locs
      in
      let ts0 =
        List.map
          (fun (arg, p) ->
            let t = Term.fresh (RefType.visible arg) (FormulaUtil.simplify (Fes.formula_of (Fes.eqelim (RefType.visible arg) (fes_of_nodes (nodes_of_path p))))) in
            (*let _ = Format.printf "%a: %a@," Var.pr arg Term.pr t in*)
            t)
          argps
      in
      let interp =
        let (tts, tps), xss =
          let trs, ps = (* no need to reverse? *)rec_calls_of (fst nd.name) loc in
          List.split
            (List.map2
              (fun tr p ->
                let arg = arg_of env (get tr) in
(*
                let _ = Format.printf "%a@," Var.pr arg in
*)
(* why not compute ts0 and nds? *)
(*
                if nd.closed then
                  Term.fresh (RefType.visible arg) (FormulaUtil.simplify (Fes.formula_of (Fes.eqelim (RefType.visible arg) (fes_of_nodes (nodes_of_tree tr))))),
                  let ts, xttys = fes_of_nodes (nds @ nodes_of_path p) in
                  Term.fresh (RefType.visible arg) (FormulaUtil.simplify (Fes.formula_of (Fes.eqelim (RefType.visible arg) (ts0 @ ts, xttys))))
                else
*)
                  let ts, xttys = fes_of_nodes (nds @ nodes_of_tree tr) in
                  Term.fresh (RefType.visible arg) (FormulaUtil.simplify (Fes.formula_of (Fes.eqelim (RefType.visible arg) (ts0 @ ts, xttys)))),
                  Term.fresh (RefType.visible arg) (FormulaUtil.simplify (Fes.formula_of (Fes.eqelim (RefType.visible arg) (fes_of_nodes (nodes_of_path p))))))
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
        interpolate_widen_bvs (RefType.visible arg) nd.closed t1 t2 tw1 tw2
      in
      let _ = Format.printf "@]@," in
      let _, parginterps =
        let tr = Node(nd, []) in
        List.fold_left
          (fun (t0::ts0, parginterps) (arg, p) ->
            let _ = Format.printf "computing a condition of %a:@,  @[<v>" Var.pr arg in
            let interp =
              let t1 = t0 in
              let t2 =
                let ts, xttys = fes_of_nodes (nds @ nodes_of_tree tr) in
                FormulaUtil.simplify (Fes.formula_of (Fes.eqelim (RefType.visible arg) ((if nd.closed then Formula.bnot interp else interp)::ts0 @ ts, xttys)))
              in
              CsisatInterface.interpolate_bvs (RefType.visible arg) t1 t2
            in
            let _ = Format.printf "@]@," in
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
        let ts1, t::ts2 = List.split_nth (List.length trs1) nd.constr in
        let xttyss1, xttys::xttyss2 = List.split_nth (List.length trs1) nd.subst in
(*
        let xts = List.map (fun (x, t, _) -> x, t) xttys in
        let sub x = List.assoc x xts in
*)
        let ts2 = match ts2 with t'::ts2' -> (Formula.band [t; (*Term.subst sub*) interp; t'])::ts2' | [] -> assert false in
(**)
        let xttyss2 = match xttyss2 with xttys'::xttyss2' -> (xttys @ xttys')::xttyss2' | [] -> assert false in
(**)
        (root (Loc(Node({ nd with constr = ts1 @ ts2;
                                  subst = xttyss1 @ xttyss2 }, trs1 @ trs2), up))))::
    Util.concat_map
      (fun (Path(up, trs1, nd, []), _, interp) ->
        if Term.equiv interp Formula.ttrue then
          []
        else
          let ts1, t::[] = List.split_nth (List.length trs1) nd.constr in
          let xttyss1, xttys::[] = List.split_nth (List.length trs1) nd.subst in
(**)
          let xts = List.map (fun (x, t, _) -> x, t) xttys in
          let sub x = List.assoc x xts in
(**)
          [root (Loc(Node({ nd with ret = None;
                                    closed = false;
                                    constr = ts1 @ [Formula.band [t; Formula.bnot ((*Term.subst sub*) interp)]];
                                    subst = xttyss1 @ [(*[]*)(**)xttys(**)] }, trs1), path_set_open up))])
      parginterps
  else
    let _ = assert (nd.ret = None) in
    [`P(arg, interp)],
    match p with
      Top -> let _ = assert (interp = Formula.ttrue) in []
    | Path(up, trs1, nd, []) ->
        let ts1, t::[] = List.split_nth (List.length trs1) nd.constr in
        let xttyss1, xttys::[] = List.split_nth (List.length trs1) nd.subst in
(**)
        let xts = List.map (fun (x, t, _) -> x, t) xttys in
        let sub x = List.assoc x xts in
(**)
        [root (Loc(Node({ nd with constr = ts1 @ [Formula.band [t; Formula.bnot ((**)Term.subst sub(**) interp)]];
                                  subst = xttyss1 @ [[](*xttys*)] }, trs1), up))]
*)


let summaries_of env constrss0 =
  let rec summaries_of_aux sums constrss =
    match constrss with
      [] -> sums
   | constrs::constrss' ->
        (**)
        let _ = Format.printf "constraints:@,  %a@," pr constrs in
        (**)
        let sums', constrss'' =
          try
            let loc =
              let locs = List.rev (find_leaves constrs) in
              let locs = List.filter (fun loc -> Util.diff (related_locs loc) locs = []) locs in
              let _ = assert (locs <> []) in
              match !Global.predicate_discovery with
                ConvexHull ->
                  let locs' = List.filter (fun (Loc(Node(nd, []), p) as loc) -> is_recursive nd.name loc) locs in
                  (match locs' with
                    [] -> List.hd locs
                  | loc::_ -> loc)
              | Backward ->
                  List.hd locs(*find_leaf constrs*)
            in
            (match !Global.predicate_discovery with
              ConvexHull -> summary_of_widen env loc
            | Backward -> summary_of env loc)
          with CsisatInterface.NoInterpolant ->
            raise (FeasibleErrorTrace(constrs(**ToDo*)))
        in
        match constrss with
          [] -> sums' @ sums
        | _ ->
            summaries_of_aux (sums' @ sums) (constrss'' @ constrss')
(*
      Format.printf "@,";
*)
  in
  summaries_of_aux [] constrss0
