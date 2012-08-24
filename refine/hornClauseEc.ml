open ExtList
open ExtString
open HornClause

(** Equivalence classes of Horn clauses *)

let fvs_of_ec ec = List.unique (Util.concat_map (function `L(p) -> Atom.fvs p | `R(t) -> Term.fvs t) ec)
let pids_of_ec ec = Util.concat_map (function `L(pid, _) -> [pid] | `R(_) -> []) ec
let preds_of_ec ec = List.filter_map (function `L(p) -> Some(p) | `R(_) -> None) ec
let terms_of_ec ec = List.filter_map (function `L(_) -> None | `R(t) -> Some(t)) ec
let embed_preds afs = List.map (fun p -> `L(p)) afs
let embed_terms ts = List.map (fun t -> `R(t)) ts

let rec rel bvs xs1 xs2 =
  match xs1, xs2 with
    `L(p1), `L(p2) ->
      Util.intersects
						  (Util.diff (Atom.fvs p1) bvs)
								(Util.diff (Atom.fvs p2) bvs)
  | `L(p1), `R(t2) ->
      Util.intersects
						  (Util.diff (Atom.fvs p1) bvs)
								(Util.diff (Term.fvs t2) bvs)
  | `R(t1), `L(p2) ->
      rel bvs (`L(p2)) (`R(t1))
  | `R(t1), `R(t2) ->
      Util.intersects
						  (Util.diff (Term.fvs t1) bvs)
								(Util.diff (Term.fvs t2) bvs)

(** unsound for non-linear expressions? *)
let rec subst_formula p afs t =
  (*if Term.coeffs t <> [] then
    afs, t
  else*)
  (*Format.printf "input: %a@," Term.pr t;*)
  let ts = Formula.conjuncts t in
  let xttys, t = Formula.extract_from2 (Util.concat_map Atom.fvs afs) p ts in
  (*Format.printf "xttys: %a@,t: %a@," TypSubst.pr xttys Term.pr t;*)
  let afs, t =
    if xttys = [] then
      afs, t
    else
      let sub = TypSubst.fun_of xttys in
      subst_formula p
        (List.map (Atom.subst_fixed sub) afs)
        (Term.subst_fixed sub t)
  in
  (*Format.printf "output: %a@," Term.pr t;*)
  afs, t

let matches env xs ttys1 ttys2 =
  let _ = Global.log_begin "HornClauseEc.matches" in
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
                  let _ = Global.log (fun () -> Format.printf "??t1: %a@,??t2: %a@," Term.pr t1 Term.pr t2) in
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
  let _ = Global.log_end "HornClauseEc.matches" in
  xttys

let xttyss_of env q afs1 afs2 =
  try
    let ttys_tss_s =
      List.map
        (fun p1 ->
          snd p1,
          let afs =
            List.filter_map
              (fun p2 ->
                if Atom.matches q env p2 p1 then
                  Some(List.map fst (snd p2))
                else
                  None)
              afs2
          in
          if afs = [] then raise Not_found else afs)
        afs1
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
            (*let _ = Format.printf "duplicate: %a@," TypSubst.pr xttys in*)
            false)
        xttyss
    in
    xttyss
  with Not_found ->
    []

let ignored_vars bvs afs =
  let _ = Global.log_begin "HornClauseEc.ignored_vars" in
  let tss =
    let afss = Util.classify (fun (pid1, _) (pid2, _) -> pid1 = pid2) afs in
    Util.concat_map
      (fun afs -> Util.transpose (List.map (fun p -> List.map fst (snd p)) afs))
      afss
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
  let _ = Global.log (fun () -> Format.printf "xs: %a@," Var.pr_list xs) in
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
        afs)
  in
  let _ = Global.log (fun () -> Format.printf "ys: %a@," Var.pr_list ys) in
  let res = xs @ ys in
  let _ = Global.log_end "HornClauseEc.ignored_vars" in
  res

let changing_vars bvs afs =
  let tss =
    let afss = Util.classify (fun (pid1, _) (pid2, _) -> pid1 = pid2) afs in
    Util.concat_map
      (fun afs -> Util.transpose (List.map (fun p -> List.map fst (snd p)) afs))
      afss
  in
  Util.diff
    (List.unique
      (Util.concat_map
        (fun ts -> let ts = List.unique ts in match ts with [_] -> [] | _ -> Util.concat_map Term.fvs ts)
        tss))
    bvs

let share_predicates bvs0 _ afs t =
  let _ = Global.log_begin ~disable:true "HornClauseEc.share_predicates" in
  let t = Formula.simplify t in
  let res =
		  if Term.coeffs t <> [] || Atom.num_dup afs = 0 then
		    afs, t
		  else
		    let share_predicates_aux cvs bvs afs t =
		      let ts = Formula.conjuncts t in
		      let ecs, env, zs =
		        let env, ts1 = List.partition (fun t -> Util.subset (Term.fvs t) bvs) ts in
		        let afs0, afs1 = List.partition (fun p -> Util.subset (Atom.fvs p) bvs) afs in
		        let afs0 =
		          Util.concat_map
		            (Util.representatives (Atom.equiv env))
		            (Util.classify (fun (pid1, _) (pid2, _) -> pid1 = pid2) afs0)
		        in
		        let ecs = Util.equiv_classes (rel bvs) (embed_preds afs1(* redundant *) @ embed_terms ts1) in
		        let zs =
		          let zs = Util.diff (List.unique (Util.concat_map Atom.fvs afs0)) bvs0 in
		          List.filter
		            (fun z ->
		              let ecs' = List.filter (fun ec -> List.mem z (fvs_of_ec ec)) ecs in
		              match ecs' with
		                [_] -> true
		              | _ -> false)
		            zs
		        in
		        (if afs0 = [] then [] else [embed_preds afs0]) @ ecs,
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
		                (*let _ = if xs <> [] then assert false in*)
		                let ts = Formula.conjuncts (AtpInterface.integer_qelim (Formula.exists (List.map (fun x -> x, SimType.Int) xs) t)) in
		                List.map (fun t -> `R(t)) ts
		              with Util.NotImplemented _ -> ec)
		            else
		              ec)
		          ecs
		      in
		      let _ =
		        Global.log (fun () ->
		          let _ = Format.printf "bvs: %a@," Var.pr_list bvs in
		          let _ = Format.printf "env: %a@," Term.pr (Formula.band env) in
		          List.iter
		            (fun ec ->
		              let afs, ts = Util.partition_map (fun x -> x) ec in
		              Format.printf "ec: %a@," pr_elem (Hc(None, afs, Formula.band ts)))
		            ecs)
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
		                    List.exists (fun ttys' -> Atom.equiv env (pid, ttys) (pid, ttys')) ttyss)
		                  pxs
		              in
		              let _ =
		                Global.log (fun () ->
		                  if b then
		                    Format.printf "succeeded@,"
		                  else
		                    let _ = Format.printf "ts: %a@," (Util.pr_list Term.pr ",") ts in
		                    let _ = List.iter (fun (pid, ttys, _, _) -> Format.printf "ttys: %a@," (Util.pr_list Term.pr ",") (List.map fst ttys)) pxs in
		                    Format.printf "failed:@,")
		              in
		              b
		              (*let pxs' =
		                List.filter
		                  (fun (pid, ttys, _, ttyss) ->
		                    List.for_all (fun ttys' -> not (Atom.equiv env (pid, ttys) (pid, ttys'))) ttyss)
		                  pxs
		              in
		              (match pxs' with
		                [] ->
		                  true
		              | [pid, ttys, _, ttyss] ->
		                  let xs = Util.diff (List.unique (Util.concat_map (fun (t, _) -> Term.fvs t) ttys)) bvs0 in
		                  let _ = Format.printf "osii: %a@," Var.pr_list xs in
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
		                  let _ = Global.log (fun () -> Format.printf "xttys: %a@," TypSubst.pr xttys) in
		                  let ys = List.map Util.fst3 xttys in
		                  let pxs =
		                    List.sort
		                      ~cmp:(fun (_, _, _, ttyss1) (_, _, _, ttyss2) -> List.length ttyss1 - List.length ttyss2)
		                      (List.map
		                        (fun (pid, ttys, xs0, ttyss) ->
		                          let xs = Util.diff xs0 ys in
		                          if xs <> xs0 then
		                            let _ = Global.log (fun () -> Format.printf "pid: %a@," Var.pr pid) in
		                            let pid, ttys = Atom.simplify (Atom.subst (TypSubst.fun_of xttys) (pid, ttys)) in
		                            let ttyss =
		                              List.filter (fun ttys' -> Atom.matches (fun x -> List.mem x xs) env (pid, ttys') (pid, ttys)) ttyss
		                            in
		                            pid, ttys, xs, ttyss
		                          else
		                            pid, ttys, xs0, ttyss)
		                        pxs)
		                  in
		                  let ts = List.map (fun t -> Formula.simplify (Term.subst (TypSubst.fun_of xttys) t)) ts in
		                  let b = aux pxs ts in
		                  let _ = Global.log (fun () -> if not b then Format.printf "backtracked@,") in
		                  b)
		                xttyss
		        in
		        let afs = preds_of_ec ec2 in
		        aux
		          (List.sort
		            ~cmp:(fun (_, _, _, ttyss1) (_, _, _, ttyss2) -> List.length ttyss1 - List.length ttyss2)
		            (List.map
		              (fun ((pid, ttys) as p1) ->
		                let xs = Util.diff (Atom.fvs (pid, ttys)) bvs in
		                let ttyss =
		                  List.filter_map
		                    (fun ((_, ttys') as p2) ->
		                      if Atom.matches (fun x -> List.mem x xs) env p2 p1 then Some(ttys') else None)
		                    afs
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
		          let _ = Global.log (fun () -> Format.printf "xs: %a@," Var.pr_list xs) in
		          let xttyss =
		            let afs1 = preds_of_ec ec1 in
		            let afs2 = preds_of_ec ec2 in
		            let xttyss = xttyss_of env (fun x -> not (List.mem x bvs)) afs1 afs2 in
		            List.filter
		              (fun xttys ->
		                if Util.subset xs (List.map Util.fst3 xttys) then
		                  true
		                else
		                  (*let _ = Format.printf "non-covered: %a@," TypSubst.pr xttys in*)
		                  false(*assert false*))
		              xttyss
		          in
		          if List.exists
		               (fun xttys ->
		                 let b =
		                   let ec1' =
		                     List.map
		                       (function
		                         `L(p) -> `L(Atom.simplify (Atom.subst (TypSubst.fun_of xttys) p))
		                       | `R(t) -> `R(Formula.simplify (Term.subst (TypSubst.fun_of xttys) t)))
		                       ec1
		                   in
		                   let afs1, ts1 = Util.partition_map (fun x -> x) (Util.diff ec1' ec2) in
		                   let afs2, ts2 = Util.partition_map (fun x -> x) ec2 in
		                   let _ = Global.log (fun () -> Format.printf "hc1: %a@,hc2: %a@," pr_elem (Hc(None, afs1, Formula.band ts1)) pr_elem (Hc(None, afs2, Formula.band ts2))) in
		                   Cvc3Interface.implies (env @ ts2) ts1 &&
		                   List.for_all
		                     (fun p1 -> List.exists (fun p2 -> Atom.equiv env(*@ ts2 not necessary?*) p1 p2) afs2)
		                     afs1
		                 in
		                 let _ = Global.log (fun () -> if b then Format.printf "xttys: %a@," TypSubst.pr xttys) in
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
		                  let _ = Global.log (fun () -> Format.printf "checking: %a@," (Util.pr_list Atom.pr ",") (preds_of_ec ec)) in
		                  (*if true then*)
		                    is_covered ec (List.flatten (ecs1 @ ecs2))
		                  (*else
		                    let ecs = min_coverings (!Global.disable_pred_sharing1 || not (Util.is_dup (pids_of_ec ec))) ec (ecs1 @ ecs2) in
		                    let _ = List.iter (fun ec -> Format.printf "%a@," (Util.pr_list Atom.pr ",") (preds_of_ec ec)) ecs in
		                    List.exists (fun ec' -> reduce ec ec') ecs*)
		            in
		            aux ecs1 (if b then ecs2 else ec :: ecs2)
		      in
		      let ecs = aux (List.sort ecs) [] in
		      let afs, ts = Util.partition_map (fun x -> x) (List.flatten ecs) in
		      afs, Formula.band (ts @ env), zs
		    in
		    let rec loop cvs bvs' afs t =
		      let _ = Global.log (fun () -> if bvs' <> [] then Format.printf "bvs': %a@," Var.pr_list bvs') in
		      let afs', t', zs = share_predicates_aux cvs (bvs0 @ bvs') afs t in
		      if List.length afs <> List.length afs' && Atom.num_dup afs' > 0 then
		        let bvs'' = ignored_vars bvs0 afs' in
		        let cvs' = Util.diff bvs' bvs'' in
		        if cvs' = [](*Util.set_equiv bvs' bvs''*) then
		          bvs'', afs', t', zs
		        else
		          loop (Some(cvs')) bvs'' afs' t'
		      else
		        bvs', afs', t', zs
		    in
		    let bvs', afs, t, zs = loop None (ignored_vars bvs0 afs) afs t in
		    if not !Global.enable_pred_sharing2 then
		      if !Global.disable_pred_sharing1 then
		        afs, t
		      else (*a-max‚ªrsn 0‚Å‚È‚¢‚Æ¬Œ÷‚µ‚È‚­‚È‚é intro3‚Írsn0‚ÅOK‚É‚È‚é*)
		        try
		          let _ = Global.log (fun () -> if zs <> [] then Format.printf "zs: %a@," Var.pr_list zs) in
		          Util.find_map
		            (fun xs ->
		              let bvs1 = Util.diff bvs' xs in
		              if List.length bvs1 = List.length bvs' then
		                raise Not_found
		              else
		                let afs', t', _ = share_predicates_aux (Some(xs)) (bvs0 @ bvs1) afs t in
		                if List.length afs <> List.length afs' then
		                  afs', t'
		                else
		                  raise Not_found)
		            (Util.nsubsets 1 zs)
		        with Not_found ->
		          afs, t
		    else
		      let zs = Util.inter bvs' (changing_vars bvs0 afs) in
		      let _ = Global.log (fun () -> if zs <> [] then Format.printf "zs: %a@," Var.pr_list zs) in
		      if List.length zs > 7 then
		        afs, t
		      else
		        try
		          Util.find_map
		            (fun xs ->
		              let bvs1 = Util.diff bvs' xs in
		              if List.length bvs1 = List.length bvs' then
		                raise Not_found
		              else
		                let afs', t', _ = share_predicates_aux (Some(xs)) (bvs0 @ bvs1) afs t in
		                if List.length afs <> List.length afs' then
		                  afs', t'
		                else
		                  raise Not_found)
		            (Util.nsubsets 1 zs @ Util.nsubsets 2 zs)
		        with Not_found ->
		          afs, t
  in
  let _ = Global.log_end "HornClauseEc.share_predicates" in
  res

(** integer quantifier elimination of formulas with integers and booleans *)
let qelim_aux fvs t =
  let _ = Global.log_begin "HornClauseEc.qelim_aux" in
  let env = List.map (fun x -> x, SimType.Int) (Util.diff (List.unique (Term.fvs_ty SimType.Int t SimType.Bool)) fvs) in
  let res =
		  if env <> [] && Term.coeffs t = [] then
		    let _ = Global.log (fun () -> Format.printf "input:@,  @[<v>t: %a@,env: %a@]@," Term.pr t SimType.pr_env env) in
		    let ts, f =
		      let tss, f = Formula.elim_boolean [t] in
						  List.map (fun [t] -> t) tss, f
						in
		    f
		      (List.map
		        (fun t ->
		          try
				          let _ = Global.log (fun () -> Format.printf "before: %a@," Term.pr t) in
												  let t' =
																Formula.simplify
																  (AtpInterface.integer_qelim
																		  (Formula.exists env t))
													 in
				          let _ = Global.log (fun () -> Format.printf "after: %a@," Term.pr t') in
				          if try Formula.disjunctive t' with Util.NotImplemented _ -> true then
		              t
														else
														  t'
		          with Util.NotImplemented _ ->
												  t)
		        ts)
		  else
		    t
  in
  let _ = Global.log_end "HornClauseEc.qelim_aux" in
  res

(** integer quantifier elimination of formulas with integers and booleans *)
let qelim fvs t =
  let _ = Global.log_begin "HornClauseEc.qelim" in
  let _ = Global.log (fun () -> Format.printf "input:@,  @[<v>t: %a@,fvs: %a@]@," Term.pr t Var.pr_list fvs) in
  let ts = Formula.conjuncts t in
  let ecs = Util.equiv_classes (rel fvs) (embed_terms ts) in
  let t = Formula.band (List.map (fun ec -> qelim_aux fvs (Formula.band (terms_of_ec ec))) ecs) in
  let _ = Global.log_end "HornClauseEc.qelim" in
  t

let simplify2 bvs t =
  let t =
    let xs = Util.diff (List.unique (Term.fvs_ty SimType.Int t SimType.Bool)) bvs in
    let t =
      let sub, t =
        Formula.extract_from [] (fun x -> not (List.mem x xs)) t
      in
      Term.subst sub t
    in
    let [], t = subst_formula (fun x -> not (List.mem x xs)) [] t in
    t
  in
  qelim bvs t
(*
  let p x = List.mem x bvs || Var.is_coeff x in
  Formula.band
    (Util.map_left_right
      (fun ls t rs ->
        let xs =
          List.filter
            (fun x -> not (p x))
            (Util.diff
              (Term.fvs_ty SimType.Int t SimType.Bool)
              (Util.concat_map (fun t -> Term.fvs_ty SimType.Int t SimType.Bool) (ls @ rs)))
        in
        if xs <> [] && Term.coeffs t = [] then
          try
            let tss, f = Formula.elim_boolean [t] in
            let ts = List.map (fun [t] -> t) tss in
            f (List.map (fun t -> AtpInterface.integer_qelim (Formula.exists (List.map (fun x -> x, SimType.Int) xs) t)) ts)
          with Util.NotImplemented _ ->
            t
        else
          t)
      (Formula.conjuncts t))
*)

let simplify_aux bvs bs (Hc(popt, afs, t)) =
  let _ = Global.log_begin ~disable:true "HornClauseEc.simplify" in
  let _ = Global.log (fun () -> Format.printf "input:@,  @[<v>%a@]@," pr_elem (Hc(popt, afs, t))) in
  let shared = ref (List.length afs) in
  let bvs = bvs @ (match popt with None -> [] | Some(_, xtys) -> List.map fst xtys) in
  let bs, afs, t =
    let _ = Global.log_begin "simplifying formula" in
    let _ = Global.log (fun () -> Format.printf "input:@,  @[<v>%a@]@," Term.pr t) in
    let afs, t =
      let sub, t =
        Formula.extract_from
          (match popt with None -> [] | Some(pid, _) -> [pid])
          (fun x -> List.mem x bvs || Var.is_coeff x) t
      in
      List.map (Atom.subst sub) afs, Term.subst sub t
    in
    let _ = Global.log (fun () -> Format.printf "a:@,  @[<v>%a@]@," Term.pr t) in
    let t =
      let xs = List.unique (bvs @ Util.concat_map Atom.fvs afs) in
      simplify2 xs t
      (*
      let t = Term.simplify (AtpInterface.qelim_fes (diff bvs (fvs afs)) t) in
      *)
    in
    let _ = Global.log (fun () -> Format.printf "b:@,  @[<v>%a@]@," Term.pr t) in
    let afs, t =
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
            (Term.subst (TypSubst.fun_of sub) t ::
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
      let afs0 = afs(*List.map (Atom.subst (TypSubst.fun_of sub)) afs*) in
      let _ = Global.log (fun () -> Format.printf "!a:%a@," Term.pr t0) in
      afs0,
      if Term.fvs_ty SimType.Bool t0 SimType.Bool = [] then
        let t' = Formula.elim_eq_neq_boolean t0 in
        let _ = Global.log (fun () -> Format.printf "!b:%a@," Term.pr t') in
        let t' = Formula.elim_imply_iff t' in
        let _ = Global.log (fun () -> Format.printf "!c:%a@," Term.pr t') in
        if t0 <> t' then
          let t' = Formula.of_dnf (Formula.dnf t') in
          let _ = Global.log (fun () -> Format.printf "!d:%a@," Term.pr t') in
          let t' = Formula.simplify t' in
          let _ = Global.log (fun () -> Format.printf "boolean equalities eliminated:@,  @[<v>before: %a@,after: %a@]@," Term.pr t Term.pr t') in
          t'
        else
          t0
      else
        t0
    in
    let _ = Global.log (fun () -> Format.printf "c:@,  @[<v>%a@]@," Term.pr t) in
    let afs, t = subst_formula (fun x -> List.mem x bvs || Var.is_coeff x) afs t in
    let _ = Global.log (fun () -> Format.printf "output:@,  @[<v>%a@]" Term.pr t) in
    let _ = Global.log_end "simplifying formula" in
    let afs = List.map Atom.simplify afs in
    let rec unique bs afs =
      match bs, afs with
         [], [] ->
           [], []
      |  b::bs, p::afs ->
           if List.mem p afs then
             let bs, afs = List.split (Util.filter_map2 (fun b p' -> if p <> p' then Some(b, p') else None) bs afs) in
             let bs, afs = unique bs afs in
             false(*??*) :: bs, p :: afs
           else
             let bs, afs = unique bs afs in
             b :: bs, p :: afs
    in
    let bs, afs = unique bs afs in
    bs, afs, t
  in
  let afs, t = share_predicates bvs bs afs t in
  let res = Hc(popt, afs, t) in
  let _ =
    let _ = shared := !shared - List.length afs in
    Global.log (fun () -> if !shared <> 0 then Format.printf "# of shared predicate variables: %d@," !shared)
  in
  let _ =
    let n = Atom.num_dup afs in
    Global.log (fun () -> if n <> 0 then Format.printf "# of duplicate predicate variables: %d@," n)
  in
  let _ = Global.log (fun () -> Format.printf "output:@,  @[<v>%a@]" pr_elem res) in
  let _ = Global.log_end "HornClauseEc.simplify" in
  res

let simplify bvs (Hc(_, afs, _) as hc) = simplify_aux bvs (List.map (fun _ -> false(*???*)) afs) hc

let subst_hcs hcs (Hc(popt, afs, t) as hc) =
  let _ = Global.log_begin "subst_hcs" in
  let _ = Global.log (fun () -> Format.printf "input:@,  @[<v>%a@]@," pr_elem hc) in
  let hc =
    if !Global.subst_hcs_inc then
      let rec aux afs t =
        try
          let lafs, (pid, ttys), rafs =
            Util.pick (fun (pid, _) -> mem pid hcs) afs
          in
          let _ = Global.log (fun () -> Format.printf "%a is substituted@," Var.pr pid) in
          let Hc(_, afs, t) =
  										let afs, t' = lookup (pid, ttys) hcs in
            let bs =
              List.map (fun _ -> false) lafs @
              List.map (fun _ -> true) afs @
              List.map (fun _ -> false) rafs
            in
            simplify_aux [] bs (Hc(popt, lafs @ afs @ rafs, Formula.band [t; t']))
          in
          aux afs t
        with Not_found ->
          Hc(popt, afs, t)
      in
      aux afs t
    else
      let bs, afs, t =
        let bss, afss, ts =
          Util.unzip3
            (List.map
              (fun (pid, ttys) ->
                try
                  let res = lookup (pid, ttys) hcs in
                  let _ = Global.log (fun () -> Format.printf "%a is being substituted@," Var.pr pid) in
                  List.map (fun _ -> true) (fst res), fst res, snd res
                with Not_found ->
                  [false], [pid, ttys], Formula.ttrue)
              afs)
        in
        List.flatten bss, List.flatten afss, Formula.band (t::ts)
      in
      simplify_aux [] bs (Hc(popt, afs, t))
  in
  let _ = Global.log (fun () -> Format.printf "output:@,  @[<v>%a@]" pr_elem hc) in
  let _ = Global.log_end "subst_hcs" in
  hc

let subst_hcs_fixed hcs hc =
  Util.fixed_point
    (fun hc ->
      (*Format.printf "%a@." pr_elem hc;*)
      subst_hcs hcs hc)
    (fun hc1 hc2 ->
      match hc1, hc2 with
        Hc(_, afs1, _), Hc(_, afs2, _) -> Util.set_equiv afs1 afs2)
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
