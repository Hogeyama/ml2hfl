open ExtList
open ExtString
open Util
open HornClause

(** Utility functions on Horn clauses *)

(** {6 Functions for eliminating quantifiers} *)

(** @param p variables in {x | p x} are not eliminated *)
let rec qelim_aux p atms t =
  if false (*Term.coeffs t <> []*) then
    (** @todo unsound for parametric linear expressions? *)
    atms, t
  else
    let _ = Global.log_begin "HornClauseUtil.qelim_aux" in
    let _ = Global.log (fun () -> Format.printf "input: %a@," Term.pr t) in
    let ts = Formula.conjuncts t in
    let xttys, t =
      let pvs = Util.concat_map Atom.fvs atms in
      let nlfvs = LinArith.nlfvs t in
      let pred x t ty=
        not (p x) &&
        (if Term.coeffs t <> [] then not (List.mem x pvs) else true) &&
        (** avoid substituting a non-linear integer expression to other one
            raising the order of parameters for parametric linear expressions sometimes
            causes a failure of the bit-vector-based non-linear constraint solving *)
        (if List.mem x nlfvs then Formula.is_linear t else true)
      in
      let xttys, t = TypSubst.xttys_of pred ts in
      let _ = Global.log (fun () ->
        Format.printf "xttys: %a@,t: %a@," TypSubst.pr xttys Term.pr t)
      in
      xttys, t
    in
    let atms, t =
      if xttys = [] then
        atms, t
      else
        let sub = TypSubst.sub_of xttys in
        let atms = List.map (Atom.subst_fixed sub) atms in
        let t = (* may not terminate *)
          FormulaUtil.subst_fixed ~simplify:FormulaUtil.simplify sub t
        in
        qelim_aux p atms t
    in
    let _ = Global.log (fun () -> Format.printf "output: %a@," Term.pr t) in
    let _ = Global.log_end "HornClauseUtil.qelim_aux" in
    atms, t

(** @param bvs variables in bvs are not eliminated *)
let qelim bvs pids atms t =
  let _ = Global.log_begin "HornClauseUtil.qelim" in
  let _ =
    Global.log (fun () -> Format.printf "input:@,  @[<v>%a@]@," Term.pr t)
  in
    (atms, t)
  |> (* fast quantifier elimination *)
    (fun (atms, t) ->
      let xttys, t =
        TypSubst.xytys_of
          pids
          (fun x -> List.mem x bvs || Var.is_coeff x)
          t
      in
      let sub = TypSubst.sub_of xttys in
      let atms = List.map (Atom.subst sub) atms in
      let t = FormulaUtil.subst sub (FormulaUtil.simplify t) in
      let _ = Global.log (fun () -> Format.printf "1:@,  @[<v>%a@]@," Term.pr t) in
      atms, t)
  |>
    (fun (atms, t) ->
      let bvs = List.unique (bvs @ Util.concat_map Atom.fvs atms) in
      let elim_vars =
        let int_vars = Formula.fvs_int t in
        Util.diff int_vars bvs
      in
      let t =
        let xttys, t =
          TypSubst.xytys_of
            []
            (fun x -> not (List.mem x elim_vars))
            t
        in
        let sub = TypSubst.sub_of xttys in
        let t' = FormulaUtil.subst sub (FormulaUtil.simplify t) in
        t' (** @todo I wonder if t = t' for any case *)
      in
      let [], t = qelim_aux (fun x -> not (List.mem x elim_vars)) [] t in
      let t = AtpInterface.simplify bvs t in
      let _ = Global.log (fun () -> Format.printf "2:@,  @[<v>%a@]@," Term.pr t) in
      atms, t)
  |>
    (fun (atms, t) ->
      if true then
        atms, t
      else
        let bvs = List.unique (bvs @ Util.concat_map Atom.fvs atms) in
        let t =
          Formula.band
            (Util.maplr
              (fun ls t rs ->
                if Term.coeffs t = [] then
                  t
                else
                  let elim_vars =
                    List.filter
                      (fun x -> not (List.mem x bvs || Var.is_coeff x))
                      (Util.diff
                        (Formula.fvs_int t)
                        (Util.concat_map Formula.fvs_int (ls @ rs)))
                  in
                  if elim_vars = [] then
                    t
                  else
                    try
                      let tss, f = FormulaUtil.split_cases_boolean [t] in
                      let ts = List.map Util.elem_of_singleton tss in
                      f
                        (List.map
                          (fun t ->
                            AtpInterface.integer_qelim
                              (Formula.exists (List.map (fun x -> x, SimType.Int) elim_vars) t))
                          ts)
                    with Util.NotImplemented _ ->
                      t)
              (Formula.conjuncts t))
        in
        let _ = Global.log (fun () -> Format.printf "3:@,  @[<v>%a@]@," Term.pr t) in
        atms, t)
  |>
    (fun (atms, t) ->
      let t =
        if true then
          t
        else
          let rec aux ts1 ts2 =
            match ts1 with
              [] -> Formula.band ts2
            | t :: ts' ->
                if Cvc3Interface.implies (ts' @ ts2) [t] then
                  aux ts' ts2
                else
                  aux ts' (t :: ts2)
          in
          aux (Formula.conjuncts t) []
      in
      let _ = Global.log (fun () -> Format.printf "4:@,  @[<v>%a@]@," Term.pr t) in
      atms, t)
  |>
    (fun (atms, t) -> (* constant propagation *)
      let sub =
        let pred x t ty =
          List.mem x bvs &&
          (ty = SimType.Bool && (t = Formula.ttrue || t = Formula.tfalse) ||
           false && ty = SimType.Int && Term.is_int_const t)
        in
        List.filter_map
          (fun t ->
            try
              Some(TypSubst.xtty_of_formula pred t)
            with Not_found ->
              None)
          (Formula.conjuncts t)
      in
      let t =
        let t0 =
          (FormulaUtil.subst (TypSubst.sub_of sub) t ::
          List.map
            (fun (x, t, _) ->
              if t = Formula.ttrue then
                Term.make_var x
              else if t = Formula.tfalse then
                Formula.bnot (Term.make_var x)
              else if Term.is_int_const t then
                let _ = assert false in Formula.eqInt (Term.make_var x) t
              else
                assert false)
            sub) |>
          Formula.band |>
          FormulaUtil.simplify
        in
        if Formula.fvs_bool t0 <> [] then
          t0
        else (* eliminate =b and <>b *)
          let t1 =
            t0 |>
            FormulaUtil.elim_eq_neq_boolean |>
            FormulaUtil.elim_imply_iff
          in
          if t0 = t1 then
            t0
          else
            t1 |>
            FormulaUtil.dnf |>
            FormulaUtil.of_dnf |>
            FormulaUtil.simplify
      in
      let atms =
        if true then
          atms
        else
          List.map (Atom.subst (TypSubst.sub_of sub)) atms
      in
      let _ = Global.log (fun () -> Format.printf "5:@,  @[<v>%a@]@," Term.pr t) in
      atms, t)
  |>
    (fun (atms, t) ->
      let atms, t = qelim_aux (fun x -> List.mem x bvs || Var.is_coeff x) atms t in
      let _ = Global.log (fun () -> Format.printf "6:@,  @[<v>%a@]@," Term.pr t) in
      atms, t)
  |>
    (fun (atms, t) ->
      let atms = List.unique (List.map Atom.simplify atms) in
      let _ = Global.log (fun () -> Format.printf "output:@,  @[<v>%a@]" Term.pr t) in
      let _ = Global.log_end "HornClauseUtil.qelim" in
      atms, t)

(** {6 Predicate sharing} *)

let ignored_vars bvs atms =
  let _ = Global.log_begin "HornClauseUtil.ignored_vars" in
  let tss =
    let atmss = Util.classify (fun (pid1, _) (pid2, _) -> pid1 = pid2) atms in
    Util.concat_map
      (fun atms -> Util.transpose (List.map (fun p -> List.map fst (snd p)) atms))
      atmss
  in
  let xs =
    Util.diff
      (List.unique
        (Util.concat_map
          (fun ts ->
            Util.get_dup_elems (Util.concat_map (fun t -> List.unique (Term.fvs t)) ts)
            (*Util.concat_map Term.fvs (Util.get_dup_elems ts)*))
          tss))
      bvs
  in
  let _ = Global.log (fun () -> Format.printf "xs: %a@," Var.pr_list xs) in
  let ys =
    List.unique
      (Util.concat_map
        (fun (_, ttys) ->
          List.flatten
            (Util.maplr
              (fun ttys1 (t, _) ttys2 ->
                let zs = List.unique (Term.fvs t) in
                if List.length zs > 1 then
                  Util.diff zs (xs @ (Util.concat_map (fun (t, _) -> Term.fvs t) (*???*)(ttys1 @ ttys2)))
                else
                  [])
              ttys))
        atms)
  in
  let _ = Global.log (fun () -> Format.printf "ys: %a@," Var.pr_list ys) in
  let res = xs @ ys in
  let _ = Global.log_end "HornClauseUtil.ignored_vars" in
  res

let changing_vars bvs atms =
  let tss =
    let atmss = Util.classify (fun (pid1, _) (pid2, _) -> pid1 = pid2) atms in
    Util.concat_map
      (fun atms -> Util.transpose (List.map (fun p -> List.map fst (snd p)) atms))
      atmss
  in
  Util.diff
    (List.unique
      (Util.concat_map
        (fun ts -> let ts = List.unique ts in match ts with [_] -> [] | _ -> Util.concat_map Term.fvs ts)
        tss))
    bvs

(** {5 Functions on bodies of Horn clauses} *)

let fvs_body bd =
  List.unique (Util.concat_map (function `L(p) -> Atom.fvs p | `R(t) -> Term.fvs t) bd)
let pids_body bd =
  Util.concat_map (function `L(pid, _) -> [pid] | `R(_) -> []) bd

let atoms_of_body bd =
  List.filter_map (function `L(p) -> Some(p) | `R(_) -> None) bd
let terms_of_body bd =
  List.filter_map (function `L(_) -> None | `R(t) -> Some(t)) bd

let body_of_atoms atms = List.map (fun p -> `L(p)) atms
let body_of_terms ts = List.map (fun t -> `R(t)) ts

(** @return whether bd1 and bd2 share a variable which is not in bvs *)
let share_variable bvs bd1 bd2 =
  Util.intersects
    (Util.diff (fvs_body bd1) bvs)
    (Util.diff (fvs_body bd2) bvs)

let canonize_tsp env (ts1, ts2) =
  Util.diff ts1 (env @ ts2), ts2

let canonize_pxs pxs =
    pxs
  |>
    (List.sort
      ~cmp:(fun (_, _, _, ttyss1) (_, _, _, ttyss2) ->
        List.length ttyss1 - List.length ttyss2))
  |>
    (List.filter
      (fun (_, ttys, _, ttyss) -> List.for_all (fun ttys' -> ttys' <> ttys) ttyss))

let rec is_covered_aux env tsp pxs =
  if List.for_all (fun (_, _, xs, _) -> xs = []) pxs then
      (Cvc3Interface.implies (env @ snd tsp) (fst tsp) &&
      List.for_all
        (fun (pid, ttys1, [], ttyss2) ->
          List.exists (fun ttys2 -> Atom.equiv env (pid, ttys1) (pid, ttys2)) ttyss2)
        pxs)
    |>
      (fun b ->
        let _ =
          Global.log (fun () ->
            if b then
              Format.printf "succeeded@,"
            else
              let _ = Format.printf "ts: %a@," Term.pr_list (fst tsp) in
              let _ =
                List.iter
                  (fun (_, ttys, _, _) ->
                    Format.printf "ttys: %a@," TypTerm.pr_list ttys)
                  pxs
              in
              Format.printf "failed:@,")
        in
        b)
  else
    let (pid, ttys1, xs, ttyss2) = List.hd pxs in
    let xttyss =
      List.filter_map
        (fun ttys2 ->
            (Util.concat_map2
              (fun tty1 tty2 ->
                try
                  TypTerm.matches ~imply:Cvc3Interface.implies env xs tty2 tty1
                with Term.MayNotMatch ->
                  []
                | Term.NeverMatch ->
                  assert false)
              ttys1 ttys2)
          |>
            (fun xttys ->
              let _ =
                if !Global.debug then
                  let _ = Global.log (fun () -> Format.printf "env: %a@," Term.pr (Formula.band env)) in
                  assert (TypSubst.non_dup ~is_valid:(fun t -> Cvc3Interface.implies env [t]) xttys)
              in
              xttys)
          |>
            opt_of_list)
        ttyss2
    in
    List.exists
      (fun xttys ->
        let _ = Global.log (fun () -> Format.printf "xttys: %a@," TypSubst.pr xttys) in
        let b =
          let tsp' =
              tsp
            |>
              (fun (ts1, ts2) ->
                List.map
                  (FormulaUtil.subst (TypSubst.sub_of xttys) |- FormulaUtil.simplify)
                  ts1,
                ts2)
            |>
              (canonize_tsp env)
          in
          let pxs' =
            let update (pid, ttys1, xs, ttyss2) =
              let xs' = Util.diff xs (List.map Util.fst3 xttys) in
              if List.length xs' = List.length xs then
                pid, ttys1, xs, ttyss2
              else
                let _ = Global.log (fun () -> Format.printf "pid: %a@," Var.pr pid) in
                let pid', ttys1' =
                    (pid, ttys1)
                  |>
                    (Atom.subst (TypSubst.sub_of xttys))
                  |>
                    Atom.simplify
                in
                let ttyss2' =
                  List.filter
                    (fun ttys2 ->
                      Atom.matches (fun x -> List.mem x xs') env (pid', ttys2) (pid', ttys1'))
                    ttyss2
                in
                pid', ttys1', xs', ttyss2'
            in
            pxs |> (List.map update) |> canonize_pxs
          in
          is_covered_aux env tsp' pxs'
        in
        let _ = Global.log (fun () -> if not b then Format.printf "backtracked@,") in
        b)
      xttyss

(** @return whether bd1 is covered by bd2 under env
            for some substitution for variables not in bvs *)
let is_covered env bvs bd1 bd2 =
  let _ = Global.log_begin ~disable:true "HornClauseUtil.is_covered" in
  let ts1 = terms_of_body bd1 in
  let ts2 = terms_of_body bd2 in
  let atms1 = atoms_of_body bd1 in
  let atms2 = atoms_of_body bd2 in

  let tsp = ts1, ts2 in
  let pxs =
    List.map
      (fun atm1 ->
        let xs = Util.diff (Atom.fvs atm1) bvs in
        let ttyss =
          List.filter_map
            (fun atm2 ->
              if Atom.matches (fun x -> List.mem x xs) env atm2 atm1 then
                Some(snd atm2)
              else
                None)
            atms2
        in
        fst atm1, snd atm1, xs, ttyss)
      atms1
  in
  let res = is_covered_aux env (canonize_tsp env tsp) (canonize_pxs pxs) in
  let _ = Global.log_end "HornClauseUtil.is_covered" in
  res

(** @param bvs are not eliminated *)
let share_predicates_aux bvs0 cvs bvs atms t =
    (atms, Formula.conjuncts t)
  |>
    (fun (atms, ts) ->
      let ts0, ts1 =
        List.partition (fun t -> Util.subset (Term.fvs t) bvs) ts
      in
      let atms0, atms1 =
        List.partition (fun p -> Util.subset (Atom.fvs p) bvs) atms
      in
      let atms0 =
        Util.concat_map
          (Util.representatives (Atom.equiv ts0))
          (Util.classify (fun (pid1, _) (pid2, _) -> pid1 = pid2) atms0)
      in
      let bds =
        (if atms0 = [] then [] else [body_of_atoms atms0]) @
        (Util.equiv_classes
          (fun b1 b2 -> share_variable bvs [b1] [b2])
          (body_of_atoms atms1(* ??redundant *) @ body_of_terms ts1))
      in
      let zs =
        List.filter
          (fun x ->
            match List.filter (fun bd -> List.mem x (fvs_body bd)) bds with
              [_] -> true
            | _ -> false)
          (Util.diff (List.unique (Util.concat_map Atom.fvs atms0)) bvs0)
      in
      bds, ts0, zs)
  |>
    (fun (bds, ts0, zs) ->
      let bds =
        List.map
          (fun bd ->
            if atoms_of_body bd = [] then
              (try
                let t = Formula.band (terms_of_body bd) in
                let xs = List.unique (Util.diff (TypTerm.fvs_ty SimType.Int (t, SimType.Bool)) bvs) in
                (*let _ = if xs <> [] then assert false in*)
                (* t does not contain unit or bools? *)
                let ts = Formula.conjuncts (AtpInterface.integer_qelim (Formula.exists (List.map (fun x -> x, SimType.Int) xs) t)) in
                List.map (fun t -> `R(t)) ts
              with Util.NotImplemented _ -> bd)
            else
              bd)
          bds
      in
      let _ =
        Global.log (fun () ->
          let _ = Format.printf "bvs: %a@," Var.pr_list bvs in
          let _ = Format.printf "ts0: %a@," Term.pr (Formula.band ts0) in
          List.iter
            (fun bd ->
              let atms, ts = Util.partition_map id bd in
              Format.printf "bd: %a@," pr_elem (Hc(None, atms, Formula.band ts)))
            bds)
      in
      bds, ts0, zs)
  |>
    (fun (bds, ts0, zs) ->
      let rec aux bds1 bds2 =
        match bds1 with
          [] -> bds2
        | ec::bds1 ->
            let b =
              match cvs with
                Some(xs) when not (Util.intersects xs (fvs_body ec)) ->
                  false
              | _ ->
                  let _ = Global.log (fun () -> Format.printf "checking: %a@," (Util.pr_list Atom.pr ",") (atoms_of_body ec)) in
                  is_covered ts0 bvs ec (List.flatten (bds1 @ bds2))
            in
            aux bds1 (if b then bds2 else ec :: bds2)
      in
      let bds = aux (List.sort bds) [] in
      let atms, ts = Util.partition_map (fun x -> x) (List.flatten bds) in
      atms, Formula.band (ts @ ts0), zs)

(** @param bvs0 *)
let share_predicates bvs0 atms t =
  let t = FormulaUtil.simplify t in
  if Atom.num_dup atms = 0 || Term.coeffs t <> [] then
    atms, t
  else
    let _ = Global.log_begin "HornClauseUtil.share_predicates" in
      (atms, t)
    |>
      (fun (atms, t) ->
        let rec loop cvs bvs atms t =
          let _ = Global.log (fun () ->
            if bvs <> [] then Format.printf "bvs: %a@," Var.pr_list bvs)
          in
          let atms', t', zs =
            share_predicates_aux bvs0 cvs (bvs0 @ bvs) atms t
          in
          if List.length atms <> List.length atms' && Atom.num_dup atms' > 0 then
            let bvs' = ignored_vars bvs0 atms' in
            let cvs' = Util.diff bvs bvs' in
            if cvs' = [](*Util.set_equiv bvs bvs'*) then
              bvs', atms', t', zs
            else
              loop (Some(cvs')) bvs' atms' t'
          else
            bvs, atms', t', zs
        in
        loop None (ignored_vars bvs0 atms) atms t)
    |>
      (fun (bvs, atms, t, zs) ->
        if not !Global.enable_pred_sharing2 then
          if !Global.disable_pred_sharing1 then
            atms, t
          else (*a-max‚ªrsn 0‚Å‚È‚¢‚Æ¬Œ÷‚µ‚È‚­‚È‚é intro3‚Írsn0‚ÅOK‚É‚È‚é*)
            try
              let _ = Global.log (fun () ->
                if zs <> [] then Format.printf "zs: %a@," Var.pr_list zs) in
              Util.find_app
                (fun xs ->
                  let bvs1 = Util.diff bvs xs in
                  if List.length bvs1 = List.length bvs then
                    None
                  else
                    let atms', t', _ = share_predicates_aux bvs0 (Some(xs)) (bvs0 @ bvs1) atms t in
                    if List.length atms <> List.length atms' then
                      Some(atms', t')
                    else
                      None)
                (Util.nsubsets 1 zs)
            with Not_found ->
              atms, t
        else
          let zs = Util.inter bvs (changing_vars bvs0 atms) in
          let _ = Global.log (fun () ->
            if zs <> [] then Format.printf "zs: %a@," Var.pr_list zs)
          in
          if List.length zs > 7(*??*) then
            atms, t
          else
            try
              Util.find_app
                (fun xs ->
                  let bvs1 = Util.diff bvs xs in
                  if List.length bvs1 = List.length bvs then
                    None
                  else
                    let atms', t', _ = share_predicates_aux bvs0 (Some(xs)) (bvs0 @ bvs1) atms t in
                    if List.length atms <> List.length atms' then
                      Some(atms', t')
                    else
                      None)
                (Util.nsubsets 1 zs @ Util.nsubsets 2 zs)
            with Not_found ->
              atms, t)
  |>
    (fun (atms, t) ->
      let _ = Global.log_end "HornClauseUtil.share_predicates" in
      atms, t)

(** {6 Functions for simplifying Horn clauses} *)

(** @param bvs variables in bvs are not eliminated *)
let simplify bvs (Hc(popt, atms, t)) =
  let _ = Global.log_begin ~disable:true "HornClauseUtil.simplify" in
  let _ = Global.log (fun () -> Format.printf "input:@,  @[<v>%a@]@," pr_elem (Hc(popt, atms, t))) in
  let shared = ref (List.length atms) in
  let bvs = bvs @ (match popt with None -> [] | Some(_, xtys) -> List.map fst xtys) in
  let atms, t =
    let pids = match popt with None -> [] | Some(pid, _) -> [pid] in
    qelim bvs pids atms t
  in
  let atms, t = share_predicates bvs atms t in
  let res = Hc(popt, atms, t) in
  let _ =
    let _ = shared := !shared - List.length atms in
    Global.log (fun () -> if !shared <> 0 then Format.printf "# of shared predicate variables: %d@," !shared)
  in
  let _ =
    let n = Atom.num_dup atms in
    Global.log (fun () -> if n <> 0 then Format.printf "# of duplicate predicate variables: %d@," n)
  in
  let _ = Global.log (fun () -> Format.printf "output:@,  @[<v>%a@]" pr_elem res) in
  let _ = Global.log_end "HornClauseUtil.simplify" in
  res

(** {6 Basic functions} *)

let subst_hcs hcs (Hc(popt, atms, t) as hc) =
  let _ = Global.log_begin "HornClauseUtil.subst_hcs" in
  let _ = Global.log (fun () -> Format.printf "input:@,  @[<v>%a@]@," pr_elem hc) in
  let hc =
    if !Global.subst_hcs_inc then
      let rec aux atms t =
        try
          let latms, (pid, ttys), ratms =
            Util.pick (fun (pid, _) -> mem pid hcs) atms
          in
          let _ = Global.log (fun () -> Format.printf "%a is substituted@," Var.pr pid) in
          let Hc(_, atms, t) =
            let atms, t' = lookup_nd (pid, ttys) hcs in
            simplify [] (Hc(popt, latms @ atms @ ratms, Formula.band [t; t']))
          in
          aux atms t
        with Not_found ->
          Hc(popt, atms, t)
      in
      aux atms t
    else
      let atms, t =
        let atmss, ts =
          Util.unzip
            (List.map
              (fun (pid, ttys) ->
                try
                  let _ = Global.log (fun () -> Format.printf "%a is being substituted@," Var.pr pid) in
                  lookup_nd (pid, ttys) hcs
                with Not_found ->
                  [pid, ttys], Formula.ttrue)
              atms)
        in
        List.flatten atmss, Formula.band (t :: ts)
      in
      simplify [] (Hc(popt, atms, t))
  in
  let _ = Global.log (fun () -> Format.printf "output:@,  @[<v>%a@]" pr_elem hc) in
  let _ = Global.log_end "HornClauseUtil.subst_hcs" in
  hc

let subst_hcs_fixed hcs hc =
  Util.fixed_point
    (fun hc ->
      (*Format.printf "%a@," pr_elem hc;*)
      subst_hcs hcs hc)
    (fun hc1 hc2 ->
      match hc1, hc2 with
        Hc(_, atms1, _), Hc(_, atms2, _) -> Util.set_equiv atms1 atms2)
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

(** {6 Functions on logical formulas} *)

(** @param bvs variables in bvs are not eliminated *)
let simplify_formula pids bvs t =
  let xttys, t = TypSubst.xytys_of pids (fun x -> List.mem x bvs || Var.is_coeff x) t in
  let sub = TypSubst.sub_of xttys in
  let t = FormulaUtil.subst sub (FormulaUtil.simplify t) in
  let [], t = qelim_aux (fun x -> List.mem x bvs || Var.is_coeff x) [] t in
  t
