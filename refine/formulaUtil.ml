open ExtList
open Term
open Formula
open Util

(** Simplifiers for propositional formulas *)

(** @param pfs propositional formulas *)
let simplify_conjuncts_pfs pfs =
  let pxs, nxs =
    Util.partition_map
      (fun t ->
        match fun_args t with
          Var(_, x), [] ->
            `L(x)
        | Const(_, Const.Not), [Var(_, x)] ->
            `R(x)
        | _ -> assert false)
      pfs
  in
  if Util.inter pxs nxs <> [] then
    [tfalse]
  else
    pfs

let simplify_disjuncts_pfs pfs =
  let pxs, nxs =
    Util.partition_map
      (fun t ->
        match fun_args t with
          Var(_, x), [] ->
            `L(x)
        | Const(_, Const.Not), [Var(_, x)] ->
            `R(x)
        | _ -> assert false)
      pfs
  in
  if Util.inter pxs nxs <> [] then
    [ttrue]
  else
    pfs

(** Simplifiers for logical formulas *)

let simplify_unit t = t

let simplify_int t = t

let rec simplify_ubrel c t1 t2=
  (*let t1 = simplify_unit t1 in
  let t2 = simplify_unit t2 in*)
  match c with
    Const.EqUnit ->
      ttrue
      (*if t1 = t2 then
        ttrue
      else
        eqUnit t1 t2*)
  | Const.NeqUnit ->
      tfalse
      (*if t1 = t2 then
        tfalse
      else
        neqUnit t1 t2*)
  | _ -> assert false
and simplify_bbrel c t1 t2=
  let t1 = simplify t1 in
  let t2 = simplify t2 in
  match c with
    Const.EqBool ->
      if t1 = t2 then
        ttrue
      else if t1 = ttrue then
        t2
      else if t1 = tfalse then
        simplify (bnot t2)
      else if t2 = ttrue then
        t1
      else if t2 = tfalse then
        simplify (bnot t1)
      else
        eqBool t1 t2
  | Const.NeqBool ->
      if t1 = t2 then
        tfalse
      else if t1 = ttrue then
        simplify (bnot t2)
        (*eqBool t2 tfalse*)
      else if t1 = tfalse then
        t2
        (*eqBool t2 ttrue*)
      else if t2 = ttrue then
        simplify (bnot t1)
        (*eqBool t1 tfalse*)
      else if t2 = tfalse then
        t1
        (*eqBool t1 ttrue*)
      else
        neqBool t1 t2
  | _ -> assert false
and simplify_ibrel t=
  try
    LinArith.term_of_aif (LinArith.aif_of t)
  with Invalid_argument _ ->
    try
      ParLinArith.term_of_aif (ParLinArith.aif_of t)
    with Invalid_argument _ ->
      try
        Poly.term_of_aif (Poly.aif_of t)
      with Invalid_argument _ ->
        let _ = Format.printf "%a@," pr t in
        assert false
(** @param t a formula
    @require t does not contain Const.Imply and Const.Iff
    @ensure the return value does not contain a subexpression of the type unit *)
and simplify t =
  match fun_args t with
    Var(_, _), [] ->
      t (*eqBool t ttrue*)
  | Const(_, Const.True), [] ->
      ttrue
  | Const(_, Const.False), [] ->
      tfalse
  | Const(attr, Const.Not), [t] ->
      (match fun_args t with
        Var(_, _), [] ->
          bnot t (* eqBool t tfalse *)
      | Const(_, Const.True), [] ->
          tfalse
      | Const(_, Const.False), [] ->
          ttrue
      | Const(_, Const.Not), [t] ->
          simplify t
      | Const(attr, Const.And), [t1; t2] ->
          simplify (bor [bnot t1; bnot t2])
      | Const(attr, Const.Or), [t1; t2] ->
          simplify (band [bnot t1; bnot t2])
      | Const(attr, Const.Imply), [t1; t2] ->
          assert false(*simplify (band [t1; bnot t2])*)
      | Const(attr, Const.Iff), [t1; t2] ->
          assert false(*simplify (bor [band [t1; bnot t2]; band [bnot t1; t2]])*)
      | Const(attr, c), [t1; t2] when Const.is_ubrel c ->
          simplify (bop (Const.bnot_ubrel c) t1 t2)
      | Const(attr, c), [t1; t2] when Const.is_bbrel c ->
          simplify (bop (Const.bnot_bbrel c) t1 t2)
      | Const(attr, c), [t1; t2] when Const.is_ibrel c ->
          simplify (bop (Const.bnot_ibrel c) t1 t2)
      | Forall(attr, env, t), [] ->
          simplify (exists env (bnot t))
      | Exists(attr, env, t), [] ->
          simplify (forall env (bnot t))
      | _ ->
          let _ = Format.printf "not %a@," pr t in
          assert false)
  | Const(attr, Const.And), [t1; t2] ->
      band (simplify_conjuncts (conjuncts (band [simplify t1; simplify t2])))
  | Const(attr, Const.Or), [t1; t2] ->
      bor (simplify_disjuncts (disjuncts (bor [simplify t1; simplify t2])))
  | Const(attr, Const.Imply), [t1; t2] ->
      assert false
  | Const(attr, Const.Iff), [t1; t2] ->
      assert false
  | Const(attr, c), [t1; t2] when Const.is_ibrel c ->
      simplify_ibrel t
  | Const(attr, c), [t1; t2] when Const.is_ubrel c ->
      simplify_ubrel c t1 t2
  | Const(attr, c), [t1; t2] when Const.is_bbrel c ->
      simplify_bbrel c t1 t2
  | Forall(a, env, t), [] ->
      Forall(a, env, simplify t)
  | Exists(a, env, t), [] ->
      Exists(a, env, simplify t)
  | _ ->
      let _ = Format.printf "%a@," pr t in
      assert false
and factor_out_disjuncts ts =
  let _ = Global.log_begin ~disable:true "factor_out_disjuncts" in
  let _ = Global.log (fun () -> Format.printf "input: %a@," Term.pr (band ts)) in
  let res =
    let tss = List.map disjuncts ts in
    if List.length tss < 2 then
      ts
    else
      let shared = List.fold_left Util.inter (List.hd tss) (List.tl tss) in
      if shared <> [] then
        let _ = Global.log (fun () -> Format.printf "shared: %a@," (Util.pr_list Term.pr ", ") shared) in
        let ts = simplify_conjuncts (List.map (fun ts -> simplify (bor (Util.diff ts shared))) tss) in
        let t = simplify (bor shared) in
        [bor [t; band ts]]
      else
        ts
  in
  let _ = Global.log (fun () -> Format.printf "output: %a@," Term.pr (band res)) in
  let _ = Global.log_end "factor_out_disjuncts" in
  res
and factor_out_conjuncts ts =
  let _ = Global.log_begin ~disable:true "factor_out_conjuncts" in
  let _ = Global.log (fun () -> Format.printf "input: %a@," Term.pr (bor ts)) in
  let res =
    let tss = List.map conjuncts ts in
    if List.length tss < 2 then
      ts
    else
      let shared = List.fold_left Util.inter (List.hd tss) (List.tl tss) in
      if shared <> [] then
        let _ = Global.log (fun () -> Format.printf "shared: %a@," (Util.pr_list Term.pr ", ") shared) in
        let ts = simplify_disjuncts (List.map (fun ts -> simplify (band (Util.diff ts shared))) tss) in
        let t = simplify (band shared) in
        [band [t; bor ts]]
      else
        ts
  in
  let _ = Global.log (fun () -> Format.printf "output: %a@," Term.pr (bor res)) in
  let _ = Global.log_end "factor_out_conjuncts" in
  res
(** trivial syntactic simplification *)
and simplify_conjuncts_fast ts =
  let ts = List.unique ts in
  if List.mem tfalse ts then
    [tfalse]
  else
    List.filter (fun t -> t <> ttrue) ts
(** trivial syntactic simplification *)
and simplify_disjuncts_fast ts =
  let ts = List.unique ts in
  if List.mem ttrue ts then
    [ttrue]
  else
    List.filter (fun t -> t <> tfalse) ts
(** @todo refactoring *)
and simplify_conjuncts_aifs_ts aifs ts =
  let sub, ts' =
    Util.partition_map
      (function
        (Const.EqInt, [1, x], n) ->
          `L(x, Term.tint (-n))
      | (Const.EqInt, [-1, x], n) ->
          `L(x, Term.tint n)
      | aif ->
          `R(LinArith.term_of_aif aif))
      aifs
  in
  List.map (fun (x, t) -> eqInt (make_var x) t) sub @
  List.map
    (fun t ->
      let t' = simplify (TypSubst.subst (fun x -> List.assoc x sub) t) in
      if t' = ttrue || t' = tfalse then
        let _ = Global.log (fun () -> Format.printf "eliminated: %a@," Term.pr t) in
        t'
      else
        t)
    (ts' @ ts)
and elim_subsumed_disjuncts ts =
  List.map
    band
    (Util.filter_maplac
      (fun tss1 ts tss2 ->
        if List.exists (fun ts' -> Util.subset ts' ts) (tss1 @ tss2) then
          None
        else
          Some(ts))
      (List.map conjuncts ts))
(** @require List.for_all (fun t -> simplify t = t) ts *)
and simplify_conjuncts ts =
  let _ = Global.log_begin ~disable:true "simplify_conjuncts" in
  let _ = Global.log (fun () -> Format.printf "input: @[<v>%a@]@," pr (band ts)) in
  let ts' =
    let aifs, pfs, ts =
      Util.partition3_map
        (fun t ->
          try
            `A(LinArith.aif_of t)
          with Invalid_argument _ ->
            match fun_args t with
              Var(_, _), []
            | Const(_, Const.Not), [Var(_, _)] ->
                `B(t)
            | _ ->
                `C(t))
        (simplify_conjuncts_fast ts)
    in
    let ts =
      let aifs = LinArith.simplify_conjuncts_aifs aifs in
      let ts = simplify_conjuncts_pfs pfs @ ts in
      if true then
        simplify_conjuncts_aifs_ts aifs ts
      else
        List.map LinArith.term_of_aif aifs @ ts
    in
    ts |> simplify_conjuncts_fast |> factor_out_disjuncts
  in
  let _ = Global.log (fun () -> Format.printf "output: @[<v>%a@]" pr (band ts')) in
  let _ = Global.log_end "simplify_conjuncts" in
  ts'
(** @require List.for_all (fun t -> simplify t = t) ts *)
and simplify_disjuncts ts =
  let _ = Global.log_begin ~disable:true "simplify_disjuncts" in
  let _ = Global.log (fun () -> Format.printf "input: @[<v>%a@]@," pr (bor ts)) in
  let ts' =
    let aifs, pfs, ts =
      Util.partition3_map
        (fun t ->
          try
            `A(LinArith.aif_of t)
          with Invalid_argument _ ->
            match fun_args t with
              Var(_, _), []
            | Const(_, Const.Not), [Var(_, _)] ->
                `B(t)
            | _ ->
                `C(t))
        (simplify_disjuncts_fast ts)
    in
    let ts =
      let aifs = LinArith.simplify_disjuncts_aifs aifs in
      let ts = simplify_disjuncts_pfs pfs @ ts in
      List.map LinArith.term_of_aif aifs @ ts
    in
    ts |> simplify_disjuncts_fast |> factor_out_conjuncts
  in
  let _ = Global.log (fun () -> Format.printf "output: @[<v>%a@]" pr (bor ts')) in
  let _ = Global.log_end "simplify_disjuncts" in
  ts'

(** Utility functions on logical formulas *)

let forall_imply conds_envs t =
  List.fold_right
    (fun (cond, env) t ->
      let sub = List.filter (fun (x, _, _) -> List.mem_assoc x env) (TypSubst.sub_of cond) in
      try
        TypSubst.subst (TypSubst.fun_of sub) t
      with Not_found ->
        forall env (imply cond t))
    conds_envs t

(** @todo first compute the fixed-point of sub *)
let subst_fixed sub t =
  let _ = Global.log_begin "FormulaUtil.subst_fixed" in
  let _ = Global.log (fun () -> Format.printf "input: %a@," pr t) in
  let t =
    Util.fixed_point
      (fun t ->
        let t = simplify (TypSubst.subst sub t) in
        t)
      equiv
      t
  in
  let _ = Global.log (fun () -> Format.printf "output: %a" pr t) in
  let _ = Global.log_end "FormulaUtil.subst_fixed" in
  t

(** @ensure the return value does not contain a subexpression of the type unit *)
let elim_unit t =
  if true then
    simplify t
  else
    let uvs = List.unique (TypTerm.fvs_ty SimType.Unit (t, SimType.Bool)) in
    let t =
      if uvs = [] then
        t
      else
        let sub x = if List.mem x uvs then tunit else raise Not_found in
        TypSubst.subst sub t
    in
    simplify t

let rec elim_imply_iff t =
  match fun_args t with
    Var(attr, x), [] ->
      Var(attr, x)
  | Const(attr, Const.Imply), [t1; t2] ->
      let t1 = elim_imply_iff t1 in
      let t2 = elim_imply_iff t2 in
      bor [bnot t1; t2]
  | Const(attr, Const.Iff), [t1; t2] ->
      let t1 = elim_imply_iff t1 in
      let t2 = elim_imply_iff t2 in
      bor [band [t1; t2]; band [bnot t1; bnot t2]]
  | Const(attr, c), ts ->
      apply (Const(attr, c)) (List.map elim_imply_iff ts)
  | Forall(attr, env, t), [] ->
      Forall(attr, env, elim_imply_iff t)
  | Exists(attr, env, t), [] ->
      Exists(attr, env, elim_imply_iff t)
  | _ ->
      let _ = Format.printf "%a@," pr t in
      assert false

(** eliminate formulas of the form "t1 =b t2" and "t1 <>b t2"
    by replacing them with "t1 iff t2" and "not (t1 iff t2)" respectively *)
let rec elim_eq_neq_boolean t =
  match fun_args t with
    Var(attr, x), [] ->
      Var(attr, x)
  | Const(attr, Const.EqBool), [t1; t2] ->
      iff t1 t2
  | Const(attr, Const.NeqBool), [t1; t2] ->
      bnot (iff t1 t2)
  | Const(attr, c), ts ->
      apply (Const(attr, c)) (List.map elim_eq_neq_boolean ts)
  | Forall(attr, env, t), [] ->
      Forall(attr, env, elim_eq_neq_boolean t)
  | Exists(attr, env, t), [] ->
      Exists(attr, env, elim_eq_neq_boolean t)
  | _ ->
      let _ = Format.printf "%a@," pr t in
      assert false

(** eliminate formulas of the form "t1 <>i t2"
    by replacing them with "t1 < t2 || t1 > t2" *)
let rec elim_neq_int t =
  match fun_args t with
    Var(attr, x), [] ->
      Var(attr, x)
  | Const(attr, Const.NeqInt), [t1; t2] ->
      bor [lt t1 t2; gt t1 t2]
  | Const(attr, c), ts ->
      apply (Const(attr, c)) (List.map elim_neq_int ts)
  | Forall(attr, env, t), [] ->
      Forall(attr, env, elim_neq_int t)
  | Exists(attr, env, t), [] ->
      Exists(attr, env, elim_neq_int t)
  | _ ->
      let _ = Format.printf "%a@," pr t in
      assert false

(** eliminate formulas of the form "t1 =i t2" and "t1 <>i t2"
    by replacing them with "t1 <= t2 && t1 >= t2" and "t1 < t2 || t1 > t2" respectively *)
let rec elim_eq_neq_int t =
  match fun_args t with
    Var(attr, x), [] ->
      Var(attr, x)
  | Const(attr, Const.EqInt), [t1; t2] ->
      band [leq t1 t2; geq t1 t2]
  | Const(attr, Const.NeqInt), [t1; t2] ->
      bor [lt t1 t2; gt t1 t2]
  | Const(attr, c), ts ->
      apply (Const(attr, c)) (List.map elim_eq_neq_int ts)
  | Forall(attr, env, t), [] ->
      Forall(attr, env, elim_eq_neq_int t)
  | Exists(attr, env, t), [] ->
      Exists(attr, env, elim_eq_neq_int t)
  | _ ->
      let _ = Format.printf "%a@," pr t in
      assert false

let rec elim_minus t =
  match fun_args t with
    Var(attr, x), [] ->
      Var(attr, x)
  | Const(attr, c), [_; _] when Const.is_ibrel c ->
      (try
        Poly.term_of_aif (Poly.aif_of t)
      with Invalid_argument _ ->
        assert false)
  | Const(attr, c), ts ->
      apply (Const(attr, c)) (List.map elim_minus ts)
  | Forall(attr, env, t), [] ->
      Forall(attr, env, elim_minus t)
  | Exists(attr, env, t), [] ->
      Exists(attr, env, elim_minus t)
  | _ ->
      let _ = Format.printf "%a@," pr t in
      assert false

(** case splitting on boolean variables *)
let split_cases_boolean ts =
  let bool_vars =
    List.unique
      (Util.concat_map
        (fun t -> TypTerm.fvs_ty SimType.Bool (t, SimType.Bool))
        ts)
  in
  if bool_vars = [] then
    [ts],
    function [t] -> t | _ -> assert false
  else
    let subs =
      Util.product_list (@)
        (List.map
          (fun b ->
            [[b, ttrue, SimType.Bool];
             [b, tfalse, SimType.Bool]])
          bool_vars)
    in
    List.map
      (fun sub ->
         List.map (fun t -> simplify (TypSubst.subst (TypSubst.fun_of sub) t)) ts)
      subs,
    fun ts ->
      bor
        (List.map2
          (fun t sub ->
            band [t; of_subst sub])
          ts
          subs)




let rec linearize_linprod c ts =
  match ts with
    [] -> assert false
  | [t] -> bop c t (tint 0)
  | t :: ts ->
      (match c with
        Const.EqInt ->
          bor (List.map (fun t -> eqInt t (tint 0)) (t::ts))
      | Const.NeqInt ->
          band (List.map (fun t -> neqInt t (tint 0)) (t::ts))
      | Const.Lt
      | Const.Gt ->
          bor [band [gt t (tint 0); linearize_linprod c ts]; band [lt t (tint 0); linearize_linprod (Const.minus_ibrel c) ts]]
      | Const.Leq
      | Const.Geq ->
          bor [band [geq t (tint 0); linearize_linprod c ts]; band [leq t (tint 0); linearize_linprod (Const.minus_ibrel c) ts]]
      | _ -> assert false)

let linearize_ibrel t =
  try
    let _ = LinArith.aif_of t in
    t
  with Invalid_argument _ ->
    (try
      let c, nxs, n = ParLinArith.aif_of t in
      let ts =
        if ParLinArith.is_zero (nxs, n) then
          [tint 0]
        else
          ParLinArith.factorize (nxs, n)
      in
      if List.for_all is_linear ts then
        linearize_linprod c ts
        (*bop c (prod ts) (tint 0)*)
      else
        invalid_arg "linearize_ibrel"
    with Invalid_argument _ ->
      let c, pol = Poly.aif_of t in
      let ts =
        if Poly.is_zero pol then
          [tint 0]
        else
          Poly.factorize pol
      in
      if List.for_all is_linear ts then
        linearize_linprod c ts
        (*bop c (prod ts) (tint 0)*)
      else
        invalid_arg "linearize_ibrel")
let rec linearize t =
  match fun_args t with
    Const(attr, Const.True), [] ->
      Const(attr, Const.True)
  | Const(attr, Const.False), [] ->
      Const(attr, Const.False)
  | Const(attr, Const.Not), [t] ->
      bnot (linearize t)
  | Const(attr, Const.And), [t1; t2] ->
      band [linearize t1; linearize t2]
  | Const(attr, Const.Or), [t1; t2] ->
      bor [linearize t1; linearize t2]
  | Const(attr, c), [_; _] when Const.is_ibrel c ->
      linearize_ibrel t
  | Forall(attr, env, t), [] ->
      Forall(attr, env, linearize t)
  | Exists(attr, env, t), [] ->
      Exists(attr, env, linearize t)
  | _ -> assert false

(** {6 Functions on DNF formulas} *)

let rec dnf t =
  match fun_args t with
    Var(_, _), [] ->
      [[t]]
  | Const(_, Const.True), [] ->
      [[]]
  | Const(_, Const.False), [] ->
      []
  | Const(_, Const.And), [t1; t2] ->
      let tss1 = dnf t1 in
      Util.concat_map (fun ts2 -> List.map (fun ts1 -> ts1 @ ts2) tss1) (dnf t2)
  | Const(_, Const.Or), [t1; t2] ->
      dnf t1 @ dnf t2
  | Const(_, Const.Imply), [t1; t2] ->
      dnfn t1 @ dnf t2
  | Const(_, Const.Iff), [t1; t2] ->
      raise (Util.NotImplemented "FormulaUtil.dnf")
  | Const(_, Const.Not), [t] -> 
      dnfn t
  | Const(_, bop), [_; _] ->
      [[t]]
  | Forall(_, _, _), []
  | Exists(_, _, _), [] ->
      raise (Util.NotImplemented "FormulaUtil.dnf")
  | t, _-> Format.printf "@,%a@," Term.pr t; assert false
and dnfn t =
  match fun_args t with
    Var(_, _), [] ->
      [[bnot t]]
  | Const(_, Const.True), [] ->
      []
  | Const(_, Const.False), [] ->
      [[]]
  | Const(_, Const.And), [t1; t2] ->
      dnfn t1 @ dnfn t2
  | Const(_, Const.Or), [t1; t2] ->
      let tss1 = dnfn t1 in
      Util.concat_map (fun ts2 -> List.map (fun ts1 -> ts1 @ ts2) tss1) (dnfn t2)
  | Const(_, Const.Imply), [t1; t2] ->
      let tss1 = dnf t1 in
      Util.concat_map (fun ts2 -> List.map (fun ts1 -> ts1 @ ts2) tss1) (dnfn t2)
  | Const(_, Const.Iff), [t1; t2] ->
      raise (Util.NotImplemented "FormulaUtil.dnfn")
  | Const(_, Const.Not), [t] -> 
      dnf t
  | Const(a, c), [t1; t2] ->
      (match Const.bnot_ibrel c with
        Const.NeqInt ->
          [[lt t1 t2]; [gt t1 t2]]
      | c ->
          [[bop c t1 t2]])
  | Forall(_, _, _), []
  | Exists(_, _, _), [] ->
      raise (Util.NotImplemented "FormulaUtil.dnfn")
  | t, _-> Format.printf "@,%a@," Term.pr t; assert false

let is_disjunctive t =
  List.length (dnf t) >= 2

let of_dnf tss =
  bor (List.map band tss)
