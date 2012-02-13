open ExtList
open Term

(** Logical formulas *)

(** {6 Constructors} *)

(** tautology *)
let ttrue = Const([], Const.True)
(** contradiction *)
let tfalse = Const([], Const.False)

let band ts =
  let rec aux ts =
    match ts with
      [] -> Const([], Const.True)
    | [t] -> t
    | (Const(_, Const.True))::ts' -> aux ts'
    | t::ts' ->
        let t' = aux ts' in
        (match t' with
          Const(_, Const.True) ->
            t
        | _ ->
            apply (Const([], Const.And)) [t; t'])
  in
  aux (List.unique ts)

let bor ts =
  let rec aux ts =
    match ts with
      [] -> Const([], Const.False)
    | [t] -> t
    | (Const(_, Const.False))::ts' -> aux ts'
    | t::ts' ->
        let t' = aux ts' in
        (match t' with
          Const(_, Const.False) ->
            t
        | _ ->
            apply (Const([], Const.Or)) [t; t'])
  in
  aux (List.unique ts)

let bnot t =
  match fun_args t with
    Const(a, Const.True), [] -> Const(a, Const.False)
  | Const(a, Const.False), [] -> Const(a, Const.True)
  | Const(a, Const.Not), [t] -> t
  | _ -> apply (Const([], Const.Not)) [t]

let imply t1 t2 =
  if Term.equiv t1 ttrue then
    t2
  else if Term.equiv t1 tfalse then
    ttrue
  else if Term.equiv t2 ttrue then
    ttrue
  else if Term.equiv t2 tfalse then
    bnot t1
  else
    apply (Const([], Const.Imply)) [t1; t2]

let forall env t =
  let xs = fvs t in
(*
  let _ = Format.printf "env: %a@.xs: %a@." (Util.pr_list SimType.pr_bind ",") env (Util.pr_list Var.pr ",") xs in
*)
  let env = List.filter (fun (x, _) -> List.mem x xs) env in
  if env = [] then t else Forall([], env, t)

let exists env t =
  let xs = fvs t in
(*
  let _ = Format.printf "env: %a@.xs: %a@." (Util.pr_list SimType.pr_bind ",") env (Util.pr_list Var.pr ",") xs in
*)
  let env = List.filter (fun (x, _) -> List.mem x xs) env in
  if env = [] then t else Exists([], env, t)
(*
  bnot (forall env (bnot t))
*)

let iff t1 t2 =
  if Term.equiv t1 t2 then
    ttrue
  else
    apply (Const([], Const.Iff)) [t1; t2]

let iff2 t1 t2 =
  if Term.equiv t1 t2 then
    ttrue
  else
    band [imply t1 t2; imply t2 t1]

let eqBool t1 t2 = apply (Const([], Const.EqBool)) [t1; t2]
let neqBool t1 t2 = apply (Const([], Const.NeqBool)) [t1; t2]
let eqInt t1 t2 = apply (Const([], Const.EqInt)) [t1; t2]
let neqInt t1 t2 = apply (Const([], Const.NeqInt)) [t1; t2]
let eqUnit t1 t2 = apply (Const([], Const.EqUnit)) [t1; t2]
let neqUnit t1 t2 = apply (Const([], Const.NeqUnit)) [t1; t2]
(** ignore equalities of functions *)
let eq_ty ty t1 t2 =
  match ty with
    SimType.Unit ->
      eqUnit t1 t2
  | SimType.Bool ->
      eqBool t1 t2
  | SimType.Int ->
      eqInt t1 t2
  | SimType.Fun(_, _) ->
      ttrue(*???assert false*)
(*
  | _ ->
      let _ = Format.printf "%a@." SimType.pr ty in
      assert false
*)
(*let neq t1 t2 = apply (Const([], Const.Not)) [apply (Const([], Const.Eq)) [t1; t2]]*)
let lt t1 t2 = apply (Const([], Const.Lt)) [t1; t2]
let gt t1 t2 = apply (Const([], Const.Gt)) [t1; t2]
let leq t1 t2 = apply (Const([], Const.Leq)) [t1; t2]
let geq t1 t2 = apply (Const([], Const.Geq)) [t1; t2]
(*let gt t1 t2 = apply (Const([], Const.Lt)) [t2; t1]*)
(*let leq t1 t2 = apply (Const([], Const.Or)) [lt t1 t2; eq t1 t2]*)
(*let geq t1 t2 = apply (Const([], Const.Or)) [gt t1 t2; eq t1 t2]*)

(** ignore equalities of functions *)
let eq_xtty (x, t, ty) =
  eq_ty ty (make_var x) t

let forall_imply conds_envs t =
  List.fold_right
    (fun (cond, env) t ->
      (*
      let _ = Format.printf "cond: %a@.xs: %a@." pr cond (Util.pr_list Var.pr ", ") xs in
      *)
      match cond, env, t with
        App([], App([], Const([], c), Var([], x)), t'), [y, _], _
        when (c = Const.EqBool || c = Const.EqInt) && Var.equiv x y && not (List.mem x (fvs t')) ->
          subst (fun z -> if Var.equiv z x then t' else raise Not_found) t
      (*
      | _, [y], App([], App([], Const([], Const.Eq), t1), App([], App([], Const([], Const.Add), t2), Var([], x)))
        when Var.equiv x y ->
          (* is this sound for any case??? *)
          subst (fun z -> if Var.equiv z x then sub t1 t2 else raise Not_found) cond
        *)
      | _ -> forall env (imply cond t))
    conds_envs t

(** {6 Basic functions} *)

let rec atoms t =
  match fun_args t with
    Var(_, _), [] ->
      [t]
  | Const(_, Const.True), []
  | Const(_, Const.False), [] ->
      []
  | Const(_, Const.And), [t1; t2]
  | Const(_, Const.Or), [t1; t2]
  | Const(_, Const.Imply), [t1; t2]
  | Const(_, Const.Iff), [t1; t2] ->
      atoms t1 @ atoms t2
  | Const(_, Const.Not), [t] -> 
      atoms t
  | Const(_, bop), [_; _] ->
      [t]
  | t, _->
      Format.printf "@.%a@." Term.pr t; assert false

let rec conjuncts t =
  match fun_args t with
    Const(_, Const.And), [t1; t2] ->
      conjuncts t1 @ conjuncts t2
  | _, _ -> [t]

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
      raise Util.NotImplemented
  | Const(_, Const.Not), [t] -> 
      dnfn t
  | Const(_, bop), [_; _] ->
      [[t]]
  | t, _-> Format.printf "@.%a@." Term.pr t; assert false
and dnfn t =
  match fun_args t with
    Var(_, _), [] ->
      [[t]]
  | Const(_, Const.True), [] ->
      []
  | Const(_, Const.False), [] ->
      [[]]
  | Const(_, Const.And), [t1; t2] ->
      dnfn t1 @ dnfn t2
  | Const(_, Const.Or), [t1; t2] ->
      let tss1 = dnfn t1 in Util.concat_map (fun ts2 -> List.map (fun ts1 -> ts1 @ ts2) tss1) (dnfn t2)
  | Const(_, Const.Imply), [t1; t2] ->
      let tss1 = dnf t1 in Util.concat_map (fun ts2 -> List.map (fun ts1 -> ts1 @ ts2) tss1) (dnfn t2)
  | Const(_, Const.Iff), [t1; t2] ->
      raise Util.NotImplemented
  | Const(_, Const.Not), [t] -> 
      dnf t
  | Const(a, bop), [t1; t2] ->
      [[apply (Const(a, Const.bnot_ibin bop)) [t1; t2]]]
  | t, _-> Format.printf "@.%a@." Term.pr t; assert false

let formula_of_dnf tss =
  bor (List.map band tss)

(** {6 Functions on formulas with explicit substitutions} *)

(** ignore equalities on functions *)
let formula_of_fes (ts, xttys) = band (ts @ List.map eq_xtty xttys)

(** eliminate as many existentially quantified variables as possible
    @param p variables not satifying p are existentially quantified
    ignore equalities on functions *)
let eqelim p (ts, xttys) =
  let xttys1, xttys2 = List.partition (fun (x, _, _) -> p x) xttys in
  let sub x = List.assoc x (List.map (fun (x, t, _) -> x, t) xttys2) in
  let ts =
    List.map
				  (fun t ->
				    Util.fixed_point
						    (fun t ->
						      (*Format.printf "%a@." pr t;*)
						      subst sub t)
						    (fun t1 t2 -> Term.equiv t1 t2)
						    t)
		    ts
  in
  let xttys1 =
    List.map
				  (fun xtty ->
		      Util.fixed_point
						    (fun (x, t, ty) ->
						      (*Format.printf "%a@." pr t;*)
						      x, subst sub t, ty)
						    (fun (x1, t1, ty1) (x2, t2, ty2) ->
            x1 = x2 && Term.equiv t1 t2 && ty1 = ty2)
          xtty)
		    xttys1
  in
  let rec aux xttys (xttys1, xttys2) =
    match xttys with
      [] ->
        (xttys1, xttys2)
    | (x, t, ty)::xttys ->
        (match t with
          Var(_, y)
            when not (p y) &&
                 not (List.exists (fun (x, _, _ ) -> Var.equiv y x) xttys2) ->
            aux xttys (xttys1, (y, make_var x, ty)::xttys2)
        | _ ->
            aux xttys ((x, t, ty)::xttys1, xttys2))
  in
  let xttys11, xttys12 = aux xttys1 ([], []) in
  let sub x = List.assoc x (List.map (fun (x, t, _) -> x, t) xttys12) in
  let ts =
    List.map
				  (fun t ->
				    Util.fixed_point
						    (fun t ->
						      (*Format.printf "%a@." pr t;*)
						      subst sub t)
						    (fun t1 t2 -> Term.equiv t1 t2)
						    t)
		    (ts @ List.map eq_xtty xttys11)
  in
  let ts = Util.concat_map conjuncts ts in
  band ts

(*
let eqelim p (ts, xttys) =
  let xttys1, xttys2 = List.partition (fun (x, _, _) -> p x) xttys in
  let sub x = List.assoc x (List.map (fun (x, t, _) -> x, t) xttys2) in
		Util.fixed_point
		  (fun t ->
		    (*Format.printf "%a@." pr t;*)
		    subst sub t)
		  (fun t1 t2 -> equiv t1 t2)
		  (band (ts @ List.map eq_xtty xttys1))
*)

(** {6 Other functions} *)

(** ensure: () is eliminated if a unit variable does not occur in t
    ensure: "t1 =b t2" and "t1 <>b t2" are eliminated by replacing them with "t1 iff t2" and "not (t1 iff t2)" respectively.
    ToDo: check whether they are actually ensured *)
let rec simplify t =
  match fun_args t with
    Const(attr, Const.Not), [t] ->
      let t = simplify t in
      (match fun_args t with
        Const(_, Const.Not), [t] ->
          t
      | Const(attr, c), [t1; t2] when Const.is_ibin c ->
          apply (Const(attr, Const.bnot_ibin c)) [t1; t2]
      | _ -> apply (Const(attr, Const.Not)) [t])
  | Const(attr, Const.And), [t1; t2] ->
      let t1 = simplify t1 in
      let t2 = simplify t2 in
      (try
		      (match LinArith.aif_of t1, LinArith.aif_of t2 with
		        (Const.Leq, nxs1, n1), (Const.Leq, nxs2, n2)
		      | (Const.Geq, nxs1, n1), (Const.Geq, nxs2, n2) ->
		          if LinArith.equiv nxs1 (LinArith.minus nxs2) && n1 = -n2 then
		            eqInt (LinArith.term_of (nxs1, n1)) (tint 0)
		          else t
		      | _ -> apply (Const(attr, Const.And)) [t1; t2])
						with Invalid_argument _ ->
        (match t1, t2 with
          Const(_, Const.True), _ -> t2
        | _, Const(_, Const.True) -> t1
        | _, _ ->
  						    apply (Const(attr, Const.And)) [t1; t2]))
  | Const(attr, c), [t1; t2] when Const.is_ibin c ->
      (try
		      let t1, t2 = LinArith.pos_neg_terms_of (LinArith.of_term (sub t1 t2)) in
		      if c = Const.EqInt && t1 = t2 then
		        ttrue
		      else if c = Const.NeqInt && t1 = t2 then
		        tfalse
		(* unsound??
		      else if c = Const.NeqInt then
				      (match t1, t2 with
				        Const(_, Const.Int(n1)), Const(_, Const.Int(n2)) when n1 <> n2 ->
				          ttrue
				      | _ -> )
		*)
		      else
		        apply (Const(attr, c)) [t1; t2]
      with Invalid_argument _ -> t)
  | Const(attr, Const.EqUnit), [t1; t2] ->
      let t1 = simplify t1 in
      let t2 = simplify t2 in
      if t1 = t2 then
        ttrue
      else
        apply (Const(attr, Const.EqUnit)) [t1; t2]
  | Const(attr, Const.NeqUnit), [t1; t2] ->
      let t1 = simplify t1 in
      let t2 = simplify t2 in
      if t1 = t2 then
        tfalse
      else
        apply (Const(attr, Const.NeqUnit)) [t1; t2]
  | Const(attr, Const.EqBool), [t1; t2] ->
      let t1 = simplify t1 in
      let t2 = simplify t2 in
      if t1 = t2 then
        ttrue
      else
        iff2 t1 t2
  | Const(attr, Const.NeqBool), [t1; t2] ->
      let t1 = simplify t1 in
      let t2 = simplify t2 in
      if t1 = t2 then
        tfalse
      else
        bnot (iff2 t1 t2)
  | Forall(a, env, t), [] ->
      Forall(a, env, simplify t)
  | Exists(a, env, t), [] ->
      Exists(a, env, simplify t)
  | _ -> LinArith.simplify t

(** require: ts are formulas
    ensure: the result does not use =b *)
let elim_unit_boolean ts =
  let _ = assert (ts <> []) in
  let uvs = List.unique (Util.concat_map (unit_vars false) ts) in
  let ts =
    if uvs = [] then
      ts
    else
      let sub x = if List.mem x uvs then tunit else raise Not_found in
      List.map (fun t -> simplify (subst sub t)) ts
  in
  let bvs = List.unique (Util.concat_map (boolean_vars true) ts) in
  if bvs = [] then
    [ts],
    function [t] -> t | _ -> assert false
  else
		  let subs = Util.multiply_list_list (@) (List.map (fun b -> [[b, ttrue]; [b, tfalse]]) bvs) in
		  List.map
		    (fun sub ->
		       List.map (fun t -> simplify (subst (fun x -> List.assoc x sub) t)) ts)
		    subs,
		  fun ts ->
      bor (List.map2 (fun t sub -> band (t::(List.map (fun (b, t) -> eqBool (make_var b) t) sub))) ts subs)

let rec elim_int_equality t =
  match fun_args t with
    Var(attr, x), [] ->
      Var(attr, x)
  | Const(attr, Const.EqInt), [t1; t2] ->
      band [leq t1 t2; geq t1 t2]
  | Const(attr, Const.NeqInt), [t1; t2] ->
      bor [lt t1 t2; gt t1 t2]
  | Const(attr, c), ts ->
      apply (Const(attr, c)) (List.map elim_int_equality ts)
