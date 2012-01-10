open ExtList
open ExtString

type t =
  Var of Attr.t * Var.t
| Const of Attr.t * Const.t
| App of Attr.t * t * t
| Call of Attr.t * t * t list
| Ret of Attr.t * t * t * SimType.t
| Error of Attr.t
| Forall of Attr.t * (Var.t * SimType.t) list * t

(* up to attributes *)
let equiv t1 t2 = t1 = t2(*ToDo*)

let rec fun_args t =
  match t with
    App(_, t1, t2) ->
      let f, args = fun_args t1 in
      f, args @ [t2]
  | _ ->
      t, []

let rec pr ppf t =
  match t with
    Var(_, x) ->
      Format.fprintf ppf "%a" Var.pr x
      (*Format.fprintf ppf "%a%d" Idnt.pr id (try Attr.arity a with Not_found -> 0)*)
  | Const(_, c) ->
      Format.fprintf ppf "%a" Const.pr c
  | App(_, _, _) ->
      let f, args = fun_args t in
      (match f, args with
        Var(_, _), _ ->
          Format.fprintf ppf "(@[<hov2>%a@ @[<hov>%a@]@])" pr f (Util.pr_list pr "@ ") args
      | Const(_, _), [t] ->
          Format.fprintf ppf "(%a %a)" pr f pr t
      | Const(_, c), [t1; t2] when Const.is_binary c ->
          Format.fprintf ppf "(@[<hov>%a %a@ %a@])" pr t1 Const.pr_bin c pr t2
      | Const(_, _), _ ->
          Format.fprintf ppf "(@[<hov2>%a@ @[<hov>%a@]@])" pr f (Util.pr_list pr "@ ") args
      | _, _ ->
          assert false)
  | Call(_, f, args) ->
      Format.fprintf ppf "Call(@[<hov>%a@])" (Util.pr_list pr ",@ ") (f::args)
  | Ret(_, ret, t, _) ->
      Format.fprintf ppf "Ret(@[<hov>%a,@ %a@])" pr ret pr t
  | Error(_) ->
      Format.fprintf ppf "Error"
  | Forall(_, env, t) ->
      Format.fprintf ppf "Forall(%a, %a)" (Util.pr_list SimType.pr_bind ",") env pr t

let rec pr2 ppf t =
  match t with
    Var(_, x) ->
      Format.fprintf ppf "%a" Var.pr x
  | Const(_, c) ->
      Format.fprintf ppf "%a" Const.pr c
  | App(_, _, _) ->
      let f, args = fun_args t in
      (match f, args with
        Var(_, _), _ ->
          Format.fprintf ppf "(%a %a)" pr2 f (Util.pr_list pr2 " ") args
      | Const(_, _), [t] ->
          Format.fprintf ppf "(%a %a)" pr2 f pr2 t
      | Const(_, c), [t1; t2] when Const.is_binary c ->
          Format.fprintf ppf "(%a %a %a)" pr2 t1 Const.pr_bin c pr2 t2
      | Const(_, _), _ ->
          Format.fprintf ppf "(%a %a)" pr2 f (Util.pr_list pr2 " ") args
      | _, _ ->
          assert false)
  | Call(_, f, args) ->
      Format.fprintf ppf "Call(%a)" (Util.pr_list pr2 ", ") (f::args)
  | Ret(_, ret, t, _) ->
      Format.fprintf ppf "Ret(%a, %a)" pr2 ret pr2 t
  | Error(_) ->
      Format.fprintf ppf "Error"
  | Forall(_, _, _) ->
      assert false

let string_of t =
  Format.fprintf Format.str_formatter "%a" pr2 t;
  Format.flush_str_formatter ()

let rec fvs t =
  match t with
    Var(_, x) -> [x]
  | Const(_, _) -> []
  | App(_, t1, t2) -> List.unique (fvs t1 @ fvs t2)
  | Forall(_, env, t) -> Util.diff (fvs t) (List.map fst env)
  | _ -> assert false

let make_var id = Var([], Var.V(Idnt.make id))
let make_var2 x = Var([], x)
let tint n = Const([], Const.Int(n))
let tunit = Const([], Const.Unit)
let ttrue = Const([], Const.True)
let tfalse = Const([], Const.False)
let tevent id = Const([], Const.Event(Idnt.make id))

let rec apply t ts =
  match ts with
    [] ->
      t
  | t'::ts' ->
      apply (App([], t, t')) ts'

let tfail = apply (tevent "fail") [tunit]

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
  if equiv t1 ttrue then
    t2
  else if equiv t1 tfalse then
    ttrue
  else if equiv t2 ttrue then
    ttrue
  else if equiv t2 tfalse then
    bnot t1
  else
    apply (Const([], Const.Imply)) [t1; t2]

let forall env t =
  let xs = fvs t in
  let _ = Format.printf "env: %a@.xs: %a@." (Util.pr_list SimType.pr_bind ",") env (Util.pr_list Var.pr ",") xs in
  let env = List.filter (fun (x, _) -> List.mem x xs) env in
  if env = [] then t else Forall([], env, t)

let exists xs t =
  bnot (forall xs (bnot t))

let iff t1 t2 =
  if equiv t1 t2 then
    ttrue
  else
    apply (Const([], Const.Iff)) [t1; t2]

let iff2 t1 t2 =
  if equiv t1 t2 then
    ttrue
  else
    band [imply t1 t2; imply t2 t1]

let add t1 t2 = apply (Const([], Const.Add)) [t1; t2]
let rec sum ts =
  match ts with
    [] -> tint 0
  | [t] -> t
  | (Const(_, Const.Int(0)))::ts' -> sum ts'
  | t::ts' ->
      let t' = sum ts' in
      (match t' with
        Const(_, Const.Int(0)) ->
          t
      | _ ->
          apply (Const([], Const.Add)) [t; t'])

(*let sub t1 t2 = apply (Const([], Const.Add)) [t1; apply (Const([], Const.Minus)) [t2]]*)
let sub t1 t2 = apply (Const([], Const.Sub)) [t1; t2]
let minus t = apply (Const([], Const.Minus)) [t]
let mul t1 t2 = apply (Const([], Const.Mul)) [t1; t2]
let eqBool t1 t2 = apply (Const([], Const.EqBool)) [t1; t2]
let neqBool t1 t2 = apply (Const([], Const.NeqBool)) [t1; t2]
let eqInt t1 t2 = apply (Const([], Const.EqInt)) [t1; t2]
let neqInt t1 t2 = apply (Const([], Const.NeqInt)) [t1; t2]
let eqUnit t1 t2 = apply (Const([], Const.EqUnit)) [t1; t2]
let neqUnit t1 t2 = apply (Const([], Const.NeqUnit)) [t1; t2]
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
  | _ ->
      let _ = Format.printf "%a@." SimType.pr ty in
      assert false
(*let neq t1 t2 = apply (Const([], Const.Not)) [apply (Const([], Const.Eq)) [t1; t2]]*)
let lt t1 t2 = apply (Const([], Const.Lt)) [t1; t2]
let gt t1 t2 = apply (Const([], Const.Gt)) [t1; t2]
let leq t1 t2 = apply (Const([], Const.Leq)) [t1; t2]
let geq t1 t2 = apply (Const([], Const.Geq)) [t1; t2]
(*let gt t1 t2 = apply (Const([], Const.Lt)) [t2; t1]*)
(*let leq t1 t2 = apply (Const([], Const.Or)) [lt t1 t2; eq t1 t2]*)
(*let geq t1 t2 = apply (Const([], Const.Or)) [gt t1 t2; eq t1 t2]*)

let rec subst sub t =
  match t with
    Var(a, x) -> (try sub x with Not_found -> Var(a, x))
  | Const(a, c) -> Const(a, c)
  | App(a, t1, t2) -> App(a, subst sub t1, subst sub t2)
  | Forall(a, env, t) ->
      let xs = List.map fst env in
      Forall
        (a, env,
        subst
          (fun x -> if List.mem x xs then raise Not_found else sub x)
          t)
  | _ -> assert false

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

let rec redex_of env t =
  match t with
(*
				Const(a, Const.RandInt) ->
      (fun t -> t), Const(a, Const.RandInt)
*)
    App(_, _, _) ->
      let f, args = fun_args t in
      let rec r args1 args =
        match args with
          [] -> raise Not_found
        | arg::args2 ->
            (try
              args1, redex_of env arg, args2
            with Not_found ->
              r (args1 @ [arg]) args2)
      in
      (try
        let args1, (ctx, red), args2 = r [] args in
        (fun t -> apply f (args1 @ [ctx t] @ args2)), red
      with Not_found ->
		      (match f with
						    Const(_, Const.Event(id)) when id = "fail" ->
            let ar = 1 in
		          if List.length args >= ar then
		            let args1, args2 = Util.split_at args ar in
		            (fun t -> apply t args2), apply f args1
		          else raise Not_found
						  | Const(_, Const.RandInt) ->
            let ar = 1 in
		          if List.length args >= ar then
		            let args1, args2 = Util.split_at args ar in
		            (fun t -> apply t args2), apply f args1
		          else raise Not_found
		      | Var(attr, ff) ->
		          let ar =
		            try
		              SimType.arity (env ff)
		            with Not_found ->
		              raise Not_found (* ff is not a function name *)
		              (*(Format.printf "%a@." Var.pr ff; assert false)*)
		          in
		          if List.length args >= ar then
		            let args1, args2 = Util.split_at args ar in
		            (fun t -> apply t args2), apply f args1
		          else raise Not_found
		      | Const(attr, c) ->
		          raise Not_found
		      | _ -> assert false))
  | Call(a, f, args) ->
      (fun t -> t), Call(a, f, args)
  | Ret(a, ret, t, ty) ->
      (try
        let ctx, red = redex_of env t in
        (fun t -> Ret(a, ret, ctx t, ty)), red
      with Not_found ->
        (fun t -> t), Ret(a, ret, t, ty))
  | _ -> raise Not_found


let rec dnf t =
  match fun_args t with
    Var(_, _), [] ->
      [[t]]
  | Const(_, Const.True), [] ->
      [[]]
  | Const(_, Const.False), [] ->
      []
  | Const(_, Const.And), [t1; t2] ->
      let tss1 = dnf t1 in Util.concat_map (fun ts2 -> List.map (fun ts1 -> ts1 @ ts2) tss1) (dnf t2)
  | Const(_, Const.Or), [t1; t2] ->
      dnf t1 @ dnf t2
  | Const(_, Const.Imply), [t1; t2] ->
      dnfn t1 @ dnf t2
  | Const(_, Const.Iff), [t1; t2] ->
      raise Util.ToBeImplemented
  | Const(_, Const.Not), [t] -> 
      dnfn t
  | Const(_, bop), [_; _] ->
      [[t]]
  | t, _-> Format.printf "@.%a@." pr t; assert false
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
      raise Util.ToBeImplemented
  | Const(_, Const.Not), [t] -> 
      dnf t
  | Const(a, bop), [t1; t2] ->
      [[apply (Const(a, Const.bnot_ibin bop)) [t1; t2]]]
  | t, _-> Format.printf "@.%a@." pr t; assert false

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
      Format.printf "@.%a@." pr t; assert false


let rec unit_vars is_unit t =
		match fun_args t with
		  Var(_, v), [] ->
      if is_unit then [v] else []
		| Const(_, c), [] ->
		    []
		| Const(a, Const.And), [t1; t2]
		| Const(a, Const.Or), [t1; t2]
		| Const(a, Const.Imply), [t1; t2]
		| Const(a, Const.Iff), [t1; t2]
		| Const(a, Const.Lt), [t1; t2]
		| Const(a, Const.Gt), [t1; t2]
		| Const(a, Const.Leq), [t1; t2]
		| Const(a, Const.Geq), [t1; t2]
		| Const(a, Const.EqBool), [t1; t2]
		| Const(a, Const.EqInt), [t1; t2]
		| Const(a, Const.NeqBool), [t1; t2]
		| Const(a, Const.NeqInt), [t1; t2]
		| Const(a, Const.Add), [t1; t2]
		| Const(a, Const.Sub), [t1; t2]
		| Const(a, Const.Mul), [t1; t2]
		| Const(a, Const.Minus), [t1; t2] ->
		    unit_vars false t1 @ unit_vars false t2
		| Const(a, Const.Not), [t] -> 
		    unit_vars false t
		| Const(a, Const.EqUnit), [t1; t2]
		| Const(a, Const.NeqUnit), [t1; t2] ->
		    unit_vars true t1 @ unit_vars true t2
		| t, _-> Format.printf "@.%a@." pr t; assert false

let rec boolean_vars is_boolean t =
		match fun_args t with
		  Var(_, v), [] ->
      if is_boolean then [v] else []
		| Const(_, c), [] ->
		    []
		| Const(a, Const.And), [t1; t2]
		| Const(a, Const.Or), [t1; t2]
		| Const(a, Const.Imply), [t1; t2]
		| Const(a, Const.Iff), [t1; t2]
		| Const(a, Const.EqBool), [t1; t2]
		| Const(a, Const.NeqBool), [t1; t2] ->
		    boolean_vars true t1 @ boolean_vars true t2
		| Const(a, Const.Lt), [t1; t2]
		| Const(a, Const.Gt), [t1; t2]
		| Const(a, Const.Leq), [t1; t2]
		| Const(a, Const.Geq), [t1; t2]
		| Const(a, Const.EqUnit), [t1; t2]
		| Const(a, Const.EqInt), [t1; t2]
		| Const(a, Const.NeqUnit), [t1; t2]
		| Const(a, Const.NeqInt), [t1; t2]
		| Const(a, Const.Add), [t1; t2]
		| Const(a, Const.Sub), [t1; t2]
		| Const(a, Const.Mul), [t1; t2]
		| Const(a, Const.Minus), [t1; t2] ->
		    boolean_vars false t1 @ boolean_vars false t2
		| Const(a, Const.Not), [t] -> 
		    boolean_vars true t
		| t, _-> Format.printf "@.%a@." pr t; assert false

let term_of_arith nxs n =
  let ts =
    (if n = 0 then [] else [tint n]) @
    (List.filter_map (fun (n, x) -> if n = 0 then None else Some(mul (tint n) (make_var2 x))) nxs)
  in
  sum ts

let rec arith_of t =
  match fun_args t with
    Var(_, x), [] ->
      [1, x], 0
  | Const(_, Const.Int(n)), [] ->
      [], n
  | Const(_, Const.Add), [t1; t2] ->
      let nxs1, n1 = arith_of t1 in
      let nxs2, n2 = arith_of t2 in
      Arith.canonize (nxs1 @ nxs2), n1 + n2
  | Const(_, Const.Sub), [t1; t2] ->
      let nxs1, n1 = arith_of t1 in
      let nxs2, n2 = arith_of t2 in
      let nxs2, n2 =  Arith.minus nxs2, -n2 in
      Arith.canonize (nxs1 @ nxs2), n1 + n2
  | Const(_, Const.Mul), [Const(_, Const.Int(m)); t]
  | Const(_, Const.Mul), [t; Const(_, Const.Int(m))] ->
      let nxs, n = arith_of t in
      Arith.mul m nxs, m * n
  | Const(_, Const.Minus), [t] ->
      let nxs, n = arith_of t in
      Arith.minus nxs, -n
  | Const(_, Const.Unit), [] ->
      [], 0 (*????*)
  | _ ->
      invalid_arg "Term.arith_of"
    
let int_rel_of t =
  match fun_args t with
    Const(_, c), [t1; t2] when Const.is_int_rel c ->
      let nxs, n = arith_of (sub t1 t2) in
      c, nxs, n
  | _ -> invalid_arg "Term.int_rel_of"

(* ensure that:
  the result does not use () if t does not use a unit variable and
  t1 =b t2 and t1 <>b t2 are replaced with t1 iff t2 and not (t1 iff t2) resp.
  ToDo: check whether they are actually ensured *)
let rec simplify t =
  match fun_args t with
    Const(attr, Const.And), [t1; t2] ->
      let t1 = simplify t1 in
      let t2 = simplify t2 in
      (try
		      (match int_rel_of t1, int_rel_of t2 with
		        (Const.Leq, nxs1, n1), (Const.Leq, nxs2, n2)
		      | (Const.Geq, nxs1, n1), (Const.Geq, nxs2, n2) ->
		          if Arith.equiv nxs1 (Arith.minus nxs2) && n1 = -n2 then
		            eqInt (term_of_arith nxs1 n1) (tint 0)
		          else t
		      | _ -> apply (Const(attr, Const.And)) [t1; t2])
						with Invalid_argument _ ->
        (match t1, t2 with
          Const(_, Const.True), _ -> t2
        | _, Const(_, Const.True) -> t1
        | _, _ ->
  						    apply (Const(attr, Const.And)) [t1; t2]))
  | Const(attr, Const.Add), [t1; t2] ->
      let t1 = simplify t1 in
      let t2 = simplify t2 in
      (match t1, t2 with
        Const(_, Const.Int(n1)), Const(_, Const.Int(n2)) ->
          Const(attr, Const.Int(n1 + n2))
      | _ -> apply (Const(attr, Const.Add)) [t1; t2])
  | Const(attr, Const.Sub), [t1; t2] ->
      let t1 = simplify t1 in
      let t2 = simplify t2 in
      (match t1, t2 with
        Const(_, Const.Int(n1)), Const(_, Const.Int(n2)) ->
          Const(attr, Const.Int(n1 - n2))
      | _ -> apply (Const(attr, Const.Sub)) [t1; t2])
  | Const(attr, Const.Mul), [t1; t2] ->
      let t1 = simplify t1 in
      let t2 = simplify t2 in
      (match t1, t2 with
        Const(_, Const.Int(n1)), Const(_, Const.Int(n2)) ->
          Const(attr, Const.Int(n1 * n2))
      | Const(_, Const.Int(0)), t
      | t, Const(_, Const.Int(0)) ->
          tint 0
      | Const(_, Const.Int(1)), t
      | t, Const(_, Const.Int(1)) ->
          t
      | _ -> apply (Const(attr, Const.Mul)) [t1; t2])
  | Const(attr, c), [t1; t2] when c = Const.EqUnit || c = Const.EqInt ->
      let t1 = simplify t1 in
      let t2 = simplify t2 in
      if t1 = t2 then
        ttrue
      else
        apply (Const(attr, c)) [t1; t2]
  | Const(attr, c), [t1; t2] when c= Const.NeqUnit || c = Const.NeqInt ->
      let t1 = simplify t1 in
      let t2 = simplify t2 in
      if t1 = t2 then
        tfalse
      else
		      (match t1, t2 with
		        Const(_, Const.Int(n1)), Const(_, Const.Int(n2)) when n1 <> n2 ->
		          ttrue
		      | _ -> apply (Const(attr, c)) [t1; t2])
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
  | Var(attr, x), [] ->
      Var(attr, x)
  | Const(attr, c), [] ->
      Const(attr, c)
  | Const(attr, c), [t] ->
      let t = simplify t in
      apply (Const(attr, c)) [t]
  | Const(attr, c), [t1; t2] ->
      let t1 = simplify t1 in
      let t2 = simplify t2 in
      apply (Const(attr, c)) [t1; t2]
  | _ -> let _ = Format.printf "%a" pr t in assert false


(*
let rec set_arity am t =
  match t with
    Var(a, x) -> (try let ar = am x in Var(Attr.Arity(ar)::a, x) with Not_found -> Var(a, x))
  | Const(a, c) -> Const(a, c)
  | App(a, t1, t2) -> App(a, set_arity am t1, set_arity am t2)
  | Call(a, f, args) -> Call(a, set_arity am f, List.map (set_arity am) args)
  | Ret(a, ret, t) -> Ret(a, set_arity am ret, set_arity am t)
  | Error(a) -> Error(a)
*)


(* require that ts are formulas, ensures that the result does not use =b *)
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
      bor (List.map2 (fun t sub -> band (t::(List.map (fun (b, t) -> eqBool (make_var2 b) t) sub))) ts subs)


(* p for bound variables *)
let rename_fresh p t =
  let fvs = List.filter (fun x -> not (p x)) (fvs t) in
  let sub = List.map (fun x -> x, make_var2 (Var.new_var ())) fvs in
  subst (fun x -> List.assoc x sub) t
