open ExtList
open ExtString

type s = V of Id.t | T of s * int * int
type t =
  Var of Attr.t * s
| Const of Attr.t * Const.t
| App of Attr.t * t * t
| Call of Attr.t * t * t list
| Ret of Attr.t * t * t
| Error of Attr.t

let rec fun_args t =
  match t with
    App(_, t1, t2) ->
      let f, args = fun_args t1 in
      f, args @ [t2]
  | _ ->
      t, []

let rec pr_var ppf x =
  match x with
    V(id) ->
      Format.fprintf ppf "%a" Id.pr id
  | T(x, uid, arg) ->
	     Format.fprintf ppf "<%a%s%s>" pr_var x
	       (":" ^ String.of_int uid)
	       (":" ^ String.of_int arg)

let rec pr ppf t =
  match t with
    Var(_, x) ->
      Format.fprintf ppf "%a" pr_var x
      (*Format.fprintf ppf "%a%d" Id.pr id (try Attr.arity a with Not_found -> 0)*)
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
  | Ret(_, ret, t) ->
      Format.fprintf ppf "Ret(@[<hov>%a,@ %a@])" pr ret pr t
  | Error(_) ->
      Format.fprintf ppf "Error"

let rec pr2 ppf t =
  match t with
    Var(_, x) ->
      Format.fprintf ppf "%a" pr_var x
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
  | Ret(_, ret, t) ->
      Format.fprintf ppf "Ret(%a, %a)" pr2 ret pr2 t
  | Error(_) ->
      Format.fprintf ppf "Error"

let string_of t =
  Format.fprintf Format.str_formatter "%a" pr2 t;
  Format.flush_str_formatter ()

let make_var id = Var([], V(Id.make id))
let make_int n = Const([], Const.Int(n))
let make_unit = Const([], Const.Unit)
let make_true = Const([], Const.True)
let make_false = Const([], Const.False)
let make_event id = Const([], Const.Event(Id.make id))

let rec apply t ts =
  match ts with
    [] ->
      t
  | t'::ts' ->
      apply (App([], t, t')) ts'

let rec band ts =
  match ts with
    [] -> Const([], Const.True)
  | [t] -> t
  | (Const(_, Const.True))::ts' -> band ts'
  | t::ts' ->
      let t' = band ts' in
      (match t' with
        Const(_, Const.True) ->
          t
      | _ ->
          apply (Const([], Const.And)) [t; t'])
let rec bor ts =
  match ts with
    [] -> Const([], Const.False)
  | [t] -> t
  | (Const(_, Const.False))::ts' -> bor ts'
  | t::ts' ->
      let t' = bor ts' in
      (match t' with
        Const(_, Const.False) ->
          t
      | _ ->
          apply (Const([], Const.Or)) [t; t'])
let bnot t = apply (Const([], Const.Not)) [t]

let imply t1 t2 = apply (Const([], Const.Imply)) [t1; t2]

let add t1 t2 = apply (Const([], Const.Add)) [t1; t2]
let rec sum ts =
  match ts with
    [] -> make_int 0
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
let mul t1 t2 = apply (Const([], Const.Mul)) [t1; t2]
let eq t1 t2 = apply (Const([], Const.Eq)) [t1; t2]
(*let neq t1 t2 = apply (Const([], Const.Not)) [apply (Const([], Const.Eq)) [t1; t2]]*)
let neq t1 t2 = apply (Const([], Const.Neq)) [t1; t2]
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
  | _ -> assert false

let rec fvs t =
  match t with
    Var(_, x) -> [x]
  | Const(_, _) -> []
  | App(_, t1, t2) -> List.unique (fvs t1 @ fvs t2)
  | _ -> assert false

let rec redex_of env t =
		match t with
    Const(a, Const.Event(id)) when id = "fail" ->
      (fun t -> t), Const(a, Const.Event(id))
		| App(_, _, _) ->
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
				  (match f with
				    Var(attr, f) ->
          (try
            let args1, (ctx, red), args2 = r [] args in
            (fun t -> apply (Var(attr, f)) (args1 @ [ctx t] @ args2)), red
          with Not_found ->
		          let ar =
              try
                Type.arity (env f)
              with Not_found ->
                (Format.printf "%a@." pr_var f; assert false)
            in
		          if List.length args >= ar then
		  				      let args1, args2 = Util.split_at args ar in
		            (fun t -> apply t args2), apply (Var(attr, f)) args1
		          else raise Not_found)
      | Const(attr, c) ->
          (try
            let args1, (ctx, red), args2 = r [] args in
            (fun t -> apply (Const(attr, c)) (args1 @ [ctx t] @ args2)), red
          with Not_found ->
		          raise Not_found))
  | Call(a, f, args) ->
      (fun t -> t), Call(a, f, args)
  | Ret(a, ret, t) ->
      (try
		      let ctx, red = redex_of env t in
		      (fun t -> Ret(a, ret, ctx t)), red
      with Not_found ->
        (fun t -> t), Ret(a, ret, t))
  | _ -> raise Not_found


let rec dnf t =
  let f, args = fun_args t in
		match f, args with
    Const(_, Const.True), [] ->
      [[]]
  | Const(_, Const.False), [] ->
      []
  | Const(_, Const.And), [t1; t2] ->
      let tss1 = dnf t1 in Util.concat_map (fun ts2 -> List.map (fun ts1 -> ts1 @ ts2) tss1) (dnf t2)
  | Const(_, Const.Or), [t1; t2] ->
      dnf t1 @ dnf t2
  | Const(_, Const.Imply), [t1; t2] ->
      dnfn t1 @ dnf t2
  | Const(_, Const.Not), [t] -> 
      dnfn t
  | Const(_, bop), [_; _] ->
      [[t]]
  | _ -> assert false
and dnfn t =
  let f, args = fun_args t in
		match f, args with
    Const(_, Const.True), [] ->
      []
  | Const(_, Const.False), [] ->
      [[]]
  | Const(_, Const.And), [t1; t2] ->
      dnfn t1 @ dnfn t2
  | Const(_, Const.Or), [t1; t2] ->
      let tss1 = dnfn t1 in Util.concat_map (fun ts2 -> List.map (fun ts1 -> ts1 @ ts2) tss1) (dnfn t2)
  | Const(_, Const.Imply), [t1; t2] ->
      let tss1 = dnf t1 in Util.concat_map (fun ts2 -> List.map (fun ts1 -> ts1 @ ts2) tss1) (dnfn t2)
  | Const(_, Const.Not), [t] -> 
      dnf t
  | Const(a, bop), [t1; t2] ->
      [[apply (Const(a, Const.bnot_ibin bop)) [t1; t2]]]
  | _ -> assert false

let minus nxs = List.map (fun (n, x) -> -n, x) nxs
let rec conv t =
  let merge nxs =
    let res = Util.classify (fun (_, x1) (_, x2) -> x1 = x2) nxs in
    List.map
      (function ((n, x)::nxs) ->
        (List.fold_left (fun n1 n2 -> n1 + n2) n (List.map fst nxs), x)
      | _ -> assert false)
      res
  in
  let f, args = fun_args t in
  match f, args with
    Var(_, x), [] ->
      [1, x], 0
  | Const(_, Const.Int(n)), [] ->
      [], n
  | Const(_, Const.Add), [t1; t2] ->
      let nxs1, n1 = conv t1 in
      let nxs2, n2 = conv t2 in
      merge (nxs1 @ nxs2), n1 + n2
  | Const(_, Const.Sub), [t1; t2] ->
      let nxs1, n1 = conv t1 in
      let nxs2, n2 = conv t2 in
      let nxs2, n2 =  minus nxs2, -n2 in
      merge (nxs1 @ nxs2), n1 + n2
  | Const(_, Const.Mul), [Const(_, Const.Int(m)); t]
  | Const(_, Const.Mul), [t; Const(_, Const.Int(m))] ->
      let nxs, n = conv t in
      List.map (fun (n, x) -> m * n, x) nxs, m * n
  | Const(_, Const.Minus), [t] ->
      let nxs, n = conv t in
      minus nxs, -n


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

let csisat_true = CsisatAst.Application("true", [])
let csisat_false = CsisatAst.Application("false", [])

let rec string_of_var x =
  match x with
    V(id) -> id
  | T(x, uid, arg) ->
      string_of_var x ^ "_" ^ String.of_int uid ^ "_" ^ String.of_int arg

let rec csisat_of_iexp t =
  let f, args = fun_args t in
  match f, args with
    Var(_, x), [] -> CsisatAst.Variable(string_of_var x)
  | Const(_, c), _ -> csisat_of_iexp_aux c args
  | _ -> assert false
and csisat_of_iexp_aux c args =
  match c, args with
    Const.Int(n), [] -> CsisatAst.Constant(float_of_int n)
  | Const.Add, [t1; t2] -> CsisatAstUtil.simplify_expr (CsisatAst.Sum([csisat_of_iexp t1; csisat_of_iexp t2]))
  | Const.Sub, [t1; t2] -> CsisatAstUtil.simplify_expr (CsisatAst.Sum([csisat_of_iexp t1; CsisatAst.Coeff(-1.0, csisat_of_iexp t2)]))
  | Const.Mul, [Const(_, Const.Int(n)); t]
  | Const.Mul, [t; Const(_, Const.Int(n))] -> CsisatAstUtil.simplify_expr (CsisatAst.Coeff(float_of_int n, csisat_of_iexp t))
  | Const.Minus, [t] -> CsisatAstUtil.simplify_expr (CsisatAst.Coeff(-1.0, csisat_of_iexp t))

let rec csisat_of_bexp t =
  let f, args = fun_args t in
  match f, args with
    Var(_, x), [] -> CsisatAst.Eq(CsisatAst.Variable(string_of_var x), csisat_true)
  | Const(_, c), _ -> csisat_of_bexp_aux c args
  | _ -> assert false
and csisat_of_bexp_aux c args =
  match c, args with
    Const.True, [] -> CsisatAst.True
  | Const.False, [] -> CsisatAst.False
  | Const.And, [t1; t2] -> CsisatAstUtil.simplify (CsisatAst.And([csisat_of_bexp t1; csisat_of_bexp t2]))
  | Const.Or, [t1; t2] -> CsisatAstUtil.simplify (CsisatAst.Or([csisat_of_bexp t1; csisat_of_bexp t2]))
  | Const.Imply, [t1; t2] -> CsisatAstUtil.simplify (CsisatAst.Or([CsisatAst.Not(csisat_of_bexp t1); csisat_of_bexp t2]))
  | Const.Not, [t] -> CsisatAst.Not(csisat_of_bexp t)
  | Const.Lt, [t1; t2] -> CsisatAst.Lt(csisat_of_iexp t1, csisat_of_iexp t2)
  | Const.Gt, [t1; t2] -> CsisatAst.Lt(csisat_of_iexp t2, csisat_of_iexp t1)
  | Const.Leq, [t1; t2] -> CsisatAst.Leq(csisat_of_iexp t1, csisat_of_iexp t2)
  | Const.Geq, [t1; t2] -> CsisatAst.Leq(csisat_of_iexp t2, csisat_of_iexp t1)
(*t1, t2 may be boolean*)
  | Const.Eq, [t1; t2] -> CsisatAst.Eq(csisat_of_iexp t1, csisat_of_iexp t2)
  | Const.Neq, [t1; t2] -> CsisatAst.Not(CsisatAst.Eq(csisat_of_iexp t1, csisat_of_iexp t2))
  | _ -> assert false

let var_of s =
  let rec f x ss =
    match ss with
      [] -> x
    | s1::s2::ss ->
        f (T(x, int_of_string s1, int_of_string s2)) ss
    | _ -> assert false
  in
  let s::ss = String.nsplit s "_" in
  f (V(s)) ss


let rec iexp_of s =
  match s with
    CsisatAst.Constant(f) ->
      make_int (int_of_float f)
  | CsisatAst.Variable(id) ->
      Var([], var_of id)
(*
  | CsisatAst.Application("true", []) ->
  | CsisatAst.Application("false", []) ->
*)
  | CsisatAst.Sum(ss) ->
      sum (List.map iexp_of ss)
  | CsisatAst.Coeff(f, s) ->
      mul (make_int (int_of_float f)) (iexp_of s)
  | _ -> assert false

let rec bexp_of p =
  match p with
    CsisatAst.True -> make_true
  | CsisatAst.False -> make_false
  | CsisatAst.And(ps) -> band (List.map bexp_of ps)
  | CsisatAst.Or(ps) -> bor (List.map bexp_of ps)
  | CsisatAst.Not(p) -> bnot (bexp_of p)
  | CsisatAst.Eq(s1, s2) -> eq (iexp_of s1) (iexp_of s2)
  | CsisatAst.Lt(s1, s2) -> lt (iexp_of s1) (iexp_of s2)
  | CsisatAst.Leq(s1, s2) -> leq (iexp_of s1) (iexp_of s2)
  | _ -> assert false

(*???*)
let equiv t1 t2 = t1 = t2

exception No_interpolant

let interpolate t1 t2 =
  let t1 = CsisatAstUtil.simplify (csisat_of_bexp t1) in
(*
Format.printf "%s@." (CsisatAstUtil.print_pred t1);
*)
  let t2 = CsisatAstUtil.simplify (csisat_of_bexp (bnot t2)) in
(*
Format.printf "%s@." (CsisatAstUtil.print_pred t2);
*)
  let interp =
    try
      CsisatInterpolate.interpolate_with_proof t1 t2
    with CsisatAst.SAT_FORMULA(_) ->
      raise No_interpolant
  in
(*
Format.printf "%s@." (CsisatAstUtil.print_pred interp);
*)
  let interp = CsisatAstUtil.simplify (CsisatLIUtils.round_coeff interp) in
  bexp_of (CsisatAstUtil.dnf interp)

let manpk = Polka.manager_alloc_strict ()
(*let maneq = Polka.manager_alloc_equalities ()*)


let linconstr_of env t =
  let f, [t1; t2] = fun_args t in
  let nxs, n = conv (sub t1 t2) in
  
  let expr = Apron.Linexpr1.make env in
  Apron.Linexpr1.set_array expr
    (Array.of_list (List.map (fun (n, x) -> Apron.Coeff.s_of_int n, Apron.Var.of_string (string_of_var x)) nxs))
    (Some ((Apron.Coeff.s_of_int n)));
  let mexpr = Apron.Linexpr1.make env in
  Apron.Linexpr1.set_array mexpr
    (Array.of_list (List.map (fun (n, x) -> Apron.Coeff.s_of_int n, Apron.Var.of_string (string_of_var x)) (minus nxs)))
    (Some ((Apron.Coeff.s_of_int n)));
		match f with
    Const(_, Const.Lt) -> Apron.Lincons1.make mexpr Apron.Lincons1.SUP
  | Const(_, Const.Gt) -> Apron.Lincons1.make expr Apron.Lincons1.SUP
  | Const(_, Const.Leq) -> Apron.Lincons1.make mexpr Apron.Lincons1.SUPEQ
  | Const(_, Const.Geq) -> Apron.Lincons1.make expr Apron.Lincons1.SUPEQ
  | Const(_, Const.Eq) -> Apron.Lincons1.make expr Apron.Lincons1.EQ
  | Const(_, Const.Neq) -> Apron.Lincons1.make expr Apron.Lincons1.DISEQ
  | _ -> assert false

let int_of_coeff n =
  if Apron.Coeff.is_scalar n then
    match n with
      Apron.Coeff.Scalar(s) -> int_of_string (Apron.Scalar.to_string s)
    | _ -> assert false
  else
    assert false

let of_linconstr c =
  let nxs = ref [] in
  Apron.Lincons1.iter (fun n x -> nxs := (int_of_coeff n, var_of (Apron.Var.to_string x))::!nxs) c;
  let n = int_of_coeff (Apron.Lincons1.get_cst c) in
  let t = List.fold_left
    (fun t (n, x) ->
      if n = 0 then
        t
      else
		      let t' = if n = 1 then Var([], x) else mul (make_int n) (Var([], x)) in
				    match t with
		        Const(_, Const.Int(m)) when m = 0 -> t'
				    | _ -> add t t')
    (make_int n) !nxs
  in
  let zero = make_int 0 in
  match Apron.Lincons1.get_typ c with
    Apron.Lincons1.SUP -> gt t zero
  | Apron.Lincons1.SUPEQ -> geq t zero
  | Apron.Lincons1.EQ -> eq t zero
  | Apron.Lincons1.DISEQ -> neq t zero
let of_linconstrs cs =
  band
    (List.mapi
      (fun i _ ->
        of_linconstr (Apron.Lincons1.array_get cs i))
      (List.make (Apron.Lincons1.array_length cs) ()))

let polyhedron_of env t =
  let tss = dnf t in
(*
  List.iter (fun ts -> Format.printf "cj: @[<hv>%a@]@ " (Util.pr_list pr ",@ ") ts) tss;
*)
  let abs =
		  List.map
		    (fun ts ->
						  let tab = Apron.Lincons1.array_make env (List.length ts) in
		      List.iteri
          (fun i t ->
            let ab = linconstr_of env t in
(*
            Format.printf "linconstr: %a@ " Apron.Lincons1.print ab;
*)
            Apron.Lincons1.array_set tab i ab)
          ts;
						  Apron.Abstract1.of_lincons_array manpk env tab)
		    tss
  in
(*
  Format.printf "poly': @[<hv>%a@]@ " (Util.pr_list Apron.Abstract1.print ",@ ") abs;
*)
  Apron.Abstract1.join_array manpk (Array.of_list abs)

let widen2 t1 t2 = Apron.Abstract1.widening manpk t1 t2

let widen ts =
  Format.printf "@[<hov>widen_in: @[<hv>%a@]@ " (Util.pr_list pr ",@ ") ts;

  let fvs = List.map (fun x -> Apron.Var.of_string (string_of_var x)) (List.unique (Util.concat_map fvs ts)) in
  let env = Apron.Environment.make (Array.of_list fvs) [||] in
  let ts = List.map (fun t -> polyhedron_of env t) ts in
(*
  Format.printf "poly: @[<hv>%a@]@ " (Util.pr_list Apron.Abstract1.print ",@ ") ts;
*)
  let t::ts = ts in
  let ab = List.fold_left (fun t1 t2 -> widen2 t1 t2) t ts in
(*
  Format.printf "widen: @[%a@]@ " Apron.Abstract1.print ab;
*)
  let res = of_linconstrs (Apron.Abstract1.to_lincons_array manpk ab) in
  Format.printf "widen_out: @[%a@]@]@ " pr res;
  res
