open ExtList
open ExtString
open Term

let csisat_true = CsisatAst.Application("true", [])
let csisat_false = CsisatAst.Application("false", [])

let rec csisat_of_iexp t =
  match fun_args t with
    Var(_, x), [] -> CsisatAst.Variable(Var.string_of x)
  | Const(_, c), args -> csisat_of_iexp_aux c args
  | _ -> assert false
and csisat_of_iexp_aux c args =
  match c, args with
    Const.Int(n), [] -> CsisatAst.Constant(float_of_int n)
  | Const.Add, [t1; t2] -> CsisatAstUtil.simplify_expr (CsisatAst.Sum([csisat_of_iexp t1; csisat_of_iexp t2]))
  | Const.Sub, [t1; t2] -> CsisatAstUtil.simplify_expr (CsisatAst.Sum([csisat_of_iexp t1; CsisatAst.Coeff(-1.0, csisat_of_iexp t2)]))
  | Const.Mul, [Const(_, Const.Int(n)); t]
  | Const.Mul, [t; Const(_, Const.Int(n))] -> CsisatAstUtil.simplify_expr (CsisatAst.Coeff(float_of_int n, csisat_of_iexp t))
  | Const.Minus, [t] -> CsisatAstUtil.simplify_expr (CsisatAst.Coeff(-1.0, csisat_of_iexp t))
  | Const.Unit, [] -> CsisatAst.Constant(0.0) (*???*)

let rec csisat_of_bexp t =
  match fun_args t with
    Var(_, x), [] -> CsisatAst.Eq(CsisatAst.Variable(Var.string_of x), csisat_true) (*???*)
  | Const(_, c), args -> CsisatAstUtil.integer_heuristic (csisat_of_bexp_aux c args)
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

let rec iexp_of s =
  match s with
    CsisatAst.Constant(f) ->
      make_int (int_of_float f)
  | CsisatAst.Variable(id) ->
      make_var2 (Var.parse id)
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


exception No_interpolant

let interpolate t1 t2 =
  let t1 = CsisatAstUtil.simplify (csisat_of_bexp t1) in
(*
Format.printf "%s@." (CsisatAstUtil.print_pred t1);
*)
  let t2 = CsisatAstUtil.simplify (csisat_of_bexp t2) in
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
  simplify (bexp_of (CsisatAstUtil.dnf interp))
