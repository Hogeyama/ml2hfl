open ExtList
open ExtString
open Term

let csisat_unit = CsisatAst.Application("unit", [])
let csisat_true = CsisatAst.Application("true", [])
let csisat_false = CsisatAst.Application("false", [])

let rec csisat_of_term t =
  match fun_args t with
    Var(_, x), [] -> CsisatAst.Variable(Var.string_of x)
  | Const(_, c), args -> csisat_of_term_aux c args
  | _ -> assert false
and csisat_of_term_aux c args =
  match c, args with
    Const.Int(n), [] -> CsisatAst.Constant(float_of_int n)
  | Const.Add, [t1; t2] -> CsisatAstUtil.simplify_expr (CsisatAst.Sum([csisat_of_term t1; csisat_of_term t2]))
  | Const.Sub, [t1; t2] -> CsisatAstUtil.simplify_expr (CsisatAst.Sum([csisat_of_term t1; CsisatAst.Coeff(-1.0, csisat_of_term t2)]))
  | Const.Mul, [Const(_, Const.Int(n)); t]
  | Const.Mul, [t; Const(_, Const.Int(n))] -> CsisatAstUtil.simplify_expr (CsisatAst.Coeff(float_of_int n, csisat_of_term t))
  | Const.Minus, [t] -> CsisatAstUtil.simplify_expr (CsisatAst.Coeff(-1.0, csisat_of_term t))
  | Const.Unit, [] -> csisat_unit
(*
  | Const.True, [] -> csisat_true
  | Const.False, [] -> csisat_false
*)
  | _ -> raise Not_found(*???*)(*assert false*)

let ih = ref true
let rec csisat_of_formula t =
  match fun_args t with
    Var(_, x), [] -> CsisatAst.Eq(CsisatAst.Variable(Var.string_of x), csisat_true)(*???*)
  | Const(_, c), args -> (if !ih then CsisatAstUtil.integer_heuristic else fun x -> x) (csisat_of_formula_aux c args)
  | _ -> assert false
and csisat_of_formula_aux c args =
  match c, args with
    Const.True, [] -> CsisatAst.True
  | Const.False, [] -> CsisatAst.False
  | Const.And, [t1; t2] -> CsisatAstUtil.simplify (CsisatAst.And([csisat_of_formula t1; csisat_of_formula t2]))
  | Const.Or, [t1; t2] -> CsisatAstUtil.simplify (CsisatAst.Or([csisat_of_formula t1; csisat_of_formula t2]))
  | Const.Imply, [t1; t2] -> CsisatAstUtil.simplify (CsisatAst.Or([CsisatAst.Not(csisat_of_formula t1); csisat_of_formula t2]))
  | Const.Iff, [t1; t2] ->
      CsisatAstUtil.simplify
        (CsisatAst.Or
            ([CsisatAst.And([csisat_of_formula t1; csisat_of_formula t2]);
              CsisatAst.And([CsisatAst.Not(csisat_of_formula t1); CsisatAst.Not(csisat_of_formula t2)])]))
  | Const.Not, [t] -> CsisatAst.Not(csisat_of_formula t)
  | Const.Lt, [t1; t2] -> CsisatAst.Lt(csisat_of_term t1, csisat_of_term t2)
  | Const.Gt, [t1; t2] -> CsisatAst.Lt(csisat_of_term t2, csisat_of_term t1)
  | Const.Leq, [t1; t2] -> CsisatAst.Leq(csisat_of_term t1, csisat_of_term t2)
  | Const.Geq, [t1; t2] -> CsisatAst.Leq(csisat_of_term t2, csisat_of_term t1)
(* t1, t2 may be boolean or integer *)
  | Const.Eq, [t1; t2] ->
      (* the following code is adhoc, should check the types of t1, t2 *)
      (try
        CsisatAst.Eq(csisat_of_term t1, csisat_of_term t2)
      with Not_found ->
        let t1 = csisat_of_formula t1 in
        let t2 = csisat_of_formula t2 in
        CsisatAstUtil.simplify
          (CsisatAst.Or([
            CsisatAst.And([t1; t2]);
            CsisatAst.And([CsisatAst.Not(t1); CsisatAst.Not(t2)])])))
  | Const.Neq, [t1; t2] ->
      (* the following code is adhoc, should check the types of t1, t2 *)
      (try
        CsisatAst.Not(CsisatAst.Eq(csisat_of_term t1, csisat_of_term t2))
      with Not_found ->
        let t1 = csisat_of_formula t1 in
        let t2 = csisat_of_formula t2 in
        CsisatAstUtil.simplify
          (CsisatAst.And([
            CsisatAst.Or([t1; t2]);
            CsisatAst.Or([CsisatAst.Not(t1); CsisatAst.Not(t2)])])))
  | _ -> assert false

let rec term_of s =
  match s with
    CsisatAst.Constant(f) ->
      tint (int_of_float f)
  | CsisatAst.Variable(id) ->
      make_var2 (Var.parse id)
  | CsisatAst.Application(_, _) ->
      if s = csisat_unit then
        tunit
      else if s = csisat_true then
        ttrue
      else if s = csisat_false then
        tfalse
      else
        assert false
  | CsisatAst.Sum(ss) ->
      sum (List.map term_of ss)
  | CsisatAst.Coeff(f, s) ->
      mul (tint (int_of_float f)) (term_of s)

let rec formula_of p =
  match p with
    CsisatAst.True -> ttrue
  | CsisatAst.False -> tfalse
  | CsisatAst.And(ps) -> band (List.map formula_of ps)
  | CsisatAst.Or(ps) -> bor (List.map formula_of ps)
  | CsisatAst.Not(p) -> bnot (formula_of p)
  | CsisatAst.Eq(s1, s2) -> eq (term_of s1) (term_of s2)
  | CsisatAst.Lt(s1, s2) -> lt (term_of s1) (term_of s2)
  | CsisatAst.Leq(s1, s2) -> leq (term_of s1) (term_of s2)
  | _ -> assert false

let satisfiable p =
  try
    let p = CsisatAstUtil.simplify p in
    if p = CsisatAst.True then true
    else if p = CsisatAst.False then false
    else if CsisatAstUtil.is_conj_only p then
     CsisatNelsonOppen.is_liuif_sat p
    else
     CsisatSatPL.is_sat p
  with _ ->
    assert false(*false*)

let implies t1 t2 =
  if Term.equiv t1 Term.tfalse then
    true
  else
    let oldih = !ih in
    let _ = ih := false in
    let p1 = CsisatAstUtil.simplify (csisat_of_formula t1) in
    let p2 = CsisatAstUtil.simplify (csisat_of_formula t2) in
    let _ = ih := oldih in
    (*
    Format.printf "@[<v>p1: %s@ p2: %s@ @]" (CsisatAstUtil.print_pred p1) (CsisatAstUtil.print_pred p2);
      *)
    not (satisfiable (CsisatAst.And([p1; CsisatAst.Not(p2)])))
    (*with CsisatAst.SAT_FORMULA _ ->
      false*)

let iff t1 t2 =
  if Term.equiv t1 t2 then
    true
  else
    implies t1 t2 &&
    implies t2 t1
  
exception No_interpolant

let interpolate t1 t2 =
  let p1 = CsisatAstUtil.simplify (csisat_of_formula t1) in
  let p2 = CsisatAstUtil.simplify (csisat_of_formula t2) in
  (*
  Format.printf "@[<v>p1: %s@ p2: %s@ @]" (CsisatAstUtil.print_pred p1) (CsisatAstUtil.print_pred p2);
  *)
  let interp =
    try
      CsisatInterpolate.interpolate_with_proof p1 p2
      (*
      if not (implies p1 it && implies it p2) then
        let _ = Format.printf "wrong interpolant=%a@," Fol.pr (invert it) in
        failwith "CsisatInterface.interpolate"
      *)
    with CsisatAst.SAT_FORMULA(_) ->
      raise No_interpolant
  in
  (*
  Format.printf "%s@." (CsisatAstUtil.print_pred interp);
  *)
  let interp = CsisatAstUtil.simplify (CsisatLIUtils.round_coeff interp) in
  simplify (formula_of (CsisatAstUtil.dnf interp))
