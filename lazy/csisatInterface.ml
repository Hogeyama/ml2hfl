open ExtList
open ExtString
open Term
open Formula

(** Interface to CSIsat *)

let csisat_unit = CsisatAst.Application("unit", [])
let csisat_true = CsisatAst.Application("true", [])
let csisat_false = CsisatAst.Application("false", [])

let rec of_term t =
  match fun_args t with
    Var(_, x), [] -> CsisatAst.Variable(Var.string_of x)
  | Const(_, c), args -> of_term_aux c args
  | _ -> assert false
and of_term_aux c args =
  match c, args with
    Const.Unit, [] -> csisat_unit
  | Const.True, [] -> csisat_true
  | Const.False, [] -> csisat_false
  | Const.Int(n), [] -> CsisatAst.Constant(float_of_int n)
  | Const.Add, [t1; t2] -> CsisatAstUtil.simplify_expr (CsisatAst.Sum([of_term t1; of_term t2]))
  | Const.Sub, [t1; t2] -> CsisatAstUtil.simplify_expr (CsisatAst.Sum([of_term t1; CsisatAst.Coeff(-1.0, of_term t2)]))
  | Const.Mul, [Const(_, Const.Int(n)); t]
  | Const.Mul, [t; Const(_, Const.Int(n))] -> CsisatAstUtil.simplify_expr (CsisatAst.Coeff(float_of_int n, of_term t))
  | Const.Minus, [t] -> CsisatAstUtil.simplify_expr (CsisatAst.Coeff(-1.0, of_term t))
  | _ ->
      let _ = Format.printf "%a %a@." Const.pr c (Util.pr_list Term.pr " ") args in
      assert false

let ih = ref true
let rec of_formula t =
  match fun_args t with
    Var(_, x), [] -> CsisatAst.Eq(CsisatAst.Variable(Var.string_of x), csisat_true)(*???*)
  | Const(_, c), args -> (if !ih then CsisatAstUtil.integer_heuristic else fun x -> x) (of_formula_aux c args)
  | _ -> assert false
and of_formula_aux c args =
  match c, args with
    Const.True, [] -> CsisatAst.True
  | Const.False, [] -> CsisatAst.False
  | Const.And, [t1; t2] -> CsisatAstUtil.simplify (CsisatAst.And([of_formula t1; of_formula t2]))
  | Const.Or, [t1; t2] -> CsisatAstUtil.simplify (CsisatAst.Or([of_formula t1; of_formula t2]))
  | Const.Imply, [t1; t2] -> CsisatAstUtil.simplify (CsisatAst.Or([CsisatAst.Not(of_formula t1); of_formula t2]))
  | Const.Iff, [t1; t2] ->
      CsisatAstUtil.simplify
        (CsisatAst.Or
            ([CsisatAst.And([of_formula t1; of_formula t2]);
              CsisatAst.And([CsisatAst.Not(of_formula t1); CsisatAst.Not(of_formula t2)])]))
  | Const.Not, [t] -> CsisatAst.Not(of_formula t)
  | Const.Lt, [t1; t2] -> CsisatAst.Lt(of_term t1, of_term t2)
  | Const.Gt, [t1; t2] -> CsisatAst.Lt(of_term t2, of_term t1)
  | Const.Leq, [t1; t2] -> CsisatAst.Leq(of_term t1, of_term t2)
  | Const.Geq, [t1; t2] -> CsisatAst.Leq(of_term t2, of_term t1)
  | Const.EqUnit, [t1; t2] ->
      CsisatAst.Eq(of_term t1, of_term t2)
  | Const.NeqUnit, [t1; t2] ->
      CsisatAst.Not(CsisatAst.Eq(of_term t1, of_term t2))
  | Const.EqBool, [t1; t2] ->
      let t1 = of_formula t1 in
      let t2 = of_formula t2 in
      CsisatAstUtil.simplify
        (CsisatAst.Or([
          CsisatAst.And([t1; t2]);
          CsisatAst.And([CsisatAst.Not(t1); CsisatAst.Not(t2)])]))
  | Const.NeqBool, [t1; t2] ->
      let t1 = of_formula t1 in
      let t2 = of_formula t2 in
      CsisatAstUtil.simplify
        (CsisatAst.And([
          CsisatAst.Or([t1; t2]);
          CsisatAst.Or([CsisatAst.Not(t1); CsisatAst.Not(t2)])]))
  | Const.EqInt, [t1; t2] ->
      CsisatAst.Eq(of_term t1, of_term t2)
  | Const.NeqInt, [t1; t2] ->
      CsisatAst.Not(CsisatAst.Eq(of_term t1, of_term t2))
  | _ ->
      let _ = Format.printf "%a@." Const.pr c in
      assert false

let rec term_of s =
  match s with
    CsisatAst.Constant(f) ->
      tint (int_of_float f), SimType.Int
  | CsisatAst.Variable(id) ->
      make_var (Var.parse id), SimType.Int(*???*)
  | CsisatAst.Application(_, _) ->
      if s = csisat_unit then
        tunit, SimType.Unit
      else if s = csisat_true then
        ttrue, SimType.Bool
      else if s = csisat_false then
        tfalse, SimType.Bool
      else
        assert false
  | CsisatAst.Sum(ss) ->
      sum (List.map (fun s -> let t, ty = term_of s in assert (ty = SimType.Int); t) ss), SimType.Int
  | CsisatAst.Coeff(f, s) ->
      mul (tint (int_of_float f)) (let t, ty = term_of s in assert (ty = SimType.Int); t), SimType.Int

let rec formula_of p =
  match p with
    CsisatAst.True -> ttrue
  | CsisatAst.False -> tfalse
  | CsisatAst.And(ps) -> band (List.map formula_of ps)
  | CsisatAst.Or(ps) -> bor (List.map formula_of ps)
  | CsisatAst.Not(p) -> bnot (formula_of p)
  | CsisatAst.Eq(s1, s2) ->
      let t1, ty1 = term_of s1 in
      let t2, ty2 = term_of s2 in
      if ty1 = SimType.Unit ||(*???*) ty2 = SimType.Unit then
        eqUnit t1 t2
      else if ty1 = SimType.Bool ||(*???*) ty2 = SimType.Bool then
        eqBool t1 t2
      else if ty1 = SimType.Int && ty2 = SimType.Int then
        eqInt t1 t2
      else
        assert false
  | CsisatAst.Lt(s1, s2) ->
      let t1, ty1 = term_of s1 in
      let t2, ty2 = term_of s2 in
      let _ = assert (ty1 = SimType.Int && ty2 = SimType.Int) in
      lt t1 t2
  | CsisatAst.Leq(s1, s2) ->
      let t1, ty1 = term_of s1 in
      let t2, ty2 = term_of s2 in
      let _ = assert (ty1 = SimType.Int && ty2 = SimType.Int) in
      leq t1 t2
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
  if Term.equiv t1 tfalse then
    true
  else
    let oldih = !ih in
    let _ = ih := false in
    let p1 = CsisatAstUtil.simplify (of_formula t1) in
    let p2 = CsisatAstUtil.simplify (of_formula t2) in
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
  if Cvc3Interface.is_valid (bnot t1) then
    tfalse (*???*)
  else if Cvc3Interface.is_valid (bnot t2) then
    ttrue (*???*)
  else
		  let p1 = CsisatAstUtil.simplify (of_formula t1) in
		  let p2 = CsisatAstUtil.simplify (of_formula t2) in
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
		    with CsisatAst.SAT | CsisatAst.SAT_FORMULA(_) ->
		      raise No_interpolant
		    | Failure(msg) ->
		      let _ = Format.printf "csisat error: %s@." msg in
		      assert false(*raise No_interpolant*)
		  in
		  (*Format.printf "%s@." (CsisatAstUtil.print_pred interp);*)
		  let interp = CsisatAstUtil.simplify (CsisatLIUtils.round_coeff interp) in
		  let interp = CsisatAstUtil.dnf interp in
		  (*Format.printf "%s@." (CsisatAstUtil.print_pred interp);*)
		  (*Formula.simplify*) (formula_of interp)

let interpolate_chk t1 t2 =
  try
    let t = interpolate t1 t2 in
    (*let _ = Format.printf "interp: %a@." Term.pr t in*)
    let t = Formula.simplify t in
    if true then
      let ts = Formula.conjuncts t in
		    (match ts with
		      [t] -> t
		    | _ ->
          let _ = if !Global.debug > 0 then Format.printf "finding minimal interpolant@." in
          let _ = if !Global.debug > 0 then Format.printf "before:@.  @[%a@]@." (Util.pr_list Term.pr ", ") ts in
          let ts = Util.minimal (fun ts -> Cvc3Interface.implies (Formula.band ts) (Formula.bnot t2)) ts in
          let _ = if !Global.debug > 0 then Format.printf "after:@.  @[%a@]@." (Util.pr_list Term.pr ", ") ts in
		        Formula.band ts)
    else
      t
  with No_interpolant ->
    if !Global.debug > 0 && Cvc3Interface.implies t1 (Formula.bnot t2) then
      let _ = Format.printf "an error of CSIsat@." in
      assert false
    else
      raise No_interpolant

let interpolate t1 t2 =
  let _ = if !Global.debug > 1 then Format.printf "@[<v>interp_in1: %a@,interp_in2: %a@," Term.pr t1 Term.pr t2 in
  let interp = interpolate_chk t1 t2 in
  let _ = if !Global.debug > 1 then Format.printf "interp_out: %a@]@." Term.pr interp in
  interp

(** @param p represents variables shared by t1 and t2 *)
let interpolate_bvs p t1 t2 =
  let t1 = simplify (band (conjuncts t1)) in
  let t2 = simplify (band (conjuncts t2)) in
  let t1 = Term.rename_fresh p t1 in
  let t2 = Term.rename_fresh p t2 in
  interpolate t1 t2
