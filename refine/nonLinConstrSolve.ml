open ExtList
open ExtString

let pr_coeffs ppf coeffs =
  let pr_aux ppf (c, n) = Format.fprintf ppf "%a = %d" Var.pr c n in
  Format.fprintf ppf "@[<v>%a@]" (Util.pr_list pr_aux "@,") coeffs

let gen_template xtys =
  let terms =
    Term.make_var (Var.make_coeff (Idnt.new_id ())) ::
    List.filter_map
      (fun (x, ty) ->
        if ty = SimType.Int then
          Some(Term.mul (Term.make_var (Var.make_coeff (Idnt.new_id ()))) (Term.make_var x))
        else
          None)
      xtys
  in
  Formula.geq (Term.sum terms) (Term.tint 0)

(** generate (possibly existentially quantified) constrants on coefficients
    @param t coefficients are existentially quantified and variables are universally quantified *)
let gen_coeff_constrs t =
  let _ = Global.log_begin "gen_coeff_constrs" in
  let ts = Farkas.farkas t in
  let ts =
    List.map
      (fun t ->
        let _ = Global.log (fun () -> Format.printf "new constraint on coefficients:@,  @[<v>%a@]@," Term.pr t) in
        if !Global.use_bit_vector then
          t
        else
          let t =
            try
              let t = FormulaUtil.simplify (FormulaUtil.linearize t) in
              let _ = Format.printf "linearized constraint on coefficients:@,  @[<v>%a@]@," Term.pr t in
              t
            with Invalid_argument _ ->
              t
          in
          let qft =
            if FormulaUtil.is_linear t then
              FormulaUtil.simplify (AtpInterface.integer_qelim t)
            else
              FormulaUtil.simplify (AtpInterface.real_qelim t)
          in
          let _ = Format.printf "quantifier eliminated constraint on coefficients:@,  @[<v>%a@]@," Term.pr qft in
          qft)
      ts
  in
  let _ = Global.log_end "gen_coeff_constrs" in
  ts

let solve_bv_aux t =
  let ibs = List.init !Global.bits_threshold (fun i -> i + 1, true) @ List.init (!Global.bits_threshold - 1(*???*)) (fun i -> i + 1, false) in
  try
    Util.find_app (fun (bit, only_pos) -> try Cvc3Interface.solve_bv only_pos bit t with Cvc3Interface.Unknown -> raise Not_found) ibs
  with Not_found ->
    raise Cvc3Interface.Unknown

exception Unknown

let solve_bv mps t =
  let _ = Global.log_begin "NonLinConstrSolve.solve_bv" in
  let sol =
    try
      if !Global.disable_parameter_inference_heuristics then
        raise Cvc3Interface.Unknown
      else
        let masked_params = Util.inter mps (Term.coeffs t) in
        if masked_params = [] then
          raise Cvc3Interface.Unknown
        else
          let _ = Global.log (fun () -> Format.printf "masked_params: %a@," Var.pr_list masked_params) in
          let coeffs = List.map (fun c -> c, 0) masked_params in
          let t' = FormulaUtil.simplify (TypSubst.subst (fun x -> Term.tint (List.assoc x coeffs)) t) in
          coeffs @ solve_bv_aux t'
    with Cvc3Interface.Unknown ->
      solve_bv_aux t
  in
  let _ = Global.log_end "NonLinConstrSolve.solve_bv" in
  sol

let solve_constrs mps old_sol t =
  let _ = Global.log_begin "NonLinConstrSolve.solve_constrs" in
  if not !Global.use_bit_vector then
    let sol = List.filter (fun (c, _) -> Var.is_coeff c) (Cvc3Interface.solve t) in
    let _ = Global.log_end "NonLinConstrSolve.solve_constrs" in
    sol
  else
    let changed = ref false in
    let t' =
      FormulaUtil.simplify
        (TypSubst.subst
          (fun x ->
            let n = List.assoc x old_sol in
            if n = 0 then
              raise Not_found
            else (* reuse old solution if possible *)
              let _ = changed := true in
              Term.tint (n))
          t)
    in
    let _ = Global.log (fun () -> Format.printf "solving a constraint on coefficients (reusing old solution):@,  @[<v>%a@]@," Term.pr t') in
    try
      List.filter (fun (c, _) -> Var.is_coeff c) (solve_bv mps t')
    with Cvc3Interface.Unknown ->
      if not !changed then
        let _ = Global.log_end "NonLinConstrSolve.solve_constrs" in
        raise Unknown
      else
        if true then
          let _ = Global.log_end "NonLinConstrSolve.solve_constrs" in
          raise Unknown
        else
          (*a-test-update fails*)
          let _ = Global.log (fun () -> Format.printf "solving a constraint on coefficients:@,  @[<v>%a@]@," Term.pr t) in
          try
            let _ = Global.log_end "NonLinConstrSolve.solve_constrs" in
            List.filter (fun (c, _) -> Var.is_coeff c) (solve_bv mps t)
          with Cvc3Interface.Unknown ->
            let _ = Global.log_end "NonLinConstrSolve.solve_constrs" in
            raise Unknown
