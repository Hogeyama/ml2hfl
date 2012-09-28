open ExtList
open ExtString
open Term

(** Parametric-linear arithmetic expressions *)

(** assume that all the expressions are normalized *)

(** {6 Functions on parametric-linear arithmetic expressions} *)

let pr ppf (nxs, n) =
  let _ =
    match nxs with
      [] -> ()
    | (n, x) :: nxs ->
        let _ =
          if eq_int n 1 then
            Format.fprintf ppf "%a" Var.pr x
          else if eq_int n (-1) then
            Format.fprintf ppf "-%a" Var.pr x
          else if eq_int n 0 then
            assert false
          else
            Format.fprintf ppf "%a %a" Term.pr n Var.pr x
        in
        List.iter
          (fun (n, x) ->
            if gt_int n 0 then
              let _ = Format.fprintf ppf " + " in
              let _ = if int_const_of n <> 1 then Format.fprintf ppf "%d " (int_const_of n) in
              Format.fprintf ppf "%a" Var.pr x
            else if lt_int n 0 then
              let _ = Format.fprintf ppf " - " in
              let _ = if -(int_const_of n) <> 1 then Format.fprintf ppf "%d " (-(int_const_of n)) in
              Format.fprintf ppf "%a" Var.pr x
            else if eq_int n 0 then
              assert false
            else
              Format.fprintf ppf " + %a %a" Term.pr n Var.pr x)
          nxs
  in
  if gt_int n 0 then
    let _ = if nxs <> [] then Format.fprintf ppf " + " in
    Format.fprintf ppf "%d" (int_const_of n)
  else if lt_int n 0 then
    let _ = if nxs <> [] then Format.fprintf ppf " - " in
    Format.fprintf ppf "%d" (-(int_const_of n))
  else if eq_int n 0 then
    ()
  else
    let _ = if nxs <> [] then Format.fprintf ppf " + " in
    Format.fprintf ppf "%a" Term.pr n

let is_zero (nxs, n) =
  nxs = [] && eq_int n 0

let coeff (nxs, n) x =
  Util.find_app
    (fun (n, y) -> if x = y then n else raise Not_found)
    nxs

let normalize (nxs, n) =
  let res = Util.classify (fun (_, x1) (_, x2) -> Var.equiv x1 x2) nxs in
  let nxs =
    List.map
      (function ((n, x) :: nxs) ->
        let n =
          Poly.simplify
            (List.fold_left
              Term.add
              n
              (List.map fst nxs))
        in
        n, x
      | _ -> assert false)
      res
  in
  let nxs =
    List.filter (fun (n, _) -> not (eq_int n 0)) nxs
  in
  nxs, Poly.simplify n

let mul_coeff m (nxs, n) =
  normalize
    (List.map (fun (n, x) -> Term.mul m n, x) nxs,
    Term.mul m n)

let minus (nxs, n) = mul_coeff (tint (-1)) (nxs, n)

let add (nxs1, n1) (nxs2, n2) = normalize (nxs1 @ nxs2, Term.add n1 n2)

let mul (nxs1, n1) (nxs2, n2) =
  if nxs1 = [] then
    mul_coeff n1 (nxs2, n2)
  else if nxs2 = [] then
    mul_coeff n2 (nxs1, n1)
  else
    invalid_arg "ParLinArith.mul"

let equiv (nxs1, n1) (nxs2, n2) =
  let xs1 = List.map snd nxs1 in
  let xs2 = List.map snd nxs2 in
  Util.set_equiv xs1 xs2 &&
  (List.for_all (fun x -> Term.equiv (coeff (nxs1, n1) x) (coeff (nxs2, n2) x)) xs1) &&
  Term.equiv n1 n2

let rec of_term t =
  match fun_args t with
    Var(_, x), [] ->
      if Var.is_coeff x then
        [], make_var x
      else
        [tint 1, x], tint 0
  | Const(_, Const.Int(n)), [] ->
      [], tint n
  | Const(_, Const.Add), [t1; t2] ->
      add (of_term t1) (of_term t2)
  | Const(_, Const.Sub), [t1; t2] ->
      add (of_term t1) (minus (of_term t2))
  | Const(_, Const.Mul), [t1; t2] ->
      mul (of_term t1) (of_term t2)
  | Const(_, Const.Minus), [t] ->
      minus (of_term t)
  | _ ->
      (*let _ = Format.printf "%a@," Term.pr t in*)
      invalid_arg "ParLinArith.of_term"

let term_of (nxs, n) =
  let ts =
    (if eq_int n 0 then [] else [n]) @
    (List.map
      (fun (n, x) ->
        if eq_int n 0 then
          assert false
        else if eq_int n 1 then
          make_var x
        else
          Term.mul n (make_var x))
      nxs)
  in
  sum ts

(** @require not (is_zero (nxs, n))
    @ensure the result does not contain tint 1 *)
let factorize (nxs, n) =
  try
    let nxs = (n, None) :: List.map (fun (n, x) -> n, Some(x)) nxs in
    let polxs = List.map (fun (n, xopt) -> Poly.of_term n, xopt) nxs in
    let cpolxs = List.map (fun (pol, xopt) -> Poly.gcd_coeff pol, xopt) polxs in
    let pol = snd (fst (List.hd cpolxs)) in
    let cxs =
      List.map
        (fun ((c, pol'), xopt) ->
          if Poly.equiv pol pol' then
            c, xopt
          else if Poly.equiv pol (Poly.minus pol') then
            -c, xopt
          else
            raise Not_found (*give up*))
        cpolxs
    in
    let d = Util.gcd (List.map (fun (c, _) -> abs c) cxs) in
    let cxs = List.map (fun (c, xopt) -> c / d, xopt) cxs in
    let nxs, ns =
      Util.partition_map
        (function (c, None) -> `R(c) | (c, Some(x)) -> `L(c, x))
        cxs
    in
    let t = LinArith.term_of (nxs, List.fold_left (+) 0 ns) in
    (if d = 1 then [] else [tint d]) @ Poly.factorize pol @ (if eq_int t 1 then [] else [t])
  with Not_found ->
    Poly.factorize (Poly.of_term (term_of (nxs, n)))

(** {6 Functions on atomic integer formulas} *)

let pr_aif ppf (c, nxs, n) =
  Format.fprintf ppf
    "%a %s 0"
    pr (nxs, n)
    (match c with
      Const.EqInt ->
        "="
    | Const.NeqInt ->
        "<>"
    | Const.Lt ->
        "<"
    | Const.Gt ->
        ">"
    | Const.Leq ->
        "<="
    | Const.Geq ->
        ">="
    | _ -> assert false)

let aif_of t =
  match fun_args t with
    Const(_, c), [t1; t2] when Const.is_ibrel c ->
      let nxs, n = of_term (sub t1 t2) in
      c, nxs, n
  | _ ->
      (*let _ = Format.printf "%a@," Term.pr t in*)
      invalid_arg "ParLinArith.aif_of"

(** the result only uses geq and eq
    require: c is not <> *)
let normalize_aif (c, nxs, n) =
  match c with
    Const.EqInt ->
      Const.EqInt, nxs, n
  | Const.NeqInt ->
      assert false
  | Const.Lt ->
      let nxs, n = minus (nxs, Term.add n (tint 1)) in
      Const.Geq, nxs, n
  | Const.Gt ->
      Const.Geq, nxs, (Poly.simplify (Term.sub n (tint 1)))
  | Const.Leq ->
      let nxs, n = minus (nxs, n) in
      Const.Geq, nxs, n
  | Const.Geq ->
      Const.Geq, nxs, n
  | _ -> assert false

(** @ensure the result does not contain Const.Minus, Const.Sub,
            and negative integer constants *)
let term_of_aif (c, nxs, n) =
  if nxs = [] && is_int_const n then
    if Const.lift_ibrel c (int_const_of n) 0 then
      Const([], Const.True)
    else
      Const([], Const.False)
  else
    let nxs =
      List.filter_map
        (fun (n, x) ->
          if eq_int n 0 then
            None
          else
            Some(n, x))
        nxs
    in
    let nxs1, nxs2 =
      List.partition
        (fun (n, _) -> not (is_int_const n) || gt_int n 0)
        nxs
    in
    let n1, n2 = if not (is_int_const n) || gt_int n 0 then n, tint 0 else if lt_int n 0 then tint 0, n else tint 0, tint 0 in
    let tp = term_of (nxs1, n1) in
    let tm = term_of (minus (nxs2, n2)) in
    bop c tp tm

(** @todo refactoring *)
let xtty_of_aif p dom (c, nxs, n) =
  if c = Const.EqInt then
    let nxs1, (n', x), nxs2 =
      Util.pick
        (fun (n, x) ->
          not (p x) &&
          (Term.equiv n (tint 1) || Term.equiv n (tint (-1))))
        nxs
    in
    let t =
      if int_const_of n' = 1 then
        term_of (minus (nxs1 @ nxs2, n))
      else if int_const_of n' = -1 then
        term_of (nxs1 @ nxs2, n)
      else
        assert false
    in
    (* @todo check whether substitution is acyclic instead *)
    if Util.inter dom (fvs t) = [] then
      x, t, SimType.Int
    else
      raise Not_found
  else
    raise Not_found
