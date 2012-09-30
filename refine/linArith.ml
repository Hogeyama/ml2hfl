open ExtList
open ExtString
open Term

(** Linear arithmetic expressions *)

(** @invariant all the expressions are normalized *)

(** {6 Printers} *)

let pr ppf (nxs, n) =
  let _ =
    match nxs with
      (n, x)::nxs ->
        let _ =
          if n = 1 then
            Format.fprintf ppf "%a" Var.pr x
          else if n = -1 then
            Format.fprintf ppf "-%a" Var.pr x
          else if n <> 0 then
            Format.fprintf ppf "%d %a" n Var.pr x
          else
            assert false
        in
        List.iter
          (fun (n, x) ->
            if n > 0 then
              let _ = Format.fprintf ppf " + " in
              let _ = if n <> 1 then Format.fprintf ppf "%d " n in
              Format.fprintf ppf "%a" Var.pr x
            else if n < 0 then
              let _ = Format.fprintf ppf " - " in
              let _ = if -n <> 1 then Format.fprintf ppf "%d " (-n) in
              Format.fprintf ppf "%a" Var.pr x
            else
              assert false)
          nxs
    | _ -> ()
  in
  if n > 0 then
    let _ = if nxs <> [] then Format.fprintf ppf " + " in
    Format.fprintf ppf "%d" n
  else if n < 0 then
    let _ = if nxs <> [] then Format.fprintf ppf " - " in
    Format.fprintf ppf "%d" (-n)
  else
    assert false

(** {6 Destructors} *)

let normalize (nxs, n) =
  List.filter (fun (n, _) -> n <> 0)
    (List.map
      (function ((n, x) :: nxs) ->
        (List.fold_left (+) n (List.map fst nxs), x)
      | _ -> assert false)
      (Util.classify (fun (_, x1) (_, x2) -> Var.equiv x1 x2) nxs)),
  n

let coeff (nxs, _) x =
  Util.find_app
    (fun (n, y) -> if x = y then Some(n) else None)
    nxs

let mul_coeff m (nxs, n) =
  normalize (List.map (fun (n, x) -> m * n, x) nxs, m * n)

let minus (nxs, n) = mul_coeff (-1) (nxs, n)

let add (nxs1, n1) (nxs2, n2) = normalize (nxs1 @ nxs2, n1 + n2)

let mul (nxs1, n1) (nxs2, n2) =
  if nxs1 = [] then
    mul_coeff n1 (nxs2, n2)
  else if nxs2 = [] then
    mul_coeff n2 (nxs1, n1)
  else
    invalid_arg "LinArith.mul"

let term_of (nxs, n) =
  let ts =
    (if n = 0 then [] else [tint n]) @
    (List.map
      (fun (n, x) ->
        if n = 0 then
          assert false
        else if n = 1 then
          make_var x
        else
          Term.mul (tint n) (make_var x))
      nxs)
  in
  sum ts

(** {6 Constructors} *)

let rec of_term t =
  match fun_args t with
    Var(_, x), [] ->
      [1, x], 0
  | Const(_, Const.Int(n)), [] ->
      [], n
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
      invalid_arg "LinArith.of_term"

(** {6 Basic functions} *)

let equiv (nxs1, n1) (nxs2, n2) =
  let xs1 = List.sort (List.map snd nxs1) in
  let xs2 = List.sort (List.map snd nxs2) in
  xs1 = xs2 &&
  (List.for_all (fun x -> coeff (nxs1, n1) x = coeff (nxs2, n2) x) xs1) &&
  n1 = n2

let rec simplify t =
  match fun_args t with
    Var(attr, x), [] ->
      Var(attr, x)
  | Const(_, c), _ when Const.is_int c ->
      (try
        term_of (of_term t)
      with Invalid_argument _ ->
        t)
  | Const(attr, c), ts ->
      apply (Const(attr, c)) (List.map simplify ts)
  | _ ->
      let _ = Format.printf "not supported: %a@," Term.pr t in
      raise (Util.NotImplemented "LinArith.simplify")

(** {6 Functions on linear atomic integer formulas} *)

let div_gcd_aif (c, nxs, n) =
  let m = Util.gcd (abs n :: (List.map (fun (n, _) -> abs n) nxs)) in
  let _ = assert (m <> 0) in
  c, List.map (fun (n, x) -> n / m, x) nxs, n / m

let aif_of t =
  match fun_args t with
    Const(_, c), [t1; t2] when Const.is_ibrel c ->
      let nxs, n = of_term (sub t1 t2) in
      div_gcd_aif (c, nxs, n)
  | _ -> invalid_arg "LinArith.aif_of"

(** @ensure the result does not contain Const.Minus, Const.Sub,
            and negative integer constants *)
let term_of_aif (c, nxs, n) =
  if c = Const.IBTrue then
    Const([], Const.True)
  else if c = Const.IBFalse then
    Const([], Const.False)
  else
    let _ = if !Global.debug then assert (Const.is_ibrel c) in
    if nxs = [] then
      if Const.lift_ibrel c n 0 then
        Const([], Const.True)
      else
        Const([], Const.False)
    else
      let nxs1, nxs2 =
        List.partition
          (fun (n, _) ->
            let _ = if !Global.debug then assert (n <> 0) in
            n > 0)
          nxs
      in
      let n1, n2 = if n > 0 then n, 0 else if n < 0 then 0, n else 0, 0 in
      let tp = term_of (nxs1, n1) in
      let tm = term_of (minus (nxs2, n2)) in
      bop c tp tm

(*
let term_of_aif (c, nxs, n) =
  match c with
    Const.EqInt ->
      eqInt (term_of (nxs, n)) (tint 0)
  | Const.NeqInt ->
      neqInt (term_of (nxs, n)) (tint 0)
  | Const.Lt ->
      lt (term_of (nxs, n)) (tint 0)
  | Const.Gt ->
      gt (term_of (nxs, n)) (tint 0)
  | Const.Leq ->
      leq (term_of (nxs, n)) (tint 0)
  | Const.Geq ->
      geq (term_of (nxs, n)) (tint 0)
*)

let simplify_conjuncts_aifs aifs =
  let aifss =
    Util.classify
      (fun (_, nxs1, n1) (_, nxs2, n2) ->
        equiv (nxs1, 0) (nxs2, 0) ||
        equiv (nxs1, 0) (minus (nxs2, 0)))
      aifs
  in
  Util.concat_map
    (fun ((c1, nxs1, n1) :: aifs) ->
      let cns =
        (c1, -n1) ::
        List.map
          (fun (c2, nxs2, n2) ->
            if equiv (nxs1, 0) (nxs2, 0) then
              c2, -n2
            else
              Const.minus_ibrel c2, n2)
          aifs
      in
      let cns = Const.candns cns in
      List.map (fun (c, n) -> c, nxs1, -n) cns)
    aifss

let simplify_disjuncts_aifs aifs =
  let aifss =
    Util.classify
      (fun (_, nxs1, n1) (_, nxs2, n2) ->
        equiv (nxs1, 0) (nxs2, 0) ||
        equiv (nxs1, 0) (minus (nxs2, 0)))
      aifs
  in
  Util.concat_map
    (fun ((c1, nxs1, n1) :: aifs) ->
      let cns =
        (c1, -n1) ::
        List.map
          (fun (c2, nxs2, n2) ->
            if equiv (nxs1, 0) (nxs2, 0) then
              c2, -n2
            else
              Const.minus_ibrel c2, n2)
          aifs
      in
      let cns = Const.corns cns in
      List.map (fun (c, n) -> c, nxs1, -n) cns)
    aifss

(** {6 Utility functions} *)

(** @return the set of free variables in t that may occur in a non-linear integer expression *)
let rec nlfvs t =
  match fun_args t with
    Var(_, _), [] -> []
  | Const(_, Const.Mul), [_; _] ->
      (try
        let _ = of_term t in
        []
      with Invalid_argument _ ->
        fvs t(**@todo*))
  | Const(_, Const.Div), [t1; t2] | Const(_, Const.Mod), [t1; t2] ->
      assert false
  | Const(_, c), ts ->
      Util.concat_map nlfvs ts
  | Call(_, _, _), [] | Ret(_, _, _, _), [] | Error(_), [] ->
      assert false
  | Forall(_, env, t), [] | Exists(_, env, t), [] ->
      Util.diff (nlfvs t) (List.map fst env)
