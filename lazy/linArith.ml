open ExtList
open ExtString
open Term

(** Linear arithmetic expressions *)

(** assume that all the expressions are canonized *)

(** {6 Functions on linear arithmetic expressions} *)

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

let coeff (nxs, _) x =
  Util.find_map
    (fun (n, y) -> if x = y then n else raise Not_found)
    nxs

let canonize (nxs, n) =
  List.filter (fun (n, _) -> n <> 0)
		  (List.map
		    (function ((n, x)::nxs) ->
		      (List.fold_left (+) n (List.map fst nxs), x)
		    | _ -> assert false)
		    (Util.classify (fun (_, x1) (_, x2) -> Var.equiv x1 x2) nxs)),
  n

let mul_coeff m (nxs, n) =
  canonize (List.map (fun (n, x) -> m * n, x) nxs, m * n)

let minus (nxs, n) = mul_coeff (-1) (nxs, n)

let add (nxs1, n1) (nxs2, n2) = canonize (nxs1 @ nxs2, n1 + n2)

let mul (nxs1, n1) (nxs2, n2) =
  if nxs1 = [] then
    mul_coeff n1 (nxs2, n2)
  else if nxs2 = [] then
    mul_coeff n2 (nxs1, n1)
  else
    invalid_arg "LinArith.mul"

let equiv (nxs1, n1) (nxs2, n2) =
  let xs1 = List.sort (List.map snd nxs1) in
  let xs2 = List.sort (List.map snd nxs2) in
  xs1 = xs2 &&
  (List.for_all (fun x -> coeff (nxs1, n1) x = coeff (nxs2, n2) x) xs1) &&
  n1 = n2

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
      (*let _ = Format.printf "%a@." Term.pr t in*)
      invalid_arg "LinArith.of_term"

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

let pos_neg_terms_of (nxs, n) =
  let nxs1, nxs2 = List.partition (fun (n, _) -> assert (n <> 0); n > 0) nxs in
  sum ((if n > 0 then [tint n] else []) @ List.map (fun (n, x) -> if n = 1 then make_var x else Term.mul (tint n) (make_var x)) nxs1),
  sum ((if n < 0 then [tint (-n)] else []) @ List.map (fun (n, x) -> if n = -1 then make_var x else Term.mul (tint (-n)) (make_var x)) nxs2)

(** {6 Functions on linear atomic integer formulas} *)

let aif_of t =
		match fun_args t with
		  Const(_, c), [t1; t2] when Const.is_ibrel c ->
		    let nxs, n = of_term (sub t1 t2) in
		    c, nxs, n
		| _ -> invalid_arg "LinArith.aif_of"

(** {6 Other functions} *)

let rec simplify t =
  match fun_args t with
    Var(attr, x), [] ->
      Var(attr, x)
  | Const(_, c), _ when Const.is_iexp c ->
      (try
        term_of (of_term t)
      with Invalid_argument _ ->
        t)
  | Const(attr, c), ts ->
      apply (Const(attr, c)) (List.map simplify ts)
  | _ ->
      let _ = Format.printf "not supported: %a@." Term.pr t in
      raise (Util.NotImplemented "LinArith.simplify")
