open ExtList
open ExtString
open Term

(** Linear arithmetic expressions *)

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
          else
            Format.fprintf ppf "%d %a" n Var.pr x
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
              Format.fprintf ppf "%a" Var.pr x)
				      nxs
    | _ -> ()
  in
  if n > 0 then
    let _ = if nxs <> [] then Format.fprintf ppf " + " in
    Format.fprintf ppf "%d" n
  else if n < 0 then
    let _ = if nxs <> [] then Format.fprintf ppf " - " in
    Format.fprintf ppf "%d" (-n)

let mul m nxs = List.map (fun (n, x) -> m * n, x) nxs

let minus nxs = mul (-1) nxs

let canonize nxs =
  let res = Util.classify (fun (_, x1) (_, x2) -> Var.equiv x1 x2) nxs in
  List.map
    (function ((n, x)::nxs) ->
      (List.fold_left (fun n1 n2 -> n1 + n2) n (List.map fst nxs), x)
    | _ -> assert false)
    res

let coeff nxs x = Util.find_map (fun (n, y) -> if x = y then n else raise Not_found) nxs

let equiv nxs1 nxs2 =
  let nxs1 = canonize nxs1 in
  let nxs2 = canonize nxs2 in
  let xs1 = List.sort (List.map snd nxs1) in
  let xs2 = List.sort (List.map snd nxs2) in
  xs1 = xs2 &&
  (List.for_all (fun x -> coeff nxs1 x = coeff nxs2 x) xs1)

let rec of_term t =
  match fun_args t with
    Var(_, x), [] ->
      [1, x], 0
  | Const(_, Const.Int(n)), [] ->
      [], n
  | Const(_, Const.Add), [t1; t2] ->
      let nxs1, n1 = of_term t1 in
      let nxs2, n2 = of_term t2 in
      canonize (nxs1 @ nxs2), n1 + n2
  | Const(_, Const.Sub), [t1; t2] ->
      let nxs1, n1 = of_term t1 in
      let nxs2, n2 = of_term t2 in
      let nxs2, n2 =  minus nxs2, -n2 in
      canonize (nxs1 @ nxs2), n1 + n2
  | Const(_, Const.Mul), [t1; t2] ->
      let nxs1, n1 = of_term t1 in
      let nxs2, n2 = of_term t2 in
      if nxs1 = [] then
        mul n1 nxs2, n1 * n2
      else if nxs2 = [] then
        mul n2 nxs1, n2 * n1
      else
        invalid_arg "LinArith.of_term"
  | Const(_, Const.Minus), [t] ->
      let nxs, n = of_term t in
      minus nxs, -n
(*
  | Const(_, Const.Unit), [] ->
      [], 0
*)
  | _ ->
      let _ = Format.printf "%a@." Term.pr t in
      assert false

let term_of (nxs, n) =
  let ts =
    (if n = 0 then [] else [tint n]) @
    (List.filter_map
      (fun (n, x) ->
        if n = 0 then
          None
        else if n = 1 then
          Some(make_var x)
        else
          Some(Term.mul (tint n) (make_var x)))
      nxs)
  in
  sum ts

let pos_neg_terms_of (nxs, n) =
  let nxs =
    List.filter_map
      (fun (n, x) ->
        if n = 0 then
          None
        else
          Some(n, x))
      nxs
  in
  let nxs1, nxs2 = List.partition (fun (n, _) -> n > 0) nxs in
  sum ((if n > 0 then [tint n] else []) @ List.map (fun (n, x) -> if n = 1 then make_var x else Term.mul (tint n) (make_var x)) nxs1),
  sum ((if n < 0 then [tint (-n)] else []) @ List.map (fun (n, x) -> if n = -1 then make_var x else Term.mul (tint (-n)) (make_var x)) nxs2)

(** {6 Functions on linear atomic integer formulas} *)

let aif_of t =
		match fun_args t with
		  Const(_, c), [t1; t2] when Const.is_ibin c ->
		    let nxs, n = of_term (sub t1 t2) in
		    c, nxs, n
		| _ -> invalid_arg "LinArith.aif_of"

(** let aif use only geq *)
let canonize_aif (c, nxs, n) =
  match c with
    Const.Lt ->
      minus nxs, -(n + 1)
  | Const.Gt ->
      nxs, n - 1
  | Const.Leq ->
      minus nxs, -n
  | Const.Geq ->
      nxs, n
  | _ -> assert false

let pr_caif ppf (nxs, n) =
  Format.fprintf ppf "%a >= 0" pr (nxs, n)

(** {6 Other functions} *)

let rec simplify t =
  match fun_args t with
    Var(attr, x), [] ->
      Var(attr, x)
  | Const(attr, c), _ when Const.is_iexp c ->
     (try
       term_of (of_term t)
     with Invalid_argument _ -> t)
  | Const(attr, c), ts ->
      apply (Const(attr, c)) (List.map simplify ts)
  | _ -> let _ = Format.printf "not supported: %a@." Term.pr t in raise (Util.NotImplemented "LinArith.simplify")
