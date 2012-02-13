open ExtList
open ExtString
open Term

(** Non-linear arithmetic expressions *)

(** {6 Functions on non-linear arithmetic expressions} *)

let pr ppf (nxs, n) =
  let _ =
    match nxs with
      (n, x)::nxs ->
        let _ =
          if equiv n (tint 1) then
            Format.fprintf ppf "%a" Var.pr x
          else if equiv n (tint (-1)) then
            Format.fprintf ppf "-%a" Var.pr x
          else
            Format.fprintf ppf "%a %a" Term.pr n Var.pr x
        in
				    List.iter
				      (fun (n, x) ->
            try
		            let n = int_of n in
		            if n > 0 then
		  				        let _ = Format.fprintf ppf " + " in
		              let _ = if n <> 1 then Format.fprintf ppf "%d " n in
		              Format.fprintf ppf "%a" Var.pr x
		            else if n < 0 then
		  				        let _ = Format.fprintf ppf " - " in
		              let _ = if -n <> 1 then Format.fprintf ppf "%d " (-n) in
		              Format.fprintf ppf "%a" Var.pr x
            with Not_found ->
		  				      Format.fprintf ppf " + %a %a" Term.pr n Var.pr x)
				      nxs
    | _ -> ()
  in
  try
		  let n = int_of n in
		  if n > 0 then
		    let _ = if nxs <> [] then Format.fprintf ppf " + " in
		    Format.fprintf ppf "%d" n
		  else if n < 0 then
		    let _ = if nxs <> [] then Format.fprintf ppf " - " in
		    Format.fprintf ppf "%d" (-n)
  with Not_found ->
		  Format.fprintf ppf " + %a" Term.pr n


let mul m nxs = List.map (fun (n, x) -> LinArith.simplify (mul m n), x) nxs

let minus nxs = mul (tint (-1)) nxs

let canonize nxs =
  let res = Util.classify (fun (_, x1) (_, x2) -> Var.equiv x1 x2) nxs in
  List.map
    (function ((n, x)::nxs) ->
      (List.fold_left (fun n1 n2 -> LinArith.simplify (add n1 n2)) n (List.map fst nxs), x)
    | _ -> assert false)
    res

let coeff nxs x = Util.find_map (fun (n, y) -> if x = y then n else raise Not_found) nxs

let equiv nxs1 nxs2 =
  let nxs1 = canonize nxs1 in
  let nxs2 = canonize nxs2 in
  let xs1 = List.sort (List.map snd nxs1) in
  let xs2 = List.sort (List.map snd nxs2) in
  xs1 = xs2 &&
  (List.for_all (fun x -> equiv (coeff nxs1 x) (coeff nxs2 x)) xs1)


let rec of_term t =
  match fun_args t with
    Var(_, x), [] ->
      [tint 1, x], tint 0
  | Const(_, Const.Int(n)), [] ->
      [], tint n
  | Const(_, Const.Add), [t1; t2] ->
      let nxs1, n1 = of_term t1 in
      let nxs2, n2 = of_term t2 in
      canonize (nxs1 @ nxs2), LinArith.simplify (add n1 n2)
  | Const(_, Const.Sub), [t1; t2] ->
      let nxs1, n1 = of_term t1 in
      let nxs2, n2 = of_term t2 in
      let nxs2, n2 = minus nxs2, LinArith.simplify (Term.minus n2) in
      canonize (nxs1 @ nxs2), LinArith.simplify (add n1 n2)
  | Const(_, Const.Mul), [t1; t2] ->
      let nxs1, n1 = of_term t1 in
      let nxs2, n2 = of_term t2 in
      if nxs1 = [] then
        mul n1 nxs2, LinArith.simplify (Term.mul n1 n2)
      else if nxs2 = [] then
        mul n2 nxs1, LinArith.simplify (Term.mul n2 n1)
      else
        invalid_arg "NonLinArith.of_term"
  | Const(_, Const.Minus), [t] ->
      let nxs, n = of_term t in
      minus nxs, LinArith.simplify (Term.minus n)
  | _ ->
      invalid_arg "NonLinArith.of_term"

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

(** {6 Functions on non-linear atomic integer formulas} *)

let aif_of t =
		match fun_args t with
		  Const(_, c), [t1; t2] when Const.is_ibin c ->
		    let nxs, n = of_term (sub t1 t2) in
		    c, nxs, n
		| _ -> invalid_arg "Farkas.aif_of"

(** let aif use only geq *)
let canonize_aif (c, nxs, n) =
  match c with
    Const.Leq ->
      minus nxs, LinArith.simplify (Term.minus n)
  | Const.Geq ->
      nxs, n
  | Const.Lt ->
      minus nxs, LinArith.simplify (Term.minus (add n (tint 1)))
  | Const.Gt ->
      nxs, LinArith.simplify (sub n (tint 1))
  | _ -> assert false

let pr_caif ppf (nxs, n) =
  Format.fprintf ppf "%a >= 0" pr (nxs, n)
