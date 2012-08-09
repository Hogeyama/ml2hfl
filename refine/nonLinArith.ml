open ExtList
open ExtString
open Term

(** Non-linear arithmetic expressions a.k.a polynomials *)

(** assume that all the polynomials are canonized *)

type term = int * Var.t list

(** {6 Functions on non-linear arithmetic expressions} *)

let pr_pterm ppf (n, xs) =
  if n > 0 then
    if n = 1 then
      Format.fprintf ppf "%a" (Util.pr_list Var.pr " ") xs
    else
      Format.fprintf ppf "%d %a" n (Util.pr_list Var.pr " ") xs
  else if n < 0 then
    if n = -1 then
      Format.fprintf ppf "(-%a)" (Util.pr_list Var.pr " ") xs
    else
      Format.fprintf ppf "(-%d %a)" (-n) (Util.pr_list Var.pr " ") xs
  else
    assert false

let pr ppf pol =
  match pol with
    [] -> ()
  | tm::tms ->
      let _ = Format.fprintf ppf "%a" pr_pterm tm in
				  List.iter
				    (fun (n, xs) ->
		        if n > 0 then
		  				    let _ = Format.fprintf ppf " + " in
		          Format.fprintf ppf "%a" pr_pterm (n, xs)
		        else if n < 0 then
		  				    let _ = Format.fprintf ppf " - " in
		          Format.fprintf ppf "%a" pr_pterm (-n, xs)
          else
            assert false)
				    tms
  (*Format.fprintf ppf (Util.pr_list pr_pterm " + ") pol*)

let coeff pol xs =
  Util.find_map
    (fun (n, ys) -> if xs = ys then n else raise Not_found)
    pol

let canonize pol =
  let pol = List.map (fun (n, xs) -> n, List.sort (List.unique xs)) pol in
  List.filter (fun (n, _) -> n <> 0)
		  (List.map
		    (function ((n, xs)::tms) ->
		      (List.fold_left (+) n (List.map fst tms), xs)
		    | _ -> assert false)
		    (Util.classify (fun (_, xs1) (_, xs2) -> xs1 = xs2) pol))

let mul_coeff_pterm m (n, xs) = m * n, xs
let mul_coeff m pol = canonize (List.map (mul_coeff_pterm m) pol)

let minus pol = mul_coeff (-1) pol

let add pol1 pol2 = canonize (pol1 @ pol2)

let mul pol1 pol2 =
  canonize
		  (Util.multiply_list
		    (fun (n1, xs1) (n2, xs2) -> n1 * n2, xs1 @ xs2)
		    pol1
		    pol2)

let equiv pol1 pol2 =
  let xss1 = List.sort (List.map snd pol1) in
  let xss2 = List.sort (List.map snd pol2) in
  xss1 = xss2 &&
  (List.for_all (fun xs -> coeff pol1 xs = coeff pol2 xs) xss1)

let rec of_term t =
  match fun_args t with
    Var(_, x), [] ->
      [1, [x]]
  | Const(_, Const.Int(n)), [] ->
      if n = 0 then [] else [n, []]
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
      invalid_arg "NonLinArith.of_term"

let term_of_pterm (n, xs) =
  if n = 0 then
    assert false
  else if n = 1 then
    Term.prod (List.map make_var xs)
  else
    Term.prod (Term.tint n :: List.map make_var xs)

let term_of pol =
  sum (List.map term_of_pterm pol)

(** assume: pol <> [] *)
let gcd_coeff pol =
  let n = Util.gcd (List.map (fun (n, _) -> abs n) pol) in
  let _ = assert (n <> 0) in
  n, List.map (fun (m, x) -> m / n, x) pol

let gcd_vars pol =
  let xs = List.map snd pol in
  match xs with
    [] -> []
  | x::xs -> List.fold_left Util.inter x xs

let factorize pol =
  let n, pol = gcd_coeff pol in
  let xs = gcd_vars pol in
  tint n :: List.map make_var xs @ [term_of (List.map (fun (n, ys) -> n, Util.diff ys xs) pol)]

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
      raise (Util.NotImplemented "NonLinArith.simplify")

(** {6 Functions on parametric-linear atomic integer formulas} *)

let pr_aif ppf (c, pol) =
  Format.fprintf ppf
    "%a %s 0"
    pr pol
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

let div_gcd_aif (c, pol) =
  let _, pol = gcd_coeff pol in
  c, pol

let aif_of t =
  match fun_args t with
    Const(_, c), [t1; t2] when Const.is_ibrel c ->
      let pol = of_term (sub t1 t2) in
      div_gcd_aif (c, pol)
  | _ ->
      (*let _ = Format.printf "%a@." Term.pr t in*)
      invalid_arg "NonLinArith.aif_of"

let term_of_aif (c, pol) =
  match pol with
    [(n, [])] ->
      if Const.lift_ibrel c n 0 then
        Const([], Const.True)
      else
        Const([], Const.False)
  | _ ->
      let pol1, pol2 = List.partition (fun (n, _) -> assert(n <> 0); n > 0) pol in
      apply
        (Const([], c))
        [sum (List.map (fun (n, xs) -> if n = 1 then Term.prod (List.map make_var xs) else Term.prod (tint n :: List.map make_var xs)) pol1);
         sum (List.map (fun (n, xs) -> if n = -1 then Term.prod (List.map make_var xs) else Term.prod (tint (-n) :: List.map make_var xs)) pol2)]
