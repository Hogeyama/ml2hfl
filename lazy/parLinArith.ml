open ExtList
open ExtString
open Term

(** Parametric-linear arithmetic expressions *)

(** assume that all the expressions are canonized *)

(** {6 Functions on parametric-linear arithmetic expressions} *)

let pr ppf (nxs, n) =
  let _ =
    match nxs with
      [] -> ()
    | (n, x)::nxs ->
        let _ =
          if equiv n (tint 1) then
            Format.fprintf ppf "%a" Var.pr x
          else if equiv n (tint (-1)) then
            Format.fprintf ppf "-%a" Var.pr x
          else if not (equiv n (tint 0)) then
            Format.fprintf ppf "%a %a" Term.pr n Var.pr x
          else
            Format.fprintf ppf "0"(*???*)
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
              else
                Format.fprintf ppf "+ 0"(*???*)
            with Not_found ->
		  				      Format.fprintf ppf " + %a %a" Term.pr n Var.pr x)
				      nxs
  in
  try
		  let n = int_of n in
		  if n > 0 then
		    let _ = if nxs <> [] then Format.fprintf ppf " + " in
		    Format.fprintf ppf "%d" n
		  else if n < 0 then
		    let _ = if nxs <> [] then Format.fprintf ppf " - " in
		    Format.fprintf ppf "%d" (-n)
    else
      ()(*???*)
  with Not_found ->
    let _ = if nxs <> [] then Format.fprintf ppf " + " in
    Format.fprintf ppf "%a" Term.pr n

let coeff nxs x =
  Util.find_map
    (fun (n, y) -> if x = y then n else raise Not_found)
    nxs

let canonize (nxs, n) =
  let res = Util.classify (fun (_, x1) (_, x2) -> Var.equiv x1 x2) nxs in
  List.map
    (function ((n, x)::nxs) ->
      NonLinArith.simplify
		      (List.fold_left
		        Term.add
		        n
		        (List.map fst nxs)),
      x
    | _ -> assert false)
    res,
  NonLinArith.simplify n

let mul_coeff m (nxs, n) =
  canonize
		  (List.map (fun (n, x) -> Term.mul m n, x) nxs,
		  Term.mul m n)

let minus (nxs, n) = mul_coeff (tint (-1)) (nxs, n)

let add (nxs1, n1) (nxs2, n2) = canonize (nxs1 @ nxs2, Term.add n1 n2)

let mul (nxs1, n1) (nxs2, n2) =
  if nxs1 = [] then
    mul_coeff n1 (nxs2, n2)
  else if nxs2 = [] then
    mul_coeff n2 (nxs1, n1)
  else
    invalid_arg "ParLinArith.mul"

let equiv (nxs1, n1) (nxs2, n2) =
  let xs1 = List.sort (List.map snd nxs1) in
  let xs2 = List.sort (List.map snd nxs2) in
  xs1 = xs2 &&
  (List.for_all (fun x -> Term.equiv (coeff nxs1 x) (coeff nxs2 x)) xs1) &&
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
      (*let _ = Format.printf "%a@." Term.pr t in*)
      invalid_arg "ParLinArith.of_term"

let term_of (nxs, n) =
  let ts =
    (if Term.equiv n (Term.tint 0) then [] else [n]) @
    (List.filter_map
      (fun (n, x) ->
        if Term.equiv n (Term.tint 0) then
          None
        else if Term.equiv n (Term.tint 1) then
          Some(make_var x)
        else
          Some(Term.mul n (make_var x)))
      nxs)
  in
  sum ts

let pos_neg_terms_of (nxs, n) =
  let nxs =
    List.filter_map
      (fun (n, x) ->
        if Term.equiv n (Term.tint 0) then
          None
        else
          Some(n, x))
      nxs
  in
  let nxs1, nxs2 = List.partition (fun (n, _) -> try int_of n > 0 with Not_found -> true) nxs in
  sum ((try if int_of n > 0 then [n] else [] with Not_found -> [n]) @ List.map (fun (n, x) -> if Term.equiv n (Term.tint 1) then make_var x else Term.mul n (make_var x)) nxs1),
  sum ((try if int_of n < 0 then [Term.minus n] else [] with Not_found -> []) @ List.map (fun (n, x) -> if Term.equiv n (Term.tint (-1)) then make_var x else Term.mul (Term.minus n) (make_var x)) nxs2)

(** {6 Functions on parametric-linear atomic integer formulas} *)

let aif_of t =
		match fun_args t with
		  Const(_, c), [t1; t2] when Const.is_ibrel c ->
		    let nxs, n = of_term (sub t1 t2) in
		    c, nxs, n
		| _ ->
      (*let _ = Format.printf "%a@." Term.pr t in*)
      invalid_arg "ParLinArith.aif_of"

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

(** the result only uses geq and eq *)
let canonize_aif (c, nxs, n) =
  match c with
    Const.EqInt ->
      Const.EqInt, nxs, n
  | Const.NeqInt ->
      assert false
  | Const.Lt ->
      let nxs, n = minus (nxs, Term.add n (tint 1)) in
      Const.Geq, nxs, n
  | Const.Gt ->
      Const.Geq, nxs, (NonLinArith.simplify (Term.sub n (tint 1)))
  | Const.Leq ->
      let nxs, n = minus (nxs, n) in
      Const.Geq, nxs, n
  | Const.Geq ->
      Const.Geq, nxs, n
  | _ -> assert false

let factorize (nxs, n) =
  try
		  let nxs = (n, None) :: List.map (fun (n, x) -> n, Some(x)) nxs in
		  let polxs = List.map (fun (n, xopt) -> NonLinArith.of_term n, xopt) nxs in
		  let polxs = List.filter (fun (pol, xopt) -> pol <> []) polxs in
		  let cpolxs = List.map (fun (pol, xopt) -> NonLinArith.gcd_coeff pol, xopt) polxs in
		  let pol = snd (fst (List.hd cpolxs)) in
		  let cxs =
		    List.map
		      (fun ((c, pol'), xopt) ->
		        if NonLinArith.equiv pol pol' then
		          c, xopt
		        else if NonLinArith.equiv pol (NonLinArith.minus pol') then
		          -c, xopt
		        else
		          raise Not_found (*give up*))
		      cpolxs
		  in
		  let d = Util.gcd (List.map (fun (c, _) -> abs c) cxs) in
		  let cxs = List.map (fun (c, xopt) -> c / d, xopt) cxs in
    tint d ::
		  NonLinArith.factorize pol @
		  let nxs, ns = Util.partition_map (function (c, None) -> `R(c) | (c, Some(x)) -> `L(c, x)) cxs in
		  [LinArith.term_of (nxs, List.fold_left (+) 0 ns)]
  with Not_found ->
    NonLinArith.factorize (NonLinArith.of_term (term_of (nxs, n)))
