open ExtList
open ExtString

(** Interface to Apron *)

let manpk = Polka.manager_alloc_strict ()
(*let maneq = Polka.manager_alloc_equalities ()*)

let linconstr_of env t =
  let c, nxs, n =
    try
      LinArith.aif_of t
    with Invalid_argument(_) ->
      (match Term.fun_args t with
        Term.Const(_, Const.True), [] ->
          Const.EqInt, [], 0
      | Term.Const(_, Const.False), [] ->
          Const.EqInt, [], 1
      | _ ->
          let _ = Format.printf "Boolean and unit variables are not supported: %a@," Term.pr t in
          assert false)
  in
  let expr = Apron.Linexpr1.make env in
  Apron.Linexpr1.set_array expr
    (Array.of_list
      (List.map
        (fun (n, x) -> Apron.Coeff.s_of_int n, Apron.Var.of_string (Var.print x))
      nxs))
    (Some ((Apron.Coeff.s_of_int n)));
  let mexpr = Apron.Linexpr1.make env in
  let (nxs, n) = LinArith.minus (nxs, n) in
  Apron.Linexpr1.set_array mexpr
    (Array.of_list
      (List.map
        (fun (n, x) -> Apron.Coeff.s_of_int n, Apron.Var.of_string (Var.print x))
      nxs))
    (Some ((Apron.Coeff.s_of_int n)));
  match c with
    Const.Lt -> Apron.Lincons1.make mexpr Apron.Lincons1.SUP
  | Const.Gt -> Apron.Lincons1.make expr Apron.Lincons1.SUP
  | Const.Leq -> Apron.Lincons1.make mexpr Apron.Lincons1.SUPEQ
  | Const.Geq -> Apron.Lincons1.make expr Apron.Lincons1.SUPEQ
  | Const.EqInt -> Apron.Lincons1.make expr Apron.Lincons1.EQ
  | Const.NeqInt -> Apron.Lincons1.make expr Apron.Lincons1.DISEQ
  | _ -> assert false

let int_of_coeff n =
  if Apron.Coeff.is_scalar n then
    match n with
      Apron.Coeff.Scalar(s) ->
        let str = Apron.Scalar.to_string s in
(*
        let _ = Format.printf "%s@," str in
*)
        (int_of_float (float_of_string str))
    | _ -> assert false
  else
    assert false

let of_linconstr c =
  let nxs = ref [] in
  Apron.Lincons1.iter (fun n x -> nxs := (int_of_coeff n, Var.parse (Apron.Var.to_string x))::!nxs) c;
  let t = List.fold_left
    (fun t (n, x) ->
      if n = 0 then
        t
      else
        let t' =
          if n = 1 then
            Term.make_var x
          else
            Term.mul (Term.tint n) (Term.make_var x)
        in
        match t with
          Term.Const(_, Const.Int(m)) when m = 0 -> t'
        | _ -> Term.add t t')
    (Term.tint (int_of_coeff (Apron.Lincons1.get_cst c)))
    !nxs
  in
  let zero = Term.tint 0 in
  match Apron.Lincons1.get_typ c with
    Apron.Lincons1.SUP -> Formula.gt t zero
  | Apron.Lincons1.SUPEQ -> Formula.geq t zero
  | Apron.Lincons1.EQ -> Formula.eqInt t zero
  | Apron.Lincons1.DISEQ -> Formula.neqInt t zero
  | _ -> assert false
let of_linconstrs cs =
  Formula.band
    (List.mapi
      (fun i _ ->
        of_linconstr (Apron.Lincons1.array_get cs i))
      (List.make (Apron.Lincons1.array_length cs) ()))

let polyhedron_of env t =
  let tss = Formula.dnf t in
(*
  List.iter (fun ts -> Format.printf "cj: @[<hv>%a@]@ " (Util.pr_list Term.pr ",@ ") ts) tss;
*)
  let tss = if tss = [] then [[Formula.tfalse]] else tss in
  let abs =
    List.map
      (fun ts ->
        let ts = if ts = [] then [Formula.ttrue] else ts in
        let tab = Apron.Lincons1.array_make env (List.length ts) in
        List.iteri
          (fun i t ->
            let ab = linconstr_of env t in
(*
            Format.printf "linconstr: %a@ " Apron.Lincons1.print ab;
*)
            Apron.Lincons1.array_set tab i ab)
          ts;
        Apron.Abstract1.of_lincons_array manpk env tab)
      tss
  in
(*
  Format.printf "poly': @[<hv>%a@]@ " (Util.pr_list Apron.Abstract1.print ",@ ") abs;
*)
  Apron.Abstract1.join_array manpk (Array.of_list abs)

let widen2 t1 t2 = Apron.Abstract1.widening manpk t1 t2

let convex_hull t =
  let fvs =
    List.map
      (fun x -> Apron.Var.of_string (Var.print x))
      (List.unique (Term.fvs t))
  in
  let env = Apron.Environment.make (Array.of_list fvs) [||] in
  let tss, f = Formula.elim_boolean [Formula.elim_unit t] in
		let ts =
    List.map
      (fun [t] ->
        of_linconstrs (Apron.Abstract1.to_lincons_array manpk (polyhedron_of env t)))
      tss
  in
  f ts

let widen ts =
  Format.printf "@[<hov>widen_in: @[<hv>%a@]@ " (Util.pr_list Term.pr ",@ ") ts;

  let fvs = List.map
    (fun x -> Apron.Var.of_string (Var.print x))
    (List.unique (Util.concat_map Term.fvs ts))
  in
  let env = Apron.Environment.make (Array.of_list fvs) [||] in
  let tss, f = Formula.elim_boolean (List.map Formula.elim_unit ts) in
  let aux ts =
		  let ts = List.map (fun t -> polyhedron_of env t) ts in
(*
		  Format.printf "poly: @[<hv>%a@]@ " (Util.pr_list Apron.Abstract1.print ",@ ") ts;
*)
		  let widen_aux ts =
				  match ts with
				    t::ts ->
				      let ab = List.fold_left (fun t1 t2 -> widen2 t1 t2) t ts in
(*
				      Format.printf "widen: @[%a@]@ " Apron.Abstract1.print ab;
*)
				      let res = of_linconstrs (Apron.Abstract1.to_lincons_array manpk ab) in
(*
				      Format.printf "widen_out: @[%a@]" Term.pr res;
*)
				      res
				  | _ -> assert false
		  in
		  widen_aux ts
  in
  let res = f (List.map aux tss) in
		let _ = Format.printf "widen_out: @[%a@]@]@ " Term.pr res in
  res
