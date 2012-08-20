open ExtList
open Term
open Formula

exception Nonpresburger

let rec of_term t =
  match fun_args t with
    Var(_, x), [] ->
      Atp_batch.Var(Var.print x)
  | Const(_, Const.Event(_)), [] ->
      assert false
  | Const(_, Const.Unit), [] ->
      raise (Util.NotImplemented  "AtpInterface.of_term")
  | Const(_, Const.Int(n)), [] ->
      Atp_batch.Fn((*Num.*)string_of_int n, [])
  | Const(_, Const.RandInt), [] ->
      assert false
  | Const(_, Const.Minus), [t] ->
      Atp_batch.Fn("-", [of_term t])
  | Const(_, c), [t1; t2] ->
      let t1 = of_term t1 in
      let t2 = of_term t2 in
      (match c with
				    Const.Add -> Atp_batch.Fn("+", [t1; t2])
				  | Const.Sub -> Atp_batch.Fn("-", [t1; t2])
				  | Const.Mul -> Atp_batch.Fn("*", [t1; t2])
				  | Const.Div ->
				      raise (Util.NotImplemented "AtpInterface.of_term")
				  | Const.Mod ->
				      raise (Util.NotImplemented "AtpInterface.of_term"))
  | _ ->
      let _ = Format.printf "%a@." Term.pr t in
      assert false

let rec of_formula t =
  match fun_args t with
    Var(_, _), [] ->
      raise (Util.NotImplemented "AtpInterface.of_formula")
  | Const(_, Const.True), [] ->
      Atp_batch.True
  | Const(_, Const.False) , [] ->
      Atp_batch.False
  | Const(_, Const.Not), [t] ->
      Atp_batch.Not(of_formula t)
  | Const(_, Const.And), [t1; t2] ->
      Atp_batch.And(of_formula t1, of_formula t2)
  | Const(_, Const.Or), [t1; t2] ->
      Atp_batch.Or(of_formula t1, of_formula t2)
  | Const(_, Const.Imply), [t1; t2] ->
      Atp_batch.Imp(of_formula t1, of_formula t2)
  | Const(_, Const.Iff), [t1; t2] ->
      Atp_batch.Iff(of_formula t1, of_formula t2)
  | Const(_, c), [t1; t2] ->
      let t1 = of_term t1 in
      let t2 = of_term t2 in
      (match c with
        Const.EqUnit | Const.NeqUnit | Const.EqBool | Const.NeqBool ->
          raise (Util.NotImplemented "AtpInterface.of_formula")
      | Const.EqInt ->
          Atp_batch.Atom(Atp_batch.R("=", [t1; t2]))
      | Const.NeqInt ->
          (* necessary since Nelson-Oppen does not support "<>"*)
          Atp_batch.Not(Atp_batch.Atom(Atp_batch.R("=", [t1; t2])))
      | Const.Lt ->
          Atp_batch.Atom(Atp_batch.R("<", [t1; t2]))
				  | Const.Gt ->
          Atp_batch.Atom(Atp_batch.R(">", [t1; t2]))
				  | Const.Leq ->
          Atp_batch.Atom(Atp_batch.R("<=", [t1; t2]))
				  | Const.Geq ->
          Atp_batch.Atom(Atp_batch.R(">=", [t1; t2]))
				  (*| Const.Divides -> raise (Util.NotImplemented "AtpInterface.of_formula") (*"divides"*)*)
      | _ -> assert false)
  | Forall(_, env, t), [] ->
      List.fold_right (fun (x, _) phi -> Atp_batch.Forall(Var.print x , phi)) env (of_formula t)
  | Exists(_, env, t), [] ->
      List.fold_right (fun (x, _) phi -> Atp_batch.Exists(Var.print x , phi)) env (of_formula t)
  | _ ->
      let _ = Format.printf "%a@." Term.pr t in
      assert false

let rec term_of = function
    Atp_batch.Var(id) ->
      make_var (Var.parse id)
  | Atp_batch.Fn(s, []) when Util.is_int s ->
      tint (int_of_string s)
  | Atp_batch.Fn("+", [t1; t2]) ->
      add (term_of t1) (term_of t2)
  | Atp_batch.Fn("-", [t1; t2]) ->
      sub (term_of t1) (term_of t2)
  | Atp_batch.Fn("*", [t1; t2]) ->
      mul (term_of t1) (term_of t2)
  | Atp_batch.Fn("-", [t]) ->
      minus (term_of t)
  | Atp_batch.Fn(s, ts) -> assert false

let rec formula_of p =
  match p with
    Atp_batch.True ->
      ttrue
  | Atp_batch.False ->
      tfalse
  | Atp_batch.Not(p) ->
      bnot (formula_of p)
  | Atp_batch.And(p1, p2) ->
      band [formula_of p1; formula_of p2]
  | Atp_batch.Or(p1, p2) ->
      bor [formula_of p1; formula_of p2]
  | Atp_batch.Imp(p1, p2) ->
      imply (formula_of p1) (formula_of p2)
  | Atp_batch.Iff(p1, p2) ->
      iff (formula_of p1) (formula_of p2)
  | Atp_batch.Forall(x, p) ->
      assert false(*forall [Var.parse id, SimType.Int???] (formula_of p)*)
  | Atp_batch.Exists(x, p) ->
      assert false(*exists [Var.parse id, SimType.Int???] (formula_of p)*)
  | Atp_batch.Atom(Atp_batch.R("=", [t1; t2])) ->
      eqInt (term_of t1) (term_of t2)
  | Atp_batch.Atom(Atp_batch.R("<", [t1; t2])) ->
      lt (term_of t1) (term_of t2)
  | Atp_batch.Atom(Atp_batch.R("<=", [t1; t2])) ->
      leq (term_of t1) (term_of t2)
  | Atp_batch.Atom(Atp_batch.R(">", [t1; t2])) ->
      gt (term_of t1) (term_of t2)
  | Atp_batch.Atom(Atp_batch.R(">=", [t1; t2])) ->
      geq (term_of t1) (term_of t2)
  | Atp_batch.Atom(Atp_batch.R("divides", [t1; t2])) ->
      raise (Util.NotImplemented "AtpInterface.formula_of")
  | _ ->
      let _ = Atp_batch.print_formula Atp_batch.print_atom p; print_newline() in
      assert false

(** {6 Functions on ATP formulas} *)

let theory =
  let tneqf = Atp_batch.Not(Atp_batch.Atom(Atp_batch.R("=", [Atp_batch.Fn("true", []); Atp_batch.Fn("false", [])]))) in
  let lengtz =
    let id = Idnt.string_of (Idnt.new_id ()) in
    Atp_batch.Forall(id, Atp_batch.Atom(Atp_batch.R(">=", [Atp_batch.Fn("len", [Atp_batch.Var(id)]); Atp_batch.Fn("0", [])])))
  in
  tneqf
  (*Atp_batch.And(tneqf, lengtz)*)

let langs = Atp_batch.add_default [Atp_batch.int_lang]

let is_valid p =
  let _ = Global.log_begin "is_valid" in
  let _ = Global.log (fun () -> Format.printf "%a@," (Term.pr) (formula_of p)) in
  let res = Atp_batch.nelop langs (Atp_batch.Imp(theory, p)) in
  let _ = Global.log_end "is_valid" in
  res

(** {6 Functions on formulas} *)
let integer_qelim t =
  let debug = false && !Global.debug in
  let _ = if debug then Format.printf "intqelim input: @[<v>%a@]@," Term.pr t in
  let t = simplify (formula_of (Atp_batch.integer_qelim (of_formula (elim_eq_neq_boolean t)))) in
  let _ = if debug then Format.printf "intqelim output: @[<v>%a@]@," Term.pr t in
		t

let real_qelim t =
  simplify (formula_of (Atp_batch.real_qelim (of_formula (elim_eq_neq_boolean t))))




let qelim_fes bvs (Fes.FES(xttys, ts) as fes) =
  let _ = Global.log_begin "qelim_fes" in
  let _ = Global.log (fun () -> Format.printf "input: @[<v>%a@]@," Fes.pr fes) in
  let ts =
		  try
				  let fvs =
				    let fvs = List.unique (Util.diff (Util.concat_map Term.fvs ts) (bvs @ TypSubst.fvs xttys)) in
		      let _ = Global.log (fun () -> Format.printf "bvs: %a@,fvs: %a@," Var.pr_list bvs Var.pr_list fvs) in
				    fvs
				  in
      let t = band ts in
		    if fvs <> [] && is_linear t then
		      conjuncts (integer_qelim (exists (List.map (fun x -> x, SimType.Int(*???*)) fvs) t))
		    else
		      raise (Util.NotImplemented "subst_lbs")
		  with Util.NotImplemented _ ->
				  Util.map_left
				    (fun ts1 t ts2 ->
				      let fvs =
            let fvs = List.unique (Util.diff (Term.fvs t) (bvs @ TypSubst.fvs xttys @ Util.concat_map Term.fvs ts1 @ Util.concat_map Term.fvs ts2)) in
	  			      let _ = Global.log (fun () -> Format.printf "bvs: %a@,fvs: %a@," Var.pr_list bvs Var.pr_list fvs) in
            fvs
          in
				      if fvs <> [] && is_linear t then
				        let _ = Global.log (fun () -> Format.printf "before:@,  @[%a@]@," Term.pr t) in
				        let t =
				          try
				            integer_qelim (exists (List.map (fun x -> x, SimType.Int(*???*)) fvs) t)
				          with Util.NotImplemented _ ->
				            t
				        in
				        let _ = Global.log (fun () -> Format.printf "after:@,  @[%a@]@," Term.pr t) in
				        t
				      else
				        t)
				    ts
  in
  let res = Fes.make xttys ts in
  let _ = Global.log (fun () -> Format.printf "output: @[<v>%a@]" Fes.pr res) in
  let _ = Global.log_end "qelim_fes" in
  res


(*
(** @deprecated unsound *)
let merge_fess xs fess =
  let xttyss, tss = List.split (List.map (fun (Fes.FES(xttys, ts)) -> xttys, ts) fess) in
  let xttys = List.flatten xttyss in
  let xttys =
    Util.concat_map
      (fun (((x, t, ty) as xtty)::xttys) ->
        if xttys <> [] && List.mem x xs then
          let _ = Global.log (fun () -> Format.printf "xttys: %a@," TypSubst.pr (xtty::xttys)) in
          [xtty] (* ToDo: prove that this is sound and complete *)
        else
          xtty::xttys)
      (Util.classify (fun (x1, _, _) (x2, _, _) -> x1 = x2) xttys)
  in
  let ts = List.flatten tss in
  Fes.make xttys ts




(*
let cnf phi = Fol.simplify (formula_of (Atp_batch.cnf (Atp_batch.simplify (of_formula phi))))
*)


(*
let rec eval = function
    Atp_batch.Var(_) -> failwith "eval"
  | Atp_batch.Fn("+", [s; t]) -> (eval s) + (eval t)
  | Atp_batch.Fn("-", [s; t]) -> (eval s) - (eval t)
  | Atp_batch.Fn("*", [s; t]) -> (eval s) * (eval t)
  | Atp_batch.Fn("-", [s]) -> -(eval s)
  | Atp_batch.Fn(s, []) when Util.is_int s -> int_of_string s
  | _ -> failwith "eval"
*)

let rec ufst bv s =
  match s with
    Atp_batch.Var(id) ->
      if id = bv then raise Not_found else []
  | Atp_batch.Fn("len", [Atp_batch.Var(id)]) when id = bv ->
      [Atp_batch.Fn("len", [Atp_batch.Var(id)])]
  | Atp_batch.Fn(str, [Atp_batch.Var(id)])
      when let str1, str2 = Util.split_at 4 str in
           str1 = "proj" && Util.is_int str2 && id = bv ->
      [Atp_batch.Fn(str, [Atp_batch.Var(id)])]
  | Atp_batch.Fn("hd", [Atp_batch.Var(id)]) when id = bv ->
      [Atp_batch.Fn("hd", [Atp_batch.Var(id)])]
  | Atp_batch.Fn(_, ss) ->
      Util.catmap (ufst bv) ss

let rec ufs bv p =
  match p with
    Atp_batch.True -> []
  | Atp_batch.False -> []
  | Atp_batch.Not(p) -> ufs bv p
  | Atp_batch.And(p1, p2) | Atp_batch.Or(p1, p2) | Atp_batch.Imp(p1, p2) | Atp_batch.Iff(p1, p2) ->
      (ufs bv p1) @ (ufs bv p2)
  | Atp_batch.Forall(id, p) | Atp_batch.Exists(id, p) ->
      if bv = id then [] else ufs bv p
  | Atp_batch.Atom(Atp_batch.R(_, ss)) ->
      Util.catmap (ufst bv) ss

let rec assignt sub s =
  match s with
    Atp_batch.Var(id) -> Atp_batch.Var(id)
  | Atp_batch.Fn("len", [Atp_batch.Var(id)]) ->
      Util.assoc_default s s sub
  | Atp_batch.Fn(str, [Atp_batch.Var(id)]) when let str1, str2 = Util.split_at 4 str in str1 = "proj" && Util.is_int str2 ->
      Util.assoc_default s (Atp_batch.Fn(str, [Atp_batch.Var(id)])) sub
  | Atp_batch.Fn("hd", [Atp_batch.Var(id)]) ->
      Util.assoc_default s s sub
  | Atp_batch.Fn(str, ss) -> Atp_batch.Fn(str, List.map (assignt sub) ss)

let rec assign sub p =
  match p with
    Atp_batch.True -> Atp_batch.True
  | Atp_batch.False -> Atp_batch.False
  | Atp_batch.Not(p) -> Atp_batch.Not(assign sub p)
  | Atp_batch.And(p1, p2) -> Atp_batch.And(assign sub p1, assign sub p2)
  | Atp_batch.Or(p1, p2) -> Atp_batch.Or(assign sub p1, assign sub p2)
  | Atp_batch.Imp(p1, p2) -> Atp_batch.Imp(assign sub p1, assign sub p2)
  | Atp_batch.Iff(p1, p2) -> Atp_batch.Iff(assign sub p1, assign sub p2)
  | Atp_batch.Forall(r, p) -> failwith "assign"
  | Atp_batch.Exists(r, p) -> failwith "assign"
  | Atp_batch.Atom(Atp_batch.R(s, ss)) -> Atp_batch.Atom(Atp_batch.R(s, List.map (assignt sub) ss))

let rec qelim_ufs bvs1 bvs2 fms =
  match bvs1 with
    [] ->
      bvs2, fms
  | bv::bvs ->
(*
      let _ = Format.printf "%a,%a@." Id.pr bv (Util.pr_list Fol.pr ", ") (List.map formula_of fms) in
*)
      try
				    let sub = List.map (fun x -> x, Atp_batch.Var(Id.gen_exp_var ())) (Util.catmap (ufs bv) fms) in
				    let bvs' = List.map (fun (_, Atp_batch.Var(id)) -> id) sub in
						  let fms =
		        (List.filter_map
								    (function
								      (Atp_batch.Fn("len", [Atp_batch.Var(_)]), Atp_batch.Var(id)) ->
								        Some(Atp_batch.Atom(Atp_batch.R(">=", [Atp_batch.Var(id); Atp_batch.Fn(string_of_int 0, [])])))
								      | _ -> None)
								    sub) @
								  (List.map (assign sub) fms) in
        qelim_ufs (bvs' @ bvs) bvs2 fms
      with Not_found ->
(*
        let _ = Format.printf "%a,%a@." Id.pr bv (Util.pr_list Fol.pr ", ") (List.map formula_of fms) in
*)
        qelim_ufs bvs (bv::bvs2) fms

let find bv =
  let rec g _s _t =
  		match _s with
  				Atp_batch.Var(id) ->
  						assert (id = bv); _t
  		| Atp_batch.Fn("+", [s; t]) when List.mem bv (Atp_batch.fvt s) && not (List.mem bv (Atp_batch.fvt t)) ->
        g s (Atp_batch.Fn("-", [_t; t]))
    | Atp_batch.Fn("+", [t; s]) when List.mem bv (Atp_batch.fvt s) && not (List.mem bv (Atp_batch.fvt t)) ->
        g s (Atp_batch.Fn("-", [_t; t]))
  		| Atp_batch.Fn("-", [s; t]) when List.mem bv (Atp_batch.fvt s) && not (List.mem bv (Atp_batch.fvt t)) ->
        g s (Atp_batch.Fn("+", [_t; t]))
  		| Atp_batch.Fn("-", [t; s]) when List.mem bv (Atp_batch.fvt s) && not (List.mem bv (Atp_batch.fvt t)) ->
        g s (Atp_batch.Fn("-", [t; _t]))
  		| Atp_batch.Fn("*", [s; Atp_batch.Fn("1", [])]) | Atp_batch.Fn("*", [Atp_batch.Fn("1", []); s]) ->
        g s _t
  		| Atp_batch.Fn("*", [s; Atp_batch.Fn("-1", [])]) | Atp_batch.Fn("*", [Atp_batch.Fn("-1", []); s]) | Atp_batch.Fn("-", [s]) ->
        g s (Atp_batch.Fn("-", [_t]))
  		| _ -> raise Not_found
  in
  function
  		Atp_batch.Atom(Atp_batch.R("=", [s; t])) when List.mem bv (Atp_batch.fvt s) && not (List.mem bv (Atp_batch.fvt t)) ->
  		  (try Some(g s t) with Not_found -> None)
  | Atp_batch.Atom(Atp_batch.R("=", [t; s])) when List.mem bv (Atp_batch.fvt s) && not (List.mem bv (Atp_batch.fvt t)) ->
  		  (try Some(g s t) with Not_found -> None)
  | _ -> None

let rec qelim_eq bvs1 bvs2 fms =
  match bvs1 with
    [] ->
      bvs2, fms
  | bv::bvs ->
      try
        let rhs = Util.find_map (find bv) fms in
        qelim_eq
          bvs
          bvs2
          (List.map
            (fun p ->
              Atp_batch.simplify
                (Atp_batch.subst
                  (Atp_batch.(|->) bv rhs Atp_batch.undefined)
                  p))
            fms)
      with Not_found ->
        qelim_eq bvs (bv::bvs2) fms

let rec exists ids p = List.fold_left (fun p' id -> Atp_batch.Exists(id, p')) p ids

let rec lineart =
  function
    Atp_batch.Var(id) -> true
  | Atp_batch.Fn("+", [s; t]) | Atp_batch.Fn("-", [s; t]) ->
      lineart s && lineart t
  | Atp_batch.Fn("*", [Atp_batch.Fn(s, []); t]) when Util.is_int s -> lineart t
  | Atp_batch.Fn("*", [t; Atp_batch.Fn(s, [])]) when Util.is_int s -> lineart t
  | Atp_batch.Fn("-", [s]) -> lineart s
  | Atp_batch.Fn(s, []) when Util.is_int s -> true
  | _ -> false

let rec linear =
  function
    Atp_batch.True | Atp_batch.False -> true
  | Atp_batch.Not(p) -> linear p
  | Atp_batch.And(p1, p2) | Atp_batch.Or(p1, p2) | Atp_batch.Imp(p1, p2) | Atp_batch.Iff(p1, p2) ->
      linear p1 && linear p2
  | Atp_batch.Forall(_, p) | Atp_batch.Exists(_, p) -> linear p
  | Atp_batch.Atom(Atp_batch.R("=", [s; t]))
  | Atp_batch.Atom(Atp_batch.R("<", [s; t]))
  | Atp_batch.Atom(Atp_batch.R("<=", [s; t]))
  | Atp_batch.Atom(Atp_batch.R(">", [s; t]))
  | Atp_batch.Atom(Atp_batch.R(">=", [s; t])) ->
      lineart s && lineart t
  | _ -> false

let rec partition_linear fms =
  List.partition linear fms

let rec qelim_lin bvs [] fms =
  if List.mem Atp_batch.False fms then
    [], []
  else begin
				if !Global.debug > 0 then
						Format.printf "%a@." Fol.pr (formula_of (Atp_batch.list_conj fms));
				let fms' = Atp_batch.homogenize langs fms in
				let bvs' = List.unique (Util.diff (Util.catmap Atp_batch.fv fms') (Util.catmap Atp_batch.fv fms)) in
				let fms_lin, fms_nonlin = partition_linear fms' in
				(*let [fms_lin; fms_nonlin] = Atp_batch.langpartition langs fms' in*)
				let fms_lin_ineq, fms_lin_eq =
						List.partition
								(function Atp_batch.Atom(Atp_batch.R("=", [Atp_batch.Var(_); Atp_batch.Var(_)])) -> false
								| _ -> true)
								fms_lin in
				if !Global.debug > 0 then
						Format.printf "%a@.%a@." Fol.pr (formula_of (Atp_batch.list_conj fms_lin_ineq)) Fol.pr (formula_of (Atp_batch.list_conj fms_lin_eq));
				let fms_lin_eq1, fms_lin_eq2 =
						List.partition
								(fun p -> List.exists (fun fv -> List.mem fv (Util.catmap Atp_batch.fv fms_lin_ineq)) (Atp_batch.fv p))
								fms_lin_eq in
				let fms1 = fms_lin_ineq @ fms_lin_eq1 in
				let fms2 = fms_lin_eq2 @ fms_nonlin in
				let bvs1, bvs2 = List.partition (fun bv -> not (List.mem bv (Util.catmap Atp_batch.fv fms2))) bvs in
				let p = Atp_batch.list_conj fms1 in
				let p = Atp_batch.simplify (if bvs1 = [] then p else Atp_batch.integer_qelim (exists bvs1 p)) in
				let fmss =
						List.map
							 (fun fms ->
							   let bvs, fms = qelim_eq bvs' [] (fms @ fms2) in
							   (if bvs <> [] then
							     (Format.printf "%a@." (Util.pr_list Id.pr ",") bvs;
							     Format.printf "%a@." Fol.pr (formula_of (Atp_batch.list_conj fms));
							     failwith "qelim_lin"));
							   fms)
							 (Atp_batch.simpdnf p) in
				bvs2, fmss
		(*
				match Atp_batch.simpdnf p with
						[] ->
							 [], [Atp_batch.False]
				| [fms] ->
							 let [], fms = qelim_eq bvs' [] (fms @ fms2) in
							 bvs2, fms
				| fmss -> (*possible*)
							 let _ = Format.printf "%a,%a@." Fol.pr (formula_of (Atp_batch.list_conj fms)) Fol.pr (formula_of p) in
							 failwith "qelim_lin"
		*)
  end


(*theory‚Í?*)
let implies p1 p2 = is_valid (Atp_batch.Imp(p1, p2))

let is_valid_dnf fms =
  let fvs = Util.catmap Atp_batch.fv fms in
  let fms = List.map (fun p -> Atp_batch.simplify (Atp_batch.Not(p))) fms in
  let fvs, fms = qelim_ufs fvs [] fms in
  let fms = List.map (fun p -> of_formula (Fol.simplify (formula_of p))) fms in
  let fvs, fms = qelim_eq fvs [] fms in
  let fms = List.map (fun p -> of_formula (Fol.simplify (formula_of p))) fms in
  let fvs, fms = qelim_ufs fvs [] fms in
  let fms = List.map (fun p -> of_formula (Fol.simplify (formula_of p))) fms in
  let p = Atp_batch.list_disj (List.map (fun p -> Atp_batch.simplify (Atp_batch.Not(p))) fms) in
(*
  let _ = Format.printf "%a@." (Fol.pr) (formula_of p) in
*)
  is_valid p

(*
  let passed =
    let st = Unix.times () in
    (fun () ->
      let en = Unix.times () in
      (en.Unix.tms_utime -. st.Unix.tms_utime) +.
      (en.Unix.tms_cutime -. st.Unix.tms_cutime)) in
*)
exception Ret of bool

let is_valid_timeout p tm =
  try
    let t = Thread.create (fun p -> raise (Ret(is_valid p))) p in
    Unix.alarm tm;
    Thread.wait_signal [Sys.sigalrm];
    None
  with Ret(b) -> Some(b)

let remove_trivial fms =
  if List.mem Atp_batch.False fms then
    [Atp_batch.False]
  else
		  List.filter_map
		    (function
(*
		    		Atp_batch.Atom(Atp_batch.R("=", [s; t])) when s = t ->
		        None
		    | Atp_batch.Not(Atp_batch.Atom(Atp_batch.R("=", [s; t]))) when s = t ->
		        Some(Atp_batch.False)
*)
		      p when is_valid p ->
		        None
		    | p when is_valid (Atp_batch.Not(p)) ->
		        Some(Atp_batch.False)
		    |	Atp_batch.Atom(Atp_batch.R("=", [Atp_batch.Fn("true", []); Atp_batch.Fn("false", [])]))
		    |	Atp_batch.Atom(Atp_batch.R("=", [Atp_batch.Fn("false", []); Atp_batch.Fn("true", [])])) ->
		        None
		    |	Atp_batch.Not(Atp_batch.Atom(Atp_batch.R("=", [s; Atp_batch.Fn("true", [])])))
		    |	Atp_batch.Not(Atp_batch.Atom(Atp_batch.R("=", [Atp_batch.Fn("true", []); s]))) ->
		        Some(Atp_batch.Atom(Atp_batch.R("=", [s; Atp_batch.Fn("false", [])])))
		    |	Atp_batch.Not(Atp_batch.Atom(Atp_batch.R("=", [s; Atp_batch.Fn("false", [])])))
		    |	Atp_batch.Not(Atp_batch.Atom(Atp_batch.R("=", [Atp_batch.Fn("false", []); s]))) ->
		        Some(Atp_batch.Atom(Atp_batch.R("=", [s; Atp_batch.Fn("true", [])])))
		    | p -> Some(p))
		    fms

let atp_mini_conj fms =
  let rec loop fms1 fms2 =
    match fms1 with
      [] -> fms2
    | p::fms1' ->
        if is_valid_dnf (p::(List.map (fun p -> Atp_batch.simplify (Atp_batch.Not(p))) (fms1' @ fms2))) then
(*
        if is_valid (Atp_batch.Imp(Atp_batch.list_conj (fms1' @ fms2), p)) then
*)
          loop fms1' fms2
        else
          loop fms1' (p::fms2)
  in
  loop fms []

let mini_conj phis =
  List.map formula_of (atp_mini_conj (List.map of_formula phis))

let atp_mini_disj fms =
  let rec loop fms1 fms2 =
    match fms1 with
      [] ->
        fms2
    | p::fms1' ->
        if is_valid_dnf ((Atp_batch.simplify (Atp_batch.Not(p)))::(fms1' @ fms2)) then
(*
        if is_valid (Atp_batch.Imp(p, Atp_batch.list_disj (fms1' @ fms2))) then
*)
          loop fms1' fms2
        else
          loop fms1' (p::fms2)
  in
  loop fms []

let mini_disj phis =
  List.map formula_of (atp_mini_disj (List.map of_formula phis))

let qelim bvs p =
  let bvs = List.filter (fun bv -> List.mem bv (Atp_batch.fv p)) bvs in
  if bvs = [] then
    [],
    (*p*)
    Atp_batch.simplify (Atp_batch.list_disj (List.map (fun fms -> Atp_batch.list_conj (atp_mini_conj fms)) (Atp_batch.simpdnf p)))
  else
    let bvss, fms =
      List.split
  				  (List.map
		  		    (fun fms ->
            let rec loop bvs fms =
              let old_bvs = bvs in
              ((if !Global.debug > 0 then
                Format.printf "%a;%a@."
                  (Util.pr_list Id.pr ",") bvs
                  (Util.pr_list Fol.pr ", ") (List.map formula_of fms));
              if !Global.debug > 0 then Format.printf "eliminating uninterpreted function symbols@.";
              let bvs, fms = qelim_ufs bvs [] fms in
              (if !Global.debug > 0 then
                Format.printf "%a;%a@."
                  (Util.pr_list Id.pr ",") bvs
                  (Util.pr_list Fol.pr ", ") (List.map formula_of fms));
              if !Global.debug > 0 then Format.printf "eliminating equalities@.";
              let bvs, fms = qelim_eq bvs [] fms in
              (if !Global.debug > 0 then
                Format.printf "%a;%a@."
                  (Util.pr_list Id.pr ",") bvs
                  (Util.pr_list Fol.pr ", ") (List.map formula_of fms));
              if !Global.debug > 0 then Format.printf "eliminating linear inequalities and equalities@.";
              let bvs, fmss = qelim_lin bvs [] fms in
              (if !Global.debug > 0 then
                Format.printf "%a;%a@."
                  (Util.pr_list Id.pr ",") bvs
                  Fol.pr (formula_of (Atp_batch.list_disj (List.map Atp_batch.list_conj fmss))));
	           		let fmss = List.map remove_trivial fmss in
              let bvs, fms =
                if Util.diff old_bvs bvs = [] then
				              bvs, List.map (fun fms -> Atp_batch.simplify (Atp_batch.list_conj (atp_mini_conj fms))) fmss
                else
                  let bvss, fms = List.split (List.map (loop bvs) fmss) in
                  List.concat bvss, fms
              in
              bvs, Atp_batch.list_disj fms)
            in
            loop bvs fms)
				      (Atp_batch.simpdnf p)) in
    List.concat bvss,
    Atp_batch.simplify (Atp_batch.list_disj ((*atp_mini_disj*)fms))

(*
    | Atp_batch.Not(Atp_batch.Atom(Atp_batch.R("=", [Atp_batch.Var(bv'); s]))) when bv' = bv && not (List.mem bv (Atp_batch.fvt s)) -> None
    | Atp_batch.Not(Atp_batch.Atom(Atp_batch.R("=", [s; Atp_batch.Var(bv')]))) when bv' = bv && not (List.mem bv (Atp_batch.fvt s)) -> None
    | p when List.mem bv (Atp_batch.fv p)->
    		  let _ = Format.printf "%a,%a@." Id.pr bv (Util.pr_list Fol.pr ", ") (List.map formula_of fms) in
    		  failwith "qelim_eq"
*)

(*
let rec flatten p =
  match p with
    Atp_batch.And(p1, p2) -> (flatten p1) @ (flatten p2)
  | _ -> [p]
*)

let rec qelim_ p =
  let bvs, p =
    let rec f bvs = function
      Exists(bv, p) ->
        let bvs, p = f bvs p in bv::bvs, p
      | p -> List.filter (fun bv -> List.mem bv (fv p)) bvs, p in
    f [] (pnf p) in
  let p =
    match p with
      Forall(bv, p) ->
        simplify (Not(qelim_ (Exists(bv, Not(p)))))
    | _ -> simplify p in
  let [], p = qelim bvs p in p

let minimize1 p1 p2 =
		let ps =
		  (List.map (fun p -> `L(p)) (List.map Atp_batch.list_disj (Atp_batch.simpcnf p1))) @
		  (List.map (fun p -> `R(p)) (List.map Atp_batch.list_conj (Atp_batch.simpdnf p2))) in
		let f ps = List.fold_left (fun p -> function `L(q) -> Atp_batch.Or(p, Atp_batch.Not(q)) | `R(q) -> Atp_batch.Or(p, q)) Atp_batch.False ps in
		let gl ps = List.fold_left (fun p -> function `L(q) -> Atp_batch.And(p, q) | `R(_) -> p) Atp_batch.True ps in
		let gr ps = List.fold_left (fun p -> function `L(_) -> p | `R(q) -> Atp_batch.Or(p, q)) Atp_batch.False ps in
		let rec loop ps qs =
		  if is_valid (f (ps @ qs)) then
		    (match ps with
		      p::ps' ->
		        (try loop ps' qs with Not_found -> loop ps' (p::qs))
		    | [] ->
		        let p = Atp_batch.simplify (gr qs) in
		        Fol.simplify (formula_of p))
		  else
		    raise Not_found
		in loop ps []

let minimize2 p1 p2 =
		let ps = List.map Atp_batch.list_disj (Atp_batch.simpcnf p1) in
		let rec loop ps qs =
		  if is_valid (Atp_batch.Imp(Atp_batch.list_conj (ps @ qs), p2)) then
		    (match ps with
		      p::ps' ->
		        (try loop ps' qs with Not_found -> loop ps' (p::qs))
		    | [] ->
          let p1' = Atp_batch.simplify (Atp_batch.list_conj qs) in
          let p2' = Atp_batch.simplify p2 in
		        if List.length qs = 1 && p1' <> Atp_batch.False then
            Fol.simplify (formula_of p1') (*this is too adhoc*)
          else Fol.simplify (formula_of p2'))
		  else
		    raise Not_found
		in loop ps []

let interpolate ids phi1 phi2 =
  let ids1 = List.unique (Util.diff (Fol.fv phi1) ids) in
  let ids2 = List.unique (Util.diff (Fol.fv phi2) ids) in
  assert (Util.disjoint ids1 ids2);
  let p1 = Atp_batch.simplify (of_formula (Fol.exists ids1 phi1)) in
  let p2 = Atp_batch.simplify (of_formula (Fol.forall ids2 phi2)) in
  let p1 = qelim_ p1 in
  let p2 = qelim_ p2 in
  if (*p1 = Atp_batch.False && p2 = Atp_batch.True*)false then
    (*Fol.simplify (formula_of p2)*) Fol.False (*can we justify this heuristics?*)
  else
    minimize2 p1 p2

(* integer arithmetics may not supported by Atp_batch.interpolate *)
(*
		  let phi1 = Fol.And(Fol.Const(Arith.Var("x"), Const.Equal, Arith.Var("z")), Fol.Const(Arith.Var("y"), Const.Equal, Arith.Var("z"))) in
		  let phi2 = Fol.Imply(Fol.Const(Arith.Var("x"), Const.Equal, Arith.Var("0")), Fol.Const(Arith.Var("y"), Const.Equal, Arith.Var("0"))) in
		  Format.printf "%a@." Fol.pr (interpolate phi1 phi2)
*)
(*let interpolate phi1 phi2 = formula_of (Atp_batch.interpolate (of_formula phi1) (of_formula phi2))*)
*)

let test () =
		let test1 _ =
		  let p = Atp_batch.interpolate
		    (Atp_batch.Atom(Atp_batch.R("=", [Atp_batch.Var("x"); Atp_batch.Fn("0", [])])))
		    (Atp_batch.Atom(Atp_batch.R(">=", [Atp_batch.Var("x"); Atp_batch.Fn("0", [])]))) in
		  Atp_batch.print_formula Atp_batch.print_atom p; print_newline()
  in

		let test2 _ =
		  let p = Atp_batch.Exists("x", Atp_batch.Atom(Atp_batch.R("=", [Atp_batch.Var("x"); Atp_batch.Fn("0", [])]))) in
		  let p = Atp_batch.integer_qelim p in
		  Atp_batch.print_formula Atp_batch.print_atom p; print_newline()
  in

		let test3 _ =
		  let p = Atp_batch.And(Atp_batch.Atom(Atp_batch.R("=", [Atp_batch.Var("y"); Atp_batch.Fn("0", [])])), Atp_batch.Atom(Atp_batch.R("=", [Atp_batch.Var("x"); Atp_batch.Fn("0", [])]))) in
		  let p = Atp_batch.cnf (Atp_batch.integer_qelim p) in
		  Atp_batch.print_formula Atp_batch.print_atom p; print_newline()
  in

		let test4 _ =
		  let p = Atp_batch.Atom(Atp_batch.R("=", [Atp_batch.Fn("-", [Atp_batch.Var("x"); Atp_batch.Var("y")]); Atp_batch.Fn("-", [Atp_batch.Var("x")])])) in
		  Atp_batch.print_formula Atp_batch.print_atom p; print_newline() in

		let test5 _ =
		  let p = Atp_batch.Atom(Atp_batch.R("=", [Atp_batch.Fn("-", [Atp_batch.Var("x"); Atp_batch.Var("x")]); Atp_batch.Fn("1", [])])) in
		  let _ = Atp_batch.print_formula Atp_batch.print_atom p; print_newline() in
    assert (is_valid p)
  in

		let test6 _ =
		  let p = Atp_batch.Imp(Atp_batch.Atom(Atp_batch.R("=", [Atp_batch.Fn("false", []); Atp_batch.Fn("true", [])])), Atp_batch.Atom(Atp_batch.R("=", [Atp_batch.Fn("true", []); Atp_batch.Fn("false", [])]))) in
		  let _ = Atp_batch.print_formula Atp_batch.print_atom p; print_newline() in
    assert (is_valid p)
  in

		let test7 _ =
		  let p = Atp_batch.Exists("x", Atp_batch.And(Atp_batch.Atom(Atp_batch.R("=", [Atp_batch.Var("x"); Atp_batch.Var("y")])), Atp_batch.And(Atp_batch.Atom(Atp_batch.R("=", [Atp_batch.Var("y"); Atp_batch.Fn("0", [])])), Atp_batch.Atom(Atp_batch.R("=", [Atp_batch.Var("y"); Atp_batch.Fn("0", [])]))))) in
		  let _ = Atp_batch.print_formula Atp_batch.print_atom p; print_newline() in
		  let p = Atp_batch.integer_qelim p in
		  Atp_batch.print_formula Atp_batch.print_atom p; print_newline()
  in
  ()
