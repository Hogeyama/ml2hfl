open ExtList
open ExtString

(** Horn clauses *)

(** Horn clause
    @assume an expression containing a coefficient variables never occurs in an argument of a predicate*)
type t = Hc of (Var.t * (Var.t * SimType.t) list) option * Pred.t list * Term.t

let pr ppf (Hc(popt, ps, t)) =
		let _ = Format.fprintf ppf "@[<hov>" in
		let _ =
    if ps <> [] then
      Format.fprintf ppf "%a,@ " (Util.pr_list Pred.pr ",@ ") ps
  in
		let _ = Format.fprintf ppf "%a@ " Term.pr t in
  match popt with
    None ->
      Format.fprintf ppf "|- bot@]"
  | Some(pid, xtys) ->
      Format.fprintf ppf "|- %a@]"
        Pred.pr
        (Pred.make pid (List.map (fun (x, ty) -> Term.make_var x, ty) xtys))

(** @require dom(sub) and fv(popt) do not intersect *)
let subst sub (Hc(popt, ps, t)) =
  Hc(popt, List.map (Pred.subst sub) ps, Term.subst sub t)

let fvs (Hc(popt, ps, t)) =
  Util.diff
    (List.unique (Util.concat_map Pred.fvs ps @ Term.fvs t))
    (match popt with None -> [] | Some(_, xtys) -> List.map fst xtys)

let coeffs (Hc(popt, ps, t)) =
  List.unique (Util.concat_map Pred.coeffs ps @ Term.coeffs t)

let lhs_pids hcs =
  Util.concat_map
    (fun (Hc(_, ps, _)) ->
      List.map fst ps)
    hcs

let rhs_pids hcs =
  Util.concat_map
    (function
				  Hc(None, _, _) -> []
				| Hc(Some(pid, _), _, _) -> [pid])
    hcs

let is_root (Hc(popt, _, _)) = popt = None
let is_coeff (Hc(popt, _, _)) =
  match popt with
    None -> false
  | Some(pid, _) -> Var.is_coeff pid

let is_non_disjunctive hcs =
  let popts = List.map (function Hc(None, _, _) -> None | Hc(Some(pid, _), _, _) -> Some(pid)) hcs in
  not (Util.is_dup popts)

let is_well_defined hcs =
  Util.subset (lhs_pids hcs) (rhs_pids hcs)

(** alpha renaming *)
let alpha (Hc(popt, ps, t) as hc) =
  let fvs = fvs hc in
  let sub = List.map (fun x -> x, Term.make_var (Var.new_var ())) fvs in
  Hc(popt,
    List.map (Pred.subst (fun x -> List.assoc x sub)) ps,
    Term.subst (fun x -> List.assoc x sub) t)

let lookup_hcs (pid, ttys) hcs =
  let hcs' =
    List.find_all
      (function Hc(Some(pid', _), _, _) -> pid = pid' | _ -> false)
						hcs
  in
		match hcs' with
    [Hc(Some(_, xtys), _, _) as hc] ->
						let Hc(_, ps, t) = alpha hc in
						let sub = List.combine (List.map fst xtys) (List.map fst ttys) in
						List.map (Pred.subst (fun x -> List.assoc x sub)) ps,
						Term.subst (fun x -> List.assoc x sub) t
  | [] -> raise Not_found
  | _ -> assert false

(** unsound for non-linear expressions? *)
let rec subst_formula p ps t =
  (*if Term.coeffs t <> [] then
    ps, t
  else*)
		(*Format.printf "input: %a@," Term.pr t;*)
		let ts = Formula.conjuncts t in
		let xttys, t = Formula.extract_from2 (Util.concat_map Pred.fvs ps) p ts in
		(*Format.printf "xttys: %a@,t: %a@," TypSubst.pr xttys Term.pr t;*)
		let ps, t =
				if xttys = [] then
				  ps, t
				else
				  let sub = TypSubst.fun_of xttys in
				  subst_formula p
						  (List.map (Pred.subst_fixed sub) ps)
								(Term.subst_fixed sub t)
		in
		(*Format.printf "output: %a@," Term.pr t;*)
		ps, t

let matches env xs ttys1 ttys2 =
  let xttys =
				(*try*)
						Util.concat_map2
						  (fun (t1, ty1) (t2, ty2) ->
								  let _ = if !Global.debug then assert (ty1 = ty2) in
						    if t1 = t2 then
										  []
										else if Util.inter (Term.fvs t1) xs = [] then
										  if Cvc3Interface.implies env [Formula.eq_ty ty1 t1 t2] then
				  						  []
												else
												  let _ = Format.printf "t1: %a@,t2: %a@," Term.pr t1 Term.pr t2 in
												  assert false
										else
												match t1 with
												  Term.Var(_, x) when List.mem x xs ->
												    [x, t2, ty1]
												| _ ->
																(try
																  let nxs, n' = LinArith.of_term t1 in
																  match nxs with
																    [n, x] when n = 1 && List.mem x xs ->
						                [x, LinArith.simplify (Term.sub t2 (Term.tint n')), ty1]
																  | _ ->
																      raise (Invalid_argument "")
																with Invalid_argument _ ->
																  let _ = if !Global.debug then Format.printf "??t1: %a@,??t2: %a@," Term.pr t1 Term.pr t2 in
																		[](*raise Not_found*)(*assert false*)))
								ttys1 ttys2
						(*with Not_found ->
						  []*)
		in
		let _ =
				if !Global.debug then
		  		let xttys = List.unique xttys in
						assert
								(List.for_all
								   (fun xttys ->
													match xttys with
															[] -> assert false
													| (_, t, ty)::xttys ->
															  List.for_all (fun (_, t', _) -> Cvc3Interface.implies env [Formula.eq_ty ty t t']) xttys)
								   (Util.classify (fun (x, _, _) (y, _, _) -> x = y) xttys))
		in
		xttys


let xttyss_of env q ps1 ps2 =
  try
		  let ttys_tss_s =
				  List.map
						  (fun p1 ->
								  snd p1,
										let ps =
												List.filter_map
												  (fun p2 ->
														  if Pred.matches q env p2 p1 then
																  Some(List.map fst (snd p2))
															 else
																  None)
														ps2
										in
										if ps = [] then raise Not_found else ps)
								ps1
				in
				let xttyss =
						Util.multiply_list_list
						  (fun xttys1 xttys2 -> xttys1 @ xttys2)
						  (List.map
						    (fun (ttys, tss) ->
						      List.map
						        (fun ts ->
														  Util.concat_map2
																  (fun (t1, ty) t2 ->
						              if t1 = t2 then
																				  []
																				else
																						match t1 with
																						  Term.Var(_, x) when q x ->
																						    [x, t2, ty]
																						| _ ->
																										(try
																										  let nxs, n' = LinArith.of_term t1 in
																										  match nxs with
																										    [n, x] when n = 1 && q x ->
																                [x, LinArith.simplify (Term.sub t2 (Term.tint n')), ty]
																										  | _ ->
																										      raise (Invalid_argument "")
																										with Invalid_argument _ ->
																										  []))
																		ttys ts)
										  tss)
								  ttys_tss_s)
				in
				let xttyss =
				  List.filter
						  (fun xttys ->
								  let xttys = List.unique xttys in
										if List.for_all
										     (fun xttys ->
																	match xttys with
																	  [] -> assert false
																	| (_, t, ty)::xttys ->
																	    List.for_all (fun (_, t', _) -> Cvc3Interface.implies env [Formula.eq_ty ty t t']) xttys)
										     (Util.classify (fun (x, _, _) (y, _, _) -> x = y) xttys) then
										  true
										else
								    (*let _ = Format.printf "duplicate: %a@," TypSubst.pr xttys in*)
										  false)
						  xttyss
				in
				xttyss
  with Not_found ->
		  []

let dup_num ps =
		let pss = Util.classify (fun (pid1, _) (pid2, _) -> pid1 = pid2) ps in
  List.fold_left (+) 0 (List.map (fun ps -> List.length ps - 1) pss)

let ignored_vars bvs ps =
  let tss =
    let pss = Util.classify (fun (pid1, _) (pid2, _) -> pid1 = pid2) ps in
    Util.concat_map
      (fun ps -> Util.transpose (List.map (fun p -> List.map fst (snd p)) ps))
      pss
  in
		let xs =
				Util.diff
		    (List.unique
		      (Util.concat_map
		        (fun ts ->
										  Util.redundant (Util.concat_map (fun t -> List.unique (Term.fvs t)) ts)
												(*Util.concat_map Term.fvs (Util.redundant ts)*))
		        tss))
		    bvs
  in
		let _ = if !Global.debug then Format.printf "xs: %a@," (Util.pr_list Var.pr ",") xs in
		let ys =
		  List.unique
				  (Util.concat_map
						  (fun (_, ttys) ->
								  List.flatten
										  (Util.map_left_right
												  (fun ttys1 (t, _) ttys2 ->
														  let zs = List.unique (Term.fvs t) in
																if List.length zs > 1 then
																  Util.diff zs (xs @ (Util.concat_map (fun (t, _) -> Term.fvs t) (*???*)(ttys1 @ ttys2)))
																else
																  [])
														ttys))
						  ps)
		in
		let _ = if !Global.debug then Format.printf "ys: %a@," (Util.pr_list Var.pr ",") ys in
		xs @ ys

let changing_vars bvs ps =
  let tss =
    let pss = Util.classify (fun (pid1, _) (pid2, _) -> pid1 = pid2) ps in
    Util.concat_map
      (fun ps -> Util.transpose (List.map (fun p -> List.map fst (snd p)) ps))
      pss
  in
		Util.diff
    (List.unique
      (Util.concat_map
        (fun ts -> let ts = List.unique ts in match ts with [_] -> [] | _ -> Util.concat_map Term.fvs ts)
        tss))
    bvs
