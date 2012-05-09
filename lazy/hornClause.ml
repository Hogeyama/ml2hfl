open ExtList
open ExtString

(** Horn clauses *)

(** Horn clause
    @assume an expression that contains a coefficient variables never occurs in an argument of a predicate*)
type t = Hc of (Var.t * Var.t list) option * Pred.t list * Term.t

let pr ppf (Hc(popt, ps, t)) =
		let _ = Format.fprintf ppf "@[<hov>" in
		let _ = if ps <> [] then Format.fprintf ppf "%a,@ " (Util.pr_list Pred.pr ",@ ") ps in
		let _ = Format.fprintf ppf "%a@ " Term.pr t in
  match popt with
    None ->
      Format.fprintf ppf "|- bot@]"
  | Some(pid, xs) ->
      Format.fprintf ppf "|- %a@]" Pred.pr (Pred.make pid xs)

(** require: variables that popt depend do not occur in the domain of sub *)
let subst sub (Hc(popt, ps, t)) =
  Hc(popt, List.map (Pred.subst sub) ps, Term.subst sub t)

let fvs (Hc(popt, ps, t)) =
  Util.diff
    (List.unique (Util.concat_map Pred.fvs ps @ Term.fvs t))
    (match popt with None -> [] | Some(_, xs) -> xs)

let coeffs (Hc(popt, ps, t)) = List.unique (Util.concat_map Pred.coeffs ps @ Term.coeffs t)

let get_lhs_pids hcs =
  Util.concat_map
    (fun (Hc(_, ps, _)) ->
      List.map fst ps)
    hcs

let alpha (Hc(popt, ps, t) as hc) =
  let fvs = fvs hc in
  let sub = List.map (fun x -> x, Term.make_var (Var.new_var ())) fvs in
  Hc(popt,
    List.map (Pred.subst (fun x -> List.assoc x sub)) ps,
    Term.subst (fun x -> List.assoc x sub) t)

let lookup_hcs (pid, ts) hcs =
		match List.find_all (function Hc(Some(pid', _), _, _) -> pid = pid' | _ -> false) hcs with
    [Hc(Some(_, xs), _, _) as hc] ->
						let Hc(_, ps, t) = alpha hc in
						let sub = List.combine xs ts in
						List.map (Pred.subst (fun x -> List.assoc x sub)) ps,
						Term.subst (fun x -> List.assoc x sub) t
  | [] -> raise Not_found
  | _ -> assert false

let rec subst_formula p ps t =
  (*Format.printf "input: %a@." Term.pr t;*)
  let ts = Formula.conjuncts t in
  let xttys, t = Tsubst.extract_from2 (Util.concat_map Pred.fvs ps) p ts in
  (*Format.printf "xttys: %a, t: %a@." Tsubst.pr xttys Term.pr t;*)
  let ps, t =
		  if xttys = [] then
		    ps, t
		  else
		    let sub = Tsubst.fun_of xttys in
		    subst_formula p (List.map (Pred.subst_fixed sub) ps) (Term.subst_fixed sub t)
  in
  (*Format.printf "output: %a@." Term.pr t;*)
  ps, t

let simplify (Hc(popt, ps, t)) =
  let _ = Global.log_begin "HornClause.simplify" in
  let _ = Global.log (fun () -> Format.printf "input:@,  @[<v>%a@]@," pr (Hc(popt, ps, t))) in
  let bvs = (match popt with None -> [] | Some(_, xs) -> xs) in
  let ps, t =
    let _ = Global.log_begin "simplifying formula" in
    let _ = Global.log (fun () -> Format.printf "input:@,  @[<v>%a@]@," Term.pr t) in
    let ps, t =
		    let sub, t =
		      Tsubst.extract_from
		        (match popt with None -> [] | Some(pid, _) -> [pid])
		        (fun x -> List.mem x bvs || Var.is_coeff x) t
		    in
      List.map (Pred.subst sub) ps, Term.subst sub t
    in
    let t =
      let xs = List.unique (bvs @ Util.concat_map Pred.fvs ps) in
      AtpInterface.simplify2 (fun x -> List.mem x xs || Var.is_coeff x) t
      (*
      let t = Term.simplify (AtpInterface.qelim_fes (diff bvs (fvs ps)) t) in
      *)
    in
    let ps, t = subst_formula (fun x -> List.mem x bvs || Var.is_coeff x) ps t in
    let _ = Global.log (fun () -> Format.printf "output:@,  @[<v>%a@]" Term.pr t) in
    let _ = Global.log_end "simplifying formula" in
    List.unique ps, t
  in
  let ps, t =
    let ps = List.map Pred.simplify ps in
    let t = Formula.simplify t in
    (* ToDo: make the following simplification procedure more scalable *)
    let xttys =
      let pss = Util.classify (fun (pid1, _) (pid2, _) -> pid1 = pid2) ps in
      List.unique
		      (Util.concat_map
		        (fun ps ->
            List.flatten
				          (Util.multiply_list
				            (fun (_, ts1) (_, ts2) ->
				              Util.concat_map
				                (fun (t1, t2) ->
                      try
                        let nxs, n' = LinArith.of_term t1 in
                        match nxs with
                          [n, x] when n = 1 && not (List.mem x bvs) && t1 <> t2 ->
  		                        [x, LinArith.simplify (Term.sub t2 (Term.tint n')), SimType.Int(*???*)]
                        | _ ->
                            raise (Invalid_argument "")
                      with Invalid_argument _ ->
                        [])
				                (List.combine ts1 ts2))
				            ps ps))
		        pss)
    in
    let _ = Global.log (fun () -> Format.printf "xttys: %a@," Tsubst.pr xttys) in
    let xs = List.unique (List.map Util.fst3 xttys) in
    let _ = Global.log (fun () -> Format.printf "xs: %a@," (Util.pr_list Var.pr ",") xs) in
    let rec aux ps t xss =
      match xss with
        [] ->
          ps, t
      | xs::xss' ->
          let xttyss =
            if xs = [] then
              []
            else
              Util.multiply_list_list
                (fun xttys1 xttys2 -> xttys1 @ xttys2)
                (List.map
                  (fun x ->
                    List.filter_map
                      (fun (x', t, ty) -> if x = x' then Some([x, t, ty]) else None)
                      xttys)
                  xs)
          in
          let xttyss =
            List.filter
              (fun xttys ->
                Util.inter
                  (List.map Util.fst3 xttys)
                  (Util.concat_map (fun (_, t, _) -> Term.fvs t) xttys)
                = [])
              xttyss
          in
          if xttyss = [] then
            aux ps t xss'
          else
		          let ts = Formula.conjuncts t in
            let ps1, ps2 = List.partition (fun p -> Util.inter (Pred.fvs p) xs <> []) ps in
            let ts1, ts2 = List.partition (fun t -> Util.inter (Term.fvs t) xs <> []) ts in
            (try
              let ps, t =
		              Util.find_map
																  (fun xttys ->
                    (*let _ = Global.log (fun () -> Format.printf "sub: %a@," Tsubst.pr xttys) in*)
                    let ps1 = List.map (Pred.subst (Tsubst.fun_of xttys)) ps1 in
                    let ts1 = List.map (fun t -> Formula.simplify (Term.subst (Tsubst.fun_of xttys) t)) ts1 in
                    if Util.subset ps1 ps2 (*???*) &&
                       Cvc3Interface.implies ts2 ts1 then
                      ps2, Formula.band ts2
                    else
                      raise Not_found)
		                xttyss
              in
              aux ps t (List.filter (fun xs' -> Util.inter xs' xs = []) xss')
            with Not_found ->
              aux ps t xss')
    in
    aux ps t
      (*(List.sort ~cmp:(fun xs ys -> List.length xs - List.length ys) (Util.power xs))*)
      (Util.pick 1 xs @ Util.pick 2 xs @ Util.pick 3 xs @ Util.pick 4 xs)
  in
  let res = Hc(popt, ps, t) in
  let _ = Global.log (fun () -> Format.printf "output:@,  @[<v>%a@]" pr res) in
  let _ = Global.log_end "HornClause.simplify" in
  res


let subst_hcs hcs (Hc(popt, ps, t) as hc) =
  let _ = Global.log_begin "subst_hcs" in
  let _ = Global.log (fun () -> Format.printf "input:@,  @[<v>%a@]@," pr hc) in
  let hc =
		  if true then
      let rec aux ps t =
        try
		        let lps, (ps, t'), rps =
				        Util.find_split_map
				          (fun (pid, ts) ->
																let res = lookup_hcs (pid, ts) hcs in
												    let _ = Global.log (fun () -> Format.printf "%a is being substituted@," Var.pr pid) in
				            res)
				          ps
		        in
		        let Hc(_, ps, t) = simplify (Hc(popt, lps @ rps @ ps, Formula.band [t; t'])) in
		        aux ps t
        with Not_found ->
          Hc(popt, ps, t)
      in
      aux ps t
		  else
				  let ps, t =
				    let pss, ts =
						    List.split
										  (List.map
										    (fun (pid, ts) ->
						          try
														    let res = lookup_hcs (pid, ts) hcs in
						            let _ = Global.log (fun () -> Format.printf "%a is being substituted@," Var.pr pid) in
						            res
						          with Not_found ->
						            [pid, ts], Formula.ttrue)
										    ps)
				    in
				    List.flatten pss, Formula.band (t::ts)
				  in
		    simplify (Hc(popt, ps, t))
  in
  let _ = Global.log (fun () -> Format.printf "output:@,  @[<v>%a@]" pr hc) in
  let _ = Global.log_end "subst_hcs" in
  hc

let subst_hcs_fixed hcs hc =
		Util.fixed_point
				(fun hc ->
		    (*Format.printf "%a@." pr hc;*)
		    subst_hcs hcs hc)
				(fun hc1 hc2 ->
		    match hc1, hc2 with
		      Hc(_, ps1, _), Hc(_, ps2, _) -> Util.set_equiv ps1 ps2)
		  hc
(*
let rec fixpoint hcs =
  match hcs with
    [] -> []
  | hc::hcs' ->
      let hc' = subst_hcs_fixed hcs' hc in
      hc' :: fixpoint (List.map (subst_hcs [hc']) hcs')
  
let subst_hcs_fixed hcs hc =
  subst_hcs (fixpoint hcs) hc
*)
