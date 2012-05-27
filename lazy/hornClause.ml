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

(** require: variables that popt depend do not occur in the domain of sub *)
let subst sub (Hc(popt, ps, t)) =
  Hc(popt, List.map (Pred.subst sub) ps, Term.subst sub t)

let fvs (Hc(popt, ps, t)) =
  Util.diff
    (List.unique (Util.concat_map Pred.fvs ps @ Term.fvs t))
    (match popt with None -> [] | Some(_, xtys) -> List.map fst xtys)

let coeffs (Hc(popt, ps, t)) =
  List.unique (Util.concat_map Pred.coeffs ps @ Term.coeffs t)

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
		(*Format.printf "input: %a@," Term.pr t;*)
		let ts = Formula.conjuncts t in
		let xttys, t = Tsubst.extract_from2 (Util.concat_map Pred.fvs ps) p ts in
		(*Format.printf "xttys: %a@,t: %a@," Tsubst.pr xttys Term.pr t;*)
		let ps, t =
				if xttys = [] then
				  ps, t
				else
				  let sub = Tsubst.fun_of xttys in
				  subst_formula p
						  (List.map (Pred.subst_fixed sub) ps)
								(Term.subst_fixed sub t)
		in
		(*Format.printf "output: %a@," Term.pr t;*)
		ps, t

let subst_of env p ps1 ps2 x =
  let flag = true in
  List.unique
				(Util.concat_map
				  (fun p1 ->
				    let tss =
          List.filter_map
            (fun p2 ->
              if Pred.matches p env p2 p1 then
                Some(List.map fst (snd p2))
              else
                None)
            ps2
        in
				    List.concat
				      (List.mapi
				        (fun i (t, ty) ->
				          if List.mem x (Term.fvs t) then
				            let ts = List.map (fun ts -> List.nth ts i) tss in
                let ts = List.filter (fun t' -> t <> t') ts in
																match t with
																  Term.Var(_, x) ->
																    List.map (fun t -> x, t, ty) ts
																| _ ->
																				(try
																				  let nxs, n' = LinArith.of_term t in
																				  match nxs with
																				    [n, x] when n = 1 ->
																				      List.map
                            (fun t ->
                              x,
                              LinArith.simplify (Term.sub t (Term.tint n')),
                              ty)
                            ts
																				  | _ ->
																				      raise (Invalid_argument "")
																				with Invalid_argument _ ->
																				  [])
				          else
				            [])
				        (snd p1)))
				  ps1)

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
		Util.diff
    (List.unique
      (Util.concat_map
        (fun ts -> Util.concat_map Term.fvs (Util.redundant ts))
        tss))
    bvs
						(*let ts =
								Util.concat_map
								  (fun ((pid1, ts1) as p) ->
								    Util.concat_map
								      (fun (pid2, ts2) ->
								        if pid1 = pid2 then
								          Util.filter_map2
				                (fun t1 t2 -> if t1 = t2 then Some(t1) else None)
				                ts1 ts2
								        else
				              [])
								      (List.filter (fun p' -> p <> p') ps))
								  ps
						in
						Util.diff (List.unique (Util.concat_map Term.fvs ts)) bvs*)

let share_predicates bvs _ ps t =
		let t = Formula.simplify t in
		if Term.coeffs t <> [] || dup_num ps = 0 then
		  ps, t
  else
		  let share_predicates_aux bvs ps t =
						let ts = Formula.conjuncts t in
		    let rec rel xs1 xs2 =
		      match xs1, xs2 with
						    `L((pid1, ttys1) as p1), `L((pid2, ttys2) as p2) ->
				        if pid1 <> pid2 then
				          let fvs1 = Util.diff (Pred.fvs p1) bvs in
				          let fvs2 = Util.diff (Pred.fvs p2) bvs in
				          List.exists (fun x -> List.mem x fvs2) fvs1
				        else
		            let ts, tts =
                Util.partition_map
                  (fun ((t1, _), (t2, _)) -> if t1 = t2 then `L(t1) else `R(t1, t2))
                  (List.combine ttys1 ttys2)
              in
		            let ts1, ts2 = List.split tts in
						        let fvs1 = Util.diff (Util.concat_map (fun (t, _) -> Term.fvs t) ttys1) bvs in
						        let fvs2 = Util.diff (Util.concat_map (fun (t, _) -> Term.fvs t) ttys2) bvs in
		            if Util.inter (Util.concat_map Term.fvs ts) (fvs1 @ fvs2) = [] then
						          List.exists (fun x -> List.mem x fvs2) fvs1
		            else
		              let _ = Format.printf "ts: %a@,p1: %a@,p2: %a@," (Util.pr_list Term.pr ",") ts Pred.pr p1 Pred.pr p2 in
		              assert false
						  | `L(p1), `R(t2) ->
				        let fvs1 = Util.diff (Pred.fvs p1) bvs in
				        let fvs2 = Util.diff (Term.fvs t2) bvs in
				        List.exists (fun x -> List.mem x fvs2) fvs1
						  | `R(t1), `L(p2) ->
				        rel (`L(p2)) (`R(t1))
						  | `R(t1), `R(t2) ->
				        let fvs1 = Util.diff (Term.fvs t1) bvs in
				        let fvs2 = Util.diff (Term.fvs t2) bvs in
				        List.exists (fun x -> List.mem x fvs2) fvs1
		    in
		    let ecs, env =
				    let env, ts1 = List.partition (fun t -> Util.subset (Term.fvs t) bvs) ts in
				    let ps0, ps1 = List.partition (fun p -> Util.subset (Pred.fvs p) bvs) ps in
				    let ps0 =
				      Util.concat_map
				        (Util.representatives (Pred.equiv env))
				        (Util.classify (fun (pid1, _) (pid2, _) -> pid1 = pid2) ps0)
				    in
        (if ps0 = [] then [] else [List.map (fun p -> `L(p)) ps0]) @
        Util.equiv_classes rel (List.map (fun p -> `L(p)) ps1 @ List.map (fun t -> `R(t)) ts1),
        env
      in
		    let _ =
		      Format.printf "bvs: %a@," (Util.pr_list Var.pr ",") bvs;
		      Format.printf "env: %a@," Term.pr (Formula.band env);
		      List.iter
		        (fun ec ->
		          let ps, ts = Util.partition_map (fun x -> x) ec in
		          Format.printf "ec: %a@," pr (Hc(None, ps, Formula.band ts)))
		        ecs
		    in
		    let reduce ec1 ec2 =
		      let xs = List.sort (Util.diff (List.unique (Util.concat_map (function `L(p) -> Pred.fvs p | `R(t) -> Term.fvs t) ec1)) bvs) in
        if xs = [] then
          ec1
        else
				    		(*let _ = Global.log (fun () -> Format.printf "xs: %a@," (Util.pr_list Var.pr ",") xs) in*)
						    let xttyss =
				        let ps1 = List.filter_map (function `L(p) -> Some(p) | `R(_) -> None) ec1 in
				        let ps2 = List.filter_map (function `L(p) -> Some(p) | `R(_) -> None) ec2 in
								    Util.multiply_list_list
								      (fun xttys1 xttys2 -> xttys1 @ xttys2)
						        (List.map
				            (fun x ->
				              let ys = (*flag must be false*)subst_of env (fun x -> not (List.mem x bvs)) ps1 ps2 x in
				              if ys = [] then [[]](*???*) else List.map (fun x -> [x]) ys)
				            xs)
						    in
				      if List.exists
				           (fun xttys ->
				             let b =
																			let ec1' =
						               List.map
						                 (function
						                   `L(p) -> `L(Pred.simplify (Pred.subst (Tsubst.fun_of xttys) p))
						                 | `R(t) -> `R(Formula.simplify (Term.subst (Tsubst.fun_of xttys) t)))
						                 ec1
						             in
		  		             let ps1, ts1 = Util.partition_map (fun x -> x) (Util.diff ec1' ec2) in
		                 let ps2, ts2 = Util.partition_map (fun x -> x) ec2 in
				               List.for_all
		                   (fun p1 -> List.exists (fun p2 -> Pred.equiv env p1 p2) ps2)
		                   ps1 &&
				               Cvc3Interface.implies (env @ ts2) ts1
				             in
				             (if b then Format.printf "xttys: %a@," Tsubst.pr xttys);
				             b)
				           xttyss then
				        []
				      else
				        ec1
		    in
		    let rec aux ecs1 ecs2 =
		      match ecs1 with
		        [] -> ecs2
		      | ec::ecs1 ->
            (*let covers ec1 ec2 =
				          Util.subset
                (Util.concat_map (function `L(p) -> [fst p] | `R(_) -> []) ec1)
		              (Util.concat_map (function `L(p) -> [fst p] | `R(_) -> []) ec2)
            in*)
            let min_coverings ec ecs =
              let pids_of_ec ec = List.unique (Util.concat_map (function `L(pid, _) -> [pid] | `R(_) -> []) ec) in
              let pids0 = pids_of_ec ec in
              let ec_pids_s = List.map (fun ec -> ec, pids_of_ec ec) ecs in
              let ec_pids_s = List.filter (fun (_, pids) -> List.exists (fun pid -> List.mem pid pids) pids0) ec_pids_s in
              let ec_pids_s1, ec_pids_s2 = List.partition (fun (ec, pids) -> Util.subset pids0 pids) ec_pids_s in
              let rec aux ec_pids_b_s =
                let ec_pids_b_s' =
                  Util.concat_map
                    (fun (ec, pids, b) ->
                      if b then
                        [ec, pids, b]
                      else
                        let insf_pids = Util.diff pids0 pids in
                        List.map
                          (fun (ec', pids') ->
                            ec @ ec', List.unique (pids @ pids'), Util.subset insf_pids pids')
                          (List.filter
                            (fun (_, pids) ->
                              List.exists (fun pid -> List.mem pid pids) insf_pids)
                            ec_pids_s2))
																				ec_pids_b_s
																in
                if List.length ec_pids_b_s = List.length ec_pids_b_s' then
                  List.filter_map (fun (ec, pids, b) -> if b then Some(ec) else None) ec_pids_b_s'
                else
                  aux ec_pids_b_s'
              in
              List.map fst ec_pids_s1 @ aux (List.map (fun (ec, pids) -> ec, pids, false) ec_pids_s2)
            in
		          let ec' =
								      List.fold_left
								        (fun ec1 ec2 ->
		                if ec1 = [] then
		                  []
		                else
		                  reduce ec1 ec2)
								        ec
								        (min_coverings ec (ecs1 @ ecs2))
		          in
		          aux ecs1 (if ec' = [] then ecs2 else ec' :: ecs2)
		    in
		    let ecs = aux (List.sort ecs) [] in
		    let ps, ts = Util.partition_map (fun x -> x) (List.flatten ecs) in
		    ps, Formula.band (ts @ env)
		  in
    let rec loop bvs' ps t =
		    let _ = if bvs' <> [] then Format.printf "bvs': %a@," (Util.pr_list Var.pr ",") bvs' in
      let ps', t' = share_predicates_aux (bvs @ bvs') ps t in
      if List.length ps <> List.length ps' && dup_num ps' > 0 then
				    let bvs'' = ignored_vars bvs ps' in
        if Util.set_equiv bvs' bvs'' then
          ps', t'
        else
          loop bvs'' ps' t'
      else
        ps', t'
    in
    loop (ignored_vars bvs ps) ps t
(*
								let bvs'' =
				      let tss =
				    		  let pss = Util.classify (fun (pid1, _) (pid2, _) -> pid1 = pid2) ps in
				        Util.concat_map (fun ps -> Util.transpose (List.map snd ps)) pss
				      in
		        let pxss, nxss = List.split (List.map (fun ts -> let ts = List.unique ts in match ts with [t] -> Term.fvs t, [] | _ -> [], Util.concat_map Term.fvs ts) tss) in
										Util.diff (List.unique (Util.diff (List.flatten pxss) (List.flatten nxss))) bvs
		      in
				    let _ = Format.printf "bvs'': %a@," (Util.pr_list Var.pr ",") bvs'' in
				    loop (bvs @ bvs'') ps t
*)

let simplify_aux bs (Hc(popt, ps, t)) =
  let _ = Global.log_begin "HornClause.simplify" in
  let _ = Global.log (fun () -> Format.printf "input:@,  @[<v>%a@]@," pr (Hc(popt, ps, t))) in
  let shared = ref (List.length ps) in
  let bvs = (match popt with None -> [] | Some(_, xtys) -> List.map fst xtys) in
  let bs, ps, t =
    (*let _ = Global.log_begin "simplifying formula" in
    let _ = Global.log (fun () -> Format.printf "input:@,  @[<v>%a@]@," Term.pr t) in*)
    let ps, t =
		    let sub, t =
		      Tsubst.extract_from
		        (match popt with None -> [] | Some(pid, _) -> [pid])
		        (fun x -> List.mem x bvs || Var.is_coeff x) t
		    in
      List.map (Pred.subst sub) ps, Term.subst sub t
    in
    (*let _ = Global.log (fun () -> Format.printf "a:@,  @[<v>%a@]@," Term.pr t) in*)
    let t =
      let xs = List.unique (bvs @ Util.concat_map Pred.fvs ps) in
      AtpInterface.simplify2 (fun x -> List.mem x xs || Var.is_coeff x) t
      (*
      let t = Term.simplify (AtpInterface.qelim_fes (diff bvs (fvs ps)) t) in
      *)
    in
    (*let _ = Global.log (fun () -> Format.printf "b:@,  @[<v>%a@]@," Term.pr t) in*)
    let ps, t =
				  (*let rec aux ts1 ts2 =
						  match ts1 with
						    [] -> Formula.band ts2
						  | t::ts' ->
				        if Cvc3Interface.implies (ts' @ ts2) [t] then
				          aux ts' ts2
				        else
				          aux ts' (t::ts2)
				  in
				  if true then aux (Formula.conjuncts t) [] else t*)
      let sub = List.filter_map (fun t -> try Some(Formula.xtty_of (fun x -> not (List.mem x bvs)) [] t) with Not_found -> None) (Formula.conjuncts t) in
      let sub = List.filter (fun (_, t, ty) -> ty = SimType.Bool && (t = Formula.ttrue || t = Formula.tfalse) (*|| ty = SimType.Int && Term.is_int_const t*)) sub in
      let t0 =
        Formula.simplify
          (Formula.band
            (Term.subst (Tsubst.fun_of sub) t ::
            List.map
              (fun (x, t, _) -> if t = Formula.ttrue then Term.make_var x else if t = Formula.tfalse then Formula.bnot (Term.make_var x) else (*if Term.is_int_const t then Formula.eqInt (Term.make_var x) t else*) assert false)
              sub))
      in
      let ps0 = ps(*List.map (Pred.subst (Tsubst.fun_of sub)) ps*) in
      (*let _ = Format.printf "a:%a@," Term.pr t0 in*)
      ps0,
      if Term.fvs_ty SimType.Bool t0 SimType.Bool = [] then
        let t' = Formula.elim_eq_neq_boolean t0 in
        (*let _ = Format.printf "b:%a@," Term.pr t' in*)
        let t' = Formula.elim_imply_iff t' in
        (*let _ = Format.printf "c:%a@," Term.pr t' in*)
        if t0 <> t' then
          let t' = Formula.formula_of_dnf (Formula.dnf t') in
          (*let _ = Format.printf "d:%a@," Term.pr t' in*)
          let t' = Formula.simplify t' in
          let _ = Format.printf "boolean equalities eliminated:@,  @[<v>before: %a@,after: %a@]@," Term.pr t Term.pr t' in
          t'
        else
          t0
      else
        t0
    in
    (*let _ = Global.log (fun () -> Format.printf "c:@,  @[<v>%a@]@," Term.pr t) in*)
    let ps, t = subst_formula (fun x -> List.mem x bvs || Var.is_coeff x) ps t in
    (*let _ = Global.log (fun () -> Format.printf "output:@,  @[<v>%a@]" Term.pr t) in
    let _ = Global.log_end "simplifying formula" in*)
    let ps = List.map Pred.simplify ps in
    let rec unique bs ps =
      match bs, ps with
         [], [] ->
           [], []
      |  b::bs, p::ps ->
           if List.mem p ps then
             let bs, ps = List.split (Util.filter_map2 (fun b p' -> if p <> p' then Some(b, p') else None) bs ps) in
             let bs, ps = unique bs ps in
             false(*??*) :: bs, p :: ps
           else
             let bs, ps = unique bs ps in
             b :: bs, p :: ps
    in
    let bs, ps = unique bs ps in
    bs, ps, t
  in
  let ps, t = share_predicates bvs bs ps t in
  let res = Hc(popt, ps, t) in
  let _ =
    let _ = shared := !shared - List.length ps in
    Global.log (fun () -> if !shared <> 0 then Format.printf "# of shared predicate variables: %d@," !shared)
  in
  let _ =
    let n = dup_num ps in
    Global.log (fun () -> if n <> 0 then Format.printf "# of duplicate predicate variables: %d@," n)
  in
  let _ = Global.log (fun () -> Format.printf "output:@,  @[<v>%a@]" pr res) in
  let _ = Global.log_end "HornClause.simplify" in
  res

let simplify (Hc(_, ps, _) as hc) = simplify_aux (List.map (fun _ -> false(*???*)) ps) hc

let subst_hcs hcs (Hc(popt, ps, t) as hc) =
  let _ = Global.log_begin "subst_hcs" in
  let _ = Global.log (fun () -> Format.printf "input:@,  @[<v>%a@]@," pr hc) in
  let hc =
		  if !Global.subst_hcs_inc then
      let rec aux ps t =
        try
		        let lps, (ps, t'), rps =
				        Util.find_split_map
				          (fun (pid, ttys) ->
																let res = lookup_hcs (pid, ttys) hcs in
												    let _ = Global.log (fun () -> Format.printf "%a is substituted@," Var.pr pid) in
				            res)
				          ps
		        in
		        let Hc(_, ps, t) =
            let bs =
              List.map (fun _ -> false) lps @
              List.map (fun _ -> true) ps @
              List.map (fun _ -> false) rps
            in
            simplify_aux bs (Hc(popt, lps @ ps @ rps, Formula.band [t; t']))
          in
		        aux ps t
        with Not_found ->
          Hc(popt, ps, t)
      in
      aux ps t
		  else
				  let bs, ps, t =
				    let bss, pss, ts =
						    Util.unzip3
										  (List.map
										    (fun (pid, ttys) ->
						          try
														    let res = lookup_hcs (pid, ttys) hcs in
						            let _ = Global.log (fun () -> Format.printf "%a is being substituted@," Var.pr pid) in
						            List.map (fun _ -> true) (fst res), fst res, snd res
						          with Not_found ->
						            [false], [pid, ttys], Formula.ttrue)
										    ps)
				    in
				    List.flatten bss, List.flatten pss, Formula.band (t::ts)
				  in
		    simplify_aux bs (Hc(popt, ps, t))
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
