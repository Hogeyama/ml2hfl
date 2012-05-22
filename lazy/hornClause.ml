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
  (*Format.printf "input: %a@," Term.pr t;*)
  let ts = Formula.conjuncts t in
  let xttys, t = Tsubst.extract_from2 (Util.concat_map Pred.fvs ps) p ts in
  (*Format.printf "xttys: %a@,t: %a@," Tsubst.pr xttys Term.pr t;*)
  let ps, t =
		  if xttys = [] then
		    ps, t
		  else
		    let sub = Tsubst.fun_of xttys in
		    subst_formula p (List.map (Pred.subst_fixed sub) ps) (Term.subst_fixed sub t)
  in
  (*Format.printf "output: %a@," Term.pr t;*)
  ps, t


let subst_of p pbs1 pbs2 x =
  let flag = true in
  List.unique
				(Util.concat_map
				  (fun (((pid, ts), b) as pb) ->
				    let tss =
          List.filter_map
            (fun ((pid', ts'), b') ->
              let matches t1 t2 =
                let b = 
                  t1 = t2 || List.exists p (Term.fvs t2)
                in
                (*Format.printf "t1: %a@,t2: %a@,b:%s@," Term.pr t1 Term.pr t2 (if b then "true" else "false");*)
                b
              in
              if pid = pid' (*&& (b || b')*) && List.for_all2 matches ts' ts then
                Some(ts')
              else None)
            pbs2
        in
				    List.concat
				      (List.mapi
				        (fun i t ->
				          if List.mem x (Term.fvs t) then
				            let ts = List.map (fun ts -> List.nth ts i) tss in
                let ts = List.filter (fun t' -> t <> t') ts in
																match t with
																  Term.Var(_, x) ->
																    List.map (fun t -> x, t, SimType.Int(*???*)) ts
																| _ ->
																				(try
																				  let nxs, n' = LinArith.of_term t in
																				  match nxs with
																				    [n, x] when n = 1 ->
																				      List.map (fun t -> x, LinArith.simplify (Term.sub t (Term.tint n')), SimType.Int(*???*)) ts
																				  | _ ->
																				      raise (Invalid_argument "")
																				with Invalid_argument _ ->
																				  [])
				          else
				            [])
				        ts))
				  pbs1)


let rec hoge bvs y ys pbs1 ts1 pbs2 ts2 =
  let able bvs x = (match y with None -> true | Some(y) -> y > x) && not (List.mem x bvs) in
  let zs = List.rev (List.sort (List.unique (Util.concat_map (fun (p, _) -> Pred.fvs p) (pbs1 @ pbs2(*if y = None then pbs2 else pbs1*))))) in
  let x = List.find (able (ys @ bvs)) zs in
  (*
  let _ = Global.log (fun () -> Format.printf "x: %a@," Var.pr x) in
		Format.printf "zs: %a@," (Util.pr_list Var.pr ",") zs;
		Format.printf "ys: %a@," (Util.pr_list Var.pr ",") ys;
		Format.printf "pbs1: %a@," pr (Hc(None, List.map fst pbs1, Formula.ttrue));
		Format.printf "pbs2: %a@," pr (Hc(None, List.map fst pbs2, Formula.ttrue));
		Format.printf "ts1: %a@," Term.pr (Formula.band ts1);
		Format.printf "ts2: %a@," Term.pr (Formula.band ts2);
  *)
  let opbs1 = pbs1 in
  let opbs2 = pbs2 in
		let pbs1, pbs2 =
				let pbs1', pbs2 = List.partition (fun (p, _) -> List.mem x (Pred.fvs p)) pbs2 in
		  pbs1 @ pbs1', pbs2
		in
  let ots1 = ts1 in
  let ots2 = ts2 in
		let ts1, ts2 =
				let ts1', ts2 = List.partition (fun t -> List.mem x (Term.fvs t)) ts2 in
		  ts1 @ ts1', ts2
		in
  try
		  let _ = if Util.subset (List.map (fun (p, _) -> fst p) pbs1) (List.map (fun (p, _) -> fst p) pbs2) then () else raise Not_found in
		  let xttys = subst_of (able (ys @ bvs)) pbs1 pbs2 x in
(*
    let _ = Global.log (fun () -> Format.printf "xttys: %a@," Tsubst.pr xttys) in
*)
    let ts11, ts12 = List.partition (fun t -> List.mem x (Term.fvs t)) ts1 in
    let ts12'= Util.diff ts12 ts2 in
    let b12 = Cvc3Interface.implies ts2 ts12' in
		  Util.find_map
		    (fun xtty ->
        match Util.snd3 xtty with
          Term.Var(_, x') when Util.fst3 xtty = x' ->
  						    hoge bvs (Some(x)) [] opbs1 ots1 opbs2 ots2
        | _ ->
				        (try
						        (*let _ = Global.log (fun () -> Format.printf "%a@, @[<v>" Tsubst.pr_elem xtty) in*)
														let pbs = List.filter (fun (p, b) -> not (List.mem p (List.map fst pbs2))) (List.map (fun (p, b) -> Pred.simplify (Pred.subst (Tsubst.fun_of [xtty]) p), b) pbs1) in
														let ts11' = Util.diff (List.map (fun t -> Formula.simplify (Term.subst (Tsubst.fun_of [xtty]) t)) ts11) ts2 in
				          let b11 = Cvc3Interface.implies ts2 ts11' in
												  if pbs = [] (*???*) && b12 && b11 then
								        try hoge bvs None [] [] [] pbs2 ts2 with Not_found -> List.map fst pbs2, Formula.band ts2
								      else
								        hoge bvs (Some(x)) [] pbs ((if b11 then [] else ts11') @ (if b12 then [] else ts12')) pbs2 ts2
				        with Not_found ->
				          (*let _ = Global.log (fun () -> Format.printf "@]@,") in*)
				          raise Not_found))
		    xttys
  with Not_found ->
    if y = None then (* ToDo: check only each representative element of each cluster *) hoge bvs y (x::ys) opbs1 ots1 opbs2 ots2 else raise Not_found

let unifiers bvs ts1 ts2 =
  Util.concat_map
    (fun (t1, t2) ->
      match t1 with
        Term.Var(_, x) when not (List.mem x bvs) && t1 <> t2 ->
          [x, t2, SimType.Int(*???*)]
      | _ ->
          (try
            let nxs, n' = LinArith.of_term t1 in
            match nxs with
              [n, x] when n = 1 && not (List.mem x bvs) && t1 <> t2 ->
                [x, LinArith.simplify (Term.sub t2 (Term.tint n')), SimType.Int(*???*)]
            | _ ->
                raise (Invalid_argument "")
          with Invalid_argument _ ->
            []))
    (List.combine ts1 ts2)

let simplify_aux bs (Hc(popt, ps, t)) =
  let _ = Global.log_begin "HornClause.simplify" in
  let _ = Global.log (fun () -> Format.printf "input:@,  @[<v>%a@]@," pr (Hc(popt, ps, t))) in
  let shared = ref (List.length ps) in
  let bvs = (match popt with None -> [] | Some(_, xs) -> xs) in
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
    let ps, t = subst_formula (fun x -> List.mem x bvs || Var.is_coeff x) ps t in
    (*let _ = Global.log (fun () -> Format.printf "c:@,  @[<v>%a@]@," Term.pr t) in*)
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
  let ps, t =
		  if Term.coeffs t <> [] then
		    ps, t
    else if true then
		    let t = Formula.simplify t in
				  let ts = Formula.conjuncts t in
      let bvs' =
        let ts =
          Util.concat_map
            (fun p ->
              Util.concat_map
                (fun p' ->
                  if fst p = fst p' then
                    Util.filter_map2 (fun t1 t2 -> if t1 = t2 then Some(t1) else None) (snd p) (snd p')
                  else [])
                (List.filter (fun p' -> p <> p') ps))
            ps
       in
        Util.diff (List.unique (Util.concat_map Term.fvs ts)) bvs
      in
      let bvs = bvs @ bvs' in
      let rec rel xs1 xs2 =
        match xs1, xs2 with
				      `L((pid1, ts1) as p1), `L((pid2, ts2) as p2) ->
		          if pid1 <> pid2 then
		            let fvs1 = Util.diff (Pred.fvs p1) bvs in
		            let fvs2 = Util.diff (Pred.fvs p2) bvs in
		            List.exists (fun x -> List.mem x fvs2) fvs1
		          else
              let ts, tts = Util.partition_map (fun (t1, t2) -> if t1 = t2 then `L(t1) else `R(t1, t2)) (List.combine ts1 ts2) in
              let ts1, ts2 = List.split tts in
				          let fvs1 = Util.diff (Util.concat_map Term.fvs ts1) bvs in
				          let fvs2 = Util.diff (Util.concat_map Term.fvs ts2) bvs in
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
      let ts0, ts1 = List.partition (fun t -> Util.subset (Term.fvs t) bvs) ts in
      let ps0, ps1 = List.partition (fun p -> Util.subset (Pred.fvs p) bvs) ps in
      let ps0 =
				    let pss = Util.classify (fun (pid1, _) (pid2, _) -> pid1 = pid2) ps0 in
        Util.concat_map
          (Util.representatives
            (fun (_, ts1) (_, ts2) -> Cvc3Interface.implies ts0 (List.map2 (fun t1 t2 -> Formula.simplify (Formula.eqInt t1 t2(*???*))) ts1 ts2)))
          pss
      in
      let ecs = Util.equiv_classes rel (List.map (fun p -> `L(p)) ps1 @ List.map (fun t -> `R(t)) ts1) in
      let _ =
        (if bvs' <> [] then Format.printf "bvs': %a@," (Util.pr_list Var.pr ",") bvs');
        Format.printf "bvs: %a@," (Util.pr_list Var.pr ",") bvs;
        Format.printf "ec0: %a@," pr (Hc(None, ps0, Formula.band ts0));
        List.iter
          (fun ec ->
            let ps, ts = Util.partition_map (fun x -> x) ec in
            Format.printf "ec: %a@," pr (Hc(None, ps, Formula.band ts)))
          ecs
      in
      let reduce ec1 ec2 ecr =
        let xs = List.sort (Util.diff (List.unique (Util.concat_map (function `L(p) -> Pred.fvs p | `R(t) -> Term.fvs t) ec1)) bvs) in
        let ec2' = ec2 @ List.map (fun p -> `L(p)) ps0 in
        let ecr' = List.map (fun p -> `L(p)) ps0 @ ecr in
		      if Util.subset (Util.concat_map (function `L(p) -> [fst p] | `R(_) -> []) ec1)
                       (Util.concat_map (function `L(p) -> [fst p] | `R(_) -> []) ecr') then
    		    let _ = Global.log (fun () -> Format.printf "xs: %a@," (Util.pr_list Var.pr ",") xs) in
		        let xttyss =
            let pbs1 = List.filter_map (function `L(p) -> Some(p, false) | `R(_) -> None) ec1 in
            let pbs2 = List.filter_map (function `L(p) -> Some(p, false) | `R(_) -> None) ec2' in
				        Util.multiply_list_list
				          (fun xttys1 xttys2 -> xttys1 @ xttys2)
		            (List.map
                (fun x ->
                  let ys = (*flag must be false*)subst_of (fun x -> not (List.mem x bvs)) pbs1 pbs2 x in
                  if ys = [] then [[]] else List.map (fun x -> [x]) ys)
                xs)
		        in
		        (*let rec aux ps1 ts1 ps2 ts2 in
		        let ps2, ts2 = Util.partition_map (fun x -> x) ec1 in
		        aux [] [] ps2 ts2*)
          if List.exists
               (fun xttys ->
														 		let ec1' =
                   List.map
                     (function
                       `L(p) -> `L(Pred.simplify (Pred.subst (Tsubst.fun_of xttys) p))
                     | `R(t) -> `R(Formula.simplify (Term.subst (Tsubst.fun_of xttys) t)))
                     ec1
                 in
                 let ps, ts = Util.partition_map (fun x -> x) (Util.diff ec1' ecr') in
                 let b = ps = [] && Cvc3Interface.implies (List.filter_map (function `L(_) -> None | `R(t) -> Some(t)) ecr @ ts0) ts in
                 (if b then Format.printf "xttys: %a@," Tsubst.pr xttys);
                 b)
               xttyss then
            []
          else
            ec1
        else
          ec1
      in
      let rec aux ecs1 ecs2 =
        match ecs1 with
          [] -> ecs2
        | ec::ecs1 ->
            let ec' =
						        List.fold_left
						          (fun ec1 ec2 -> if ec1 = [] then [] else reduce ec1 ec2 (List.flatten (ecs1 @ ecs2)))
						          ec
						          ([] :: ecs1 @ ecs2)
            in
            aux ecs1 (if ec' = [] then ecs2 else ec' :: ecs2)
      in
      let ecs = aux (List.sort ecs) [] in
      let ps, ts = Util.partition_map (fun x -> x) (List.flatten ecs) in
      ps @ ps0, Formula.band (ts @ ts0)
		  else if true then
		    let t = Formula.simplify t in
				  let ts = Formula.conjuncts t in
      (try hoge bvs None [] [] [] (List.combine ps bs) ts with Not_found -> ps, Formula.band ts)
    else
		    let t = Formula.simplify t in
		    (* ToDo: make the following predicate sharing procedure more scalable *)
		    let xttys =
		      if true then
		        let ps1, ps2 = Util.partition_map (fun (b, p) -> if b then `L(p) else `R(p)) (List.combine bs ps) in
		        List.unique
				        (Util.concat_map
				          (fun ((pid1, ts1) as p) ->
				            Util.concat_map
				              (fun (pid2, ts2) ->
				                if pid1 = pid2 then
				                  unifiers bvs ts1 ts2 @ unifiers bvs ts2 ts1
				                else
				                  [])
				              ((List.filter (fun p' -> p <> p') ps1) @ ps2))
				          ps1)
		      else
				      let pss = Util.classify (fun (pid1, _) (pid2, _) -> pid1 = pid2) ps in
				      List.unique
						      (Util.concat_map
						        (fun ps ->
				            List.flatten
								          (Util.multiply_list
								            (fun (_, ts1) (_, ts2) -> unifiers bvs ts1 ts2)
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
		                    let ps1 = List.map (fun p -> Pred.simplify (Pred.subst (Tsubst.fun_of xttys) p)) ps1 in
		                    let ts1 = List.map (fun t -> Formula.simplify (Term.subst (Tsubst.fun_of xttys) t)) ts1 in
		                    (*let _ = Global.log (fun () -> Format.printf "hc1: %a@,hc2: %a@," pr (Hc(None, ps1, Formula.band ts1)) pr (Hc(None, ps2, Formula.band ts2))) in*)
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
		    let xss =
		      let p xs =
		        let pids1, pids2 = Util.partition_map (fun ((pid, _) as p) -> if Util.inter xs (Pred.fvs p) <> [] then `L(pid) else `R(pid)) ps in
		        Util.subset pids1 pids2
		      in
		      if true then
				      let xs = List.filter (fun x -> p [x]) xs in
		        let rec aux xss =
			  	      let _ = Global.log (fun () -> Format.printf "#%d@." (List.length xss)) in
		          let yss = Util.concat_map (fun ys -> List.map (fun x -> x::ys) (Util.diff xs ys)) xss in
		          let yss = List.filter p yss in
		          xss @ (if yss = [] || List.length yss > 30000 then [] else aux yss)
		        in
				      let xss = aux (List.map (fun x -> [x]) xs) in
				      let _ = Global.log (fun () -> Format.printf "# of sets of variables: %d@." (List.length xss)) in
		        xss
		      else
		        List.sort ~cmp:(fun xs ys -> List.length xs - List.length ys) (Util.power xs)
		    in
		    aux ps t xss
  in
  let res = Hc(popt, ps, t) in
  let _ =
    let _ = shared := !shared - List.length ps in
    Global.log (fun () -> if !shared <> 0 then Format.printf "# of shared predicate variables: %d@," !shared)
  in
  let _ =
		  let pss = Util.classify (fun (pid1, _) (pid2, _) -> pid1 = pid2) ps in
    let n = List.fold_left (+) 0 (List.map (fun ps -> List.length ps - 1) pss) in
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
				          (fun (pid, ts) ->
																let res = lookup_hcs (pid, ts) hcs in
												    let _ = Global.log (fun () -> Format.printf "%a is being substituted@," Var.pr pid) in
				            res)
				          ps
		        in
		        let Hc(_, ps, t) =
            let bs = List.map (fun _ -> false) lps @ List.map (fun _ -> true) ps @ List.map (fun _ -> false) rps in
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
										    (fun (pid, ts) ->
						          try
														    let res = lookup_hcs (pid, ts) hcs in
						            let _ = Global.log (fun () -> Format.printf "%a is being substituted@," Var.pr pid) in
						            List.map (fun _ -> true) (fst res), fst res, snd res
						          with Not_found ->
						            [false], [pid, ts], Formula.ttrue)
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
