open ExtList
open ExtString

(** Horn clauses *)

type pred = Var.t * Var.t list
type t = Hc of pred option * pred list * Term.t list

let pr_pred ppf (x, xs) =
  Format.fprintf ppf "P[%a](%a)" Var.pr x (Util.pr_list Var.pr ",") xs

let fvs_pred (_, xs) = xs

let pr ppf (Hc(popt, ps, ts)) =
		let _ = Format.fprintf ppf "@[<hov>" in
		let _ = if ps <> [] then Format.fprintf ppf "%a,@ " (Util.pr_list pr_pred ",@ ") ps in
		let _ = Format.fprintf ppf "%a@ " (Util.pr_list Term.pr ",@ ") ts in
  match popt with
    None ->
      Format.fprintf ppf "|- bot@]"
  | Some(p) ->
      Format.fprintf ppf "|- %a@]" pr_pred p

let pred_of env x =
  x, RefType.visible_vars env x

let subst_pred sub (x, xs) =
  (x, List.map (fun x -> try let Term.Var(_, y) = sub x in y with Not_found -> x) xs)

(** require: variables in ps do not occur in the domain of sub *)
let subst sub (Hc(popt, ps, ts)) =
  Hc(popt, ps, List.map (Term.subst sub) ts)

let alpha (Hc(popt, ps, ts)) =
  let bvs = List.unique (match popt with None -> [] | Some(_, xs) -> xs) in
  let fvs = Util.diff (List.unique (Util.concat_map (fun (_, xs) -> xs) ps @ Util.concat_map Term.fvs ts)) bvs in
  let sub = List.map (fun x -> x, Term.make_var (Var.new_var ())) fvs in
  let sub x = List.assoc x sub in
  Hc(popt, List.map (subst_pred sub) ps, List.map (Term.subst sub) ts)

let coefficients (Hc(popt, ps, ts)) =
  Util.concat_map Term.coefficients ts

let lookup pid lbs =
  let xs, ts = List.assoc pid lbs in
  xs,
  if !Global.rename_lookup then
		  let fvs = List.filter (fun x -> not (List.mem x xs)) (List.unique (Util.concat_map Term.fvs ts)) in
		  let sub = List.map (fun x -> x, Term.make_var (Var.new_var ())) fvs in
		  List.map (Term.subst (fun x -> List.assoc x sub)) ts
  else
    ts

let subst_lbs lbs (Hc(popt, ps, ts)) =
  let ts =
    List.unique
      (ts @
      Util.concat_map
        (fun (pid, xs) ->
          let ys, ts = lookup pid lbs in
          let sub = List.map2 (fun y x -> y, Term.make_var x) ys xs in
          List.map (Term.subst (fun x -> List.assoc x sub)) ts)
        ps)
  in
(*
  Format.printf "1:%a@." (Util.pr_list Term.pr ",") ts;
*)
  let ts = Formula.conjuncts (Formula.simplify (Formula.elim_unit (Formula.band ts))) in
(*
  Format.printf "2:%a@." (Util.pr_list Term.pr ",") ts;
*)
  let bvs = match popt with None -> [] | Some(_, xs) -> xs in
  let ts =
    if !Global.rename_lookup then
		    Formula.eqelim (fun x -> List.mem x bvs || Var.is_coeff x) ts
    else (* sound? *)
		    let xs = Util.diff (Util.concat_map snd ps) bvs in
		    Formula.eqelim (fun x -> not (List.mem x xs) || Var.is_coeff x) ts
  in
(*
  Format.printf "3:%a@." (Util.pr_list Term.pr ",") ts;
*)
  let ts = Formula.simplify_conjuncts ts in
  Format.printf "4:%a@." (Util.pr_list Term.pr ",") ts;
  let ts =
    if true then
      let t = Formula.band ts in
      if !Global.rename_lookup && Formula.is_linear t then
        let fvs = List.unique (Util.diff (Term.fvs t) bvs) in
        try
          Formula.conjuncts (AtpInterface.integer_qelim (Formula.exists (List.map (fun x -> x, SimType.Int(*???*)) fvs) t))
        with Util.NotImplemented _ ->
          ts
      else
        ts
    else
      Util.map_left
        (fun ts1 t ts2 ->
          let fvs = List.unique (Util.diff (Term.fvs t) (bvs @ Util.concat_map Term.fvs ts1 @ Util.concat_map Term.fvs ts2)) in
          (*
          let _ = Format.printf "bvs: %a@.fvs: %a@." (Util.pr_list Var.pr ",") bvs (Util.pr_list Var.pr ",") fvs in
          *)
          if fvs <> [] && !Global.rename_lookup && Formula.is_linear t then
            let _ = Format.printf "before:@.  @[%a@]@." Term.pr t in
            let t =
              if true then
                try
                  AtpInterface.integer_qelim (Formula.exists (List.map (fun x -> x, SimType.Int(*???*)) fvs) t)
                with Util.NotImplemented _ ->
                  t
              else
                t
            in
            (*
            let t = Formula.simplify (Formula.bor (List.map Formula.band (Formula.dnf t))) in
            *)
            let _ = Format.printf "after:@.  @[%a@]@." Term.pr t in
            t
          else
            t)
        ts
  in
  Format.printf "5:%a@." (Util.pr_list Term.pr ",") ts;
  Hc(popt, [], Formula.conjuncts (Formula.simplify (Formula.band ts)))

let compute_lb lbs (Hc(Some(pid, xs), ps, ts)) =
  let Hc(_, [], ts) = subst_lbs lbs (Hc(Some(pid, xs), ps, ts)) in
  pid, (xs, ts)

let compute_lbs hcs =
  let rec aux hcs lbs =
    let hcs1, hcs2 =
      List.partition
       (function (Hc(Some(_), ps, _)) ->
         List.for_all (fun (pid, _) -> List.mem_assoc pid lbs) ps
       | (Hc(None, _, _)) -> false)
       hcs
    in
    if hcs1 = [] then
      lbs (* hcs2 are all false *)
    else
      aux hcs2 (lbs @ (* need to merge? *)(List.map (compute_lb lbs) hcs1))
  in
  aux hcs []

let pr_lb ppf (pid, (xs, ts)) =
  Format.fprintf ppf "@[<hov>%a =@ %a@]" pr_pred (pid, xs) Term.pr (Formula.band ts)

let pr_lbs pps lbs =
  Format.printf "@[<v>%a@]" (Util.pr_list pr_lb "@,") lbs


let formula_of hcs =
  let lbs = compute_lbs hcs in
  let _ = Format.printf "lower bounds:@.  %a@." pr_lbs lbs in
  let hcs = List.filter (function (Hc(None, _, _)) -> true | _ -> false) hcs in
  Formula.simplify (Formula.band (Util.concat_map (fun hc -> let Hc(None, [], ts) = subst_lbs lbs hc in ts) hcs))

(** @deprecated only used by a deprecated function bwd_formula_of *)
let subst_hcs sub hc =
(*
  let _ = Format.printf "before:@.  @[%a@]@." pr hc in
*)
  match hc with
    Hc(popt, ps, ts) ->
      let pss, tss =
        List.split
          (List.map
            (fun ((x, xs) as p) ->
              let hcs = List.find_all (fun (Hc(Some(y, _), _, _)) -> Var.equiv x y) sub in
              match hcs with
                [Hc(Some(_, ys), ps, ts)] ->
                  let ps, ts =
                    let fvs = List.filter (fun x -> not (List.mem x ys)) (List.unique (Util.concat_map fvs_pred ps @ Util.concat_map Term.fvs ts)) in
                    let sub = List.map (fun x -> x, Term.make_var (Var.new_var ())) fvs in
                    List.map (subst_pred (fun x -> List.assoc x sub)) ps,
                    List.map (Term.subst (fun x -> List.assoc x sub)) ts
                  in
                  let sub = List.combine ys (List.map Term.make_var xs) in
                  let sub x = List.assoc x sub in
                  List.map (subst_pred sub) ps, List.map (Term.subst sub) ts
              | [] ->
                  [p], []
              | _ ->
                  let _ = Format.printf "@.@[<v>%a@]@." (Util.pr_list pr "@,") hcs in
                  raise (Util.NotImplemented "HornClause.subst_hcs"))
            ps)
      in
      let ps = List.concat pss in
      let ts = ts @ List.concat tss in
      let ts = (* this makes the return value of bwd_formula_of huge, e.g. ICFP2012 intro3*)Formula.conjuncts (Formula.simplify (Formula.band ts)) in
      let ts =
        let bvs = (match popt with None -> [] | Some(_, xs) -> xs) @ Util.concat_map fvs_pred ps in
        Formula.eqelim (fun x -> List.mem x bvs || Var.is_coeff x) ts
      in
      let hc = Hc(popt, ps, ts) in
(*
      let _ = Format.printf "after:@.  @[%a@]@." pr hc in
*)
      hc

(** @deprecated use formula_of instead *)
let bwd_formula_of hcs =
  let hcs1, hcs2 = List.partition (function Hc(None, _, _) -> true | _ -> false) hcs in
  let hcs =
    Util.fixed_point
      (fun hcs ->
        List.map (subst_hcs hcs2) hcs)
      (fun hcs hcs' -> hcs =(*???*) hcs')
      hcs1
  in
  Formula.simplify
    (Formula.bor
      (List.map
        (fun (Hc(None, ps, ts)) ->
          if ps = [] then
            Formula.band ts
          else
            let _ = Format.printf "%a@." pr (Hc(None, ps, ts)) in
            assert false)
        hcs))
