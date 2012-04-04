open ExtList
open ExtString

(** Horn clauses *)

(** Predicate variables *)

type pred = Var.t * Var.t list

let pr_pred ppf (x, xs) =
  Format.fprintf ppf "P[%a](%a)" Var.pr x (Util.pr_list Var.pr ",") xs

let fvs_pred (_, xs) = xs

let rename_pred sub (x, xs) =
  (x, List.map Term.subst_var xs)

(** @deprecated *)
let subst_pred sub (x, xs) =
  (x, List.map (Term.subst_var sub) xs)

(** Horn clause *)

type t = Hc of pred option * pred list * Formula.fes

let pr ppf (Hc(popt, ps, fes)) =
		let _ = Format.fprintf ppf "@[<hov>" in
		let _ = if ps <> [] then Format.fprintf ppf "%a,@ " (Util.pr_list pr_pred ",@ ") ps in
		let _ = Format.fprintf ppf "%a@ " Formula.pr_fes fes in
  match popt with
    None ->
      Format.fprintf ppf "|- bot@]"
  | Some(p) ->
      Format.fprintf ppf "|- %a@]" pr_pred p

let pred_of env x =
  x, RefType.visible_vars env x

(** require: variables in ps do not occur in the domain of sub *)
let subst sub (Hc(popt, ps, fes)) =
  Hc(popt, ps, Formula.subst_fes sub fes)

let alpha (Hc(popt, ps, fes)) =
  let bvs = List.unique (match popt with None -> [] | Some(_, xs) -> xs) in
  let fvs = Util.diff (List.unique (Util.concat_map (fun (_, xs) -> xs) ps @ Formula.fvs_fes fes)) bvs in
  let sub = List.map (fun x -> x, Term.make_var (Var.new_var ())) fvs in
  Hc(popt,
    List.map (subst_pred (fun x -> List.assoc x sub)) ps,
    Formula.subst_fes (fun x -> List.assoc x sub) fes)

let coefficients (Hc(popt, ps, fes)) = Formula.coefficients_fes fes



let atp_qelim_fes bvs (Formula.FES(xttys, ts) as fes) =
  let ts =
		  try
				  let fvs =
				    let fvs = List.unique (Util.diff (Util.concat_map Term.fvs ts) (bvs @ Util.concat_map Formula.fvs_xtty xttys)) in
		      let _ = if !Global.debug > 1 then Format.printf "bvs: %a@.fvs: %a@." (Util.pr_list Var.pr ",") bvs (Util.pr_list Var.pr ",") fvs in
				    fvs
				  in
      let t = Formula.band ts in
		    if fvs <> [] && Formula.is_linear t then
		      Formula.conjuncts (AtpInterface.integer_qelim (Formula.exists (List.map (fun x -> x, SimType.Int(*???*)) fvs) t))
		    else
		      raise (Util.NotImplemented "subst_lbs")
		  with Util.NotImplemented _ ->
				  Util.map_left
				    (fun ts1 t ts2 ->
				      let fvs =
            let fvs = List.unique (Util.diff (Term.fvs t) (bvs @ Util.concat_map Formula.fvs_xtty xttys @ Util.concat_map Term.fvs ts1 @ Util.concat_map Term.fvs ts2)) in
	  			      let _ = if !Global.debug > 1 then Format.printf "bvs: %a@.fvs: %a@." (Util.pr_list Var.pr ",") bvs (Util.pr_list Var.pr ",") fvs in
            fvs
          in
				      if fvs <> [] && Formula.is_linear t then
				        let _ = if !Global.debug > 1 then Format.printf "before:@.  @[%a@]@." Term.pr t in
				        let t =
				          try
				            AtpInterface.integer_qelim (Formula.exists (List.map (fun x -> x, SimType.Int(*???*)) fvs) t)
				          with Util.NotImplemented _ ->
				            t
				        in
				        let _ = if !Global.debug > 1 then Format.printf "after:@.  @[%a@]@." Term.pr t in
				        t
				      else
				        t)
				    ts
  in
  Formula.make_fes xttys ts



(** @deprecated unsound *)
let merge_fess xs fess =
  let xttyss, tss = List.split (List.map (fun (Formula.FES(xttys, ts)) -> xttys, ts) fess) in
  let xttys = List.flatten xttyss in
  let xttys =
    Util.concat_map
      (fun (((x, t, ty) as xtty)::xttys) ->
        if xttys <> [] && List.mem x xs then
          let _ = if !Global.debug > 1 then Format.printf "xttys: %a@." (Util.pr_list Formula.pr_xtty ",") (xtty::xttys) in
          [xtty] (* ToDo: prove that this is sound and complete *)
        else
          xtty::xttys)
      (Util.classify (fun (x1, _, _) (x2, _, _) -> x1 = x2) xttys)
  in
  let ts = List.flatten tss in
  Formula.make_fes xttys ts


let lookup pid lbs =
  let xs, fes = List.assoc pid lbs in
  xs,
		let fvs = List.filter (fun x -> not (List.mem x xs)) (List.unique (Formula.fvs_fes fes)) in
		let sub = List.map (fun x -> x, Term.make_var (Var.new_var ())) fvs in
		Formula.subst_fes (fun x -> List.assoc x sub) fes

let subst_lbs lbs (Hc(popt, ps, fes)) =
  let fess =
    fes::
    List.map
      (fun (pid, xs) ->
        let ys, fes = lookup pid lbs in
        let sub = List.map2 (fun y x -> y, Term.make_var x) ys xs in
        Formula.subst_fes (fun x -> List.assoc x sub) fes)
      ps
  in
  let fes =
    let xttyss, tss = List.split (List.map (fun (Formula.FES(xttys, ts)) -> xttys, ts) fess) in
    let xttys = List.flatten xttyss in
    let ts = List.flatten tss in
    Formula.make_fes xttys ts
  in

  let _ = if !Global.debug > 1 then Format.printf "original: %a@." Formula.pr_fes fes in
  let fes = Formula.elim_duplicate_fes (*(List.unique (Util.concat_map snd ps))*) fes in
  let fes =
    let bvs = match popt with None -> [] | Some(_, xs) -> xs in
    let fes = Formula.simplify_fes fes in
    let fes = Formula.equantify_fes (fun x -> Var.is_coeff x) fes in
    let fes = Formula.eqelim_fes (fun x -> List.mem x bvs || Var.is_coeff x) fes in
    let fes = Formula.simplify_fes fes in
    let _ = if !Global.debug > 1 then Format.printf "simplified:@.  @[<v>%a@]@." Formula.pr_fes fes in
    let fes = atp_qelim_fes bvs fes in
    let fes = Formula.simplify_fes fes in
    let _ = if !Global.debug > 1 then Format.printf "quantifier eliminated:@.  @[<v>%a@]@." Formula.pr_fes fes in
    fes
  in
  Hc(popt, [], fes)

let pr_lb ppf (pid, (xs, fes)) =
  Format.fprintf ppf "@[<hov>%a =@ %a@]" pr_pred (pid, xs) Formula.pr_fes fes

let pr_lbs pps lbs =
  Format.printf "@[<v>%a@]" (Util.pr_list pr_lb "@,") lbs

let compute_lb lbs (Hc(Some(pid, xs), ps, fes)) =
  let Hc(_, [], fes) = subst_lbs lbs (Hc(Some(pid, xs), ps, fes)) in
  pid, (xs, fes)

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
      let lbs' =
        List.map
          (fun hc ->
            let lb = compute_lb lbs hc in
            let _ = if !Global.debug > 1 then Format.printf "%a@." pr_lb lb in
            lb)
        hcs1
      in
      aux hcs2 (lbs @ (* need to merge? *)lbs')
  in
  aux hcs []

let formula_of hcs =
  let lbs = compute_lbs hcs in
  let _ = Format.printf "lower bounds:@.  %a@." pr_lbs lbs in
  let hcs = List.filter (function (Hc(None, _, _)) -> true | _ -> false) hcs in
  let fess =
		  List.map
		    (fun hc ->
		      let Hc(None, [], fes) = subst_lbs lbs hc in
		      fes)
		    hcs
  in
  Formula.simplify (Formula.band (List.map Formula.formula_of_fes fess))
