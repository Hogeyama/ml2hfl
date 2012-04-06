open ExtList
open ExtString

(** Horn clauses *)

(** Predicate variables *)

type pred = Var.t * Var.t list

let pr_pred ppf (x, xs) =
  Format.fprintf ppf "P[%a](%a)" Var.pr x (Util.pr_list Var.pr ",") xs

let fvs_pred (_, xs) = xs

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

let coefficients (Hc(popt, ps, fes)) = Formula.coefficients_fes fes

let lookup_hcs pid hcs =
		let Hc(Some(_, xs), ps, fes) = List.find (function Hc(Some(pid', _), _, _) -> pid = pid' | _ -> false) hcs in
		xs,
		let fvs = List.filter (fun x -> not (List.mem x xs)) (List.unique (Util.concat_map fvs_pred ps @ Formula.fvs_fes fes)) in
		let sub = List.map (fun x -> x, Term.make_var (Var.new_var ())) fvs in
  List.map (subst_pred (fun x -> List.assoc x sub)) ps,
		Formula.subst_fes (fun x -> List.assoc x sub) fes

let subst_hcs hcs (Hc(popt, ps, fes)) =
  let ps, fes =
    let pss, fess =
		    List.split
						  (List.map
						    (fun (pid, xs) ->
            try
								      let ys, (ps, fes) = lookup_hcs pid hcs in
              let sub = List.combine ys xs in
              let sub x = Term.make_var (List.assoc x sub) in
						        List.map (subst_pred sub) ps,
								      Formula.subst_fes sub fes
            with Not_found ->
              [pid, xs], Formula.make_fes [] [])
						    ps)
    in
		  let fess = fes::fess in
    let xttyss, tss = List.split (List.map (fun (Formula.FES(xttys, ts)) -> xttys, ts) fess) in
    let xttys = List.flatten xttyss in
    let ts = List.flatten tss in
    List.flatten pss, Formula.make_fes xttys ts
  in

  let _ = if !Global.debug > 1 then Format.printf "original: %a@." Formula.pr_fes fes in
  let fes =
    let fes = Formula.elim_duplicate_fes (*(List.unique (Util.concat_map snd ps))*) fes in
    let fes = Formula.simplify_fes fes in

    let fes = Formula.equantify_fes (fun x -> Var.is_coeff x) fes in

    let bvs = (match popt with None -> [] | Some(_, xs) -> xs) @ Util.concat_map snd ps in
    let fes = Formula.eqelim_fes (fun x -> List.mem x bvs || Var.is_coeff x) fes in
    let fes = Formula.simplify_fes fes in
    let _ = if !Global.debug > 1 then Format.printf "simplified:@.  @[<v>%a@]@." Formula.pr_fes fes in

    let fes = AtpInterface.qelim_fes bvs fes in
    let fes = Formula.simplify_fes fes in
    let _ = if !Global.debug > 1 then Format.printf "quantifier eliminated:@.  @[<v>%a@]@." Formula.pr_fes fes in
    fes
  in
  Hc(popt, ps, fes)


let inline fs hcs =
  let hcs1, hcs2 =
		  List.partition
      (function Hc(Some(pid, _), _, _) ->
        List.exists
          (fun f ->
            let Var.V(id), _ = Var.tlfc_of pid in
            Idnt.string_of id = f)
          fs
      | _ -> false)
		    hcs
  in
  List.map
				(Util.fixed_point
				  (fun hc ->
        (*Format.printf "%a@." pr hc;*)
        subst_hcs hcs1 hc)
				  (fun hc1 hc2 ->
        match hc1, hc2 with
          Hc(_, ps1, _), Hc(_, ps2, _) -> ps1 = ps2))
				hcs2


(** Least solutions *)

let lookup_lbs pid lbs =
		let xs, fes = List.assoc pid lbs in
		xs,
		let fvs = List.filter (fun x -> not (List.mem x xs)) (List.unique (Formula.fvs_fes fes)) in
		let sub = List.map (fun x -> x, Term.make_var (Var.new_var ())) fvs in
		Formula.subst_fes (fun x -> List.assoc x sub) fes

let subst_lbs lbs (Hc(popt, ps, fes)) =
  let fes =
		  let fess =
		    fes::
		    List.map
		      (fun (pid, xs) ->
		        let ys, fes = lookup_lbs pid lbs in
		        let sub = List.map2 (fun y x -> y, Term.make_var x) ys xs in
		        Formula.subst_fes (fun x -> List.assoc x sub) fes)
		      ps
		  in
    let xttyss, tss = List.split (List.map (fun (Formula.FES(xttys, ts)) -> xttys, ts) fess) in
    let xttys = List.flatten xttyss in
    let ts = List.flatten tss in
    Formula.make_fes xttys ts
  in

  let _ = if !Global.debug > 1 then Format.printf "original: %a@." Formula.pr_fes fes in
  let fes =
    let fes = Formula.elim_duplicate_fes (*(List.unique (Util.concat_map snd ps))*) fes in
    let fes = Formula.simplify_fes fes in

    let fes = Formula.equantify_fes (fun x -> Var.is_coeff x) fes in

    let bvs = match popt with None -> [] | Some(_, xs) -> xs in
    let fes = Formula.eqelim_fes (fun x -> List.mem x bvs || Var.is_coeff x) fes in
    let fes = Formula.simplify_fes fes in
    let _ = if !Global.debug > 1 then Format.printf "simplified:@.  @[<v>%a@]@." Formula.pr_fes fes in

    let fes = AtpInterface.qelim_fes bvs fes in
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


(***)

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



(** @deprecated *)
let alpha (Hc(popt, ps, fes)) =
  let bvs = List.unique (match popt with None -> [] | Some(_, xs) -> xs) in
  let fvs = Util.diff (List.unique (Util.concat_map (fun (_, xs) -> xs) ps @ Formula.fvs_fes fes)) bvs in
  let sub = List.map (fun x -> x, Term.make_var (Var.new_var ())) fvs in
  Hc(popt,
    List.map (subst_pred (fun x -> List.assoc x sub)) ps,
    Formula.subst_fes (fun x -> List.assoc x sub) fes)
