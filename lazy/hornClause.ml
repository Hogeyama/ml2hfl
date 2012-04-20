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

let subst_fixed_pred sub (x, xs) =
  (x, List.map (Term.subst_fixed_var sub) xs)

(** Horn clause *)

type t = Hc of pred option * pred list * Fes.t

let pr ppf (Hc(popt, ps, fes)) =
		let _ = Format.fprintf ppf "@[<hov>" in
		let _ = if ps <> [] then Format.fprintf ppf "%a,@ " (Util.pr_list pr_pred ",@ ") ps in
		let _ = Format.fprintf ppf "%a@ " Fes.pr fes in
  match popt with
    None ->
      Format.fprintf ppf "|- bot@]"
  | Some(p) ->
      Format.fprintf ppf "|- %a@]" pr_pred p

let pr_hc ppf (ub, ps, fes) =
		let _ = Format.fprintf ppf "@[<hov>" in
		let _ = if ps <> [] then Format.fprintf ppf "%a,@ " (Util.pr_list pr_pred ",@ ") ps in
		let _ = Format.fprintf ppf "%a@ " Fes.pr fes in
  Format.fprintf ppf "|- %a@]" Term.pr ub

let pred_of env x =
  x, RefType.visible_vars env x

(** require: variables in ps do not occur in the domain of sub *)
let subst sub (Hc(popt, ps, fes)) =
  Hc(popt, ps, Fes.subst sub fes)

let coefficients (Hc(popt, ps, fes)) = Fes.coefficients fes

let lookup_hcs (pid, xs) hcs =
		match List.find_all (function Hc(Some(pid', _), _, _) -> pid = pid' | _ -> false) hcs with
    [Hc(Some(_, ys), ps, fes)] ->
						let fvs = List.filter (fun x -> not (List.mem x ys)) (List.unique (Util.concat_map fvs_pred ps @ Fes.fvs fes)) in
						let sub = List.map (fun x -> x, Term.make_var (Var.new_var ())) fvs in
				  let ps = List.map (subst_pred (fun x -> List.assoc x sub)) ps in
						let fes = Fes.subst (fun x -> List.assoc x sub) fes in
						let sub = List.map2 (fun y x -> y, Term.make_var x) ys xs in
						List.map (subst_pred (fun x -> List.assoc x sub)) ps,
						Fes.subst (fun x -> List.assoc x sub) fes
  | [] -> raise Not_found

let eqelim (Hc(popt, ps, Fes.FES(xttys, ts))) =
		let bvs = match popt with None -> [] | Some(_, xs) -> xs in

		let sub1, xttys =
    List.partition
      (function
        ((x, (Term.Var(_, _) as t), _) as xtty) ->
          not (List.mem x bvs)
      | xtty -> false)
      xttys
  in

		let sub2, xttys =
    let dom = Tsubst.dom sub1 in
    Util.partition_map
      (function
        ((x, (Term.Var(_, y)), ty) as xtty) ->
          if not (List.mem y bvs) && not (List.mem y dom) then
            `L(y, Term.make_var x, ty)
          else
            `R(xtty)
      | xtty -> `R(xtty))
      xttys
  in
  let sub, ts' = Tsubst.elim_duplicate (sub1 @ sub2) in
  (* ToDo: check whether sub is cyclic *)
  let ps = List.unique (List.map (subst_fixed_pred (Tsubst.fun_of sub)) ps) in
  let fes = Fes.subst_fixed (Tsubst.fun_of sub) (Fes.make xttys (ts @ ts')) in
  Hc(popt, ps, Fes.simplify fes)




let subst_hcs hcs (Hc(popt, ps, fes) as hc) =
  let _ = Global.log_begin "subst_hcs" in
  let _ = Global.log (fun () -> Format.printf "input:@,  @[<v>%a@]@," pr hc) in
  let ps, fes =
    let pss, fess =
		    List.split
						  (List.map
						    (fun (pid, xs) ->
            try
								      let res = lookup_hcs (pid, xs) hcs in
              let _ = Global.log (fun () -> Format.printf "%a is being substituted@," Var.pr pid) in
              res
            with Not_found ->
              [pid, xs], Fes.make [] [])
						    ps)
    in
		  let fess = fes::fess in
    let xttyss, tss = List.split (List.map (fun (Fes.FES(xttys, ts)) -> xttys, ts) fess) in
    let xttys = List.flatten xttyss in
    let ts = List.flatten tss in
    List.flatten pss, Fes.make xttys ts
  in

  let fes =
    let _ = Global.log_begin "simplifying formula" in
    let _ = Global.log (fun () -> Format.printf "input:@,  @[<v>%a@]@," Fes.pr fes) in

    let fes = Fes.elim_duplicate (*(List.unique (Util.concat_map snd ps))*) fes in
    let fes = Fes.simplify fes in

    let fes = Fes.equantify (fun x -> Var.is_coeff x) fes in

    let bvs = (match popt with None -> [] | Some(_, xs) -> xs) @ Util.concat_map snd ps in
    let fes = Fes.eqelim (fun x -> List.mem x bvs || Var.is_coeff x) fes in
    let fes = Fes.simplify fes in

    let fes = AtpInterface.qelim_fes bvs fes in
    let fes = Fes.simplify fes in
    let _ = Global.log (fun () -> Format.printf "output:@,  @[<v>%a@]" Fes.pr fes) in
    let _ = Global.log_end "simplifying formula" in
    fes
  in
  let hc = eqelim (Hc(popt, ps, fes)) in
  let _ = Global.log (fun () -> Format.printf "output:@,  @[<v>%a@]" pr hc) in
  let _ = Global.log_end "subst_hcs" in
  hc


let inline fs hcs =
  let hcs1, hcs2 =
		  List.partition
      (function Hc(Some(pid, _), _, _) ->
        List.exists
          (fun f ->
            let Var.V(id), _ = CallId.tlfc_of pid in
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

let lookup_lbs (pid, xs) lbs =
		let ys, fes = List.assoc pid lbs in
		let fvs = List.filter (fun x -> not (List.mem x ys)) (List.unique (Fes.fvs fes)) in
		let sub = List.map (fun x -> x, Term.make_var (Var.new_var ())) fvs in
		let fes = Fes.subst (fun x -> List.assoc x sub) fes in
		let sub = List.map2 (fun y x -> y, Term.make_var x) ys xs in
		Fes.subst (fun x -> List.assoc x sub) fes

let subst_lbs lbs (Hc(popt, ps, fes)) =
  let _ = Global.log_begin "subst_lbs" in
  let fes =
		  let fess =
		    fes::
		    List.map
		      (fun (pid, xs) -> lookup_lbs (pid, xs) lbs)
		      ps
		  in
    let xttyss, tss = List.split (List.map (fun (Fes.FES(xttys, ts)) -> xttys, ts) fess) in
    let xttys = List.flatten xttyss in
    let ts = List.flatten tss in
    Fes.make xttys ts
  in

  let _ = Global.log (fun () -> Format.printf "original: %a@," Fes.pr fes) in
  let fes =
    let fes = Fes.elim_duplicate (*(List.unique (Util.concat_map snd ps))*) fes in
    let fes = Fes.simplify fes in

    let fes = Fes.equantify (fun x -> Var.is_coeff x) fes in

    let bvs = match popt with None -> [] | Some(_, xs) -> xs in
    let fes = Fes.eqelim (fun x -> List.mem x bvs || Var.is_coeff x) fes in
    let fes = Fes.simplify fes in
    let _ = Global.log (fun () -> Format.printf "simplified:@,  @[<v>%a@]@," Fes.pr fes) in

    let fes = AtpInterface.qelim_fes bvs fes in
    let fes = Fes.simplify fes in
    let _ = Global.log (fun () -> Format.printf "quantifier eliminated:@,  @[<v>%a@]@," Fes.pr fes) in
    fes
  in
  let _ = Global.log_end "subst_lbs" in
  Hc(popt, [], fes)

let pr_lb ppf (pid, (xs, fes)) =
  Format.fprintf ppf "@[<v>%a =@ %a@]" pr_pred (pid, xs) Fes.pr fes

let pr_lbs pps lbs =
  Format.printf "@[<v>%a@]" (Util.pr_list pr_lb "@,") lbs

let compute_lb lbs (Hc(Some(pid, xs), ps, fes)) =
  let Hc(_, [], fes) = subst_lbs lbs (Hc(Some(pid, xs), ps, fes)) in
  pid, (xs, fes)

let compute_lbs hcs =
  let _ = Global.log_begin "compute_lbs" in
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
            let _ = Global.log (fun () -> Format.printf "%a@," pr_lb lb) in
            lb)
        hcs1
      in
      aux hcs2 (lbs @ (* need to merge? *)lbs')
  in
  let res = aux hcs [] in
  let _ = Global.log_end "compute_lbs" in
  res


(***)

let formula_of_forward hcs =
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
  Formula.simplify (Formula.bor (List.map Fes.formula_of fess))


let formula_of_backward hcs =
  let hcs1, hcs2 = List.partition (function Hc(None, _, _) -> true | _ -> false) hcs in
  let hcs =
		  List.map
						(Util.fixed_point
						  (fun hc ->
		        subst_hcs hcs2 hc)
						  (fun hc1 hc2 ->
		        match hc1, hc2 with
		          Hc(_, ps1, _), Hc(_, ps2, _) -> ps1 = ps2))
						hcs1
  in
  Formula.simplify
    (Formula.bor
      (List.map
        (fun (Hc(None, ps, fes)) ->
          if ps = [] then
            Fes.formula_of fes
          else
            let _ = Format.printf "%a@." pr (Hc(None, ps, fes)) in
            assert false)
        hcs))



(** @deprecated ?? *)
let alpha (Hc(popt, ps, fes)) =
  let bvs = List.unique (match popt with None -> [] | Some(_, xs) -> xs) in
  let fvs = Util.diff (List.unique (Util.concat_map (fun (_, xs) -> xs) ps @ Fes.fvs fes)) bvs in
  let sub = List.map (fun x -> x, Term.make_var (Var.new_var ())) fvs in
  Hc(popt,
    List.map (subst_pred (fun x -> List.assoc x sub)) ps,
    Fes.subst (fun x -> List.assoc x sub) fes)

let get_lhs_pids hcs =
  Util.concat_map
    (fun (Hc(_, ps, _)) ->
      List.map fst ps)
    hcs
