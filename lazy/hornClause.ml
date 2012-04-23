open ExtList
open ExtString

(** Horn clauses *)

(** Horn clause *)
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
  (*Format.printf "orig: %a@." Term.pr t;*)
  let ts = Formula.conjuncts t in
  let xttys, t = Tsubst.extract_from2 p ts in
  (*Format.printf "xttys: %a, t: %a@." Tsubst.pr xttys Term.pr t;*)
  if xttys = [] then
    ps, t
  else
    let sub = Tsubst.fun_of xttys in
    subst_formula p (List.map (Pred.subst_fixed sub) ps) (Term.subst_fixed sub t)

let subst_hcs hcs (Hc(popt, ps, t) as hc) =
  let _ = Global.log_begin "subst_hcs" in
  let _ = Global.log (fun () -> Format.printf "input:@,  @[<v>%a@]@," pr hc) in
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

  let ps, t =
    let _ = Global.log_begin "simplifying formula" in
    let _ = Global.log (fun () -> Format.printf "input:@,  @[<v>%a@]@," Term.pr t) in

    let bvs = (match popt with None -> [] | Some(_, xs) -> xs) in

    let sub, t =
      Tsubst.extract_from
        (match popt with None -> [] | Some(pid, _) -> [pid])
        (fun x -> List.mem x bvs || Var.is_coeff x) t
    in
    let ps, t = List.map (Pred.subst sub) ps, Term.subst sub t in
(*
    let t = Term.simplify (AtpInterface.qelim_fes (diff bvs (fvs ps)) t) in
*)
    let ps, t = subst_formula (fun x -> List.mem x bvs || Var.is_coeff x) ps t in
    let _ = Global.log (fun () -> Format.printf "output:@,  @[<v>%a@]" Term.pr t) in
    let _ = Global.log_end "simplifying formula" in
    List.unique ps, t
  in
  let hc = Hc(popt, ps, t) in
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
