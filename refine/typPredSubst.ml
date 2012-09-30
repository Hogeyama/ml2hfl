open ExtList
open HornClause

(** Typed substitutions for predicates *)

(** {6 Basic functions} *)

let pr_elem ppf (pid, (xtys, t)) =
  Format.fprintf ppf
    "@[<hov>%a =@ %a@]"
    Atom.pr (pid, List.map (fun (x, ty) -> Term.make_var x, ty) xtys)
    Term.pr t

let pr ppf psub =
  Format.fprintf ppf "@[<v>%a@]" (Util.pr_list pr_elem "@,") psub

(** @ensure not (Util.is_dup ret) *)
let fvs_elem (_, (xtys, t)) = Util.diff (List.unique (Term.fvs t)) (List.map fst xtys)
let fvs psub = Util.concat_map fvs_elem psub

(** {6 Functions on substitutions for predicates} *)

(** @require fvs psub = [] *)
let lookup (pid, ttys) psub =
  let _ = if !Global.debug then assert (List.mem_assoc pid psub) in
  FormulaUtil.simplify
    (Formula.band
      (List.map
        (fun (_, (xtys, t)) ->
          let sub =
            List.map2 (fun (x, ty) (t, ty') -> let _ = if !Global.debug then assert (ty = ty') in x, t) xtys ttys
          in
          FormulaUtil.subst (fun x -> List.assoc x sub) t)
        (List.filter (fun (pid', _) -> pid = pid') psub)))

(** @require fvs psub = [] *)
let merge psub =
  List.map
    (fun (pid, xtys) ->
      pid, (xtys, lookup (Atom.of_pred (pid, xtys)) psub))
    (List.map
      (fun ((pid, (xtys, _))::_) -> pid, xtys)
      (Util.classify (fun (pid1, _) (pid2, _) -> pid1 = pid2) psub))


(* @raise Not_found if pid is not defined in psub *)
let args_of pid psub =
  let xtys, _ = List.assoc pid psub in
  xtys

(** @require Util.is_map psub
    @raise Not_found if pid is not defined in psub *)
let lookup_map (pid, ttys) psub =
  let xtys, t = List.assoc pid psub in
  (*let _ = Format.printf "???: %a@,%a@," Term.pr t SimType.pr_env xtys in*)
  let xs = fvs_elem (pid, (xtys, t)) in
  (*let _ = Format.printf "???: %a@," Var.pr_list xs in*)
  let t = FormulaUtil.fresh_vars xs t in
  (*let _ = Format.printf "???: %a@," Term.pr t in*)
  let sub =
    List.map2
      (fun (x, ty1) (t, ty2) ->
        let _ = if !Global.debug then assert (ty1 = ty2) in
        x, t)
      xtys ttys
  in
  FormulaUtil.subst (fun x -> List.assoc x sub) t

(** @require Util.is_map psub *)
let subst_lhs ?(bvs = []) psub (Hc(popt, ps, t) as hc) =
  let _ = Global.log_begin "subst_lhs" in
  let _ = Global.log (fun () -> Format.printf "input: %a@," HornClause.pr_elem hc) in
  let ts, ps =
    Util.partition_map
      (fun (pid, ttys) ->
        try
          `L(lookup_map (pid, ttys) psub)
        with Not_found ->
          `R(pid, ttys))
      ps
  in
  let t = Formula.band (t :: ts) in
  (*let _ = Global.log (fun () -> Format.printf "interm: %a@," Term.pr t) in*)
  let hc = HornClauseUtil.simplify bvs (Hc(popt, ps, t)) in
  let _ = Global.log (fun () -> Format.printf "output: %a" HornClause.pr_elem hc) in
  let _ = Global.log_end "subst_lhs" in
  hc

(** @require Util.is_map psub *)
let subst ?(bvs = []) psub hc =
  let Hc(popt, ps, t) = subst_lhs ~bvs:bvs psub hc in
  match popt with
    None ->
      Hc(popt, ps, t)
  | Some(pid, xtys) ->
      (try
        let t' = lookup_map (Atom.of_pred (pid, xtys)) psub in
        HornClauseUtil.simplify bvs (Hc(None, ps, Formula.band [t; Formula.bnot t']))
      with Not_found ->
        Hc(popt, ps, t))

(** @require fvs psub = [] && Util.is_map psub && Util.subset (pids hcs) (Util.dom psub) *)
let check_validity psub hcs =
  List.iter
    (fun hc ->
      match subst psub hc with
        Hc(None, [], t) ->
          if not (Cvc3Interface.is_valid (Formula.bnot t)) then
            let _ = Format.printf "%a@,%a => bot@," HornClause.pr_elem hc Term.pr t in
            assert false
      | _ ->
          let _ = Format.printf "%a@," HornClause.pr_elem hc in
          assert false)
    hcs
