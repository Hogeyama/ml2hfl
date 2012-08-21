open ExtList
open ExtString

(** Horn clauses *)

(** The type of Horn clauses
    @invariant for any Hc(popt, afs, t), List.for_all (fun p -> Atom.coeffs p = []) afs *)
type t = Hc of (Var.t * (Var.t * SimType.t) list) option * Atom.t list * Term.t

(** {6 Basic functions} *)

let pr_elem ppf (Hc(popt, afs, t)) =
  let _ = Format.fprintf ppf "@[<hov>" in
  let _ =
    if afs <> [] then
      Format.fprintf ppf "%a,@ " (Util.pr_list Atom.pr ",@ ") afs
  in
  let _ = Format.fprintf ppf "%a@ " Term.pr t in
  match popt with
    None ->
      Format.fprintf ppf "|- bot@]"
  | Some(p) ->
      Format.fprintf ppf "|- %a@]"
        Atom.pr
        (Atom.of_pred p)

let pr ppf hcs =
  Format.fprintf ppf "@[<v>%a@]" (Util.pr_list pr_elem "@,@,") hcs

let fvs (Hc(popt, afs, t)) =
  Util.diff
    (List.unique (Util.concat_map Atom.fvs afs @ Term.fvs t))
    (Util.fold_opt [] (fun (_, xtys) -> List.map fst xtys) popt)

let coeffs (Hc(popt, afs, t)) =
  List.unique (Util.concat_map Atom.coeffs afs @ Term.coeffs t)

let lhs_pids hcs =
  Util.concat_map
    (fun (Hc(_, afs, _)) ->
      List.map fst afs)
    hcs

let rhs_pids hcs =
  Util.concat_map
    (function
      Hc(None, _, _) -> []
    | Hc(Some(pid, _), _, _) -> [pid])
    hcs

let pids hcs = rhs_pids hcs @ lhs_pids hcs

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

let is_non_redundant hcs =
  Util.subset (rhs_pids hcs) (lhs_pids hcs)

(** @require not (Util.intersects (Util.dom sub) (fvs popt)) *)
let subst sub (Hc(popt, afs, t)) =
  Hc(popt, List.map (Atom.subst sub) afs, Term.subst sub t)


(** rename free variables to fresh ones *)
let fresh (Hc(popt, afs, t) as hc) =
  let fvs = fvs hc in
  let sub = List.map (fun x -> x, Term.make_var (Var.new_var ())) fvs in
  Hc(popt,
    List.map (Atom.subst (fun x -> List.assoc x sub)) afs,
    Term.subst (fun x -> List.assoc x sub) t)

let mem pid hcs =
  List.exists (function Hc(Some(pid', _), _, _) -> pid = pid' | _ -> false) hcs

(** @require is_non_disjunctive hcs *)
let lookup (pid, ttys) hcs =
  let hcs' =
    List.find_all
      (function Hc(Some(pid', _), _, _) -> pid = pid' | _ -> false)
      hcs
  in
  match hcs' with
    [Hc(Some(_, xtys), _, _) as hc] ->
      let Hc(_, afs, t) = fresh hc in
      let sub = List.combine (List.map fst xtys) (List.map fst ttys) in
      List.map (Atom.subst (fun x -> List.assoc x sub)) afs,
      Term.subst (fun x -> List.assoc x sub) t
  | [] -> raise Not_found
  | _ -> assert false
