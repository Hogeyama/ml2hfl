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

let alpha hc =
  match hc with
    Hc(popt, ps, ts) ->
      let bvs = List.unique (match popt with None -> [] | Some(_, xs) -> xs) in
      let fvs = Util.diff (List.unique (Util.concat_map (fun (_, xs) -> xs) ps @ Util.concat_map Term.fvs ts)) bvs in
      let sub = List.map (fun x -> x, Term.make_var (Var.new_var ())) fvs in
      let sub x = List.assoc x sub in
      Hc(popt, List.map (subst_pred sub) ps, List.map (Term.subst sub) ts)

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
      let ts =
        let bvs = (match popt with None -> [] | Some(_, xs) -> xs) @ Util.concat_map fvs_pred ps in
        Formula.eqelim (fun x -> List.mem x bvs) ts
      in
      let hc = Hc(popt, ps, ts) in
(*
      let _ = Format.printf "after:@.  @[%a@]@." pr hc in
*)
      hc


let formula_of hcs =
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
