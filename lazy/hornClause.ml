open ExtList
open ExtString

(** Horn clauses *)

type pred = Var.t * Var.t list
type t = Hc of pred option * pred list * Term.t list

let pr_pred ppf (x, xs) =
  Format.fprintf ppf "P[%a](%a)" Var.pr x (Util.pr_list Var.pr ",") xs

let pr ppf hc =
  match hc with
    Hc(popt, ps, ts) ->
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

let alpha hc =
  match hc with
    Hc(popt, ps, ts) ->
      let bvs = List.unique (match popt with None -> [] | Some(_, xs) -> xs) in
      let fvs = Util.diff (List.unique (Util.concat_map (fun (_, xs) -> xs) ps @ Util.concat_map Term.fvs ts)) bvs in
      let sub = List.map (fun x -> x, Term.make_var (Var.new_var ())) fvs in
      let sub x = List.assoc x sub in
      Hc(popt, List.map (subst_pred sub) ps, List.map (Term.subst sub) ts)

let subst sub hc =
  match hc with
    Hc(popt, ps, ts) ->
      let pss, tss =
        List.split
          (List.map
            (fun ((x, xs) as p) ->
              try
                let Hc(Some(_, ys), ps, ts) = List.find (fun (Hc(Some(y, _), _, _)) -> Var.equiv x y) sub in
                let sub = List.combine ys (List.map Term.make_var xs) in
                let sub x = List.assoc x sub in
                List.map (subst_pred sub) ps, List.map (Term.subst sub) ts
              with Not_found ->
                [p], [])
            ps)
      in
      Hc(popt, List.concat pss, ts @ List.concat tss)

let formula_of hcs =
  let hcs1, hcs2 = List.partition (function Hc(None, _, _) -> true | _ -> false) hcs in
  let hcs =
		  Util.fixed_point
						(fun hcs ->
						  List.map (subst hcs2) hcs)
						(fun hcs hcs' -> hcs =(*???*) hcs')
						hcs1
  in
  List.map (fun (Hc(None, [], ts)) -> Formula.band ts) hcs
