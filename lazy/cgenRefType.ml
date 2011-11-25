open Cgen

(* generate a set of constraints from an error trace *)
let cgen etr =
  let rec aux (Loc(tr, p) as loc) etr0 =
    match etr0 with
      [] ->
        assert false
    | s::etr ->
        (match s with
          CompTree.Call(y, g) ->
            if Var.is_top (fst y) then
              aux (insert_down loc (make y true g [])) etr
            else if Var.is_pos (fst y) then
              let _ = assert (g = Term.ttrue) in
              aux (insert_down loc (make y true g [])) etr (* changed *)
            else if Var.is_neg (fst y) then
              let _ = assert (g = Term.ttrue) in
              let nd = get tr in (* changed *)
              let nd' = { nd with ret = Some(y) } in (* changed *)
		            aux (up (Loc(set tr nd', p))) etr (* changed *)
            else assert false
        | CompTree.Arg(xttys) ->
            let nd = get tr in
            aux (Loc(set tr { nd with subst = xttys @ nd.subst }, p)) etr
        | CompTree.Ret(x, t, ty) ->
            let nd = get tr in
            let nd' = { nd with subst = (x, t, ty)::nd.subst } in
            let Var.T(f, _, _) = x in
            if Var.is_pos f then
              aux (up (Loc(set tr nd', p))) etr
            else if Var.is_neg f then
              aux (insert_down (Loc(set tr nd', p)) (make (Var.fc_ref_of f) true Term.ttrue [])) etr (* changed *)
            else assert false
        | CompTree.Nop ->
            aux loc etr
        | CompTree.Error ->
            let _ = assert (etr = []) in
            let nd = get tr in
            root (Loc(set tr { nd with closed = false }, path_set_open p)))
  in
  match etr with
    CompTree.Call(x, g)::etr -> aux (zipper (make x true g [])) etr
  | _ -> assert false

let infer_env prog sums fcs =
  let env = RefType.of_summaries prog sums fcs in
  let env' =
    List.map
      (fun (f, sty) ->
        f, RefType.of_simple_type sty)
      (List.find_all
        (fun (f, sty) -> not (List.mem_assoc f env))
        (List.map
          (fun (f, sty) -> Var.make f, sty)
          prog.Prog.types))
  in
  env @ env'
