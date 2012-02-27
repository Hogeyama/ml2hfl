open ExtList
open ExtString
open Zipper
open TraceConstr

(** Trace constraint generation for interaction type inference *)

(** generate a set of constraints from an error trace *)
let cgen etr =
  let rec aux (Loc(tr, p) as loc) etr0 =
    match etr0 with
      [] ->
        assert false
    | s::etr ->
        (match s with
          Trace.Call(y, g) ->
            if Var.is_top (fst y) then
              aux (insert_down loc (make y true g [] [])) etr
            else if Var.is_pos (fst y) then
              let _ = assert (g = Formula.ttrue) in
              aux (down loc (Var.tlfc_of (fst y))) etr
            else if Var.is_neg (fst y) then
              let _ = assert (g = Formula.ttrue) in
		            aux (up loc) etr
            else assert false
        | Trace.Arg(xttys) ->
            let xttys = List.filter (fun (_, _, ty) -> SimType.is_base ty) xttys in
            let nd = get tr in
            aux (Loc(set tr { nd with constr = nd.constr @ [Formula.ttrue]; subst = nd.subst @ [xttys] }, p)) etr
        | Trace.Ret(x, t, ty) ->
            let xttys = List.filter (fun (_, _, ty) -> SimType.is_base ty) [x, t, ty] in
            let nd = get tr in
            let nd' = { nd with constr = nd.constr @ [Formula.ttrue]; subst = nd.subst @ [xttys] } in
            let Var.T(f, _, _) = x in
            if Var.is_pos f then
              aux (up (Loc(set tr nd', p))) etr
            else if Var.is_neg f then
              aux (down (Loc(set tr nd', p)) (Var.tlfc_of f)) etr
            else assert false
        | Trace.Nop ->
            aux loc etr
        | Trace.Error ->
            let _ = assert (etr = []) in
            let nd = get tr in
            root (Loc(set tr { nd with closed = false }, path_set_open p)))
  in
  match etr with
    Trace.Call(x, g)::etr ->
      let tr = aux (zipper (make x true g [] [])) etr in
       if !Global.use_min_unsat_prefix then (*ToDo: test*) get_min_unsat_prefix tr else tr
  | _ -> assert false
