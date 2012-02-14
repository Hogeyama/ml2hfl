open ExtList
open ExtString
open Zipper
open CallTree
open HornClause

(** Horn clauses generation for refinement type inference *)

(** generate a set of constraints from an error trace *)
let cgen env etr =
  let rec aux (Loc(tr, p) as loc) hcs etr0 =
    let pre = SimType.find_last_base env (get tr).name in
    match etr0 with
      [] ->
        assert false
    | s::etr ->
        (match s with
          Trace.Call(y, guard) ->
            if Var.is_top (fst y) then
              aux (insert_down loc (make y true [guard])) hcs etr
            else if Var.is_pos (fst y) then
              let _ = assert (guard = Formula.ttrue) in
              aux (insert_down loc (make y true [guard])) hcs etr
            else if Var.is_neg (fst y) then
              let _ = assert (guard = Formula.ttrue) in
		            let nd = get tr in
		            let nd' = { nd with ret = Some(y) } in
				          aux (up (Loc(set tr nd', p))) hcs etr
            else assert false
        | Trace.Arg(xttys) ->
            let ts = List.filter_map (fun (x, t, ty) -> if SimType.is_base ty then Some(Formula.eq_xtty (x, t, ty)) else None) xttys in
            if ts <> [] then
		            let x, _, _ =
		               List.find (fun (_, _, ty) -> SimType.is_base ty) (List.rev xttys)
		            in
		            let pres =
		             (if Var.is_avail pre then [pred_of env pre] else []) @ 
		             (List.filter_map
		               (fun tr ->
		                 match (get tr).ret with
		                   None -> assert false
		                 | Some(x_uid) ->
		                     (try Some(pred_of env (SimType.find_last_base2 env x_uid)) with Not_found -> None))
		               (children tr))
		            in
		            let nd = get tr in
		            let hcs = (Hc(Some(pred_of env x), pres, nd.data @ ts))::hcs in
		            aux (Loc(set tr { nd with data = nd.data @ ts }, p)) hcs etr
            else
              aux (Loc(tr, p)) hcs etr
        | Trace.Ret(x, t, ty) ->
            let _ = assert (SimType.is_base ty) in
            let hcs = (Hc(Some(pred_of env x), [assert false], t::(get tr).data))::hcs in
            let Var.T(f, _, _) = x in
            if Var.is_pos f then
              aux (up (Loc(tr, p))) hcs etr
            else if Var.is_neg f then
              aux (insert_down (Loc(tr, p)) (make (Var.fc_ref_of f) true [])) hcs etr
            else assert false
        | Trace.Nop ->
            aux loc hcs etr
        | Trace.Error ->
            let _ = assert (etr = []) in
            let nd = get tr in
            root (Loc(set tr { nd with closed = false }, path_set_open p)),
            (* assume that fail is NOT a proper subterm of the function definition *)
            (Hc(None, [pred_of env pre], (get tr).data))::hcs)
  in
  match etr with
    Trace.Call(x, guard)::etr ->
      aux (zipper (make x true [guard])) [] etr
  | _ -> assert false
