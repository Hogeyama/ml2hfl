open ExtList
open ExtString
open Zipper
open CallTree
open HornClause

(** Horn clauses generation for refinement type inference *)

let related n1 n2 =
  let tmp = CallId.tlfc_of (Var.T(fst n1, snd n1, (*dummy*)-1)) in
  CallId.ancestor_of tmp n2
let related_locs loc =
  let Loc(tr, _) = loc in
		find_all
		  (fun nd ->
		    related (get tr).name nd.name)
		  (root loc)

(** generate a set of constraints from an error trace *)
let cgen env etr =
  let flag_coeff = true in
  let rec aux (Loc(tr, p) as loc) hcs etr0 =
    match etr0 with
      [] ->
        assert false
    | s::etr ->
        (match s with
          Trace.Call(y, guard) ->
            if Var.is_top (fst y) then
              aux (insert_down loc (make y true (guard, []))) hcs etr
            else if Var.is_pos (fst y) then
              let _ = assert (guard = Formula.ttrue) in
              aux (insert_down loc (make y true (guard, []))) hcs etr
            else if Var.is_neg (fst y) then
              let _ = assert (guard = Formula.ttrue) in
		            let nd = get tr in
		            let nd' = { nd with ret = Some(y) } in
				          aux (up (Loc(set tr nd', p))) hcs etr
            else assert false
        | Trace.Arg(xttys) ->
            (try
						        let pre, _, _ =
								        List.find
						            (fun (_, _, ty) -> SimType.refinable ty)
						            (List.rev xttys)
						        in
		            let xttys =
		              List.filter_map
		                (fun (x, t, ty) ->
		                  if SimType.is_base ty && ty <> SimType.Unit(*sound???*) then
		                    Some(x, t, ty)
		                  else
		                    None)
		                xttys
		            in
              let xttys1, xttys2 = List.partition (fun (_, t, _) -> Term.coeffs t = []) xttys in
				          let hcs, pres' =
				  								  let locs = related_locs loc (*(Loc(tr, left_of_path p))*) in
																(*let locs = if let Trace.Call(x_uid, _)::_ = etr in Var.is_pos (fst x_uid) then locs else List.filter (fun (Loc(tr', _)) -> RefType.visible (SimType.find_last_base env (get tr).name(*???*)) (SimType.find_last_base env (get tr').name(*???*))) locs in*)
						          let pres =
				              Util.concat_map
				                (fun (Loc(tr, _)) ->
										            (try [Pred.of_pid env (SimType.find_last_base2 env (get tr).name)] with Not_found -> []) @ 
										            (List.filter_map
										              (fun tr ->
										                match (get tr).ret with
										                  None -> assert false
										                | Some(x_uid) ->
										                    (try Some(Pred.of_pid env (SimType.find_last_base2 env x_uid)) with Not_found -> None))
										              (children tr)))
				                locs
						          in
		              let t =
                  Formula.band
																		  (List.map (fun (Loc(tr, _)) -> fst (get tr).data) locs @
                    List.map Tsubst.formula_of_elem xttys1)
																in
                let hcs', pres' =
                  List.split
		                  (List.map
		                    (fun (x, t, ty) ->
		                      let xtys = List.sort (List.map (fun x -> x, SimType.Int) (Term.fvs_ty SimType.Int t ty)) @ [x, ty] in
                        let pid =
                          if flag_coeff then
                            Var.make_coeff (Idnt.new_cid ())
                          else
                            List.hd (List.sort (Term.coeffs t))
																								in
		                      Hc(Some(pid, xtys), [], Tsubst.formula_of_elem (x, t, ty)),
		                      (pid, List.map (fun (x, ty) -> Term.make_var x, ty) xtys))
		                    xttys2)
                in
		              (Hc(Some(Pred.of_pid_vars env pre), (Util.concat_map (fun (Loc(tr, _)) -> snd (get tr).data) locs) @ pres' @ pres, t)) :: hcs' @ hcs,
                pres'
		            in
		  		        let nd = get tr in
				          aux (Loc(set tr { nd with data = Formula.band (fst nd.data :: List.map Tsubst.formula_of_elem xttys1), snd nd.data @ pres' }, p)) hcs etr
            with Not_found ->
              if !Global.refine_function then
                (* ToDo: need function type refinement *)
                raise (Util.NotImplemented "HcGenRefType.cgen")
              else
                aux (Loc(tr, p)) hcs etr)
        | Trace.Ret(x, t, ty) ->
            let _ = assert (SimType.is_base ty) in
            let xttys = if SimType.is_base ty && ty <> SimType.Unit(*sound???*) then [x, t, ty] else [] in
            let t = Formula.band (fst (get tr).data :: List.map Tsubst.formula_of_elem xttys) in
            let hcs = (Hc(Some(Pred.of_pid_vars env x), [assert false], t))::hcs in
            let Var.T(f, _, _) = x in
            if Var.is_pos f then
              aux (up (Loc(tr, p))) hcs etr
            else if Var.is_neg f then
              aux (insert_down (Loc(tr, p)) (make (CallId.fc_ref_of f) true (Formula.ttrue, []))) hcs etr
            else assert false
        | Trace.Nop ->
            aux loc hcs etr
        | Trace.Error ->
            let _ = assert (etr = []) in
            let nd = get tr in
            root (Loc(set tr { nd with closed = false }, path_set_open p)),
            let pres = try [Pred.of_pid env (SimType.find_last_base2 env (get tr).name)] with Not_found -> [] in
            (* assume that fail is NOT a proper subterm of the function definition *)
            Hc(None, snd nd.data @ pres, fst (get tr).data) :: hcs)
  in
  match etr with
    Trace.Call(x, guard)::etr ->
      let ctr, hcs = aux (zipper (make x true (guard, []))) [] etr in
      if flag_coeff then
        let _ = if !Global.debug then assert (not (Util.is_dup (List.filter_map (function Hc(Some(pid, _), _, _) -> Some(pid) | _ -> None) hcs))) in
        ctr, hcs
      else
        let hcss = Util.classify (fun hc1 hc2 -> match hc1, hc2 with Hc(Some(pid1, _), _, _), Hc(Some(pid2, _), _, _) -> pid1 = pid2 | _ -> false) hcs in
        ctr, List.map List.hd hcss
  | _ -> assert false
