open ExtList

(** Verifier *)

let ext_fdefs = ref ([] : Fdef.t list)
let ext_coeffs = ref  ([] : (Var.t * Term.t) list)
let ext_constrs = ref ([] : Term.t list)

let refineRefTypes prog etrs =
  let sums =
    if true then
      let ctrs, hcss = List.split (List.map (HcGenRefType.cgen (Prog.type_of prog)) etrs) in
      let hcs = List.concat hcss in
      let _ = Format.printf "call trees:@.  @[<v>%a@]@." (Util.pr_list CallTree.pr "@,") ctrs in
      let _ = Format.printf "horn clauses:@.  @[<v>%a@]@." (Util.pr_list HornClause.pr "@,") hcs in
      let hcs =
        if Util.concat_map HornClause.coefficients hcs = [] then
          hcs
        else
          let t = HornClause.formula_of hcs in
          let _ = Format.printf "verification condition:@.  @[<v>%a |= bot@]@." Term.pr t in
(*
          let fvs = Term.fvs t in
          let t = Formula.exists (List.map (fun x -> x, SimType.Int(*???*)) fvs) t in
*)
          let _ =
		          let pr_aux ppf (c, t) = Format.fprintf ppf "%a = %a" Var.pr c Term.pr t in
		          if Cvc3Interface.is_valid (Term.subst (fun x -> List.assoc x !ext_coeffs) t) then
		            Format.printf "solutions (not changed):@.  @[<v>%a@]@." (Util.pr_list pr_aux "@,") !ext_coeffs
		          else
				          let ts =
				            let ts = Farkas.farkas t in
				            List.map
				              (fun t ->
				                let _ = Format.printf "constraint on coefficients:@.  @[<v>%a@]@." Term.pr t in
				                if !Global.use_bit_vector then
				                  t
				                else
				                  let t = Formula.simplify (Formula.linearize t) in
				                  let _ = Format.printf "linearized constraint on coefficients:@.  @[<v>%a@]@." Term.pr t in
				                  let qft =
				                    if Formula.is_linear t then
				                      Formula.simplify (AtpInterface.integer_qelim t)
				                    else
				                      Formula.simplify (AtpInterface.real_qelim t)
				                  in
				                  let _ = Format.printf "quantifier eliminated constraint on coefficients:@.  @[<v>%a@]@." Term.pr qft in
				                  qft)
				              ts
				          in
				          let _ = ext_constrs := ts @ !ext_constrs in
				          let coeffs =
				            if !Global.use_bit_vector then
				              (* find positive coefficients *)
				              let t = Formula.elim_minus (Formula.band !ext_constrs) in
				              let _ = Format.printf "constraint on coefficients:@.  @[<v>%a@]@." Term.pr t in
				              Cvc3Interface.solve_bv t
				            else
				              Cvc3Interface.solve (Formula.band !ext_constrs)
				          in
				          let _ = ext_coeffs := List.filter (fun (c, _) -> Var.is_coeff c) coeffs @ List.filter (fun (c, _) -> not (List.mem_assoc c coeffs)) !ext_coeffs in
				          Format.printf "solutions:@.  @[<v>%a@]@." (Util.pr_list pr_aux "@,") !ext_coeffs
		        in
		        List.map (HornClause.subst (fun x -> List.assoc x !ext_coeffs)) hcs
      in
      let sol = HcSolve.solve ctrs hcs in
      List.map (fun (x, (_, t)) -> `P(x, t)) sol
    else
      let constrss = List.map TcGenRefType.cgen etrs in
      (*
      let _ = Format.printf "constraints:@.  @[<v>%a@]@." (Util.pr_list TraceConstr.pr "@,") constrss in
      *)
      TcSolveRefType.summaries_of (Prog.type_of prog) constrss
  in
  (*
  let _ = List.iter (fun (`P(x, t)) ->
    Format.printf "P[%a]: %a@." Var.pr x Term.pr t) sums
  in
  *)
  let fcs = List.unique (Util.concat_map Trace.function_calls_of etrs) in
  let env = TcSolveRefType.infer_env prog sums fcs in
  env

let refineIntTypes prog etrs =
  let constrss = List.map TcGenIntType.cgen etrs in
(*
  let _ = Format.printf "constraints:@.  @[<v>%a@]@." (Util.pr_list TraceConstr.pr "@,") constrss in
*)
  let sums = Util.concat_map
    (fun constrs ->
      Format.printf "@.";
      TcSolveIntType.summaries_of (Prog.type_of prog) constrs)
    constrss
  in
(*
  let _ = List.iter (function `Pre((x, uid), pre) ->
    Format.printf "Pre(%a,%d): %a@." Var.pr x uid Term.pr pre
  | `Post((x, uid), post) ->
    Format.printf "Post(%a,%d): %a@." Var.pr x uid Term.pr post) sums
  in
*)
  let fcs = List.unique (Util.concat_map Trace.function_calls_of etrs) in
  let env = TcSolveIntType.infer_env prog sums fcs in
  env

let verify prog =
  let _ = if !Global.debug then Format.printf "%a" Prog.pr prog in
  try
    let rt = CompTree.init prog in
    let strategy = if true then CompTreeExpander.bf_strategy rt else CompTreeExpander.df_strategy rt in
    let rec loop old_etrs i =
      let _ = CompTreeExpander.expand_until_new_error_trace_found prog rt strategy in
      let etrs = CompTree.error_traces_of rt in
      let etrs' = Util.diff etrs old_etrs in
      let _ = if !Global.debug then
        Format.printf "error traces:@.";
        List.iter (fun ep -> Format.printf "  %a@." Trace.pr ep) etrs'
      in
      match Global.refine with
        `RefType ->
          let env = refineRefTypes prog etrs(*etrs'*) in
          let _ = Format.printf "refinement types:@.  %a@." RefType.pr_env env in
          if RefTypeCheck.check_prog env prog then
            Format.printf "@.The program is safe@."
          else
            loop etrs (i + 1)
      | `IntType ->
          let env = refineIntTypes prog etrs(*etrs'*) in
          let _ = Format.printf "interaction types:@.  %a@." IntType.pr_env env in
          if IntTypeCheck.check_prog env prog then
            Format.printf "@.The program is safe@."
          else
            loop etrs (i + 1)
    in
    loop [] 1
  with TraceConstr.FeasibleErrorTrace(eptr) ->
   Format.printf "@.The program is unsafe@.Error trace: %a@." TraceConstr.pr eptr

let infer_abst_type cexs prog =
  try
    let prog =
      let xs = List.map (fun fdef -> fdef.Fdef.name) prog.Prog.fdefs in
      let ext_env = List.filter (fun (x, _) -> not (List.mem x xs)) prog.Prog.types in
      if ext_env = [] then
        prog
      else
        let _ =
          if !ext_fdefs = [] then
            let fdefs =
              List.map
                (fun (f, ty) ->
                  let args, ret = SimType.args_ret ty in
                  let xs = List.map (fun _ -> Idnt.new_id ()) args in
                  let body =
                    if (*cps*)true then
                      let args, [cont] = List.split_nth (List.length args - 1) args in
                      let xs, [k] = List.split_nth (List.length xs - 1) xs in
                      let _ = if !Global.debug then assert (List.for_all (fun arg -> SimType.is_base arg) args && not (SimType.is_base cont)) in
                      let const =
                        let c = Var.make_coeff (Idnt.new_cid ()) in
                        let _ = ext_coeffs := (c, Term.tint 0)::!ext_coeffs in
                        Term.make_var c
                      in
                      let coeffs =
                        List.map
                          (fun _ ->
                            let c = Var.make_coeff (Idnt.new_cid ()) in
                            let _ = ext_coeffs := (c, Term.tint 0)::!ext_coeffs in
                            Term.make_var c)
                          xs
                      in
                      let retval = ParLinArith.term_of (List.combine coeffs (List.map (fun x -> Var.make x) xs), const) in
                      Term.apply (Term.make_var (Var.make k)) [retval]
                    else
                      assert false
                  in
                  { Fdef.attr = []; Fdef.name = f; Fdef.args = xs; guard = Formula.ttrue; body = body })
                ext_env
            in
            ext_fdefs := fdefs
        in
        { prog with Prog.fdefs = prog.Prog.fdefs @ !ext_fdefs }
    in
    let _ = if !Global.debug then Format.printf "%a" Prog.pr prog in
    let cexs = [List.hd cexs] in
    let _ = List.iter (fun cex -> Format.printf "%s@." (String.concat ":" (List.map string_of_int cex))) cexs in
    let rt = CompTree.init prog in
    let filter cts = List.filter (fun ct -> List.exists (Util.is_prefix ct.CompTree.path) cexs) cts in
    let strategy = CompTreeExpander.filter_strategy filter rt in
    let _ = CompTreeExpander.expand_all(*expand_until_new_error_trace_found*) prog rt strategy in
    let etrs = CompTree.error_traces_of rt in
    let _ = if !Global.debug then
      Format.printf "error traces:@.";
      List.iter (fun ep -> Format.printf "  %a@." Trace.pr ep) etrs
    in
    let env =
      match Global.refine with
        `RefType ->
          let env = refineRefTypes prog etrs in
          let _ = Format.printf "refinement types:@.  %a@." RefType.pr_env env in
          List.map (fun (f, sty) -> f, AbsType.of_refinement_type sty) env
      | `IntType ->
          let env = refineIntTypes prog etrs in
          let _ = Format.printf "interaction types:@.  %a@." IntType.pr_env env in
          let env = List.map (fun (f, sty) -> f, RefType.of_interaction_type sty) env in
          let _ = Format.printf "refinement types:@.  %a@." RefType.pr_env env in
          List.map (fun (f, sty) -> f, AbsType.of_refinement_type sty) env
          (*
          List.map (fun (f, sty) -> f, AbsType.of_interaction_type f sty) env
          *)
    in
    let env =
      List.map
        (function ((f, sty)::fstys) -> f, AbsType.merge (sty::List.map snd fstys) | _ -> assert false)
        (Util.classify (fun (f1, _) (f2, _) -> f1 = f2) env)
    in
    let _ = Format.printf "abstraction types:@.  %a@." AbsType.pr_env env in
    let fdefs = List.map (fun fdef -> { fdef with Fdef.body = Term.subst (fun x -> List.assoc x !ext_coeffs) fdef.Fdef.body} ) !ext_fdefs in
    let _ = Format.printf "external function definitions:@.  @[<v>%a@]@." (Util.pr_list Fdef.pr "@,") fdefs in
    env, fdefs
  with Util.NotImplemented s ->
    let _ = Format.printf "not implemented in %s@." s in
    assert false
