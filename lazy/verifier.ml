open ExtList

let verify cexs prog =
  let uid = Ctree.gen () in
  let ret, args =
    Ctree.ret_args
     (Var.V(prog.Prog.main))
     uid
     (SimType.arity (Prog.type_of prog (Var.V(prog.Prog.main))))
  in
  let _, retty = SimType.args_ret (Prog.type_of prog (Var.V(prog.Prog.main))) in
  let init = Term.Ret([], ret, Term.Call([], Term.make_var prog.Prog.main, args), retty) in
  let rt = Ctree.Node((uid, []), init, ref []) in

  let strategy =
    match 0 with
      0 -> Ctree.bf_strategy rt
    | 1 -> Ctree.df_strategy rt
    | 2 -> Ctree.cex_strategy cexs rt
    | _ -> assert false
  in

  try
    let rec loop i =
      let eps = Ctree.manual prog rt strategy in
      let eptrs = List.map Trace.of_error_path eps in
    (*
      let _ = Format.printf "error path trees:@.  @[<v>%a@]@." (Util.pr_list pr_tree "@,") eptrs in
    *)
      let sums = Util.concat_map
        (fun eptr ->
          Format.printf "@.";
          Trace.summaries_of eptr)
        eptrs
      in
      let fcs = List.unique (Util.concat_map Trace.function_calls_of eptrs) in
      let env = SizType.of_summaries (Prog.type_of prog) fcs sums in
      let _ = Format.printf "function summaries:@.  %a@." SizType.pr_fun_env env in
      let env' = List.map (fun (f, sty) -> f, SizType.of_simple_type sty) (List.find_all (fun (f, sty) -> not (List.mem_assoc f env)) (List.map (fun (f, sty) -> Var.make f, sty) prog.Prog.types)) in
      let env = env @ env' in
      if true then
        if SizType.check_prog env prog then
          Format.printf "@.The program is safe@."
        else
          loop (i + 1)
    in
    loop 1
  with Trace.FeasibleErrorTrace(eptr) ->
   Format.printf "@.The program is unsafe@.Error trace: %a@." Trace.pr eptr
