open ExtList

let aux str cex prog =
  let uid = Ctree.gen () in
  let ty_main = Prog.type_of prog (Var.V(prog.Prog.main)) in
  let ret, args =
    Ctree.ret_args
     (Var.V(prog.Prog.main))
     uid
     (SimType.arity ty_main)
  in
  let _, retty = SimType.args_ret ty_main in
  let init = Term.Ret([], ret, Term.Call([], Term.make_var prog.Prog.main, args), retty) in
  let rt = Ctree.Node((uid, []), init, ref []) in

  let strategy =
    match str with
      0 -> Ctree.bf_strategy rt
    | 1 -> Ctree.df_strategy rt
    | 2 -> Ctree.cex_strategy cex rt
    | _ -> assert false
  in
  rt, strategy

let infer_type prog rt strategy =
  let eps, strategy = Ctree.expands false prog rt strategy in
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
(*
  let _ = List.iter (function `Pre((x, uid), pre) ->
    Format.printf "Pre(%a,%d): %a@." Var.pr x uid Term.pr pre
  | `Post((x, uid), post) ->
    Format.printf "Post(%a,%d): %a@." Var.pr x uid Term.pr post) sums
  in
*)
  let fcs = List.unique (Util.concat_map Trace.function_calls_of eptrs) in
  let env = SizType.of_summaries (Prog.type_of prog) fcs sums in
  let _ = Format.printf "function summaries:@.  %a@." SizType.pr_fun_env env in
  let env' =
    List.map
      (fun (f, sty) ->
        f, SizType.of_simple_type sty)
      (List.find_all
        (fun (f, sty) -> not (List.mem_assoc f env))
        (List.map
          (fun (f, sty) -> Var.make f, sty)
          prog.Prog.types))
  in
  env @ env'

let verify cex prog =
  let rt, strategy = aux 0 cex prog in
  try
    let rec loop i =
      let env = infer_type prog rt strategy in
      if true then
        if SizType.check_prog env prog then
          Format.printf "@.The program is safe@."
        else
          loop (i + 1)
    in
    loop 1
  with Trace.FeasibleErrorTrace(eptr) ->
   Format.printf "@.The program is unsafe@.Error trace: %a@." Trace.pr eptr


let infer_abst_type cex prog =
  let rt, strategy = aux 0 cex prog in
  let env = infer_type prog rt strategy in
  let env = List.map (fun (f, sty) -> f, RefType.of_sized_type sty) env in
  let env = List.map (fun (f, sty) -> f, AbsType.of_refinement_type sty) env in  
  let env = List.map (fun ((f, sty)::fstys) -> f, AbsType.merge (sty::List.map snd fstys)) (Util.classify (fun (f1, _) (f2, _) -> f1 = f2) env) in
  let _ = Format.printf "abstraction types:@.  %a@." AbsType.pr_env env in
  env
