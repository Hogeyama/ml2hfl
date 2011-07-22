open ExtList

let infer cexs prog =
  let uid = Ctree.gen () in
  let ret, args =
    Ctree.ret_args
     (Var.V(prog.Prog.main))
     uid
     (SimType.arity (Prog.type_of prog (Var.V(prog.Prog.main))))
  in
  let init = Term.Ret([], ret, Term.Call([], Term.make_var prog.Prog.main, args)) in
  let rt = Ctree.Node((uid, []), init, ref []) in

  let strategy =
    match 2 with
      0 -> Ctree.bf_strategy rt
    | 1 -> Ctree.df_strategy rt
    | 2 -> Ctree.cex_strategy cexs rt
  in
  let eps = Ctree.auto prog rt strategy in
		let eptrs = List.map Trace.of_error_path eps in
(*
		let _ = Format.printf "error path trees:@.  @[<v>%a@]@." (Util.pr_list pr_tree "@,") eptrs in
*)
		let sumss = List.map
				(fun eptr ->
      Format.printf "@.";
      Trace.summaries_of eptr)
		  eptrs
		in
  List.iter
    (fun sums ->
						let rtys = RefType.of_summaries prog.Prog.types sums in
						let pr ppf ((f, uid), rty) = Format.fprintf ppf "<%a:%d>: %a" Idnt.pr f uid RefType.pr rty in
						Format.printf "function summaries:@.  @[<v>%a@]@." (Util.pr_list pr "@ ") rtys)
    sumss

let test1 () =
  let arg1 = Term.apply (Term.make_var "sum") [Term.make_var "n"] in
  let arg2 = Term.make_var "n" in
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "check") [arg1; arg2] } in
  let sum1 = { Fdef.attr = []; Fdef.name = Idnt.make "sum"; Fdef.args = [Idnt.make "x"]; Fdef.guard = Term.leq (Term.make_var "x") (Term.make_int 0); Fdef.body = Term.make_int 0 } in
  let sum2 = { Fdef.attr = []; Fdef.name = Idnt.make "sum"; Fdef.args = [Idnt.make "x"]; Fdef.guard = Term.gt (Term.make_var "x") (Term.make_int 0); Fdef.body = Term.add (Term.make_var "x") (Term.apply (Term.make_var "sum") [Term.sub (Term.make_var "x") (Term.make_int 1)])} in
  let check1 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.geq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_unit} in
  let check2 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.lt (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_event "fail"} in
  let tymain = SimType.Fun(SimType.Int, SimType.Unit) in
  let tysum = SimType.Fun(SimType.Int, SimType.Int) in
  let tycheck = SimType.Fun(SimType.Int, SimType.Fun(SimType.Int, SimType.Unit)) in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; sum1; sum2; check1; check2];
               Prog.types = [Idnt.make "main", tymain; Idnt.make "sum", tysum; Idnt.make "check", tycheck];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  infer [] prog

let test2 () =
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "check") [Term.apply (Term.make_var "copy") [Term.apply (Term.make_var "copy") [Term.make_var "n"]]; Term.make_var "n"] } in
  let copy1 = { Fdef.attr = []; Fdef.name = Idnt.make "copy"; Fdef.args = [Idnt.make "x"]; Fdef.guard = Term.eq (Term.make_var "x") (Term.make_int 0); Fdef.body = Term.make_int 0 } in
  let copy2 = { Fdef.attr = []; Fdef.name = Idnt.make "copy"; Fdef.args = [Idnt.make "x"]; Fdef.guard = Term.neq (Term.make_var "x") (Term.make_int 0); Fdef.body = Term.add (Term.make_int 1) (Term.apply (Term.make_var "copy") [Term.sub (Term.make_var "x") (Term.make_int 1)])} in
  let check1 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.eq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_unit} in
  let check2 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.neq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_event "fail"} in
  let tymain = SimType.Fun(SimType.Int, SimType.Unit) in
  let tycopy = SimType.Fun(SimType.Int, SimType.Int) in
  let tycheck = SimType.Fun(SimType.Int, SimType.Fun(SimType.Int, SimType.Unit)) in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; copy1; copy2; check1; check2];
               Prog.types = [Idnt.make "main", tymain; Idnt.make "copy", tycopy; Idnt.make "check", tycheck];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  infer [] prog

let test3 () =
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "apply") [Term.apply (Term.make_var "check") [Term.make_var "n"]; Term.make_var "n"] } in
  let apply = { Fdef.attr = []; Fdef.name = Idnt.make "apply"; Fdef.args = [Idnt.make "f"; Idnt.make "x"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "f") [Term.make_var "x"] } in
  let check1 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.eq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_unit} in
  let check2 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.neq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_event "fail"} in
  let tymain = SimType.Fun(SimType.Int, SimType.Unit) in
  let tyapply = SimType.Fun(SimType.Fun(SimType.Int, SimType.Unit), SimType.Fun(SimType.Int, SimType.Unit)) in
  let tycheck = SimType.Fun(SimType.Int, SimType.Fun(SimType.Int, SimType.Unit)) in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; apply; check1; check2];
               Prog.types = [Idnt.make "main", tymain; Idnt.make "apply", tyapply; Idnt.make "check", tycheck];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  infer [] prog

let test4 () =
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "checkh") [Term.apply (Term.make_var "h") [Term.make_var "n"]; Term.apply (Term.make_var "h") [Term.make_var "n"]] } in
  let checkh = { Fdef.attr = []; Fdef.name = Idnt.make "checkh"; Fdef.args = [Idnt.make "f"; Idnt.make "g"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "check") [Term.apply (Term.make_var "f") [Term.make_unit]; Term.apply (Term.make_var "g") [Term.make_unit]] } in
  let check1 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.eq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_unit} in
  let check2 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.neq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_event "fail"} in
  let h = { Fdef.attr = []; Fdef.name = Idnt.make "h"; Fdef.args = [Idnt.make "x"; Idnt.make "un"]; guard = Term.make_true; Fdef.body = Term.make_var "x"} in
  let tymain = SimType.Fun(SimType.Int, SimType.Unit) in
  let tycheckh = SimType.Fun(SimType.Fun(SimType.Unit, SimType.Int), SimType.Fun(SimType.Fun(SimType.Unit, SimType.Int), SimType.Unit)) in
  let tycheck = SimType.Fun(SimType.Int, SimType.Fun(SimType.Int, SimType.Unit)) in
  let tyh = SimType.Fun(SimType.Int, SimType.Fun(SimType.Unit, SimType.Int)) in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; checkh; check1; check2; h];
               Prog.types = [Idnt.make "main", tymain; Idnt.make "checkh", tycheckh; Idnt.make "check", tycheck; Idnt.make "h", tyh];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  infer [] prog

let test5 () =
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "h") [Term.make_var "apply"; Term.make_var "check"; Term.make_var "n"] } in
  let h = { Fdef.attr = []; Fdef.name = Idnt.make "h"; Fdef.args = [Idnt.make "f"; Idnt.make "g"; Idnt.make "x"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "f") [Term.apply (Term.make_var "g") [Term.make_var "x"]; Term.make_var "x"] } in
  let apply = { Fdef.attr = []; Fdef.name = Idnt.make "apply"; Fdef.args = [Idnt.make "f"; Idnt.make "x"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "f") [Term.make_var "x"] } in
  let check1 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.eq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_unit} in
  let check2 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.neq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_event "fail"} in
  let tymain = SimType.Fun(SimType.Int, SimType.Unit) in
  let tyh = SimType.Fun
    (SimType.Fun
      (SimType.Fun(SimType.Int, SimType.Unit),
      SimType.Fun(SimType.Int, SimType.Unit)),
    SimType.Fun
      (SimType.Fun(SimType.Int, SimType.Fun(SimType.Int, SimType.Unit)),
      SimType.Fun(SimType.Int, SimType.Unit))) in
  let tyapply = SimType.Fun(SimType.Fun(SimType.Int, SimType.Unit), SimType.Fun(SimType.Int, SimType.Unit)) in
  let tycheck = SimType.Fun(SimType.Int, SimType.Fun(SimType.Int, SimType.Unit)) in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; h; apply; check1; check2];
               Prog.types = [Idnt.make "main", tymain; Idnt.make "h", tyh; Idnt.make "apply", tyapply; Idnt.make "check", tycheck];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  infer [] prog

let test6 () =
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "h") [Term.make_var "apply"; Term.apply (Term.make_var "check") [Term.make_var "n"]; Term.make_var "n"] } in
  let h = { Fdef.attr = []; Fdef.name = Idnt.make "h"; Fdef.args = [Idnt.make "f"; Idnt.make "g"; Idnt.make "x"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "f") [Term.make_var "g"; Term.make_var "x"] } in
  let apply = { Fdef.attr = []; Fdef.name = Idnt.make "apply"; Fdef.args = [Idnt.make "f"; Idnt.make "x"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "f") [Term.make_var "x"] } in
  let check1 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.eq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_unit} in
  let check2 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.neq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_event "fail"} in
  let tymain = SimType.Fun(SimType.Int, SimType.Unit) in
  let tyh = SimType.Fun
    (SimType.Fun
      (SimType.Fun(SimType.Int, SimType.Unit),
      SimType.Fun(SimType.Int, SimType.Unit)),
    SimType.Fun
      (SimType.Fun(SimType.Int, SimType.Unit),
      SimType.Fun(SimType.Int, SimType.Unit))) in
  let tyapply = SimType.Fun(SimType.Fun(SimType.Int, SimType.Unit), SimType.Fun(SimType.Int, SimType.Unit)) in
  let tycheck = SimType.Fun(SimType.Int, SimType.Fun(SimType.Int, SimType.Unit)) in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; h; apply; check1; check2];
               Prog.types = [Idnt.make "main", tymain; Idnt.make "h", tyh; Idnt.make "apply", tyapply; Idnt.make "check", tycheck];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  infer [] prog


let test7 () =
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "apply") [Term.apply (Term.make_var "apply") [Term.apply (Term.make_var "check") [Term.make_var "n"]]; Term.make_var "n"] } in
  let apply = { Fdef.attr = []; Fdef.name = Idnt.make "apply"; Fdef.args = [Idnt.make "f"; Idnt.make "x"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "f") [Term.make_var "x"] } in
  let check1 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.eq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_unit} in
  let check2 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.neq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_event "fail"} in
  let tymain = SimType.Fun(SimType.Int, SimType.Unit) in
  let tyapply = SimType.Fun(SimType.Fun(SimType.Int, SimType.Unit), SimType.Fun(SimType.Int, SimType.Unit)) in
  let tycheck = SimType.Fun(SimType.Int, SimType.Fun(SimType.Int, SimType.Unit)) in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; apply; check1; check2];
               Prog.types = [Idnt.make "main", tymain; Idnt.make "apply", tyapply; Idnt.make "check", tycheck];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  infer [] prog

let test8 () =
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "apply") [Term.apply (Term.make_var "apply2") [Term.make_var "check"; Term.make_var "n"]; Term.make_var "n"] } in
  let apply = { Fdef.attr = []; Fdef.name = Idnt.make "apply"; Fdef.args = [Idnt.make "f"; Idnt.make "x"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "f") [Term.make_var "x"] } in
  let apply2 = { Fdef.attr = []; Fdef.name = Idnt.make "apply2"; Fdef.args = [Idnt.make "f"; Idnt.make "x"; Idnt.make "y"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "f") [Term.make_var "x"; Term.make_var "y"] } in
  let check1 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.eq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_unit} in
  let check2 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.neq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_event "fail"} in
  let tymain = SimType.Fun(SimType.Int, SimType.Unit) in
  let tyapply = SimType.Fun(SimType.Fun(SimType.Int, SimType.Unit), SimType.Fun(SimType.Int, SimType.Unit)) in
  let tyapply2 = SimType.Fun(SimType.Fun(SimType.Int, SimType.Fun(SimType.Int, SimType.Unit)), SimType.Fun(SimType.Int, SimType.Fun(SimType.Int, SimType.Unit))) in
  let tycheck = SimType.Fun(SimType.Int, SimType.Fun(SimType.Int, SimType.Unit)) in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; apply; apply2; check1; check2];
               Prog.types = [Idnt.make "main", tymain; Idnt.make "apply", tyapply; Idnt.make "apply2", tyapply2; Idnt.make "check", tycheck];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  infer [[0; 0; 0; 1]] prog

let _ = test8 ()
