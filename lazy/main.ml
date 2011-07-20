open ExtList

let rec loop old_eps prog env rt wl =
  if wl = [] then
    ()
  else
    let _ = Ctree.save_as_dot "ctree.dot" rt wl in
    let eps = List.filter (fun p -> List.last p = Ctree.Error) (Ctree.paths_of rt) in
    if old_eps = eps then
				    let env, wl = Ctree.expand_tree prog env wl in
				    loop eps prog env rt wl
    else
				  let _ =
				    Format.printf "error paths:@.";
				    List.iter (fun ep -> Format.printf "  %a@." Ctree.pr_path ep) eps
				  in
				  let _ = Format.printf "expand the computation tree ? (y/n): %!" in
				  let inp = read_line () in
				  if inp = "y" then
				    let env, wl = Ctree.expand_tree prog env wl in
				    loop eps prog env rt wl
				  else if inp = "n" then
		      let eptrs = List.map Trace.of_error_path eps in
		(*
		      let _ = Format.printf "error path trees:@.  @[<v>%a@]@." (Util.pr_list Ctree.pr_tree "@,") eptrs in
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
						      let pr ppf ((f, uid), rty) = Format.fprintf ppf "<%a:%d>: %a" Id.pr f uid RefType.pr rty in
						      Format.printf "function summaries:@.  @[<v>%a@]@." (Util.pr_list pr "@ ") rtys)
          sumss
				  else
				    loop eps prog env rt wl

let init_env x =
  let _ = Format.printf "%a not found@." Var.pr x in
  assert false

let test1 () =
  let arg1 = Term.apply (Term.make_var "sum") [Term.make_var "n"] in
  let arg2 = Term.make_var "n" in
  let main = { Fdef.attr = []; Fdef.name = Id.make "main"; Fdef.args = [Id.make "n"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "check") [arg1; arg2] } in
  let sum1 = { Fdef.attr = []; Fdef.name = Id.make "sum"; Fdef.args = [Id.make "x"]; Fdef.guard = Term.leq (Term.make_var "x") (Term.make_int 0); Fdef.body = Term.make_int 0 } in
  let sum2 = { Fdef.attr = []; Fdef.name = Id.make "sum"; Fdef.args = [Id.make "x"]; Fdef.guard = Term.gt (Term.make_var "x") (Term.make_int 0); Fdef.body = Term.add (Term.make_var "x") (Term.apply (Term.make_var "sum") [Term.sub (Term.make_var "x") (Term.make_int 1)])} in
  let check1 = { Fdef.attr = []; Fdef.name = Id.make "check"; Fdef.args = [Id.make "x1"; Id.make "x2"]; guard = Term.geq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_unit} in
  let check2 = { Fdef.attr = []; Fdef.name = Id.make "check"; Fdef.args = [Id.make "x1"; Id.make "x2"]; guard = Term.lt (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_event "fail"} in
  let tymain = Type.Fun(Type.Int, Type.Unit) in
  let tysum = Type.Fun(Type.Int, Type.Int) in
  let tycheck = Type.Fun(Type.Int, Type.Fun(Type.Int, Type.Unit)) in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; sum1; sum2; check1; check2];
               Prog.types = [Id.make "main", tymain; Id.make "sum", tysum; Id.make "check", tycheck];
               Prog.main = main.Fdef.name } in
(*
  let prog = Prog.set_arity (Prog.arities prog) prog in
*)
  Format.printf "%a" Prog.pr prog;
(*
  let arity = Prog.arities prog (Var.V(prog.Prog.main)) in
*)
  let p = Ctree.gen () in
  let ret, args = Ctree.ret_args (Var.V(prog.Prog.main)) p (Type.arity (Prog.type_of prog (Var.V(prog.Prog.main)))) in
  let init = Term.Ret([], ret, Term.Call([], Term.make_var prog.Prog.main, args)) in
(*
  let init = Term.set_arity (Prog.arities prog) init in
*)
  let rt = Ctree.Node(p, init, ref []) in
  loop [] prog init_env rt [rt]

let test2 () =
  let main = { Fdef.attr = []; Fdef.name = Id.make "main"; Fdef.args = [Id.make "n"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "check") [Term.apply (Term.make_var "copy") [Term.apply (Term.make_var "copy") [Term.make_var "n"]]; Term.make_var "n"] } in
  let copy1 = { Fdef.attr = []; Fdef.name = Id.make "copy"; Fdef.args = [Id.make "x"]; Fdef.guard = Term.eq (Term.make_var "x") (Term.make_int 0); Fdef.body = Term.make_int 0 } in
  let copy2 = { Fdef.attr = []; Fdef.name = Id.make "copy"; Fdef.args = [Id.make "x"]; Fdef.guard = Term.neq (Term.make_var "x") (Term.make_int 0); Fdef.body = Term.add (Term.make_int 1) (Term.apply (Term.make_var "copy") [Term.sub (Term.make_var "x") (Term.make_int 1)])} in
  let check1 = { Fdef.attr = []; Fdef.name = Id.make "check"; Fdef.args = [Id.make "x1"; Id.make "x2"]; guard = Term.eq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_unit} in
  let check2 = { Fdef.attr = []; Fdef.name = Id.make "check"; Fdef.args = [Id.make "x1"; Id.make "x2"]; guard = Term.neq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_event "fail"} in
  let tymain = Type.Fun(Type.Int, Type.Unit) in
  let tycopy = Type.Fun(Type.Int, Type.Int) in
  let tycheck = Type.Fun(Type.Int, Type.Fun(Type.Int, Type.Unit)) in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; copy1; copy2; check1; check2];
               Prog.types = [Id.make "main", tymain; Id.make "copy", tycopy; Id.make "check", tycheck];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  let p = Ctree.gen () in
  let ret, args =
    Ctree.ret_args
     (Var.V(prog.Prog.main))
     p
     (Type.arity (Prog.type_of prog (Var.V(prog.Prog.main))))
  in
  let init = Term.Ret([], ret, Term.Call([], Term.make_var prog.Prog.main, args)) in
  let rt = Ctree.Node(p, init, ref []) in
  loop [] prog init_env rt [rt]

let test3 () =
  let main = { Fdef.attr = []; Fdef.name = Id.make "main"; Fdef.args = [Id.make "n"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "apply") [Term.apply (Term.make_var "check") [Term.make_var "n"]; Term.make_var "n"] } in
  let apply = { Fdef.attr = []; Fdef.name = Id.make "apply"; Fdef.args = [Id.make "f"; Id.make "x"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "f") [Term.make_var "x"] } in
  let check1 = { Fdef.attr = []; Fdef.name = Id.make "check"; Fdef.args = [Id.make "x1"; Id.make "x2"]; guard = Term.eq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_unit} in
  let check2 = { Fdef.attr = []; Fdef.name = Id.make "check"; Fdef.args = [Id.make "x1"; Id.make "x2"]; guard = Term.neq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_event "fail"} in
  let tymain = Type.Fun(Type.Int, Type.Unit) in
  let tyapply = Type.Fun(Type.Fun(Type.Int, Type.Unit), Type.Fun(Type.Int, Type.Unit)) in
  let tycheck = Type.Fun(Type.Int, Type.Fun(Type.Int, Type.Unit)) in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; apply; check1; check2];
               Prog.types = [Id.make "main", tymain; Id.make "apply", tyapply; Id.make "check", tycheck];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  let p = Ctree.gen () in
  let ret, args = Ctree.ret_args (Var.V(prog.Prog.main)) p (Type.arity (Prog.type_of prog (Var.V(prog.Prog.main)))) in
  let init = Term.Ret([], ret, Term.Call([], Term.make_var prog.Prog.main, args)) in
  let rt = Ctree.Node(p, init, ref []) in
  loop [] prog init_env rt [rt]

let test4 () =
  let main = { Fdef.attr = []; Fdef.name = Id.make "main"; Fdef.args = [Id.make "n"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "checkh") [Term.apply (Term.make_var "h") [Term.make_var "n"]; Term.apply (Term.make_var "h") [Term.make_var "n"]] } in
  let checkh = { Fdef.attr = []; Fdef.name = Id.make "checkh"; Fdef.args = [Id.make "f"; Id.make "g"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "check") [Term.apply (Term.make_var "f") [Term.make_unit]; Term.apply (Term.make_var "g") [Term.make_unit]] } in
  let check1 = { Fdef.attr = []; Fdef.name = Id.make "check"; Fdef.args = [Id.make "x1"; Id.make "x2"]; guard = Term.eq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_unit} in
  let check2 = { Fdef.attr = []; Fdef.name = Id.make "check"; Fdef.args = [Id.make "x1"; Id.make "x2"]; guard = Term.neq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_event "fail"} in
  let h = { Fdef.attr = []; Fdef.name = Id.make "h"; Fdef.args = [Id.make "x"; Id.make "un"]; guard = Term.make_true; Fdef.body = Term.make_var "x"} in
  let tymain = Type.Fun(Type.Int, Type.Unit) in
  let tycheckh = Type.Fun(Type.Fun(Type.Unit, Type.Int), Type.Fun(Type.Fun(Type.Unit, Type.Int), Type.Unit)) in
  let tycheck = Type.Fun(Type.Int, Type.Fun(Type.Int, Type.Unit)) in
  let tyh = Type.Fun(Type.Int, Type.Fun(Type.Unit, Type.Int)) in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; checkh; check1; check2; h];
               Prog.types = [Id.make "main", tymain; Id.make "checkh", tycheckh; Id.make "check", tycheck; Id.make "h", tyh];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  let p = Ctree.gen () in
  let ret, args = Ctree.ret_args (Var.V(prog.Prog.main)) p (Type.arity (Prog.type_of prog (Var.V(prog.Prog.main)))) in
  let init = Term.Ret([], ret, Term.Call([], Term.make_var prog.Prog.main, args)) in
  let rt = Ctree.Node(p, init, ref []) in
  loop [] prog init_env rt [rt]

let test5 () =
  let main = { Fdef.attr = []; Fdef.name = Id.make "main"; Fdef.args = [Id.make "n"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "h") [Term.make_var "apply"; Term.make_var "check"; Term.make_var "n"] } in
  let h = { Fdef.attr = []; Fdef.name = Id.make "h"; Fdef.args = [Id.make "f"; Id.make "g"; Id.make "x"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "f") [Term.apply (Term.make_var "g") [Term.make_var "x"]; Term.make_var "x"] } in
  let apply = { Fdef.attr = []; Fdef.name = Id.make "apply"; Fdef.args = [Id.make "f"; Id.make "x"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "f") [Term.make_var "x"] } in
  let check1 = { Fdef.attr = []; Fdef.name = Id.make "check"; Fdef.args = [Id.make "x1"; Id.make "x2"]; guard = Term.eq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_unit} in
  let check2 = { Fdef.attr = []; Fdef.name = Id.make "check"; Fdef.args = [Id.make "x1"; Id.make "x2"]; guard = Term.neq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_event "fail"} in
  let tymain = Type.Fun(Type.Int, Type.Unit) in
  let tyh = Type.Fun
    (Type.Fun
      (Type.Fun(Type.Int, Type.Unit),
      Type.Fun(Type.Int, Type.Unit)),
    Type.Fun
      (Type.Fun(Type.Int, Type.Fun(Type.Int, Type.Unit)),
      Type.Fun(Type.Int, Type.Unit))) in
  let tyapply = Type.Fun(Type.Fun(Type.Int, Type.Unit), Type.Fun(Type.Int, Type.Unit)) in
  let tycheck = Type.Fun(Type.Int, Type.Fun(Type.Int, Type.Unit)) in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; h; apply; check1; check2];
               Prog.types = [Id.make "main", tymain; Id.make "h", tyh; Id.make "apply", tyapply; Id.make "check", tycheck];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  let p = Ctree.gen () in
  let ret, args = Ctree.ret_args (Var.V(prog.Prog.main)) p (Type.arity (Prog.type_of prog (Var.V(prog.Prog.main)))) in
  let init = Term.Ret([], ret, Term.Call([], Term.make_var prog.Prog.main, args)) in
  let rt = Ctree.Node(p, init, ref []) in
  loop [] prog init_env rt [rt]

let test6 () =
  let main = { Fdef.attr = []; Fdef.name = Id.make "main"; Fdef.args = [Id.make "n"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "h") [Term.make_var "apply"; Term.apply (Term.make_var "check") [Term.make_var "n"]; Term.make_var "n"] } in
  let h = { Fdef.attr = []; Fdef.name = Id.make "h"; Fdef.args = [Id.make "f"; Id.make "g"; Id.make "x"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "f") [Term.make_var "g"; Term.make_var "x"] } in
  let apply = { Fdef.attr = []; Fdef.name = Id.make "apply"; Fdef.args = [Id.make "f"; Id.make "x"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "f") [Term.make_var "x"] } in
  let check1 = { Fdef.attr = []; Fdef.name = Id.make "check"; Fdef.args = [Id.make "x1"; Id.make "x2"]; guard = Term.eq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_unit} in
  let check2 = { Fdef.attr = []; Fdef.name = Id.make "check"; Fdef.args = [Id.make "x1"; Id.make "x2"]; guard = Term.neq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_event "fail"} in
  let tymain = Type.Fun(Type.Int, Type.Unit) in
  let tyh = Type.Fun
    (Type.Fun
      (Type.Fun(Type.Int, Type.Unit),
      Type.Fun(Type.Int, Type.Unit)),
    Type.Fun
      (Type.Fun(Type.Int, Type.Unit),
      Type.Fun(Type.Int, Type.Unit))) in
  let tyapply = Type.Fun(Type.Fun(Type.Int, Type.Unit), Type.Fun(Type.Int, Type.Unit)) in
  let tycheck = Type.Fun(Type.Int, Type.Fun(Type.Int, Type.Unit)) in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; h; apply; check1; check2];
               Prog.types = [Id.make "main", tymain; Id.make "h", tyh; Id.make "apply", tyapply; Id.make "check", tycheck];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  let p = Ctree.gen () in
  let ret, args = Ctree.ret_args (Var.V(prog.Prog.main)) p (Type.arity (Prog.type_of prog (Var.V(prog.Prog.main)))) in
  let init = Term.Ret([], ret, Term.Call([], Term.make_var prog.Prog.main, args)) in
  let rt = Ctree.Node(p, init, ref []) in
  loop [] prog init_env rt [rt]

let test7 () =
  let main = { Fdef.attr = []; Fdef.name = Id.make "main"; Fdef.args = [Id.make "n"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "apply") [Term.apply (Term.make_var "apply") [Term.apply (Term.make_var "check") [Term.make_var "n"]]; Term.make_var "n"] } in
  let apply = { Fdef.attr = []; Fdef.name = Id.make "apply"; Fdef.args = [Id.make "f"; Id.make "x"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "f") [Term.make_var "x"] } in
  let check1 = { Fdef.attr = []; Fdef.name = Id.make "check"; Fdef.args = [Id.make "x1"; Id.make "x2"]; guard = Term.eq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_unit} in
  let check2 = { Fdef.attr = []; Fdef.name = Id.make "check"; Fdef.args = [Id.make "x1"; Id.make "x2"]; guard = Term.neq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_event "fail"} in
  let tymain = Type.Fun(Type.Int, Type.Unit) in
  let tyapply = Type.Fun(Type.Fun(Type.Int, Type.Unit), Type.Fun(Type.Int, Type.Unit)) in
  let tycheck = Type.Fun(Type.Int, Type.Fun(Type.Int, Type.Unit)) in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; apply; check1; check2];
               Prog.types = [Id.make "main", tymain; Id.make "apply", tyapply; Id.make "check", tycheck];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  let p = Ctree.gen () in
  let ret, args = Ctree.ret_args (Var.V(prog.Prog.main)) p (Type.arity (Prog.type_of prog (Var.V(prog.Prog.main)))) in
  let init = Term.Ret([], ret, Term.Call([], Term.make_var prog.Prog.main, args)) in
  let rt = Ctree.Node(p, init, ref []) in
  loop [] prog init_env rt [rt]

let test8 () =
  let main = { Fdef.attr = []; Fdef.name = Id.make "main"; Fdef.args = [Id.make "n"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "apply") [Term.apply (Term.make_var "apply2") [Term.make_var "check"; Term.make_var "n"]; Term.make_var "n"] } in
  let apply = { Fdef.attr = []; Fdef.name = Id.make "apply"; Fdef.args = [Id.make "f"; Id.make "x"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "f") [Term.make_var "x"] } in
  let apply2 = { Fdef.attr = []; Fdef.name = Id.make "apply2"; Fdef.args = [Id.make "f"; Id.make "x"; Id.make "y"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "f") [Term.make_var "x"; Term.make_var "y"] } in
  let check1 = { Fdef.attr = []; Fdef.name = Id.make "check"; Fdef.args = [Id.make "x1"; Id.make "x2"]; guard = Term.eq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_unit} in
  let check2 = { Fdef.attr = []; Fdef.name = Id.make "check"; Fdef.args = [Id.make "x1"; Id.make "x2"]; guard = Term.neq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_event "fail"} in
  let tymain = Type.Fun(Type.Int, Type.Unit) in
  let tyapply = Type.Fun(Type.Fun(Type.Int, Type.Unit), Type.Fun(Type.Int, Type.Unit)) in
  let tyapply2 = Type.Fun(Type.Fun(Type.Int, Type.Fun(Type.Int, Type.Unit)), Type.Fun(Type.Int, Type.Fun(Type.Int, Type.Unit))) in
  let tycheck = Type.Fun(Type.Int, Type.Fun(Type.Int, Type.Unit)) in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; apply; apply2; check1; check2];
               Prog.types = [Id.make "main", tymain; Id.make "apply", tyapply; Id.make "apply2", tyapply2; Id.make "check", tycheck];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  let p = Ctree.gen () in
  let ret, args = Ctree.ret_args (Var.V(prog.Prog.main)) p (Type.arity (Prog.type_of prog (Var.V(prog.Prog.main)))) in
  let init = Term.Ret([], ret, Term.Call([], Term.make_var prog.Prog.main, args)) in
  let rt = Ctree.Node(p, init, ref []) in
  loop [] prog init_env rt [rt]

let _ = test8 ()
