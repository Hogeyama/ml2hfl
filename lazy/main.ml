open ExtList

let test_sum () =
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = Term.ttrue; Fdef.body = Term.apply (Term.make_var "check") [Term.apply (Term.make_var "sum") [Term.make_var "n"]; Term.make_var "n"] } in
  let sum1 = { Fdef.attr = []; Fdef.name = Idnt.make "sum"; Fdef.args = [Idnt.make "x"]; Fdef.guard = Term.leq (Term.make_var "x") (Term.tint 0); Fdef.body = Term.tint 0 } in
  let sum2 = { Fdef.attr = []; Fdef.name = Idnt.make "sum"; Fdef.args = [Idnt.make "x"]; Fdef.guard = Term.gt (Term.make_var "x") (Term.tint 0); Fdef.body = Term.add (Term.make_var "x") (Term.apply (Term.make_var "sum") [Term.sub (Term.make_var "x") (Term.tint 1)])} in
  let check1 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.geq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.tunit} in
  let check2 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.lt (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.tfail} in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; sum1; sum2; check1; check2];
               Prog.types = [Idnt.make "main", SimType.tfun [SimType.Int; SimType.Unit];
                             Idnt.make "sum", SimType.tfun [SimType.Int; SimType.Int];
                             Idnt.make "check", SimType.tfun [SimType.Int; SimType.tfun [SimType.Int; SimType.Unit]]];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  prog

let test_sum_assert () =
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = Term.ttrue; Fdef.body = Term.apply (Term.make_var "assert") [Term.geq (Term.apply (Term.make_var "sum") [Term.make_var "n"]) (Term.make_var "n")] } in
  let sum1 = { Fdef.attr = []; Fdef.name = Idnt.make "sum"; Fdef.args = [Idnt.make "x"]; Fdef.guard = Term.leq (Term.make_var "x") (Term.tint 0); Fdef.body = Term.tint 0 } in
  let sum2 = { Fdef.attr = []; Fdef.name = Idnt.make "sum"; Fdef.args = [Idnt.make "x"]; Fdef.guard = Term.gt (Term.make_var "x") (Term.tint 0); Fdef.body = Term.add (Term.make_var "x") (Term.apply (Term.make_var "sum") [Term.sub (Term.make_var "x") (Term.tint 1)])} in
  let assert1 = { Fdef.attr = []; Fdef.name = Idnt.make "assert"; Fdef.args = [Idnt.make "b"]; guard = Term.eqBool (Term.make_var "b") (Term.ttrue); Fdef.body = Term.tunit} in
  let assert2 = { Fdef.attr = []; Fdef.name = Idnt.make "assert"; Fdef.args = [Idnt.make "b"]; guard = Term.eqBool (Term.make_var "b") (Term.tfalse); Fdef.body = Term.tfail} in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; sum1; sum2; assert1; assert2];
               Prog.types = [Idnt.make "main", SimType.tfun [SimType.Int; SimType.Unit];
                             Idnt.make "sum", SimType.tfun [SimType.Int; SimType.Int];
                             Idnt.make "assert", SimType.tfun [SimType.Bool; SimType.Unit]];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  prog

let test_copy_copy () =
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = Term.ttrue; Fdef.body = Term.apply (Term.make_var "check") [Term.apply (Term.make_var "copy") [Term.apply (Term.make_var "copy") [Term.make_var "n"]]; Term.make_var "n"] } in
  let copy1 = { Fdef.attr = []; Fdef.name = Idnt.make "copy"; Fdef.args = [Idnt.make "x"]; Fdef.guard = Term.eqInt (Term.make_var "x") (Term.tint 0); Fdef.body = Term.tint 0 } in
  let copy2 = { Fdef.attr = []; Fdef.name = Idnt.make "copy"; Fdef.args = [Idnt.make "x"]; Fdef.guard = Term.neqInt (Term.make_var "x") (Term.tint 0); Fdef.body = Term.add (Term.tint 1) (Term.apply (Term.make_var "copy") [Term.sub (Term.make_var "x") (Term.tint 1)])} in
  let check1 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.eqInt (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.tunit} in
  let check2 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.neqInt (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.tfail} in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; copy1; copy2; check1; check2];
               Prog.types = [Idnt.make "main", SimType.tfun [SimType.Int; SimType.Unit];
                             Idnt.make "copy", SimType.tfun [SimType.Int; SimType.Int];
                             Idnt.make "check", SimType.tfun [SimType.Int; SimType.Int; SimType.Unit]];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  prog

let test_apply () =
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = Term.ttrue; Fdef.body = Term.apply (Term.make_var "apply") [Term.apply (Term.make_var "check") [Term.make_var "n"]; Term.make_var "n"] } in
  let apply = { Fdef.attr = []; Fdef.name = Idnt.make "apply"; Fdef.args = [Idnt.make "f"; Idnt.make "x"]; Fdef.guard = Term.ttrue; Fdef.body = Term.apply (Term.make_var "f") [Term.make_var "x"] } in
  let check1 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.eqInt (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.tunit} in
  let check2 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.neqInt (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.tfail} in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; apply; check1; check2];
               Prog.types = [Idnt.make "main", SimType.tfun [SimType.Int; SimType.Unit];
                             Idnt.make "apply", SimType.tfun [SimType.tfun [SimType.Int; SimType.Unit]; SimType.Int; SimType.Unit];
                             Idnt.make "check", SimType.tfun [SimType.Int; SimType.Int; SimType.Unit]];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  prog

let test_bar_hoge () =
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = Term.ttrue; Fdef.body = Term.apply (Term.make_var "bar") [Term.apply (Term.make_var "hoge") [Term.make_var "n"]; Term.make_var "n"] } in
  let bar = { Fdef.attr = []; Fdef.name = Idnt.make "bar"; Fdef.args = [Idnt.make "f"; Idnt.make "x"]; Fdef.guard = Term.ttrue; Fdef.body = Term.apply (Term.make_var "f") [Term.make_var "x"; Term.make_var "check"] } in
  let hoge = { Fdef.attr = []; Fdef.name = Idnt.make "hoge"; Fdef.args = [Idnt.make "x"; Idnt.make "y"; Idnt.make "f"]; Fdef.guard = Term.ttrue; Fdef.body = Term.apply (Term.make_var "f") [Term.make_var "x"; Term.make_var "y"] } in
  let check1 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.eqInt (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.tunit} in
  let check2 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.neqInt (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.tfail} in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; bar; hoge; check1; check2];
               Prog.types = [Idnt.make "main", SimType.tfun [SimType.Int; SimType.Unit];
                             Idnt.make "bar", SimType.tfun [SimType.tfun [SimType.Int; SimType.tfun [SimType.Int; SimType.Int; SimType.Unit]; SimType.Unit]; SimType.Int; SimType.Unit];
                             Idnt.make "hoge", SimType.tfun [SimType.Int; SimType.Int; SimType.tfun [SimType.Int; SimType.Int; SimType.Unit]; SimType.Unit];
                             Idnt.make "check", SimType.tfun [SimType.Int; SimType.Int; SimType.Unit]];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  prog

let test_checkh () =
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = Term.ttrue; Fdef.body = Term.apply (Term.make_var "checkh") [Term.apply (Term.make_var "h") [Term.make_var "n"]; Term.apply (Term.make_var "h") [Term.make_var "n"]] } in
  let checkh = { Fdef.attr = []; Fdef.name = Idnt.make "checkh"; Fdef.args = [Idnt.make "f"; Idnt.make "g"]; Fdef.guard = Term.ttrue; Fdef.body = Term.apply (Term.make_var "check") [Term.apply (Term.make_var "f") [Term.tunit]; Term.apply (Term.make_var "g") [Term.tunit]] } in
  let check1 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.eqInt (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.tunit} in
  let check2 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.neqInt (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.tfail} in
  let h = { Fdef.attr = []; Fdef.name = Idnt.make "h"; Fdef.args = [Idnt.make "x"; Idnt.make "un"]; guard = Term.ttrue; Fdef.body = Term.make_var "x"} in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; checkh; check1; check2; h];
               Prog.types = [Idnt.make "main", SimType.tfun [SimType.Int; SimType.Unit];
                             Idnt.make "checkh", SimType.tfun [SimType.tfun [SimType.Unit; SimType.Int]; SimType.tfun [SimType.Unit; SimType.Int]; SimType.Unit];
                             Idnt.make "check", SimType.tfun [SimType.Int; SimType.Int; SimType.Unit];
                             Idnt.make "h", SimType.tfun [SimType.Int; SimType.Unit; SimType.Int]];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  prog

let test_applyh () =
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = Term.ttrue; Fdef.body = Term.apply (Term.make_var "applyh") [Term.make_var "apply"; Term.make_var "check"; Term.make_var "n"] } in
  let applyh = { Fdef.attr = []; Fdef.name = Idnt.make "applyh"; Fdef.args = [Idnt.make "f"; Idnt.make "g"; Idnt.make "x"]; Fdef.guard = Term.ttrue; Fdef.body = Term.apply (Term.make_var "f") [Term.apply (Term.make_var "g") [Term.make_var "x"]; Term.make_var "x"] } in
  let apply = { Fdef.attr = []; Fdef.name = Idnt.make "apply"; Fdef.args = [Idnt.make "f"; Idnt.make "x"]; Fdef.guard = Term.ttrue; Fdef.body = Term.apply (Term.make_var "f") [Term.make_var "x"] } in
  let check1 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.eqInt (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.tunit} in
  let check2 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.neqInt (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.tfail} in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; applyh; apply; check1; check2];
               Prog.types = [Idnt.make "main", SimType.tfun [SimType.Int; SimType.Unit];
                             Idnt.make "applyh", SimType.tfun [SimType.tfun [SimType.tfun [SimType.Int; SimType.Unit]; SimType.Int; SimType.Unit]; SimType.tfun [SimType.Int; SimType.Int; SimType.Unit]; SimType.Int; SimType.Unit];
                             Idnt.make "apply", SimType.tfun [SimType.tfun [SimType.Int; SimType.Unit]; SimType.Int; SimType.Unit];
                             Idnt.make "check", SimType.tfun [SimType.Int; SimType.Int; SimType.Unit]];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  prog

let test_applyh2 () =
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = Term.ttrue; Fdef.body = Term.apply (Term.make_var "applyh") [Term.make_var "apply"; Term.apply (Term.make_var "check") [Term.make_var "n"]; Term.make_var "n"] } in
  let applyh = { Fdef.attr = []; Fdef.name = Idnt.make "applyh"; Fdef.args = [Idnt.make "f"; Idnt.make "g"; Idnt.make "x"]; Fdef.guard = Term.ttrue; Fdef.body = Term.apply (Term.make_var "f") [Term.make_var "g"; Term.make_var "x"] } in
  let apply = { Fdef.attr = []; Fdef.name = Idnt.make "apply"; Fdef.args = [Idnt.make "f"; Idnt.make "x"]; Fdef.guard = Term.ttrue; Fdef.body = Term.apply (Term.make_var "f") [Term.make_var "x"] } in
  let check1 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.eqInt (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.tunit} in
  let check2 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.neqInt (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.tfail} in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; applyh; apply; check1; check2];
               Prog.types = [Idnt.make "main", SimType.tfun [SimType.Int; SimType.Unit];
                             Idnt.make "applyh", SimType.tfun [SimType.tfun [SimType.tfun [SimType.Int; SimType.Unit]; SimType.Int; SimType.Unit]; SimType.tfun [SimType.Int; SimType.Unit]; SimType.Int; SimType.Unit];
                             Idnt.make "apply", SimType.tfun [SimType.tfun [SimType.Int; SimType.Unit]; SimType.Int; SimType.Unit];
                             Idnt.make "check", SimType.tfun [SimType.Int; SimType.Int; SimType.Unit]];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  prog


let test_apply_apply () =
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = Term.ttrue; Fdef.body = Term.apply (Term.make_var "apply") [Term.apply (Term.make_var "apply") [Term.apply (Term.make_var "check") [Term.make_var "n"]]; Term.make_var "n"] } in
  let apply = { Fdef.attr = []; Fdef.name = Idnt.make "apply"; Fdef.args = [Idnt.make "f"; Idnt.make "x"]; Fdef.guard = Term.ttrue; Fdef.body = Term.apply (Term.make_var "f") [Term.make_var "x"] } in
  let check1 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.eqInt (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.tunit} in
  let check2 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.neqInt (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.tfail} in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; apply; check1; check2];
               Prog.types = [Idnt.make "main", SimType.tfun [SimType.Int; SimType.Unit];
                             Idnt.make "apply", SimType.tfun [SimType.tfun [SimType.Int; SimType.Unit]; SimType.Int; SimType.Unit];
                             Idnt.make "check", SimType.tfun [SimType.Int; SimType.Int; SimType.Unit]];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  prog

let test_apply_apply2 () =
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = Term.ttrue; Fdef.body = Term.apply (Term.make_var "apply") [Term.apply (Term.make_var "apply2") [Term.make_var "check"; Term.make_var "n"]; Term.make_var "n"] } in
  let apply = { Fdef.attr = []; Fdef.name = Idnt.make "apply"; Fdef.args = [Idnt.make "f"; Idnt.make "x"]; Fdef.guard = Term.ttrue; Fdef.body = Term.apply (Term.make_var "f") [Term.make_var "x"] } in
  let apply2 = { Fdef.attr = []; Fdef.name = Idnt.make "apply2"; Fdef.args = [Idnt.make "f"; Idnt.make "x"; Idnt.make "y"]; Fdef.guard = Term.ttrue; Fdef.body = Term.apply (Term.make_var "f") [Term.make_var "x"; Term.make_var "y"] } in
  let check1 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.eqInt (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.tunit} in
  let check2 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.neqInt (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.tfail} in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; apply; apply2; check1; check2];
               Prog.types = [Idnt.make "main", SimType.tfun [SimType.Int; SimType.Unit];
                             Idnt.make "apply", SimType.tfun [SimType.tfun [SimType.Int; SimType.Unit]; SimType.Int; SimType.Unit];
                             Idnt.make "apply2", SimType.tfun [SimType.tfun [SimType.Int; SimType.Int; SimType.Unit]; SimType.Int; SimType.Int; SimType.Unit];
                             Idnt.make "check", SimType.tfun [SimType.Int; SimType.Int; SimType.Unit]];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  prog

let test_foo () =
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = Term.ttrue; Fdef.body = Term.apply (Term.make_var "check") [Term.apply (Term.make_var "f") [Term.make_var "h"]; Term.tint 2] } in
  let f = { Fdef.attr = []; Fdef.name = Idnt.make "f"; Fdef.args = [Idnt.make "g"]; Fdef.guard = Term.ttrue; Fdef.body = Term.apply (Term.make_var "g") [Term.make_var "inc"] } in
  let h = { Fdef.attr = []; Fdef.name = Idnt.make "h"; Fdef.args = [Idnt.make "k"]; Fdef.guard = Term.ttrue; Fdef.body = Term.apply (Term.make_var "k") [Term.apply (Term.make_var "k") [Term.tint 0]] } in
  let inc = { Fdef.attr = []; Fdef.name = Idnt.make "inc"; Fdef.args = [Idnt.make "x"]; Fdef.guard = Term.ttrue; Fdef.body = Term.add (Term.make_var "x") (Term.tint 1) } in
  let check1 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.eqInt (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.tunit} in
  let check2 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = Term.neqInt (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.tfail} in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; f; h; inc; check1; check2];
               Prog.types = [Idnt.make "main", SimType.tfun [SimType.Int; SimType.Unit];
                             Idnt.make "f", SimType.tfun [SimType.tfun [SimType.tfun [SimType.Int; SimType.Int]; SimType.Int]; SimType.Int];
                             Idnt.make "h", SimType.tfun [SimType.tfun [SimType.Int; SimType.Int]; SimType.Int];
                             Idnt.make "inc", SimType.tfun [SimType.Int; SimType.Int];
                             Idnt.make "check", SimType.tfun [SimType.Int; SimType.Int; SimType.Unit]];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  prog

let _ =
  let _ = Cvc3Interface.open_cvc3 () in
  let _ =
		  match 0 with
		    0 -> Verifier.verify (test_sum ())
		  | 1 -> Verifier.verify (test_sum_assert ())
		  | 2 -> let _ = Verifier.infer_abst_type [0; 1; 0; 1; 0; 1] (test_copy_copy ()) in ()
		  | 3 -> Verifier.verify (test_apply ())
		  | 4 -> Verifier.verify (test_bar_hoge ())
		  | 5 -> Verifier.verify (test_checkh ())
		  | 6 -> Verifier.verify (test_applyh ())
		  | 7 -> Verifier.verify (test_applyh2 ())
		  | 8 -> Verifier.verify (test_apply_apply ())
		  | 9 -> let _ = Verifier.infer_abst_type [0; 0; 0; 1] (test_apply_apply2 ()) in ()
		  | 10 -> Verifier.verify (test_foo ())
  in
  Cvc3Interface.close_cvc3 ()
