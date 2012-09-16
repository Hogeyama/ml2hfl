open ExtList
open Term
open Formula

(** Main *)

let make_var s = make_var (Var.make (Idnt.make s))
let sum1 = { Fdef.attr = []; Fdef.name = Idnt.make "sum"; Fdef.args = [Idnt.make "x"]; Fdef.guard = leq (make_var "x") (tint 0); Fdef.body = tint 0 }
let sum2 = { Fdef.attr = []; Fdef.name = Idnt.make "sum"; Fdef.args = [Idnt.make "x"]; Fdef.guard = gt (make_var "x") (tint 0); Fdef.body = add (make_var "x") (apply (make_var "sum") [sub (make_var "x") (tint 1)]) }
let check_geq1 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = geq (make_var "x1") (make_var "x2"); Fdef.body = tunit }
let check_geq2 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = lt (make_var "x1") (make_var "x2"); Fdef.body = tfail }
let check1 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = eqInt (make_var "x1") (make_var "x2"); Fdef.body = tunit }
let check2 = { Fdef.attr = []; Fdef.name = Idnt.make "check"; Fdef.args = [Idnt.make "x1"; Idnt.make "x2"]; guard = neqInt (make_var "x1") (make_var "x2"); Fdef.body = tfail }
let assert1 = { Fdef.attr = []; Fdef.name = Idnt.make "assert"; Fdef.args = [Idnt.make "b"]; guard = eqBool (make_var "b") (ttrue); Fdef.body = tunit }
let assert2 = { Fdef.attr = []; Fdef.name = Idnt.make "assert"; Fdef.args = [Idnt.make "b"]; guard = eqBool (make_var "b") (tfalse); Fdef.body = tfail }
let apply1 = { Fdef.attr = []; Fdef.name = Idnt.make "apply"; Fdef.args = [Idnt.make "f"; Idnt.make "x"]; Fdef.guard = ttrue; Fdef.body = apply (make_var "f") [make_var "x"] }
let copy1 = { Fdef.attr = []; Fdef.name = Idnt.make "copy"; Fdef.args = [Idnt.make "x"]; Fdef.guard = eqInt (make_var "x") (tint 0); Fdef.body = tint 0 }
let copy2 = { Fdef.attr = []; Fdef.name = Idnt.make "copy"; Fdef.args = [Idnt.make "x"]; Fdef.guard = neqInt (make_var "x") (tint 0); Fdef.body = add (tint 1) (apply (make_var "copy") [sub (make_var "x") (tint 1)]) }
let inc = { Fdef.attr = []; Fdef.name = Idnt.make "inc"; Fdef.args = [Idnt.make "x"]; Fdef.guard = ttrue; Fdef.body = add (make_var "x") (tint 1) }

let test_sum () =
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = ttrue; Fdef.body = apply (make_var "check") [apply (make_var "sum") [make_var "n"]; make_var "n"] } in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; sum1; sum2; check_geq1; check_geq2];
               Prog.types = [Idnt.make "main", SimType.tfun [SimType.Int; SimType.Unit];
                             Idnt.make "sum", SimType.tfun [SimType.Int; SimType.Int];
                             Idnt.make "check", SimType.tfun [SimType.Int; SimType.tfun [SimType.Int; SimType.Unit]]];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  prog

let test_sum_assert () =
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = ttrue; Fdef.body = apply (make_var "assert") [geq (apply (make_var "sum") [make_var "n"]) (make_var "n")] } in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; sum1; sum2; assert1; assert2];
               Prog.types = [Idnt.make "main", SimType.tfun [SimType.Int; SimType.Unit];
                             Idnt.make "sum", SimType.tfun [SimType.Int; SimType.Int];
                             Idnt.make "assert", SimType.tfun [SimType.Bool; SimType.Unit]];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  prog

let test_copy_copy () =
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = ttrue; Fdef.body = apply (make_var "check") [apply (make_var "copy") [apply (make_var "copy") [make_var "n"]]; make_var "n"] } in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; copy1; copy2; check1; check2];
               Prog.types = [Idnt.make "main", SimType.tfun [SimType.Int; SimType.Unit];
                             Idnt.make "copy", SimType.tfun [SimType.Int; SimType.Int];
                             Idnt.make "check", SimType.tfun [SimType.Int; SimType.Int; SimType.Unit]];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  prog

let test_apply () =
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = ttrue; Fdef.body = apply (make_var "apply") [apply (make_var "check") [make_var "n"]; make_var "n"] } in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; apply1; check1; check2];
               Prog.types = [Idnt.make "main", SimType.tfun [SimType.Int; SimType.Unit];
                             Idnt.make "apply", SimType.tfun [SimType.tfun [SimType.Int; SimType.Unit]; SimType.Int; SimType.Unit];
                             Idnt.make "check", SimType.tfun [SimType.Int; SimType.Int; SimType.Unit]];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  prog

let test_bar_hoge () =
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = ttrue; Fdef.body = apply (make_var "bar") [apply (make_var "hoge") [make_var "n"]; make_var "n"] } in
  let bar = { Fdef.attr = []; Fdef.name = Idnt.make "bar"; Fdef.args = [Idnt.make "f"; Idnt.make "x"]; Fdef.guard = ttrue; Fdef.body = apply (make_var "f") [make_var "x"; make_var "check"] } in
  let hoge = { Fdef.attr = []; Fdef.name = Idnt.make "hoge"; Fdef.args = [Idnt.make "x"; Idnt.make "y"; Idnt.make "f"]; Fdef.guard = ttrue; Fdef.body = apply (make_var "f") [make_var "x"; make_var "y"] } in
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
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = ttrue; Fdef.body = apply (make_var "checkh") [apply (make_var "h") [make_var "n"]; apply (make_var "h") [make_var "n"]] } in
  let checkh = { Fdef.attr = []; Fdef.name = Idnt.make "checkh"; Fdef.args = [Idnt.make "f"; Idnt.make "g"]; Fdef.guard = ttrue; Fdef.body = apply (make_var "check") [apply (make_var "f") [tunit]; apply (make_var "g") [tunit]] } in
  let h = { Fdef.attr = []; Fdef.name = Idnt.make "h"; Fdef.args = [Idnt.make "x"; Idnt.make "un"]; guard = ttrue; Fdef.body = make_var "x"} in
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
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = ttrue; Fdef.body = apply (make_var "applyh") [make_var "apply"; make_var "check"; make_var "n"] } in
  let applyh = { Fdef.attr = []; Fdef.name = Idnt.make "applyh"; Fdef.args = [Idnt.make "f"; Idnt.make "g"; Idnt.make "x"]; Fdef.guard = ttrue; Fdef.body = apply (make_var "f") [apply (make_var "g") [make_var "x"]; make_var "x"] } in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; applyh; apply1; check1; check2];
               Prog.types = [Idnt.make "main", SimType.tfun [SimType.Int; SimType.Unit];
                             Idnt.make "applyh", SimType.tfun [SimType.tfun [SimType.tfun [SimType.Int; SimType.Unit]; SimType.Int; SimType.Unit]; SimType.tfun [SimType.Int; SimType.Int; SimType.Unit]; SimType.Int; SimType.Unit];
                             Idnt.make "apply", SimType.tfun [SimType.tfun [SimType.Int; SimType.Unit]; SimType.Int; SimType.Unit];
                             Idnt.make "check", SimType.tfun [SimType.Int; SimType.Int; SimType.Unit]];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  prog

let test_applyh2 () =
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = ttrue; Fdef.body = apply (make_var "applyh") [make_var "apply"; apply (make_var "check") [make_var "n"]; make_var "n"] } in
  let applyh = { Fdef.attr = []; Fdef.name = Idnt.make "applyh"; Fdef.args = [Idnt.make "f"; Idnt.make "g"; Idnt.make "x"]; Fdef.guard = ttrue; Fdef.body = apply (make_var "f") [make_var "g"; make_var "x"] } in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; applyh; apply1; check1; check2];
               Prog.types = [Idnt.make "main", SimType.tfun [SimType.Int; SimType.Unit];
                             Idnt.make "applyh", SimType.tfun [SimType.tfun [SimType.tfun [SimType.Int; SimType.Unit]; SimType.Int; SimType.Unit]; SimType.tfun [SimType.Int; SimType.Unit]; SimType.Int; SimType.Unit];
                             Idnt.make "apply", SimType.tfun [SimType.tfun [SimType.Int; SimType.Unit]; SimType.Int; SimType.Unit];
                             Idnt.make "check", SimType.tfun [SimType.Int; SimType.Int; SimType.Unit]];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  prog


let test_apply_apply () =
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = ttrue; Fdef.body = apply (make_var "apply") [apply (make_var "apply") [apply (make_var "check") [make_var "n"]]; make_var "n"] } in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; apply1; check1; check2];
               Prog.types = [Idnt.make "main", SimType.tfun [SimType.Int; SimType.Unit];
                             Idnt.make "apply", SimType.tfun [SimType.tfun [SimType.Int; SimType.Unit]; SimType.Int; SimType.Unit];
                             Idnt.make "check", SimType.tfun [SimType.Int; SimType.Int; SimType.Unit]];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  prog

let test_apply_apply2 () =
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = ttrue; Fdef.body = apply (make_var "apply") [apply (make_var "apply2") [make_var "check"; make_var "n"]; make_var "n"] } in
  let apply2 = { Fdef.attr = []; Fdef.name = Idnt.make "apply2"; Fdef.args = [Idnt.make "f"; Idnt.make "x"; Idnt.make "y"]; Fdef.guard = ttrue; Fdef.body = apply (make_var "f") [make_var "x"; make_var "y"] } in
  let prog = { Prog.attr = [];
               Prog.fdefs = [main; apply1; apply2; check1; check2];
               Prog.types = [Idnt.make "main", SimType.tfun [SimType.Int; SimType.Unit];
                             Idnt.make "apply", SimType.tfun [SimType.tfun [SimType.Int; SimType.Unit]; SimType.Int; SimType.Unit];
                             Idnt.make "apply2", SimType.tfun [SimType.tfun [SimType.Int; SimType.Int; SimType.Unit]; SimType.Int; SimType.Int; SimType.Unit];
                             Idnt.make "check", SimType.tfun [SimType.Int; SimType.Int; SimType.Unit]];
               Prog.main = main.Fdef.name } in
  Format.printf "%a" Prog.pr prog;
  prog

let test_foo () =
  let main = { Fdef.attr = []; Fdef.name = Idnt.make "main"; Fdef.args = [Idnt.make "n"]; Fdef.guard = ttrue; Fdef.body = apply (make_var "check") [apply (make_var "f") [make_var "h"]; tint 2] } in
  let f = { Fdef.attr = []; Fdef.name = Idnt.make "f"; Fdef.args = [Idnt.make "g"]; Fdef.guard = ttrue; Fdef.body = apply (make_var "g") [make_var "inc"] } in
  let h = { Fdef.attr = []; Fdef.name = Idnt.make "h"; Fdef.args = [Idnt.make "k"]; Fdef.guard = ttrue; Fdef.body = apply (make_var "k") [apply (make_var "k") [tint 0]] } in
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
      0 -> Verifier.verify [] (test_sum ())
    | 1 -> Verifier.verify [] (test_sum_assert ())
    | 2 -> let _ = Verifier.verify [] (*[[0; 1; 0; 1; 0; 1]]*) (test_copy_copy ()) in ()
    | 3 -> Verifier.verify [] (test_apply ())
    | 4 -> Verifier.verify [] (test_bar_hoge ())
    | 5 -> Verifier.verify [] (test_checkh ())
    | 6 -> Verifier.verify [] (test_applyh ())
    | 7 -> Verifier.verify [] (test_applyh2 ())
    | 8 -> Verifier.verify [] (test_apply_apply ())
    | 9 -> let _ = Verifier.verify [] (*[[0; 0; 0; 1]]*) (test_apply_apply2 ()) in ()
    | 10 -> Verifier.verify [] (test_foo ())
    | _ -> assert false
  in
  Cvc3Interface.close_cvc3 ()
