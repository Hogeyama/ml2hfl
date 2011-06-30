open ExtList

let test1 () =
  let arg1 = Term.apply (Term.make_var "sum") [Term.make_var "n"] in
  let arg2 = Term.make_var "n" in
  let main = { Fdef.attr = []; Fdef.name = Id.make "main"; Fdef.args = [Id.make "n"]; Fdef.guard = Term.make_true; Fdef.body = Term.apply (Term.make_var "check") [arg1; arg2] } in
  let sum1 = { Fdef.attr = []; Fdef.name = Id.make "sum"; Fdef.args = [Id.make "x"]; Fdef.guard = Term.leq (Term.make_var "x") (Term.make_int 0); Fdef.body = Term.make_int 0 } in
  let sum2 = { Fdef.attr = []; Fdef.name = Id.make "sum"; Fdef.args = [Id.make "x"]; Fdef.guard = Term.gt (Term.make_var "x") (Term.make_int 0); Fdef.body = Term.add (Term.make_var "x") (Term.apply (Term.make_var "sum") [Term.sub (Term.make_var "x") (Term.make_int 1)])} in
  let check1 = { Fdef.attr = []; Fdef.name = Id.make "check"; Fdef.args = [Id.make "x1"; Id.make "x2"]; guard = Term.geq (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_unit} in
  let check2 = { Fdef.attr = []; Fdef.name = Id.make "check"; Fdef.args = [Id.make "x1"; Id.make "x2"]; guard = Term.lt (Term.make_var "x1") (Term.make_var "x2"); Fdef.body = Term.make_event "fail"} in
  let prog = { Prog.attr = []; Prog.fdefs = [main; sum1; sum2; check1; check2]; Prog.main = main.Fdef.name } in
  let prog = Prog.set_arity (Prog.arities prog) prog in
  Format.printf "%a" Prog.pr prog;
  let arity = Prog.arities prog prog.Prog.main in
  let p = [] in
  let ret, args = Ctree.ret_args prog.Prog.main p arity in
  let init = Term.Ret([], ret, Term.Call([], Term.make_var prog.Prog.main, args)) in
  let init = Term.set_arity (Prog.arities prog) init in
  let rt = Ctree.Node(p, init, ref []) in
  let rec loop wl =
    let _ = Ctree.save_as_dot "ctree.dot" rt wl in
    if wl = [] then
      ()
    else
		    let _ = Format.printf "expand the computation tree ? (y/n): %!" in
		    let inp = read_line () in
		    if inp = "y" then
		      let wl = Ctree.expand_tree prog wl in
		      loop wl
		    else if inp = "n" then
		      ()
		    else
		      loop wl
  in
  loop [rt]

let _ = test1 ()
