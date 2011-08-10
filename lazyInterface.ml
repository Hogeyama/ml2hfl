open ExtList

open CEGAR_syntax
open CEGAR_type
open CEGAR_print

let conv_const c =
  match c with
    Event(x) -> Const.Event(Idnt.make x)
  | Unit -> Const.Unit
  | True -> Const.True
  | False -> Const.False
  | And -> Const.And
  | Or -> Const.Or
  | Not -> Const.Not
  | Lt -> Const.Lt
  | Gt -> Const.Gt
  | Leq -> Const.Leq
  | Geq -> Const.Geq
  | Eq -> Const.Eq
  | Int(n) -> Const.Int(n)
  | RandInt -> Const.RandInt
  | Add -> Const.Add
  | Sub -> Const.Sub
  | Mul -> Const.Mul
  | _ -> Format.printf "%a@." print_const c; assert false

let rec conv_term t =
  match t with
    Const(c) -> Term.Const([], conv_const c)
  | Var(x) -> Term.make_var x
  | App(t1, t2) -> Term.apply (conv_term t1) [conv_term t2]

let conv_fdef (f, args, guard, body) =
  { Fdef.attr = [];
    Fdef.name = Idnt.make f;
    Fdef.args = List.map Idnt.make args;
    Fdef.guard = conv_term guard;
    Fdef.body = conv_term body }

let rec conv_typ ty =
  match ty with
    TBase(TUnit, _) -> SimType.Unit
  | TBase(TInt, _) -> SimType.Int
  | TBase(TBool, _) -> SimType.Bool
  | TBase(TEvent, _) -> SimType.Unit
  | TFun(tmp) ->
      let ty1, ty2 = tmp (Const True) in
      SimType.Fun(conv_typ ty1, conv_typ ty2)
  | _ -> (*Format.printf "%a@." print_ty ty;*) assert false

let conv_prog (typs, fdefs, main) =
  { Prog.attr = [];
    Prog.fdefs = List.map conv_fdef fdefs;
    Prog.types = List.map (fun (x, ty) -> Idnt.make x, conv_typ ty) typs;
    Prog.main = Idnt.make main }

let verify(* ces*) prog =
  let prog = conv_prog prog in
    Format.printf "@[<v>BEGIN verification:@,  @[%a@]@," Prog.pr prog;
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
      match 0 with
          0 -> Ctree.bf_strategy rt
        | 1 -> Ctree.df_strategy rt
            (*
              | 2 -> Ctree.cex_strategy ces rt
            *)
    in
  (try
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
      if true then
        if SizType.check_prog env prog then
          Format.printf "@.The program is safe@."
        else
          loop (i + 1)
    in
    loop 1
  with Trace.FeasibleErrorTrace(eptr) ->
   Format.printf "@.The program is unsafe@.Error trace: %a@." Trace.pr eptr);
      Format.printf "END verification@,@]"

