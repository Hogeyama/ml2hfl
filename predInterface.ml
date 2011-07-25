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
  let eps = Ctree.auto prog rt strategy in
		let eptrs = List.map Trace.of_error_path eps in
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
    sumss;
  Format.printf "END verification@,@]"

