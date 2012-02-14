open ExtList

open CEGAR_syntax
open CEGAR_type
open CEGAR_print
open CEGAR_util

let conv_const c =
  match c with
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
  | EqUnit -> Const.EqUnit
  | EqBool -> Const.EqBool
  | EqInt -> Const.EqInt
  | Int(n) -> Const.Int(n)
  | RandInt -> Const.RandInt
  | Add -> Const.Add
  | Sub -> Const.Sub
  | Mul -> Const.Mul
  | _ -> Format.printf "%a@." print_const c; assert false

let rec conv_term t =
  match t with
  | Const(Bottom) -> Term.make_var (Var.make (Idnt.make "bottom")) (***)
  | Const(c) -> Term.Const([], conv_const c)
  | Var(x) -> Term.make_var (Var.make (Idnt.make x))
  | App(t1, t2) -> Term.apply (conv_term t1) [conv_term t2]
  | Fun _ -> assert false
  | Let _ -> assert false

let inv_const c =
  match c with
  | Const.Unit -> Unit
  | Const.True -> True
  | Const.False -> False
  | Const.And -> And
  | Const.Or -> Or
  | Const.Not -> Not
  | Const.Lt -> Lt
  | Const.Gt -> Gt
  | Const.Leq -> Leq
  | Const.Geq -> Geq
  | Const.EqBool -> EqBool
  | Const.EqInt -> EqInt
  | Const.Int(n) -> Int(n)
  | Const.RandInt -> RandInt
  | Const.Add -> Add
  | Const.Sub -> Sub
  | Const.Mul -> Mul
  | _ -> Format.printf "%a@." Const.pr c; assert false

let rec inv_term t =
  match t with
    Term.Const(_, c) -> Const(inv_const c)
  | Term.Var(_, x) -> Var(Var.string_of2 x)
  | Term.App(_, Term.App(_, t1, t2), t3) ->
      (match t1 with
        Term.Const(_, Const.NeqUnit) -> App(Const(Not), App(App(Const(EqUnit), inv_term t2), inv_term t3))
      | Term.Const(_, Const.NeqBool) -> App(Const(Not), App(App(Const(EqBool), inv_term t2), inv_term t3))
      | Term.Const(_, Const.NeqInt) -> App(Const(Not), App(App(Const(EqInt), inv_term t2), inv_term t3))
      | _ -> App(App(inv_term t1, inv_term t2), inv_term t3))
  | Term.App(_, t1, t2) -> App(inv_term t1, inv_term t2)
  | Term.Forall (_, _, _) -> assert false
  | Term.Error _ -> assert false
  | Term.Ret (_, _, _, _) -> assert false
  | Term.Call (_, _, _) -> assert false


let conv_event e = (***)
  match e with
      Event(x) ->
        assert (x = "fail");
        Term.Const([], Const.Event(Idnt.make x))
    | Branch(_) -> assert false

let conv_fdef (f, args, guard, events, body) =
  { Fdef.attr = [];
    Fdef.name = Idnt.make f;
    Fdef.args = List.map Idnt.make args;
    Fdef.guard = conv_term guard;
    Fdef.body = List.fold_right (fun e t -> Term.apply (conv_event e) [Term.Const([],Const.Unit)]) events (conv_term body) } (***)

let inv_fdef fdef =
  Idnt.string_of fdef.Fdef.name,
  List.map Idnt.string_of fdef.Fdef.args,
  inv_term fdef.Fdef.guard,
  [],
  inv_term fdef.Fdef.body

let rec conv_typ ty =
  match ty with
    TBase(TUnit, _) -> SimType.Unit
  | TBase(TInt, _) -> SimType.Int
  | TBase(TBool, _) -> SimType.Bool
  | TFun(ty1,tmp) ->
      let ty2 = tmp (Const True) in
      SimType.Fun(conv_typ ty1, conv_typ ty2)
  | _ ->
      let _ = Format.printf "%a@." print_typ ty in
      assert false

let conv_prog (typs, fdefs, main) =
  { Prog.attr = [];
    Prog.fdefs = List.map conv_fdef fdefs;
    Prog.types = List.map (fun (x, ty) -> Idnt.make x, conv_typ ty) typs;
    Prog.main = Idnt.make main }

let verify (*cexs*) prog =
  let prog = conv_prog prog in
  Format.printf "@[<v>BEGIN verification:@,  @[%a@]@," Prog.pr prog;
  let _ = Verifier.verify prog in
  Format.printf "END verification@,@]"

let rec inv_abst_type aty =
		match aty with
    AbsType.Base(AbsType.Unit, x, ts) ->
      let x = Var.string_of x in
      TBase(TUnit, fun s -> List.map (fun t -> subst x s (inv_term t)) ts)
  | AbsType.Base(AbsType.Bool, x, ts) ->
      let x = Var.string_of x in
      TBase(TBool, fun s -> List.map (fun t -> subst x s (inv_term t)) ts)
  | AbsType.Base(AbsType.Int, x, ts) ->
      let x = Var.string_of x in
      TBase(TInt, fun s -> List.map (fun t -> subst x s (inv_term t)) ts)
  | AbsType.Fun(aty1, aty2) ->
      let x = if AbsType.is_base aty1 then Var.string_of (AbsType.bv_of aty1) else "_dummy" in
      TFun(inv_abst_type aty1, fun t -> subst_typ x t (inv_abst_type aty2))


let infer cexs prog =
  let prog = conv_prog prog in
  let env, ext_fdefs = Verifier.infer_abst_type cexs prog in
  List.map
   (fun (f, rty) ->
     match f with Var.V(id) -> Idnt.string_of id, inv_abst_type rty)
   env,
  List.map inv_fdef ext_fdefs
(*
  List.map
    (fun (f, _) ->
      try
        f, conv_siz_type (List.assoc (Var.make f) env)
      with Not_found ->
        assert false)
    prog.Prog.types
*)
