open ExtList

open CEGAR_syntax
open CEGAR_type
open CEGAR_print
open CEGAR_util

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
  | TBase(TEvent, _) -> SimType.Fun(SimType.Unit, SimType.Unit)
  | TFun(tmp) ->
      let ty1, ty2 = tmp (Const True) in
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
  let _ = Verifier.verify [] prog in
  Format.printf "END verification@,@]"

let conv_siz_type sty =
(**)
  let _ = Format.printf "%a@." SizType.pr sty in
  TBase(TUnit, fun t -> [t])
(**)
(*
  let rec aux ty post sub =
		  match ty with
		    Unit(x) ->
        TBase(TUnit, fun t -> [subst_map ((x, t)::sub) (conv_term post)]),
        sub
    | Bool(x) ->
        let sub = (x, t)::sub in
        TBase(TBool, fun t -> [subst_map sub (conv_term post)]),
        sub
    | Int(x) ->
        let sub = (x, t)::sub in
        TBase(TInt, fun t -> [subst_map sub (conv_term post)]),
        sub
		  | Fun(xs) ->
        List.map
          (fun (ty1, pre, ty2) ->
            let ty1', sub = aux ty1 pre sub in
            TFun(ty1', aux ty2 post sub))
          xs,
        sub
  in
  aux sty.ty sty.cond []
*)

let infer ces prog =
  let prog = conv_prog prog in
  let env = Verifier.infer ces prog in
  List.map
    (fun (f, _) ->
      try
        f, conv_siz_type (List.assoc (Var.make f) env)
      with Not_found ->
        assert false)
    prog.Prog.types
