
open Syntax
open Type
open Type_decl

let var = Id.make (-1) "" typ_unknown

let check_var x typ =
  if Type.can_unify (Id.typ x) typ
  then ()
  else (Format.printf "check_var: (%a:%a), %a@." Id.print x Syntax.print_typ (Id.typ x) Syntax.print_typ typ; assert false)

let rec check t typ =
  if false then Format.printf "CHECK: %a, %a@." pp_print_term t Syntax.print_typ typ;
  if not (Type.can_unify t.typ typ)
  then (Format.printf "check: %a, %a@." print_term' t Syntax.print_typ typ; assert false);
  match t with
      {desc=Unit; typ=TUnit} -> ()
    | {desc=True|False|Unknown; typ=TBool} -> ()
    | {desc=(Int _ | NInt _); typ=(TInt _ | TRInt _)} -> ()
    | {desc=RandInt false; typ=TFun(x,TInt)} ->
        check_var x TUnit
    | {desc=RandInt true; typ=TFun(x,TFun(k,TUnit))} ->
        check_var x TUnit;
        check_var k (TFun(Id.new_var "" TInt, TUnit))
    | {desc=Var x; typ=typ'} ->
        check_var x typ'
    | {desc=Fun(x,t); typ=TFun(y,typ')} ->
        check_var x (Id.typ y);
        check t typ'
    | {desc=App(t,ts); typ=typ'} ->
        let rec aux = function
            [], _ -> ()
          | t::ts, TFun(x,typ) ->
              check t (Id.typ x);
              aux (ts,typ)
          | [_], typ when typ = typ_event -> ()
          | _ -> assert false
        in
        let typ'' = List.fold_right (fun t typ -> TFun(Id.set_typ var t.typ, typ)) ts typ' in
          aux (ts, t.typ);
          check t typ''
    | {desc=If(t1,t2,t3); typ=typ'} ->
        check t1 TBool;
        check t2 typ';
        check t3 typ'
    | {desc=Branch(t1,t2); typ=typ'} ->
        check t1 typ';
        check t2 typ'
    | {desc=Let(flag, bindings, t2); typ=typ'} ->
        let rec aux t = function
            x::xs,TFun(y,typ) -> check_var x (Id.typ y); aux t (xs,typ)
          | [],typ -> check t typ
          | _ -> assert false
        in
          List.iter (fun (f,xs,t) -> aux t (xs, Id.typ f)) bindings;
          check t2 typ'
    | {desc=BinOp(Eq,t1,t2); typ=TBool} ->
        assert (Type.can_unify t1.typ t2.typ);
        check t1 t1.typ;
        check t2 t2.typ;
    | {desc=BinOp((Lt|Gt|Leq|Geq),t1,t2); typ=TBool} ->
        check t1 TInt;
        check t2 TInt
    | {desc=BinOp((And|Or),t1,t2); typ=TBool} ->
        check t1 TBool;
        check t2 TBool
    | {desc=BinOp((Add|Sub|Mult),t1,t2); typ=TInt _} ->
        check t1 TInt;
        check t2 TInt
    | {desc=Not t; typ=TBool} ->
        check t TBool
    | {desc=Event(_,false); typ=typ'} -> assert (typ' = typ_event)
    | {desc=Event(_,true); typ=typ'} -> assert (typ' = typ_event_cps)
    | {desc=Pair(t1,t2); typ=TPair(typ1,typ2)} ->
        check t1 typ1;
        check t2 typ2
    | {desc=Fst t; typ=typ} ->
        let typ1 = match t.typ with TPair(typ1,_) -> typ1 | _ -> assert false in
          assert (Type.can_unify typ typ1);
          check t t.typ
    | {desc=Snd t; typ=typ} ->
        let typ2 = match t.typ with TPair(_,typ2) -> typ2 | _ -> assert false in
          assert (Type.can_unify typ typ2);
          check t t.typ
    | {desc=Record _} -> assert false
    | {desc=Proj _} -> assert false
    | {desc=SetField _} -> assert false
    | {desc=Nil; typ=TList _} -> ()
    | {desc=Cons(t1,t2); typ=TList typ'} ->
        check t1 typ';
        check t2 typ
    | {desc=Constr(s,ts)} ->
        assert (ts = []);
        assert (Type.can_unify t.typ (get_constr_typ s))
    | {desc=Match(t,pats); typ=typ'} ->
        let aux (p,cond,t) =
          match cond with None -> () | Some cond -> check cond TBool;
            check t typ'
        in
          check t t.typ;
          List.iter aux pats
    | {desc=Raise t; typ=_} ->
        check t !typ_excep
    | {desc=TryWith(t1,t2); typ=typ} ->
        let e = Id.new_var "e" !typ_excep in
          check t1 typ;
          check t2 (TFun(e,typ))
    | {desc=Bottom} -> ()
    | _ ->(*
            match t.desc with
            Unit -> assert false
            | True -> assert false
            | False -> assert false
            | Unknown -> assert false
            | Int n -> assert false
            | NInt y -> assert false
            | RandInt None -> assert false
            | RandInt (Some t1) -> assert false
            | Var y -> assert false
            | Fun(y, t1) -> assert false
            | App(t1, ts) -> assert false
            | If(t1, t2, t3) -> assert false
            | Branch(t1, t2) -> assert false
            | Let(Flag.Nonrecursive, f, xs, t1, t2) -> assert false
            | Let(Flag.Recursive, f, xs, t1, t2) -> assert false
            | BinOp(op, t1, t2) -> assert false
            | Not t1 -> assert false
            | Fail -> assert false
            | Label(b, t1) -> assert false
            | LabelInt(n, t1) -> assert false
            | Event s -> assert false
            | Record(b,fields) -> assert false
            | Proj(n,i,s,f,t1) -> assert false
            | SetField(n,i,s,f,t1,t2) -> assert false
            | Nil -> assert false
            | Cons(t1,t2) -> assert false
            | Constr(s,ts) -> assert false
            | Match(t1,pats) -> assert false
            | TryWith(t1,pats) -> assert false
          *)          Format.printf "check: %a, %a@." print_term' t Syntax.print_typ t.typ; assert false

let check t typ = if Flag.check_typ then check t typ
