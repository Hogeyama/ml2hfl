
open Syntax
open Term_util
open Type
open Type_decl

let check_var x typ =
  if Type.can_unify (Id.typ x) typ
  then ()
  else (Format.printf "check_var: (%a:%a), %a@." Id.print x Print.typ (Id.typ x) Print.typ typ; assert false)

let rec check t typ =
  if false then Format.printf "CHECK: %a, %a@." Print.term t Print.typ typ;
  if not (Type.can_unify t.typ typ)
  then (Format.printf "check: %a, %a@."
                      (Color.red Print.term') t
                      (Color.yellow Print.typ) typ;
        assert false);
  match t.desc, elim_tpred t.typ with
  | _, TFuns _ -> ()
  | Label(_, t), _ -> check t typ
  | Const Unit, TUnit -> ()
  | Const CPS_result, typ when typ = typ_result -> ()
  | Const(True|False), TBool -> ()
  | Const(Int _), (TInt | TRInt _) -> ()
  | Const _, TData _ -> ()
  | Const(RandInt false), TFun(x,TInt)->
      check_var x TUnit
  | Const(RandInt true), TFun(x,TFun(k,rtyp)) ->
      assert (rtyp = typ_result);
      check_var x TUnit;
      check_var k (TFun(Id.new_var TInt, typ_result))
  | Const(RandValue(typ1,false)), TFun({Id.typ=TUnit},typ2) ->
      assert (Type.can_unify typ1 typ2)
  | Const(RandValue(typ1,true)), TFun({Id.typ=TUnit}, TFun({Id.typ=TFun(x,rtyp1)},rtyp2)) -> ()
  (*
      assert (rtyp1 = typ_result);
      assert (rtyp2 = typ_result);
      assert (typ1 = Id.typ x)*)
  | Var x, typ' ->
      check_var x typ'
  | Fun(x,t), TFun(y,typ') ->
      check_var x (Id.typ y);
      check t typ'
  | App(t,ts), typ' ->
      let rec aux = function
        | [], _ -> ()
        | t::ts, TFun(x,typ) ->
            check t (Id.typ x);
            aux (ts,typ)
        | [_], typ when typ = typ_event -> ()
        | _ -> assert false
      in
      let typ'' = List.fold_right (fun t typ -> TFun(Id.new_var t.typ, typ)) ts typ' in
      aux (ts, t.typ);
      check t typ''
  | If(t1,t2,t3), typ' ->
      check t1 TBool;
      check t2 typ';
      check t3 typ'
  | Let(flag, bindings, t2), typ' ->
      let rec aux t xs typ =
        match xs, typ with
        | x::xs,TFun(y,typ) ->
            check_var x @@ Id.typ y;
            aux t xs typ
        | [],typ -> check t typ
        | [x],typ ->
            let f,_,_ = List.hd bindings in
            Format.printf "%a@." Print.id_typ f;
            Format.printf "[%a]@." Print.id_typ x;
            assert false
        | x::xs,typ ->
            Format.printf "%a %a@." Id.print x Print.typ typ;
            assert false
      in
      List.iter (fun (f,xs,t) -> aux t xs @@ Id.typ f) bindings;
      check t2 typ'
  | BinOp(Eq,t1,t2), TBool ->
      assert (Type.can_unify t1.typ t2.typ);
      check t1 t1.typ;
      check t2 t2.typ;
  | BinOp((Lt|Gt|Leq|Geq),t1,t2), TBool ->
      assert (Type.can_unify t1.typ t2.typ);
      check t1 t1.typ;
      check t2 t2.typ;
  (*
        check t1 TInt;
        check t2 TInt
   *)
  | BinOp((And|Or),t1,t2), TBool ->
      check t1 TBool;
      check t2 TBool
  | BinOp((Add|Sub|Mult),t1,t2), TInt ->
      check t1 TInt;
      check t2 TInt
  | Not t, TBool ->
      check t TBool
  | Event(_,false), typ' ->
      assert (Type.can_unify typ' typ_event || Type.can_unify typ' typ_event')
  | Event(s,true), typ' ->
      assert (Type.can_unify typ' typ_event_cps)
  | Tuple ts, TTuple xs ->
      List.iter2 check ts @@ List.map Id.typ xs;
  | Proj(i,t), typ ->
      assert (Type.can_unify typ @@ proj_typ i t.typ);
      check t t.typ
  | Record [], typ -> assert false
  | Record fields, typ ->
      let c,kind = kind_of_field @@ fst @@ List.hd fields in
      assert (Type.can_unify typ @@ TData(c,true));
      List.iter (fun (s,t) -> check t @@ field_arg_typ s) fields
  | Field(s,t), typ ->
      assert (Type.can_unify typ @@ field_arg_typ s);
      check t @@ field_typ s
  | SetField(s,t1,t2), typ ->
      assert (Type.can_unify typ TUnit);
      check t1 @@ field_typ s;
      check t2 @@ field_arg_typ s
  | Nil, TList _ -> ()
  | Cons(t1,t2), TList typ' ->
      check t1 typ';
      check t2 typ
  | Constr(s,ts), typ ->
      let typs = Type_decl.constr_arg_typs s in
      assert (List.length typs = List.length ts);
      List.iter2 check ts typs
  | Match(t,pats), typ' ->
      let aux (p,cond,t) =
        check cond TBool;
        check t typ'
      in
      check t t.typ;
      List.iter aux pats
  | Raise t, _ ->
      check t !typ_excep
  | TryWith(t1,t2), typ ->
      let e = Id.new_var ~name:"e" !typ_excep in
      check t1 typ;
      check t2 (TFun(e,typ))
  | Bottom, typ -> ()
  | Ref t, TRef typ ->
      check t typ
  | Deref t, typ ->
      check t (TRef typ)
  | SetRef(t1,t2), TUnit ->
      check t1 (TRef t2.typ);
      check t2 t2.typ
  | TNone, TOption typ -> ()
  | TSome t, TOption typ ->
      check t typ
  | _, TData _ -> assert false
  | _ ->
      Format.printf "check': %a, %a@." Print.term' t (Color.yellow Print.typ) t.typ;
      assert false

let check t typ = if Flag.check_typ then check t typ
