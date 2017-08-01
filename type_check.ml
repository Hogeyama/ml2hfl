open Util
open Syntax
open Term_util
open Type

module Debug = Debug.Make(struct let check = make_debug_check __MODULE__ end)

let check_var x typ =
  if Type.can_unify (Id.typ x) typ
  then ()
  else (Format.printf "check_var: (%a:%a), %a@." Id.print x Print.typ (Id.typ x) Print.typ typ; assert false)

let rec check t typ =
  Debug.printf "CHECK: %a, %a@." Print.term t Print.typ typ;
  if not (Type.can_unify t.typ typ) then
    (Format.printf "check: %a, %a@." (Color.red Print.term') t (Color.yellow Print.typ) typ;
     assert false);
  match t.desc, elim_tattr t.typ with
  | _, TFuns _ -> ()
  | Label(_, t), _ -> check t typ
  | Const Unit, TUnit -> ()
  | Const CPS_result, typ when typ = typ_result -> ()
  | Const(True|False), TBool -> ()
  | Const(Int _), TInt -> ()
  | Const _, TData _ -> ()
  | Const(RandValue(TInt,false)), TFun(x,TInt)->
      check_var x TUnit
  | Const(RandValue(TInt,true)), TFun(x,TFun(k,rtyp)) ->
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
      let rec aux (ts,typ) =
        match ts, elim_tattr typ with
        | [], _ -> ()
        | t::ts, TFun(x,typ) ->
            check t (Id.typ x);
            aux (ts,typ)
        | [_], typ when typ = typ_event -> ()
        | _ ->
            Format.printf "ts: %a@." (List.print Print.term) ts;
            Format.printf "typ: %a@." Print.typ typ;
            assert false
      in
      let typ'' = List.fold_right (fun t typ -> TFun(Id.new_var t.typ, typ)) ts typ' in
      aux (ts, t.typ);
      check t typ''
  | If(t1,t2,t3), typ' ->
      check t1 TBool;
      check t2 typ';
      check t3 typ'
  | Let(bindings, t2), typ' ->
      let rec aux t xs typ =
        match xs, elim_tattr typ with
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
      let aux' f =
        let fv = get_fv t2 in
        List.iter (fun f' -> assert (Id.same f f' => Type.can_unify (Id.typ f) (Id.typ f'))) fv
      in
      List.iter (aux' -| Triple.fst) bindings;
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
      List.iter2 check ts @@ List.map Id.typ xs
  | Proj(i,t), typ ->
      assert (Type.can_unify typ @@ proj_typ i t.typ);
      check t t.typ
  | Record fields, TRecord tfields ->
      List.iter (fun (s,t) -> check t @@ snd @@ List.assoc s tfields) fields
  | Record fields, Type(decls, name) ->
      let typ = List.assoc name decls in
      check {t with typ} typ
  | Field(t,s), typ ->
      begin
        match t.typ with
        | TRecord tfields -> assert (Type.can_unify typ @@ snd @@ List.assoc s tfields)
        | _ -> assert false
      end;
      check t t.typ
  | SetField(t1,s,t2), typ ->
      assert (Type.can_unify typ TUnit);
      begin
        match t1.typ with
        | TRecord tfields ->
            let f,typ' = List.assoc s tfields in
            assert (f = Mutable);
            check t2 typ'
        | _ -> assert false
      end;
      check t1 t1.typ
  | Nil, TApp(TList, _) -> ()
  | Cons(t1,t2), TApp(TList, [typ']) ->
      check t1 typ';
      check t2 typ
  | Constr(s,ts), TVariant labels ->
      List.iter2 check ts @@ List.assoc s labels
  | Constr _, Type(decls, name) ->
      let typ' = List.assoc name decls in
      check {t with typ=typ'} typ'
  | Match(t,pats), typ' ->
      let aux (p,cond,t) =
        check cond TBool;
        check t typ'
      in
      check t t.typ;
      List.iter aux pats
  | Raise t, _ ->
      check t t.typ
  | TryWith(t1,t2), typ ->
      check t1 typ;
      check t2 @@ make_tfun (TVar (ref None)) typ
  | Bottom, typ -> ()
  | Ref t, TApp(TRef, [typ]) ->
      check t typ
  | Deref t, typ ->
      check t (make_tref typ)
  | SetRef(t1,t2), TUnit ->
      check t1 (make_tref t2.typ);
      check t2 t2.typ
  | TNone, TApp(TOption, _) -> ()
  | TSome t, TApp(TOption, [typ]) ->
      check t typ
  | _, TData _ -> assert false
  | _ ->
      Format.printf "check': %a, %a@." Print.term' t (Color.yellow Print.typ) t.typ;
      assert false

let check t typ = if Flag.check_typ then check t typ
