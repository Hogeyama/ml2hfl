open Util
open Type
open Syntax
open Term_util


let normalize = make_trans ()

let normalize_term t =
  match t.desc with
  | App(t1,ts) ->
      let t1' = normalize.tr_term t1 in
      let ts' = List.map normalize.tr_term ts in
      let x = var_of_term t1 in
      let xs = List.map var_of_term ts' in
      let aux bindings x =
        let y,_,_ = last bindings in
        let t = make_app (make_var y) [make_var x] in
        let z = var_of_term t in
        bindings @ [z, [], t]
      in
      let bindings = List.tl @@ List.fold_left aux [x, [], make_var x] xs in
      let y,_,_ = last bindings in
      let t = make_lets bindings @@ make_var y in
      make_lets (List.rev @@ List.map2 (fun x t -> x, [], t) (x::xs) (t1'::ts')) t
  | If(t1,t2,t3) ->
      let x = var_of_term t1 in
      let t1' = normalize.tr_term t1 in
      let t2' = normalize.tr_term t2 in
      let t3' = normalize.tr_term t3 in
      make_let [x, [], t1'] @@ make_if (make_var x) t2' t3'
  | BinOp(op,t1,t2) ->
      let t1' = normalize.tr_term t1 in
      let t2' = normalize.tr_term t2 in
      let x1 = var_of_term t1 in
      let x2 = var_of_term t2 in
      make_lets [x1,[],t1'; x2,[],t2'] @@ {desc=BinOp(op,make_var x1,make_var x2); typ=t.typ}
  | Not t1 ->
      let t1' = normalize.tr_term t1 in
      let x = var_of_term t in
      make_let [x, [], t1'] @@ make_not (make_var x)
  | Pair(t1,t2) ->
      let t1' = normalize.tr_term t1 in
      let t2' = normalize.tr_term t2 in
      let x1 = var_of_term t1 in
      let x2 = var_of_term t2 in
      make_lets [x2,[],t2'; x1,[],t1'] @@ make_pair (make_var x1) (make_var x2)
  | Fst t1 ->
      let t1' = normalize.tr_term t1 in
      let x = var_of_term t1' in
      make_let [x, [], t1'] @@ make_fst (make_var x)
  | Snd t1 ->
      let t1' = normalize.tr_term t1 in
      let x = var_of_term t1' in
      make_let [x, [], t1'] @@ make_snd (make_var x)
(*
  | Let(flag,bindings,t1) ->
      let bindings' = List.map (fun (f,xs,t) -> f, [], List.fold_right make_fun xs t) bindings in
      let t1' = normalize.tr_term t1 in
      make_let_f flag bindings' t1'
*)
  | Nil
  | Cons _
  | Constr _
  | Match _
  | Raise _
  | TryWith _
  | Label _
  | Record _
  | Proj _
  | SetField _ -> assert false
  | _ -> normalize.tr_term_rec t

let () = normalize.tr_term <- normalize_term



let make_deep_pair = make_trans2 ()

let make_deep_pair_typ (rhs1,rhs2) typ =
  TPair(Id.new_var "x" typ, Id.typ rhs2)

let make_deep_pair_term (rhs1,rhs2) t =
  match t.desc with
  | If(t1,t2,t3) ->
      let t2' = make_deep_pair.tr2_term (rhs1,rhs2) t2 in
      let t3' = make_deep_pair.tr2_term (rhs1,rhs2) t3 in
      make_if t1 t2' t3'
  | Let(flag,bindings,t) ->
      make_let_f flag bindings @@ make_deep_pair.tr2_term (rhs1,rhs2) t
  | Label(InfoTerm{desc=Pair({desc=Var x},{desc=Var y})}, t) ->
      let rhs2' = if Id.same x rhs1 then y else rhs2 in
      make_deep_pair.tr2_term (rhs1,rhs2') t
  | _ ->
      make_pair t (make_var rhs2)

let () = make_deep_pair.tr2_term <- make_deep_pair_term
let () = make_deep_pair.tr2_typ <- make_deep_pair_typ

let make_deep_pair t rhs = make_deep_pair.tr2_term (rhs,rhs) t




let subst_all x y t = t
  |> subst x (make_var y)
  |> make_label (InfoTerm (make_pair (make_var x) (make_var y)))



let trans = make_trans2 ()

let is_higher x = order (Id.typ x) > 0

let trans_typ funargs typ =
  match typ with
  | TFun(x,typ) when is_higher x ->
      let x' = trans.tr2_var funargs x in
      let r = Id.new_var "r" @@ trans.tr2_typ funargs typ in
      TFun(x', TPair(r, Id.typ x'))
  | _ -> trans.tr2_typ_rec funargs typ

let trans_term funargs t =
  match t.desc with
  | Let(Nonrecursive,[x,[],{desc=App({desc=Var x1}, [{desc=Var x2}])}],t1) when is_higher x2 ->
      let x' = trans.tr2_var funargs x in
      let x1' = trans.tr2_var funargs x1 in
      let x2' = trans.tr2_var funargs x2 in
      let t1' = trans.tr2_term funargs t1 in
(*
Color.printf Color.Yellow "x1: @[<hov 4>%a %a@ %a@." pp_print_typ (Id.typ x1) Color.red "=>" pp_print_typ (Id.typ x1');
Color.printf Color.Yellow "x2: @[<hov 4>%a %a@ %a@." pp_print_typ (Id.typ x2) Color.red "=>" pp_print_typ (Id.typ x2');
*)
      let t = make_app (make_var x1') [make_var x2'] in
      let p = var_of_term t in
      let x2'' = Id.new_var_id x2' in
(*
Color.printf Color.Yellow "[%a |-> %a]: @[<hov 4>%a@." Id.print x2' Id.print x2'' pp_print_term t1';
*)
      let t1'' = subst_all x2' x2'' t1' in
      make_lets [p,  [], t;
                 x', [], make_fst @@ make_var p;
                 x2'', [], make_snd @@ make_var p] t1''
  | Let(Nonrecursive,[x,[],{desc=App({desc=Var f}, [{desc=Var y}])}],t1) when Id.mem f funargs ->
      let x' = trans.tr2_var funargs x in
      let f' = trans.tr2_var funargs f in
      let y' = trans.tr2_var funargs y in
      let t1' = trans.tr2_term funargs t1 in
      let f'' = Id.new_var_id f' in
      let z = Id.new_var_id y' in
      let tf = make_if (make_eq (make_var z) (make_var y)) (make_var x') (make_app (make_var f') [make_var z]) in
      make_lets [x',[],make_app (make_var f') [make_var y']; f'',[z],tf] @@ subst_all f' f'' t1'
  | Fun(x,t) when is_higher x ->
      let funargs' = x::funargs in
      let x' = trans.tr2_var funargs x in
      let t' = trans.tr2_term funargs' t in
      make_fun x' @@ make_deep_pair t' x'
  | Let(flag,bindings,t) ->
      let aux (f,xs,t) =
        let f' = trans.tr2_var funargs f in
        let t' = trans.tr2_term funargs @@ List.fold_right make_fun xs t in
        f', [], t'
      in
      let bindings' = List.map aux bindings in
      let t' = trans.tr2_term funargs t in
      make_let_f flag bindings' t'
  | Nil
  | Cons _
  | Constr _
  | Match _
  | Raise _
  | TryWith _
  | Label _
  | Record _
  | Proj _
  | SetField _ -> assert false
  | _ -> trans.tr2_term_rec funargs t

let () = trans.tr2_term <- trans_term
let () = trans.tr2_typ <- trans_typ

let trans t = t
  |> normalize.tr_term
  |> Trans.inline_let_var
  |*@> Format.printf "AAA:@.%a@.@." pp_print_term
  |> Trans.flatten_let
  |@> Format.printf "BBB:@.%a@.@." pp_print_term
  |@> flip Type_check.check TUnit
  |> trans.tr2_term []
  |@> Format.printf "CCC:@.%a@.@." pp_print_term_typ
  |> Trans.remove_label
  |@> Format.printf "DDD:@.%a@.@." pp_print_term_typ
(*
  |> Trans.inline_no_effect
 *)
  |@> flip Type_check.check TUnit
