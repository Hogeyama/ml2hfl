open Util
open Type
open Syntax
open Term_util


let normalize = make_trans ()

let normalize_term t =
  match t.desc with
    App(t1,ts) ->
      let t1' = normalize.tr_term t1 in
      let ts' = List.map normalize.tr_term ts in
      let x = Id.new_var "x" t1.typ in
      let xs = List.map (fun t -> Id.new_var "x" t.typ) ts' in
      let aux bindings x =
        let y,_,_ = last bindings in
        let t = make_app (make_var y) [make_var x] in
        let z = Id.new_var "x" t.typ in
        bindings @ [z, [], t]
      in
      let bindings = List.tl @@ List.fold_left aux [x, [], make_var x] xs in
      let y,_,_ = last bindings in
      let t = make_lets bindings @@ make_var y in
      make_lets (List.rev @@ List.map2 (fun x t -> x, [], t) (x::xs) (t1'::ts')) t
  | If(t1,t2,t3) ->
      let x = Id.new_var "x" t1.typ in
      let t1' = normalize.tr_term t1 in
      let t2' = normalize.tr_term t2 in
      let t3' = normalize.tr_term t3 in
      make_let [x, [], t1'] @@ make_if (make_var x) t2' t3'
  | BinOp(op,t1,t2) ->
      let t1' = normalize.tr_term t1 in
      let t2' = normalize.tr_term t2 in
      let x1 = Id.new_var "x" t1.typ in
      let x2 = Id.new_var "x" t2.typ in
      make_lets [x1,[],t1'; x2,[],t2'] @@ {desc=BinOp(op,make_var x1,make_var x2); typ=t.typ}
  | Not t1 ->
      let t1' = normalize.tr_term t1 in
      let x = Id.new_var "x" t.typ in
      make_let [x, [], t1'] @@ make_not (make_var x)
  | Pair(t1,t2) ->
      let t1' = normalize.tr_term t1 in
      let t2' = normalize.tr_term t2 in
      let x1 = Id.new_var "x" t1.typ in
      let x2 = Id.new_var "x" t2.typ in
      make_lets [x2,[],t2'; x1,[],t1'] @@ make_pair t1 t2
  | Fst t1 ->
      let t1' = normalize.tr_term t1 in
      let x = Id.new_var "x" t1'.typ in
      make_let [x, [], t1'] @@ make_fst (make_var x)
  | Snd t1 ->
      let t1' = normalize.tr_term t1 in
      let x = Id.new_var "x" t1'.typ in
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


let trans = make_trans ()

let is_higher x = order (Id.typ x) > 0

let trans_typ typ =
  match typ with
    TFun(x,typ) when is_higher x ->
      let x' = Id.set_typ x @@ trans.tr_typ @@ Id.typ x in
      let y = Id.new_var_id x' in
      TFun(x, TPair(y, trans.tr_typ typ))
  | _ -> trans.tr_typ_rec typ

let trans_desc desc =
  match desc with
    Let(Nonrecursive,[x,[],{desc=App({desc=Var x1}, [{desc=Var x2}])}],t1) when is_higher x2 ->
      let x' = trans.tr_var x in
      let x1' = trans.tr_var x1 in
      let x2' = trans.tr_var x2 in
      let t1' = trans.tr_term t1 in
      let t = make_app (make_var x1') [make_var x2'] in
      let p = Id.new_var "x" t.typ in
      let y = Id.new_var_id x' in
      let t' =
        make_lets [p,  [], t;
                   x', [], make_fst @@ make_var p;
                   y, [], make_snd @@ make_var p] t1'
      in
      t'.desc
  | Fun(x,t) when is_higher x ->
      let t' = trans.tr_term t in
      (make_fun x @@ make_pair t' (make_var x)).desc
  | Let(flag,bindings,t) ->
      let aux (f,xs,t) = f, [], trans.tr_term @@ List.fold_right make_fun xs t in
      let bindings' = List.map aux bindings in
      let t' = trans.tr_term t in
      (make_let_f flag bindings' t').desc
  | Nil
  | Cons _
  | Constr _ -> assert false
  | Match _ -> assert false
  | Raise _
  | TryWith _
  | Label _
  | Record _
  | Proj _
  | SetField _ -> assert false
  | _ -> trans.tr_desc_rec desc

let () = trans.tr_desc <- trans_desc
let () = trans.tr_typ <- trans_typ

let trans t = t
  |> normalize.tr_term
  |> do_and_return (Format.printf "AAA:@.%a@." pp_print_term)
  |> Type_check.check_and_return TUnit
  |> trans.tr_term
  |> do_and_return (Format.printf "BBB:@.%a@." pp_print_term)
  |> Type_check.check_and_return TUnit
  |> Trans.inline_no_effect
