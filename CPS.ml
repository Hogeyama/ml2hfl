
open CEGAR_syntax
open Util

let is_head_tuple t =
  match fst (decomp_app t) with
      Const (Tuple _) -> true
    | _ -> false

let rec trans_const = function
    Const (Int _ | Unit | True | False | Tuple 0 as c) -> Const c
  | Const c -> Format.printf "TRANS_CONST: %a@." CEGAR_print.print_const c; assert false
  | Var x -> Var x
  | App(App(Const Fail, (Const Unit|Var _)), t2) -> Const Fail
  | App(App(Const (Event s), t1), t2) ->
      let t1' = trans_const t1 in
      let t2' = trans_const t2 in
        App(Const (Event s), App(t2', t1'))
  | App(App(Const RandBool, (Const Unit|Var _)), t2) ->
      let t2' = trans_const t2 in
        make_app (Const Branch) [App(t2', Const True); App(t2', Const False)]
  | App(App(App(Const (And|Or|Lt|Gt|Leq|Geq|Eq|Add|Sub|Mul as c), t1), t2), t3) ->
      let t1' = trans_const t1 in
      let t2' = trans_const t2 in
      let t3' = trans_const t3 in
        App(t3', App(App(Const c, t1'), t2'))
  | App(App(Const Not, t1), t2) -> App(t2, make_not t1)
  | App _ as t when is_head_tuple t ->
      let t',ts = decomp_app t in
      let ts' = List.map trans_const ts in
      let n = match t' with Const (Tuple n) -> n | _ -> assert false in
      let ts1,ts2 = take2 ts' n in
        make_app (List.hd ts2) ((make_app t' ts1)::List.tl ts2)
  | App(App(Const (Proj i), t1), t2) -> App(t2, App(Const (Proj i), t1))
  | App(t1,t2) -> App(trans_const t1, trans_const t2)

let rec trans_simpl xs c = function
    Const x -> c (Const x)
  | Var x -> c (Var x)
  | App(App(App(Const If, t1), t2), t3) ->
      let x = new_id "x" in
      let k = new_id "k" in
      let c' y = [], App(Var k, y) in
      let defs2,t2' = trans_simpl xs c' t2 in
      let defs3,t3' = trans_simpl xs c' t3 in
      let c'' y =
        let defs,t = c (Var x) in
          (k, [x], Const True, t)::defs, make_temp_if y t2' t3'
      in
      let defs1,t1' = trans_simpl xs c'' t1 in
        defs1@@defs2@@defs3, t1'
  | App(t1, t2) ->
      let r = new_id "r" in
      let k = new_id "k" in
      let kk = List.fold_left (fun t x -> App(t, Var x)) (Var k) xs in
      let c' x = trans_simpl xs (fun y -> [], App(App(x, y), kk)) t2 in
      let defs1,t1' = c (Var r) in
      let defs2,t2' = trans_simpl xs c' t1 in
        (k, xs@[r], Const True, t1')::defs1@@defs2, t2'
let trans_simpl_def (f,xs,t1,t2) =
  let defs,t2' = trans_simpl xs (fun x -> [], x) t2 in
  let t2'' = trans_const t2' in
    (f,xs,t1,t2'')::defs



let trans (env,defs,main) =
  let defs' = rev_flatten_map trans_simpl_def defs in
  env, defs', main



