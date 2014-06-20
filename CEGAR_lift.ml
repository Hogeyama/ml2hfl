open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_util

let rec lift_term xs = function
    Const c -> [], Const c
  | Var x -> [], Var x
  | App(t1,t2) ->
      let defs1,t1' = lift_term xs t1 in
      let defs2,t2' = lift_term xs t2 in
        defs1@@@defs2, App(t1',t2')
  | Let(f,t1,t2) ->
      let ys,t1' = decomp_fun t1 in
      let ys' = List.map (fun x -> if List.mem x xs then rename_id x else x) ys in
      let f' = rename_id f in
      let f'' = make_app (Var f') (List.map (fun x -> Var x) xs) in
      let t1'' = List.fold_left2 (fun t x x' -> subst x (Var x') t) t1' ys ys' in
      let xs' = xs@ys' in
      let defs1,t1''' = lift_term xs' t1'' in
      let defs2,t2' = lift_term xs (subst f f'' t2) in
        (f',xs@ys',Const True,[],t1''') :: defs1 @ defs2, t2'
  | Fun _ as t ->
      let ys,t' = decomp_fun t in
      let f = new_id "f" in
      let ys' = List.map (fun x -> if List.mem x xs then rename_id x else x) ys in
      let t'' = List.fold_left2 (fun t x x' -> subst x (Var x') t) t' ys ys' in
      let xs' = xs@ys' in
      let f' = make_app (Var f) (List.map (fun x -> Var x) xs) in
      let defs,t''' = lift_term xs' t'' in
        (f,xs',Const True,[],t''')::defs, f'
let lift_def (f,xs,t1,e,t2) =
  let ys,t2' = decomp_fun t2 in
  let xs' = xs@ys in
  let defs1,t1' = lift_term xs t1 in
  let defs2,t2'' = lift_term xs' t2' in
    (f, xs', t1', e, t2'')::defs1@defs2
let lift {defs=defs; main=main} =
  let defs' = List.rev_flatten_map lift_def defs in
  Typing.infer {env=[];defs=defs';main=main}




let rec lift_term2 xs = function
  | Const c -> [], Const c
  | Var x -> [], Var x
  | App(t1,t2) ->
      let defs1,t1' = lift_term2 xs t1 in
      let defs2,t2' = lift_term2 xs t2 in
      defs1@@@defs2, App(t1',t2')
  | Let(f,t1,t2) ->
      let ys,t1' = decomp_fun t1 in
      let fv = List.inter xs (List.diff (get_fv t1) ys) in
      let fv' = List.map (fun x -> if List.mem x xs then rename_id x else x) fv in
      let ys' = fv' @ ys in
      let t1'' = List.fold_left2 (fun t x x' -> subst x (Var x') t) t1' fv fv' in
      let f' = rename_id f in
      let f'' = make_app (Var f') (List.map (fun x -> Var x) fv) in
      let defs1,t1''' = lift_term2 ys' t1'' in
      let defs2,t2' = lift_term2 xs (subst f f'' t2) in
      (f',ys',Const True,[],t1''') :: defs1 @ defs2, t2'
  | Fun _ as t ->
      let f = new_id "f" in
      let ys,t1 = decomp_fun t in
      let fv = List.inter xs (List.diff (get_fv t1) ys) in
      let fv' = List.map (fun x -> if List.mem x xs then rename_id x else x) fv in
      let ys' = fv' @ ys in
      let t1' = List.fold_left2 (fun t x x' -> subst x (Var x') t) t1 fv fv' in
      let f' = rename_id f in
      let f'' = make_app (Var f') (List.map (fun x -> Var x) fv) in
      let defs1,t1'' = lift_term2 ys' t1' in
      (f',ys',Const True,[],t1'') :: defs1, f''


let lift_def2 (f,xs,t1,e,t2) =
  let ys,t2' = decomp_fun t2 in
  let defs1,t1' = lift_term2 xs t1 in
  let defs2,t2'' = lift_term2 xs t2' in
    (f, xs@ys, t1', e, t2'')::defs1@defs2
let lift2 {defs=defs; main=main} =
  let defs = List.flatten_map lift_def2 defs in
  let () = if false then Format.printf "LIFTED:\n%a@." CEGAR_print.prog {env=[];defs=defs;main=main} in
  Typing.infer {env=[];defs=defs;main=main}
