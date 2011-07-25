
open CEGAR_syntax
open CEGAR_type
open Utilities


let is_head_tuple t =
  match decomp_app t with
      Const (Tuple _), _::_ -> true
    | _ -> false

let and_var = "and_cps"
let or_var = "or_cps"
let not_var = "not_cps"
let and_def = and_var, ["x"; "y"; "k"], Const True, (make_if (Var "x") (App(Var "k", Var "y")) (App(Var "k", Const False)))
let or_def = or_var, ["x"; "y"; "k"], Const True, (make_if (Var "x") (App(Var "k", Const True)) (App(Var "k", Var "y")))
let not_def = not_var, ["x"; "k"], Const True, (make_if (Var "x") (App(Var "k", Const False)) (App(Var "k", Const True)))

let rec trans_const = function
    Const (Int _ | Unit | True | False | If | Tuple 0 as c) -> Const c
  | Const And -> Var and_var
  | Const Or -> Var or_var
  | Const Not -> Var not_var
  | Const c -> Format.printf "TRANS_CONST: %a@." CEGAR_print.print_const c; assert false
  | Var x -> Var x
  | App(Const (Event s), t) ->
      let t' = trans_const t in
        App(Const (Event s), App(t', Const Unit))
  | App _ as t when is_head_tuple t ->
      let t',ts = decomp_app t in
      let ts' = List.map trans_const ts in
      let n = match t' with Const (Tuple n) -> n | _ -> assert false in
      let ts1,ts2 = take2 ts' n in
        make_app (List.hd ts2) ((make_app t' ts1)::List.tl ts2)
  | App(App(Const (Proj(n,i)), t1), t2) -> App(t2, App(Const (Proj(n,i)), t1))
  | App(t1,t2) -> App(trans_const t1, trans_const t2)
  | Let(x,t1,t2) -> Let(x, trans_const t1, trans_const t2)
  | Fun(x,t) -> Fun(x, trans_const t)

let rec trans_simpl c = function
    Const x -> c (Const x)
  | Var x -> c (Var x)
  | App(App(App(Const If, t1), t2), t3) ->
      let k = new_id "k" in
      let x = new_id "b" in
      let t2' = trans_simpl (fun y -> App(Var k, y)) t2 in
      let t3' = trans_simpl (fun y -> App(Var k, y)) t3 in
      let c' y = Let(k, Fun(x, c (Var x)), make_if y t2' t3') in
        trans_simpl c' t1
  | App(t1, t2) ->
      let k = new_id "k" in
      let r = new_id "r" in
      let c' y = trans_simpl (fun x -> Let(k, Fun(r, c (Var r)), make_app x [y; Var k])) t1 in
        trans_simpl c' t2
  | Let(x,t1,t2) ->
      let c' t = subst x t (trans_simpl c t2) in
        trans_simpl c' t1
  | Fun(x,t) ->
      let k = new_id "k" in
        c (Fun(x, Fun(k, trans_simpl (fun x -> App(Var k, x)) t)))

let trans_simpl_def (f,xs,t1,t2) =
  assert (xs = []);
  let t2 = trans_simpl (fun x -> x) t2 in
    Format.printf "TRANS: %a@." CEGAR_print.print_term t2;
  let t2 = trans_const t2 in
    (f, [], t1, t2)



let hd x = assert (List.length x = 1); List.hd x

let is_ttuple typ =
  match decomp_tapp typ with
      TBase(TTuple 0, _), _ -> true
    | _ -> false

let rec extract_tuple_var env x =
  match List.assoc x env with
      TBase(TTuple (0|1), _) -> [x]
    | TApp _ as typ when is_ttuple typ ->
        let typ,typs = decomp_tapp typ in
        let n = match typ with TBase(TTuple n, _) -> n | _ -> assert false in
          Array.to_list (Array.init n (fun i -> x ^ string_of_int i))
    | _ -> [x]
let rec extract_tuple_term env = function
    Const (Tuple 0) -> [Const Unit]
  | Const c -> [Const c]
  | Var x -> List.map (fun x -> Var x) (extract_tuple_var env x)
  | App(Const (Proj(_,i)), t2) when is_head_tuple t2 ->
      assert false;
      extract_tuple_term env (List.nth (snd (decomp_app t2)) i)
  | App(Const (Proj(_,i)), Var x) ->
      let xs = extract_tuple_var env x in
        [Var (List.nth xs i)]
  | App(t1,t2) when is_head_tuple t2 ->
      let t',ts = decomp_app t2 in
        [make_app t1 ts]
  | App(t1,t2) ->
      [make_app (hd (extract_tuple_term env t1)) (extract_tuple_term env t2)]
(*
  | Let(x,t1,t2) ->
      let t1' = hd (extract_tuple_term env t1) in
      let t2' = hd (extract_tuple_term env t2) in
        [Let(x, t1', t2')]
  | Fun(x,t) ->
      let xs = extract_tuple_var env x in
*)

        
let extract_tuple_def env (f,xs,t1,t2) =
  let env' = get_env (List.assoc f env) xs @@ env in
  let xs' = List.flatten (List.map (extract_tuple_var env') xs) in
  let t1' = hd (extract_tuple_term env t1) in
  let t2' = hd (extract_tuple_term env' t2) in
    f, xs', t1', t2'
let extract_tuple (env,defs,main) =
  let defs = List.map (extract_tuple_def env) defs in
  let () = Format.printf "EXTRACTED:\n%a@." CEGAR_print.print_prog ([],defs,main) in
    Typing.infer ([],defs,main)
  


let to_funs_def (f,xs,t1,t2) = f, [], t1, List.fold_right (fun x t-> Fun(x,t)) xs t2
let to_funs defs = List.map to_funs_def defs


let trans (env,defs,main) =
  let _ = Typing.infer (env,defs,main) in
  let defs = to_funs defs in
  let _ = Typing.infer (env,defs,main) in
  let defs = List.map trans_simpl_def defs in
  let defs = and_def::or_def::not_def::defs in
  let prog = env, defs, main in
  let () = Format.printf "CPS:\n%a@." CEGAR_print.print_prog_ML prog in
  let _ = Typing.infer prog in
  let prog = lift prog in
  let () = Format.printf "LIFTED:\n%a@." CEGAR_print.print_prog prog in
  let prog = Typing.infer prog in
    extract_tuple prog



