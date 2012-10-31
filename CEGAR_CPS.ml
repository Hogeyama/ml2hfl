
open CEGAR_syntax
open CEGAR_type
open CEGAR_print
open CEGAR_util
open Utilities

let is_head_tuple t =
  match decomp_app t with
      Const (Tuple _), _::_ -> true
    | _ -> false

let and_cps = "and_cps"
let or_cps = "or_cps"
let not_cps = "not_cps"
let and_def = and_cps, ["x"; "y"; "k"], Const True, [], (make_if (Var "x") (App(Var "k", Var "y")) (App(Var "k", Const False)))
let or_def = or_cps, ["x"; "y"; "k"], Const True, [], (make_if (Var "x") (App(Var "k", Const True)) (App(Var "k", Var "y")))
let not_def = not_cps, ["x"; "k"], Const True, [], (make_if (Var "x") (App(Var "k", Const False)) (App(Var "k", Const True)))

let rec trans_const = function
    Const (Int _ | Unit | True | False | RandBool | If | Tuple _ | Bottom as c) -> Const c
  | Const Not -> Var not_cps
  | Const c -> Format.printf "TRANS_CONST: %a@." CEGAR_print.const c; assert false
  | Var x -> Var x
(*
  | App(App(Const (Label n), t1), t2) -> App(Const (Label n), App(t2, t1))
*)
(*
  | App _ as t when is_head_tuple t ->
      let t',ts = decomp_app t in
      let ts' = List.map trans_const ts in
      let n = match t' with Const (Tuple n) -> n | _ -> assert false in
        if List.length ts' = n
        then make_app t' ts'
        else
          let ts1,ts2 = take2 ts' n in
            if ts2 = [] then (Format.printf "%a@." print_term t; assert false);
            make_app (List.hd ts2) ((make_app t' ts1)::List.tl ts2)
*)
  | App(App(Const (Proj(n,i)), t1), t2) -> App(trans_const t2, App(Const (Proj(n,i)), trans_const t1))
  | App(t1,t2) -> App(trans_const t1, trans_const t2)
  | Let(x,t1,t2) -> Let(x, trans_const t1, trans_const t2)
  | Fun(x,typ,t) -> Fun(x, typ, trans_const t)

let rec trans_simpl c = function
    Const x -> c (Const x)
  | Var x -> c (Var x)
  | App(App(App(Const If, t1), t2), t3) ->
      let k = new_id "k" in
      let x = new_id "b" in
      let t2' = trans_simpl (fun y -> App(Var k, y)) t2 in
      let t3' = trans_simpl (fun y -> App(Var k, y)) t3 in
      let c' y =
        match c (Var x) with
            App(Var k', Var x') when x = x' -> subst k (Var k') (make_if y t2' t3')
          | tk -> Let(k, Fun(x, None, tk), make_if y t2' t3')
      in
        trans_simpl c' t1
  | App(App(Const And, t1), t2) ->
      let x = new_id "b" in
      let c1 t1' t2' =
        let k' =
          match c (Var x) with
              App(Var k', Var x') when x = x' -> Var k'
            | tk -> Fun(x, None, tk)
        in
          make_app (Var and_cps) [t1';t2';k']
      in
      let c2 y1 = trans_simpl (fun y2 -> c1 y1 y2) t2 in
        trans_simpl c2 t1
  | App(App(Const Or, t1), t2) ->
      let x = new_id "b" in
      let c1 t1' t2' =
        let k' =
          match c (Var x) with
              App(Var k', Var x') when x = x' -> Var k'
            | tk -> Fun(x, None, tk)
        in
          make_app (Var or_cps) [t1';t2';k']
      in
      let c2 y1 = trans_simpl (fun y2 -> c1 y1 y2) t2 in
        trans_simpl c2 t1
  | App _ as t when is_head_tuple t ->
      let t',ts = decomp_app t in
      let n = match t' with Const (Tuple n) -> n | _ -> assert false in
      let () = assert (List.length ts = n) in
      let cc = List.fold_right (fun t cc -> fun x -> trans_simpl (fun y -> cc (App(x, y))) t) ts c in
        trans_simpl cc (Const (Tuple n))
  | App(t1, t2) ->
      let r = new_id "r" in
      let c' y =
        let k' =
          match c (Var r) with
              App(Var k', Var r') when r = r' -> Var k'
            | tk -> Fun(r, None, tk)
        in
          trans_simpl (fun x -> make_app y [x; k']) t2
      in
        trans_simpl c' t1
  | Let(x,t1,t2) ->
      let c' t = subst x t (trans_simpl c t2) in
        trans_simpl c' t1
  | Fun(x,_,t) ->
      let k = new_id "k" in
        c (Fun(x, None, Fun(k, None, trans_simpl (fun x -> App(Var k, x)) t)))

let trans_simpl_def (f,xs,t1,e,t2) =
  if f =  "mult_70" then () else ();
  assert (xs = []);
  let t2 = trans_simpl (fun x -> x) t2 in
  if false then Format.printf "TRANS: %a@." CEGAR_print.term t2;
  let t2 = trans_const t2 in
    (f, [], t1, e, t2)



let hd x = assert (List.length x = 1); List.hd x

let is_ttuple typ =
  match decomp_tapp typ with
      TBase(TTuple _, _), _ -> true
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
      let t,ts = decomp_app t2 in
      let n = match t with Const (Tuple n) -> n | _ -> assert false in
        assert (List.length ts = n);
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
  | Let _ -> assert false
  | Fun _ -> assert false


let extract_tuple_def env (f,xs,t1,e,t2) =
  let env' = get_arg_env (List.assoc f env) xs @@ env in
  let xs' = List.flatten (List.map (extract_tuple_var env') xs) in
  let t1' = hd (extract_tuple_term env t1) in
  let t2' = hd (extract_tuple_term env' t2) in
    f, xs', t1', e, t2'
let extract_tuple {env=env;defs=defs;main=main} =
  let defs = List.map (extract_tuple_def env) defs in
  let () = if false then Format.printf "EXTRACTED:\n%a@." CEGAR_print.prog {env=[];defs=defs;main=main} in
    Typing.infer {env=[];defs=defs;main=main}



let to_funs_def (f,xs,t1,e,t2) = f, [], t1, e, List.fold_right (fun x t-> Fun(x,None,t)) xs t2
let to_funs defs = List.map to_funs_def defs


let rec reduce = function
    Const c -> Const c
  | Var x -> Var x
  | App(Fun(x,_,t1),t2) -> reduce (subst x t2 t1)
  | App(t1,t2) -> App(reduce t1, reduce t2)
  | Fun(x,typ,t) -> Fun(x, typ, reduce t)
  | Let(x,t1,t2) -> reduce (subst x t1 t2)
let reduce_def (f,xs,t1,e,t2) = f,xs,t1,e,reduce t2

let trans {env=env;defs=defs;main=main} lift_opt =
  let _ = Typing.infer {env=env;defs=defs;main=main} in
  let defs = to_funs defs in
  let _ = Typing.infer {env=env;defs=defs;main=main} in
  let defs = List.map trans_simpl_def defs in
  let defs = List.map reduce_def defs in
  let defs = and_def::or_def::not_def::defs in
  let prog = {env=env; defs=defs; main=main} in
  let () = if false then Format.printf "BEFORE LIFT:\n%a@." CEGAR_print.prog prog in
  let _ = Typing.infer prog in
  let prog = if lift_opt then lift prog else lift2 prog in
  let () = if false then Format.printf "LIFTED:\n%a@." CEGAR_print.prog prog in
    extract_tuple prog



























(*
type typed_term = {t_cps:t_cps; typ_cps:typ_cps}
and typed_ident = {id_cps:var; id_typ:typ_cps}
and t_cps =
    ConstCPS of const
  | VarCPS of typed_ident
  | AppCPS of typed_term * typed_term list
  | FunCPS of typed_ident * typed_term
  | IfCPS of typed_term * typed_term * typed_term
  | LetCPS of typed_ident * typed_ident list * typed_term * typed_term
and typ_cps =
    TBaseCPS
  | TBottomCPS
  | TVarCPS of typ_cps option ref
  | TFunCPS of bool ref * typ_cps * typ_cps
  | TTupleCPS of typ_cps list

let rec print_typ_cps fm = function
    TBaseCPS -> Format.fprintf fm "o"
  | TVarCPS {contents = None} -> Format.fprintf fm "?"
  | TVarCPS {contents = Some typ} -> Format.fprintf fm "%a" print_typ_cps typ
  | TFunCPS({contents=b},typ1,typ2) ->
      Format.fprintf fm "(%a %s %a)" print_typ_cps typ1 (if b then "=>" else "->") print_typ_cps typ2
  | TTupleCPS typs -> Format.fprintf fm "(%a)" (print_list print_typ_cps " * " false) typs
  | TBottomCPS -> Format.fprintf fm "bottom"

and print_typed_term fm {t_cps=t; typ_cps=typ} =
  Format.fprintf fm "(%a : %a)" print_t_cps t print_typ_cps typ

and print_t_cps fm = function
    ConstCPS c -> Format.fprintf fm "%a" print_term (Const c)
  | VarCPS x -> Format.fprintf fm "%s" x.id_cps
  | FunCPS(x, t) ->
      Format.fprintf fm "fun %s -> %a" x.id_cps print_typed_term t
  | AppCPS(t, ts) ->
      Format.fprintf fm "%a %a" print_typed_term t (print_list print_typed_term " " false) ts
  | IfCPS(t1, t2, t3) ->
      Format.fprintf fm "@[@[if %a@]@;then @[%a@]@;else @[%a@]@]"
        print_typed_term t1 print_typed_term t2 print_typed_term t3
  | LetCPS(f, xs, t1, t2) ->
      let p_ids fm xs = Format.fprintf fm "%a" (print_list Format.pp_print_string " " false) (List.map (fun x -> x.id_cps) xs)
      in
        begin
          match t2.t_cps with
              LetCPS _ ->
                Format.fprintf fm "@[<v>@[<hov 2>let %a= @,%a@]@;in@;%a@]"
                  p_ids (f::xs) print_typed_term t1 print_typed_term t2
            | _ ->
                Format.fprintf fm "@[<v>@[<hov 2>let %a= @,%a @]@;@[<v 2>in@;@]@[<hov>%a@]@]"
                  p_ids (f::xs) print_typed_term t1 print_typed_term t2
        end

let print_typed_defs defs =
  List.iter (fun (f,xs,t1,t2) -> assert (xs=[] && t1=Const True); Format.printf "%s -> %a@." f print_typed_term t2) defs

let rec find_var_typ x env =
  try
    List.assoc x env
  with Not_found ->
    if Flag.debug
    then Format.printf "@.not found: %s@." x;
    assert false

let new_tvar () = TVarCPS (ref None)
let new_var x = {id_cps=x; id_typ=new_tvar()}

let rec flatten = function
    TVarCPS{contents = Some typ} -> flatten typ
  | typ -> typ

let rec flatten_ref r =
  match !r with
    Some (TVarCPS{contents=Some(TVarCPS r)}) -> flatten_ref r
  | _ -> r

let rec occurs r typ =
  match flatten_ref r, flatten typ with
      r, TFunCPS(_,typ1,typ2) -> occurs r typ1 || occurs r typ2
    | r, TVarCPS({contents = None} as r') -> r == r'
    | _ -> false

let rec unify typ1 typ2 =
  match typ1, typ2 with
      TBottomCPS, typ
    | typ, TBottomCPS -> typ
    | TBaseCPS, TBaseCPS -> TBaseCPS
    | TVarCPS({contents = Some typ} as r), typ'
    | typ, TVarCPS({contents = Some typ'} as r) ->
        let typ'' = unify typ typ' in
          r := Some (flatten typ'');
          TVarCPS r
    | TVarCPS r1, TVarCPS r2 when r1 == r2 -> TVarCPS r1
    | TVarCPS({contents = None} as r), typ
    | typ, TVarCPS({contents = None} as r) ->
        if occurs r typ
        then raise Typing.CannotUnify
        else r := Some typ;
        typ
    | TFunCPS(b1,typ11,typ12), TFunCPS(b2,typ21,typ22) ->
        let b = max !b1 !b2 in
        let () = b1 := b in
        let () = b2 := b in
        let typ1' = unify typ11 typ21 in
        let typ2' = unify typ12 typ22 in
          TFunCPS(b1,typ1',typ2')
    | TTupleCPS typs1, TTupleCPS typs2 ->
        let typs = List.map2 unify typs1 typs2 in
          TTupleCPS typs
let unify typ1 typ2 = ignore (unify typ1 typ2)

let infer_cont_pos_const = function
    Event s -> TBaseCPS
  | Label n ->
      let typ = new_tvar () in
        TFunCPS(ref true, typ, typ)
  | Unit -> TBaseCPS
  | True -> TBaseCPS
  | False -> TBaseCPS
  | RandInt -> TBaseCPS
  | RandBool -> TBaseCPS
  | And -> TFunCPS(ref false, TBaseCPS, TFunCPS(ref true, TBaseCPS, TBaseCPS))
  | Or -> TFunCPS(ref false, TBaseCPS, TFunCPS(ref true, TBaseCPS, TBaseCPS))
  | Not -> TFunCPS(ref true, TBaseCPS, TBaseCPS)
  | Lt -> TFunCPS(ref false, TBaseCPS, TFunCPS(ref true, TBaseCPS, TBaseCPS))
  | Gt -> TFunCPS(ref false, TBaseCPS, TFunCPS(ref true, TBaseCPS, TBaseCPS))
  | Leq -> TFunCPS(ref false, TBaseCPS, TFunCPS(ref true, TBaseCPS, TBaseCPS))
  | Geq -> TFunCPS(ref false, TBaseCPS, TFunCPS(ref true, TBaseCPS, TBaseCPS))
  | Eq -> TFunCPS(ref false, TBaseCPS, TFunCPS(ref true, TBaseCPS, TBaseCPS))
  | Int n -> TBaseCPS
  | Add -> TFunCPS(ref false, TBaseCPS, TFunCPS(ref true, TBaseCPS, TBaseCPS))
  | Sub -> TFunCPS(ref false, TBaseCPS, TFunCPS(ref true, TBaseCPS, TBaseCPS))
  | Mul -> TFunCPS(ref false, TBaseCPS, TFunCPS(ref true, TBaseCPS, TBaseCPS))
  | Tuple n ->
      let typs = Array.to_list (Array.init n (fun _ -> new_tvar())) in
      let aux typ (typ',b) = TFunCPS(ref b,typ,typ'), false in
      let typ,_ = List.fold_right aux typs (TTupleCPS typs,true) in
        typ
  | Proj(n,i) ->
      let typs = Array.to_list (Array.init n (fun _ -> new_tvar())) in
        TFunCPS(ref true, TTupleCPS typs, List.nth typs i)
  | If -> assert false
  | Bottom -> TBottomCPS

let rec infer_cont_pos env = function
    Const c -> {t_cps=ConstCPS c; typ_cps=infer_cont_pos_const c}
  | Var x ->
      let typ = try List.assoc x env with Not_found -> assert false in
        {t_cps=VarCPS{id_cps=x;id_typ=typ}; typ_cps=typ}
  | Fun(x, t1) ->
      let f = new_id "f" in
        infer_cont_pos env (Let(f,Fun(x, t1),Var f))
  | App(App(App(Const If, t1), t2), t3) ->
      let typed1 = infer_cont_pos env t1 in
      let typed2 = infer_cont_pos env t2 in
      let typed3 = infer_cont_pos env t3 in
        unify typed1.typ_cps TBaseCPS;
        unify typed2.typ_cps typed3.typ_cps;
        {t_cps=IfCPS(typed1,typed2,typed3); typ_cps=typed2.typ_cps}
  | App _ as t ->
      let t1,ts = decomp_app t in
      let typed1 = infer_cont_pos env t1 in
      let typeds = List.map (infer_cont_pos env) ts in
      let typ_result = new_tvar () in
      let aux typed (typ,b) = TFunCPS(ref b,typed.typ_cps,typ), false in
      let typ,_ = List.fold_right aux typeds (typ_result,true) in
        unify typed1.typ_cps typ;
        {t_cps=AppCPS(typed1,typeds); typ_cps=typ_result}
  | Let(f, t1, t2) ->
      let xs,t1 = decomp_fun t1 in
      let typ_f = new_tvar () in
      let f' = {id_cps=f; id_typ=typ_f} in
      let typ_args = List.map (fun _ -> new_tvar ()) xs in
      let xs' = List.map2 (fun x typ -> {id_cps=x; id_typ=typ}) xs typ_args in
      let env2 = (f, typ_f) :: env in
      let env1 = List.combine xs typ_args @@ env in
      let typed1 = infer_cont_pos env1 t1 in
      let typed2 = infer_cont_pos env2 t2 in
      let b = ref true in
      let aux typ1 typ2 =
        let typ = TFunCPS(ref !b,typ1,typ2) in
          b := false; typ
      in
      let typ = List.fold_right aux typ_args typed1.typ_cps in
        unify typ_f typ;
        {t_cps=LetCPS(f',xs',typed1,typed2); typ_cps=typed2.typ_cps}

let rec get_arg_num = function
    TBaseCPS -> 0
  | TVarCPS{contents=None} -> 0
  | TVarCPS{contents=Some typ} -> get_arg_num typ
  | TFunCPS({contents=true},typ1,typ2) -> 1
  | TFunCPS({contents=false},typ1,typ2) -> 1 + get_arg_num typ2

let rec app_typ typ typs =
  match typ,typs with
      TVarCPS{contents=Some typ},_ -> app_typ typ typs
    | TFunCPS(_,_,typ2), _::typs' -> app_typ typ2 typs'
    | _, [] -> typ
    | _ -> assert false

let rec transform c {t_cps=t; typ_cps=typ} =
  match t with
      ConstCPS cst -> c (Const cst)
    | VarCPS x -> c (Var x.id_cps)
    | FunCPS(x, t) -> assert false
    | AppCPS(t1, ts) ->
        let n = get_arg_num t1.typ_cps in
          if n = List.length ts
          then
            let k = new_id "k" in
            let r = new_id "r" in
            let c1 x = App(x, Var k) in
            let cc = List.fold_right (fun t cc -> fun x -> transform (fun y -> cc (App(x, y))) t) ts c1 in
              Let(k, Fun(r, c (Var r)), transform cc t1)
          else
            let ts1,ts2 = take2 ts n in
            let typ' = app_typ t1.typ_cps ts1 in
              transform c {t_cps=AppCPS({t_cps=AppCPS(t1,ts1);typ_cps=typ'},ts2); typ_cps=typ}
    | IfCPS(t1, t2, t3) ->
        let k = new_id "k" in
        let x = new_id "r" in
        let t2' = transform (fun y -> App(Var k, y)) t2 in
        let t3' = transform (fun y -> App(Var k, y)) t3 in
        let c' y = Let(k, Fun(x, c (Var x)), make_if y t2' t3') in
          transform c' t1
    | LetCPS(f, [], t1, t2) ->
        let c' t = subst f.id_cps t (transform c t2) in
          transform c' t1
    | LetCPS(f, xs, t1, t2) ->
        let n = get_arg_num f.id_typ in
          if n = List.length xs
          then
            let k = new_id "k" in
            let f' = f.id_cps in
            let xs' = List.map (fun x -> x.id_cps) xs in
            let t1' = transform (fun y -> App(Var k, y)) t1 in
            let t2' = transform c t2 in
              Let(f', make_fun_temp (xs'@[k]) t1', t2')
          else
            let xs1,xs2 = take2 xs n in
            let typ_g = app_typ f.id_typ xs1 in
            let g = {id_cps=new_id f.id_cps; id_typ=typ_g} in
            let t1' = {t_cps=LetCPS(g, xs2, t1, {t_cps=VarCPS g;typ_cps=typ_g}); typ_cps=typ_g} in
              transform c {t_cps=LetCPS(f, xs1, t1',t2); typ_cps=typ}

let trans_def (f,xs,t1,t2) =
  assert (xs = []);
  assert (t1 = Const True);
  let t2 = transform (fun x -> x) t2 in
  let t2 = trans_const t2 in
    (f, [], t1, t2)

let unify_env env =
  let rec aux = function
      [] -> ()
    | (f,typ)::env ->
        let env1,env2 = List.partition (fun (g,_) -> f = g) env in
        let () =
          match env1 with
              [] -> ()
            | (_,typ)::env1' -> List.iter (fun (_,typ') -> unify typ typ') env1'
        in
          aux env2
  in
    aux env

let trans (_,defs,main) =
  let _ = Typing.infer ([],defs,main) in
  let defs = to_funs defs in
  let env = List.map (fun (f,_,_,_) -> f, new_tvar()) defs in
  let () = unify_env env in
  let aux (f,xs,t1,t2) =
    let t2' = infer_cont_pos env t2 in
      unify (t2'.typ_cps) (List.assoc f env);
      f,xs,t1,t2'
  in
  let defs = List.map aux defs in
  let () = if true then print_typed_defs defs in
  let defs = List.map trans_def defs in
  let defs = and_def::or_def::not_def::defs in
  let prog = [], defs, main in
  let () = if true then Format.printf "BEFORE LIFT:\n%a@." CEGAR_print.print_prog prog in
  let _ = Typing.infer prog in
  let prog = lift prog in
  let () = if false then Format.printf "LIFTED:\n%a@." CEGAR_print.print_prog prog in
  let prog = Typing.infer prog in
    extract_tuple prog
*)
