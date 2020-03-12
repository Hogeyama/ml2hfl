open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_print
open CEGAR_util

let hd (defs,ts) =
  assert (defs = []);
  match ts with
  | [] -> assert false
  | [x] -> x
  | _ -> assert false

let print_pb fm (p,b) =
  Format.fprintf fm "%a := %a" CEGAR_print.term b CEGAR_print.term p

let print_pbs fm pbs =
  print_list print_pb ";@\n" fm pbs


let rec is_base_term env t =
  match t with
  | Const (Unit | True | False | Int _ | Rand _ | Char _ | String _ | Float _ | Int32 _ | Int64 _ | Nativeint _) -> true
  | Const _ -> false
  | Var x ->
      (try
          is_base @@ List.assoc x env
        with Not_found ->
          Format.eprintf "%s not found@." x;
          assert false)
  | App(App(Const (And|Or|Lt|Gt|Leq|Geq|EqUnit|EqInt|EqBool|CmpPoly _|Add|Sub|Mul|Div),t1),t2) ->
      assert (is_base_term env t1);
      assert (is_base_term env t2);
      true
  | App(Const Not,t) -> is_base_term env t
  | App _ -> false
  | Let _ -> false
  | Fun _ -> false



let rec make_arg_let_term = function
  | Const c -> [], Const c
  | Var x -> [], Var x
  | App _ as t ->
      let t',ts = decomp_app t in
      let aux t (bind,ts) =
        let bind',t' = make_arg_let_term t in
        bind'@bind, t'::ts
      in
      let bind,ts' = List.fold_right aux ts ([],[]) in
      let xs = List.map (fun _ -> new_id "a") ts in
      let t'' = List.fold_left (fun t x -> App(t, Var x)) t' xs in
      bind @ List.combine xs ts', t''
  | Let _ -> assert false
  | Fun _ -> assert false
let make_arg_let_term t =
  let bind,t' = make_arg_let_term t in
  List.fold_right (fun (x,t) t' -> Let(x, t, t')) bind t'

let rec reduce_let env = function
  | Const c -> Const c
  | Var x -> Var x
  | App(t1,t2) -> App(reduce_let env t1, reduce_let env t2)
  | Fun _ -> assert false
  | Let(x,t1,t2) ->
      match t1,get_typ env t1 with
      | Var _, _
      | Const _, _
      | _, TFun _ -> reduce_let env (subst x t1 t2)
      | _, (TBase _ as typ) -> Let(x, reduce_let env t1, reduce_let ((x,typ)::env) t2)
      | _ -> assert false

let make_arg_let_def env ({fn=f; args=xs; body=t2} as def) =
  let body = reduce_let (get_arg_env (List.assoc f env) xs @@@ env) (make_arg_let_term t2) in
  {def with body}

let make_arg_let prog =
  {prog with defs = List.map (make_arg_let_def prog.env) prog.defs}



let has_branch {defs} =
  let fs = List.sort compare @@ List.map (fun def -> def.fn) defs in
  let rec aux fs =
    match fs with
    | [] -> []
    | [f] -> []
    | f::g::fs' when f = g -> f :: aux fs'
    | f::fs' -> aux fs'
  in
  List.unique @@ aux fs

let rec add_label prog =
  let merge = function
    | [] -> assert false
    | [def] -> assert (def.cond = Const True); [def]
    | [{fn=f1;args=xs1;cond=t11;body=t12} as def1; {fn=f2;args=xs2;cond=t21;body=t22} as def2] when f1=f2 && xs1=xs2 && t11=make_not t21 ->
        [{def1 with body=make_label 1 t12}; {def2 with body=make_label 0 t22}]
    | [{fn=f1;args=xs1;cond=t11;body=t12} as def1; {fn=f2;args=xs2;cond=t21;body=t22} as def2] when f1=f2 && xs1=xs2 && make_not t11=t21 ->
        [{def1 with body=make_label 0 t12}; {def2 with body=make_label 1 t22}]
    | [_; _] as defs->
        Format.eprintf "%a@." CEGAR_print.prog {env=[]; defs; main=""; info=init_info};
        assert false
    | def::defs -> fatal @@ Format.sprintf "Not implemented (CEGAR_abst_util.add_label) %s %d" def.fn (1 + List.length defs)
  in
  let rec aux = function
    | [] -> []
    | ({fn=f} as def)::defs ->
        let defs1,defs2 = List.partition (fun {fn=g} -> f = g) defs in
        let defs' = merge (def::defs1) in
        defs' @ aux defs2
  in
  let defs = aux prog.defs in
  let labeled = List.unique @@ List.rev_flatten_map (function {fn=f;body=App(Const (Label _),_)} -> [f] | _ -> []) defs in
  assert List.Set.(labeled = has_branch prog);
  labeled, {prog with defs=defs}



let rec use_arg x typ t =
  match typ with
  | TBase _ -> t
  | TFun(typ1,typ2) ->
      let u = new_id "u" in
      let t' = make_br (Const Unit) (App(Var x, make_ext_fun typ1)) in
      App(Fun(u, None, t), t')
  | _ -> assert false

and make_ext_fun = function
  | TBase(TUnit, _) -> Const Unit
  | TBase(TBool, _) -> make_br (Const True) (Const False)
  | TFun(typ1,typ2) ->
      let x = new_id "x" in
      Fun(x, None, use_arg x typ1 (make_ext_fun (typ2 (Const Unit))))
  | _ -> assert false


let add_ext_funs prog =
  let env = get_ext_fun_env prog in
  let defs = List.map (fun (f,typ) -> {fn=f; args=[]; cond=Const True; events=[]; body=make_ext_fun typ}) env in
  let defs' = defs@prog.defs in
  ignore @@ Typing.infer {prog with env=[]; defs=defs'};
  {prog with defs=defs'}


