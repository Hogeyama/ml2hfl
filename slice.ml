open Syntax
open Term_util
open Type
open Util

module Debug = Debug.Make(struct let check = Flag.Debug.make_check __MODULE__ end)

type env =
  {constr: (int * int) list;
   counter: int;
   tenv: (id * typ) list;
   fv: id list}

(* id 0 represents "CARE"*)
let initial_env fv =
  let counter = 1 in
  let constr = [] in
  let tenv = [] in
  {counter; constr; tenv; fv}

let new_id env =
  let counter = env.counter + 1 in
  {env with counter}, counter

let label = "slice"
let _TAId id = TAId(label,id)

let add_id id ty =
  _TAttr [_TAId id] ty

let add_new_id env ty =
  let env', id = new_id env in
  env', add_id id ty

let get_id_attr attrs =
  List.get @@ List.filter_map (function TAId(s,x) when s = label -> Some x | _ -> None) attrs

let get_id ty =
  ty
  |> decomp_tattr
  |> fst
  |> get_id_attr

let set_id id ty =
  let attr,ty' = decomp_tattr ty in
  let attr' = List.map (function TAId(s,_) when s = label -> TAId(label,id) | a -> a) attr in
  TAttr(attr', ty')

let rec make_template env ty =
  match ty with
  | TBase _ -> add_new_id env ty
  | TVarLazy _ -> assert false
  | TVar _ -> unsupported __MODULE__
  | TFun(x, ty2) ->
      let env,ty = make_template env @@ Id.typ x in
      let x' = Id.set_typ x ty in
      let env,ty2' = make_template env ty2 in
      let env,l = new_id env in
      let env =
        let l' = get_id ty2' in
        let constr = (l',l)::env.constr in
        {env with constr}
      in
      env, _TAttr [TAId(label,l)] @@ TFun(x', ty2')
  | TTuple xs ->
      let env,l = new_id env in
      let env,xs' =
        let aux (env,xs) x =
          let env,ty = make_template env @@ Id.typ x in
          let x' = Id.set_typ x ty in
          let env =
            let l' = get_id ty in
            let constr = (l,l')::env.constr in
            {env with constr}
          in
          env, xs@[x']
        in
        List.fold_left aux (env,[]) xs
      in
      env, _TAttr [_TAId l] @@ TTuple xs'
  | TAttr(attr, ty1) ->
      let env,ty1' = make_template env ty1 in
      env, _TAttr attr ty1'
  | TFuns _ -> unsupported __MODULE__
  | TData _ -> unsupported __MODULE__
  | TVariant _ -> unsupported __MODULE__
  | TRecord _ -> unsupported __MODULE__
  | TApp _ -> unsupported __MODULE__
  | TModule _ -> unsupported __MODULE__

let rec flatten_sub_attr attr1 attr2 env =
  let x = get_id_attr attr1 in
  let y = get_id_attr attr2 in
  {env with constr=(x, y)::env.constr}

let rec flatten_sub ty1 ty2 constr =
  match ty1, ty2 with
  | TAttr(attr1, TBase _), TAttr(attr2, TBase _) ->
      flatten_sub_attr attr1 attr2 constr
  | TBase _, _ -> assert false
  | TVarLazy _, _ -> assert false
  | TVar _, _ -> unsupported __MODULE__
  | TAttr(attr1, TFun(x1,ty12)), TAttr(attr2, TFun(x2,ty22)) ->
      constr
      |> flatten_sub_attr attr2 attr1
      |> flatten_sub (Id.typ x2) (Id.typ x1)
      |> flatten_sub ty12 ty22
  | TFun _, _ ->
      assert false
  | TFuns _, _ -> unsupported __MODULE__
  | TAttr(attr1, TTuple xs1), TAttr(attr2, TTuple xs2) ->
      constr
      |> flatten_sub_attr attr1 attr2
      |> List.fold_right2 (fun x1 x2 -> flatten_sub (Id.typ x1) (Id.typ x2)) xs1 xs2
  | TTuple _, _ -> assert false
  | TData _, _ -> unsupported __MODULE__
  | TVariant _, _ -> unsupported __MODULE__
  | TRecord _, _ -> unsupported __MODULE__
  | TApp _, _ -> unsupported __MODULE__
  | TAttr(attr,ty1'), ty2'
  | ty1', TAttr(attr, ty2') -> flatten_sub ty1' ty2' constr
  | TModule _, _ -> unsupported __MODULE__
let flatten_sub ty1 ty2 constr =
  Debug.printf "FLATTEN: @[@[%a@] <:@ @[%a@." Print.typ ty1 Print.typ ty2;
  flatten_sub ty1 ty2 constr

let rec gen_constr env l t ty =
  Debug.printf "gen_constr: @[%a@],@ @[%a@.@." Print.term t Print.typ ty;
  let env,l_t = new_id env in
  let env = {env with constr= (l,l_t)::env.constr} in
  let env,t' =
    match t.desc with
    | Const (Rand(_, true)) -> unsupported __MODULE__
    | Const (Rand(_, false))
    | Const _
    | Bottom ->
        let env,typ = make_template env t.typ in
        let env = flatten_sub typ ty env in
        let env =
          let l = get_id ty in
          let constr = (l_t,l)::env.constr in
          {env with constr}
        in
        env, {t with typ}
    | Var x when Id.mem x env.fv ->
        let env,typ = make_template env t.typ in
        let typ' = set_id 0 ty in
        let x' = Id.set_typ x typ' in
        let env = flatten_sub typ' ty env in
        let env =
          let l = get_id ty in
          let constr = (l_t,l)::env.constr in
          {env with constr}
        in
        env, {desc=Var x'; typ; attr=t.attr}
    | Var x ->
        let typ =
	  try
	    Id.assoc x env.tenv
	  with
	  | Not_found ->
              Format.eprintf "%a@." Print.id x;
              assert false
        in
        let x' = Id.set_typ x typ in
        let env = flatten_sub typ ty env in
        let env =
          let l = get_id ty in
          let constr = (l_t,l)::env.constr in
          {env with constr}
        in
        env, {desc=Var x'; typ; attr=t.attr}
    | Fun(x, t1) ->
        let tenv = env.tenv in
        let env,ty' = make_template env t.typ in
        let env,x_ty,r_ty =
          match elim_tattr ty' with
          | TFun(x, ty) -> env, Id.typ x, ty
          | _ -> assert false
        in
        let l' = get_id ty in
        let x' = Id.set_typ x x_ty in
        let env = {env with tenv = (x, x_ty) :: env.tenv} in
        let env,t1' = gen_constr env l' t1 r_ty in
        let typ = add_id l' @@ TFun(x',t1'.typ) in
        let env = flatten_sub ty' ty env in
        let env = {env with constr=(l_t,l')::env.constr} in
        {env with tenv}, {desc=Fun(x',t1'); typ; attr=t.attr}
    | App(t1, []) -> assert false
    | App(t1, t2::t3::ts) ->
        let t12 = {(make_app_raw t1 [t2]) with attr=[]} in
        let t' = {t with desc=App(t12, t3::ts)} in
        let env,t'' = gen_constr env l_t t' ty in
        let attr = List.filter (function AId _ -> false | _ -> true) t''.attr in
        env, {t'' with attr}
    | App(t1, [t2]) ->
        let env, x' =
          match elim_tattr t1.typ with
          | TFun(x,ty2) ->
              let env,ty1 = make_template env (Id.typ x) in
              let env,ty2' = make_template env ty2 in
              env, Id.set_typ x ty1
          | _ -> assert false
        in
        let env,t1' =
          let ty' = add_id l_t @@ TFun(x', ty) in
          gen_constr env l_t t1 ty'
        in
        let env,t2' = gen_constr env l_t t2 (Id.typ x') in
        let env = flatten_sub t2'.typ (Id.typ x') env in
        let env =
          let l' = get_id t1'.typ in
          let constr = (l_t,l')::env.constr in
          {env with constr}
        in
        env, {Term.(t1' @ [t2']) with attr=t.attr}
    | If(t1, t2, t3) ->
        let env,l' = new_id env in
        let env,t1' = gen_constr env l_t t1 (add_id l' Ty.bool) in
        let env,t2' = gen_constr env l' t2 ty in
        let env,t3' = gen_constr env l' t3 ty in
        let env =
          env
          |> flatten_sub t2'.typ ty
          |> flatten_sub t3'.typ ty
        in
        let env =
          if not (has_safe_attr t2) || not (has_safe_attr t3) then
            {env with constr=(l',0)::env.constr}
          else
            env
        in
        env, {desc=If(t1', t2', t3'); typ=ty; attr=t.attr}
    | Local(Decl_let bindings, t1) ->
        let tenv = env.tenv in
        let env =
          let aux env (f,_) =
            let env,ty' = make_template env (Id.typ f) in
            {env with tenv = (f,ty')::env.tenv}
          in
          List.fold_left aux env bindings
        in
        let aux (f, t1) (env,bindings) =
          let f_typ = Id.assoc f env.tenv in
          let f' = Id.set_typ f f_typ in
          let env,t1' = gen_constr env l_t t1 f_typ in
          let env = flatten_sub t1'.typ f_typ env in
          env, (f', t1')::bindings
        in
        let env,bindings' = List.fold_right aux bindings (env,[]) in
        let env,t1' = gen_constr env l_t t1 ty in
        let desc = Local(Decl_let bindings', t1') in
        let typ = t1'.typ in
        {env with tenv}, {t with desc; typ}
    | BinOp(op, t1, t2) ->
        let l = get_id ty in
        let typ = add_id l t.typ in
        let env,t1' = gen_constr env l_t t1 (add_id l t1.typ) in
        let env,t2' = gen_constr env l_t t2 (add_id l t2.typ) in
        env, {desc=BinOp(op,t1',t2'); typ; attr=t.attr}
    | Not t1 ->
        let l = get_id ty in
        let typ = add_id l t.typ in
        let env,t1' = gen_constr env l_t t1 (add_id l t1.typ) in
        env, {desc=Not t1'; typ; attr=t.attr}
    | Event(s,false) ->
        let env,typ = make_template env t.typ in
        let env = flatten_sub typ ty env in
        let env = {env with constr=(get_id typ, 0)::env.constr} in
        env, {t with typ}
    | Event(s,true) -> unsupported __MODULE__
    | Proj(i,t1) ->
        let env,ty' = make_template env t1.typ in
        let typ = proj_typ i ty' in
        let env = flatten_sub typ ty env in
        let env,t1' = gen_constr env l_t t1 ty' in
        env, {desc=Proj(i,t1'); typ; attr=t.attr}
    | Tuple ts ->
        let env,ts' =
          let tys = decomp_ttuple ty in
          let l = get_id ty in
          let aux t_i ty_i (env,acc) =
            let env,t_i' = gen_constr env l_t t_i ty_i in
            let env =
              let l' = get_id ty_i in
              let constr = (l,l')::env.constr in
              {env with constr}
            in
            env, t_i'::acc
          in
          List.fold_right2 aux ts tys (env,[])
        in
        env, {desc=Tuple ts'; typ=ty; attr=t.attr}
    | TryWith(t1, t2) ->
        let env,ty2 = make_template env t2.typ in
        let ty_exn, ty2' =
          match elim_tattr ty2 with
          | TFun(x,ty) -> Id.typ x, ty
          | _ ->
              Format.eprintf "ty: %a@." Print.typ ty;
              assert false
        in
        let env,t1' = gen_constr env l_t t1 ty in
        let env,t2' = gen_constr env l_t t2 ty2 in
        let env = flatten_sub ty2' ty env in
        env, {desc=TryWith(t1',t2'); typ=ty; attr=t.attr}
    | Raise t1 ->
        let env,ty' = make_template env t1.typ in
        let env,t1' = gen_constr env 0 t1 ty' in
        let env = {env with constr= (l_t,0)::env.constr} in
        env, {desc=Raise t1'; typ=ty; attr=t.attr}
    | _ ->
        Format.eprintf "%a@." Print.term t;
        assert false
  in
  env, add_attr (AId l_t) t'


(* sol.(x) holds if "id x" is "DONT CARE" *)
(* t is used just for debug *)
let solve env t =
  let n = env.counter + 1 in
  let lower = Array.make n [] in
  let sol = Array.make n true in
  sol.(0) <- false;
  List.iter (fun (x,y) -> lower.(y) <- x::lower.(y)) env.constr;
  let rec solve rest =
    match rest with
    | [] -> ()
    | x::rest' ->
        let low = List.filter ((<>) 0) lower.(x) in
        let low' = List.filter (fun y -> sol.(y)) low in
        Debug.printf "low: %a <: %d@." Print.(list int) low x;
        List.iter (fun y -> sol.(y) <- false) low;
        solve (low'@rest')
  in
  solve [0];
  let ids = col_id t in
  Debug.printf  "DONT CARE:@.";
  Array.iteri (fun i x -> if x then Debug.printf  "  %d%s@." i (if List.mem i ids then "*" else "")) sol;
  fun x ->
    if x < 0 || n < x then invalid_arg "solve";
    sol.(x)



let infer t =
  assert (is_base_typ t.typ);
  let fv = get_fv t in
  let env = initial_env fv in
  let env,ty = make_template env t.typ in
  let env,t' = gen_constr env 0 t ty in
  Debug.printf "Add evar: %a@." Print.term' t';
  Debug.printf "CONSTRAINTS:@.";
  List.iter (fun (e1,e2) -> Debug.printf "  %d <: %d@." e1 e2) env.constr;
  Debug.printf "@.";
  let sol = solve env t' in
  sol, t'

let rec can_remove sol t =
  let ty = t.typ in
  is_base_typ ty &&
  sol (get_id ty) &&
  has_safe_attr t

let slice =
  let tr = make_trans2 () in
  let tr_term sol t =
    if can_remove sol t then
      let () = Debug.printf "REMOVE %d@." (get_id t.typ) in
      let () = Debug.printf "%a@.@." Print.term' t in
      make_term t.typ
    else
      let attr = List.filter (function AId _ -> false | _ -> true) t.attr in
      {(tr.tr2_term_rec sol t) with attr}
  in
  tr.tr2_term <- tr_term;
  tr.tr2_typ <- Fun.snd;
  Fun.uncurry tr.tr2_term

let slice t =
  t
  |> infer
  |@> Debug.printf "INFERRED: %a@." Print.term -| snd
  |@> Debug.printf "INFERRED: %a@." Print.term' -| snd
  |> slice
  |@> Debug.printf "SLICED: %a@." Print.term
  |> Trans.remove_tid label
  |> Trans.reconstruct
