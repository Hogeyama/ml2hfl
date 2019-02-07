open Syntax
open Term_util
open Type
open Util

module Debug = Debug.Make(struct let check = Flag.Debug.make_check __MODULE__ end)

type env =
  {mutable constraints: (effect * effect) list;
   mutable counter: int;
   for_cps: bool}

let initial_env for_cps =
  let counter = 0 in
  let constraints = [ECont, EVar 0] in
  {counter; constraints; for_cps}

let new_evar env =
  env.counter <- env.counter + 1;
  EVar env.counter

let add_effect_typ e ty =
  _TAttr [TAEffect e] ty

let rec make_template env ty =
  let ty', is_attr =
    match ty with
    | TBase _ -> ty, false
    | TVar _ -> unsupported __MODULE__
    | TFun(x, ty2) ->
        let x' = Id.map_typ (make_template env) x in
        let ty2' = make_template env ty2 in
        if env.for_cps then
          match elim_tattr ty2 with
          | TBase _ -> env.constraints <- (ECont, effect_of_typ ty2') :: env.constraints
          | _ -> ()
        else
          ();
        TFun(x', ty2'), false
    | TTuple xs -> TTuple (List.map (Id.map_typ (make_template env)) xs), false
    | TAttr(attr, ty1) -> _TAttr attr (make_template env ty1), true
    | TFuns _ -> unsupported __MODULE__
    | TData _ -> unsupported __MODULE__
    | TVariant _ -> unsupported __MODULE__
    | TRecord _ -> unsupported __MODULE__
    | TApp _ -> unsupported __MODULE__
    | TModule _ -> unsupported __MODULE__
  in
  if is_attr then
    ty'
  else
    add_effect_typ (new_evar env) ty'

let set_effect e t =
  let ty =
    match t.typ with
    | TAttr(TAEffect _::attrs, ty1) -> TAttr(TAEffect e::attrs, ty1)
    | _ -> add_effect_typ e t.typ
  in
  add_attr (AEffect e) {t with typ=ty}

let add_evar env t =
  let typ = make_template env t.typ in
  let e = effect_of_typ typ in
  set_effect e {t with typ}

let rec force_cont ty =
  let attrs,ty1 =
    match ty with
    | TAttr(TAEffect e::attrs, ty1) -> TAEffect ECont::attrs, ty1
    | _ -> assert false
  in
  let ty1' =
    match ty1 with
    | TBase _ -> ty
    | TFun(x, ty2) -> TFun(Id.map_typ force_cont x, force_cont ty2)
    | TTuple xs -> TTuple (List.map (Id.map_typ force_cont) xs)
    | TVar _ -> unsupported __MODULE__
    | TFuns _ -> unsupported __MODULE__
    | TData _ -> unsupported __MODULE__
    | TVariant _ -> unsupported __MODULE__
    | TRecord _ -> unsupported __MODULE__
    | TApp _ -> unsupported __MODULE__
    | TModule _ -> unsupported __MODULE__
    | TAttr _ -> assert false
  in
  TAttr(attrs, ty1')

let rec flatten_sub ?(ignore_top=false) env ty1 ty2 =
  if not ignore_top then
    env.constraints <- (effect_of_typ ty1, effect_of_typ ty2) :: env.constraints;
  match elim_tattr ty1, elim_tattr ty2 with
  | TBase _, _ -> ()
  | TVar _, _ -> unsupported __MODULE__
  | TFun(x1,ty12), TFun(x2,ty22) ->
      flatten_sub ~ignore_top:true env (Id.typ x2) (Id.typ x1);
      flatten_sub env ty12 ty22
  | TFun _, _ ->
      Format.printf "ty1: %a@." Print.typ ty1;
      Format.printf "ty2: %a@." Print.typ ty2;
      assert false
  | TFuns _, _ -> unsupported __MODULE__
  | TTuple xs1, TTuple xs2 ->
      List.iter2 (fun x1 x2 -> flatten_sub env (Id.typ x1) (Id.typ x2)) xs1 xs2
  | TTuple _, _ -> assert false
  | TData _, _ -> unsupported __MODULE__
  | TVariant _, _ -> unsupported __MODULE__
  | TRecord _, _ -> unsupported __MODULE__
  | TApp _, _ -> unsupported __MODULE__
  | TAttr _, _ -> assert false
  | TModule _, _ -> unsupported __MODULE__

let get_tfun_effect ty =
  match elim_tattr ty with
  | TFun(_, ty2) -> effect_of_typ ty2
  | _ -> assert false

let rec gen_constr env tenv t =
  match t.desc with
  | Const (Rand(_, false)) ->
      let t' = add_evar env t in
      let e = get_tfun_effect t'.typ in
      env.constraints <- (ECont, e) :: env.constraints;
      t'
  | Const (Rand(_, true)) -> unsupported __MODULE__
  | Const _ -> add_evar env t
  | Bottom ->
      let t' = add_evar env t in
      let e = effect_of t' in
      env.constraints <- (ECont, e) :: env.constraints;
      t'
  | Var x ->
      let typ =
	try
	  Id.assoc x tenv
	with
	| Not_found when Fpat.RefTypInfer.is_parameter (Id.name x) ->
            add_effect_typ ENone Ty.int
	| Not_found ->
            Format.eprintf "%a@." Print.id x; assert false
      in
      let x' = Id.set_typ x typ in
      set_effect ENone {desc=Var x'; typ; attr=t.attr}
  | Fun(x, t1) ->
      let x_typ = make_template env (Id.typ x) in
      let x' = Id.set_typ x x_typ in
      let tenv' = (x, x_typ) :: tenv in
      let t1' = gen_constr env tenv' t1 in
      let typ = add_effect_typ ENone @@ TFun(x',t1'.typ) in
      set_effect ENone @@ {desc=Fun(x',t1'); typ; attr=t.attr}
  | App(t1, []) -> assert false
  | App(t1, t2::t3::ts) ->
      let typ = (make_app_raw t1 [t2]).typ in
      let t12 = {desc=App(t1,[t2]);typ;attr=[]} in
      let t' = {desc=App(t12, t3::ts); typ=t.typ; attr=[]} in
      gen_constr env tenv t'
  | App(t1, [t2]) ->
      let t1' = gen_constr env tenv t1 in
      let t2' = gen_constr env tenv t2 in
      let ty_arg,e1 =
        match elim_tattr t1'.typ with
        | TFun(x,ty2) -> Id.typ x, effect_of_typ ty2
        | _ -> assert false
      in
      let e = new_evar env in
      env.constraints <- (e1, e) :: env.constraints;
      env.constraints <- (effect_of t1', e) :: env.constraints;
      env.constraints <- (effect_of t2', e) :: env.constraints;
      flatten_sub ~ignore_top:true env t2'.typ ty_arg;
      set_effect e @@ {(make_app_raw t1' [t2']) with attr=t.attr}
  | If(t1, t2, t3) ->
      let t1' = gen_constr env tenv t1 in
      let t2' = gen_constr env tenv t2 in
      let t3' = gen_constr env tenv t3 in
      let typ = make_template env t.typ in
      let e = effect_of_typ typ in
      env.constraints <- (effect_of t1', e) :: env.constraints;
      flatten_sub env t2'.typ typ;
      flatten_sub env t3'.typ typ;
      if env.for_cps then env.constraints <- (ECont, e) :: env.constraints;
      set_effect e @@ {Term.(if_ t1' t2' t3') with typ; attr=t.attr}
  | Local(Decl_let bindings, t1) ->
      let tenv' =
        let make_env (f,_) = f, make_template env (Id.typ f) in
        List.map make_env bindings @@@ tenv
      in
      let e = new_evar env in
      let aux (f, t1) =
        let f_typ = Id.assoc f tenv' in
        let f' = Id.set_typ f f_typ in
        let t1' = gen_constr env tenv' t1 in
        flatten_sub env t1'.typ f_typ;
        env.constraints <- (effect_of t1', e) :: env.constraints;
        f', t1'
      in
      let bindings' = List.map aux bindings in
      let t1' = gen_constr env tenv' t1 in
      env.constraints <- (effect_of t1', e) :: env.constraints;
      let typ = add_effect_typ e @@ elim_tattr t1'.typ in
      set_effect e @@ {(make_let bindings' t1') with typ; attr=t.attr}
  | BinOp(op, t1, t2) ->
      let t1' = gen_constr env tenv t1 in
      let t2' = gen_constr env tenv t2 in
      let typ = make_template env t.typ in
      let e = effect_of_typ typ in
      env.constraints <- (effect_of t1', e) :: env.constraints;
      env.constraints <- (effect_of t2', e) :: env.constraints;
      set_effect e @@ {desc=BinOp(op,t1',t2'); typ; attr=t.attr}
  | Not t1 ->
      let t1' = gen_constr env tenv t1 in
      let typ = make_template env t.typ in
      let e = effect_of_typ typ in
      env.constraints <- (effect_of t1', e) :: env.constraints;
      set_effect e @@ {desc=Not t1'; typ; attr=t.attr}
  | Event(s,true) -> unsupported __MODULE__
  | Event(s,false) ->
      let t' = add_evar env t in
      let e = get_tfun_effect t'.typ in
      env.constraints <- (ECont, e) :: env.constraints;
      t'
  | Proj(i,t1) ->
      let t1' = gen_constr env tenv t1 in
      let typ = make_template env t.typ in
      let e = effect_of_typ typ in
      env.constraints <- (effect_of t1', e) :: env.constraints;
      flatten_sub env (Type.proj_typ i t1'.typ) typ;
      set_effect e @@ {desc=Proj(i,t1'); typ; attr=t.attr}
  | Tuple ts ->
      let ts' = List.map (gen_constr env tenv) ts in
      let ty = make_template env t.typ in
      let e = effect_of_typ ty in
      let ty' = make_ttuple @@ List.map Syntax.typ ts' in
      flatten_sub ~ignore_top:true env ty' ty;
      List.iter (fun t -> env.constraints <- (effect_of t, e) :: env.constraints) ts';
      set_effect e @@ {desc=Tuple ts'; typ=ty; attr=t.attr}
  | TryWith(t1, t2) ->
      let t1' = gen_constr env tenv t1 in
      let t2' = gen_constr env tenv t2 in
      let ty = make_template env t2.typ in
      let ty_exn, ty' =
        match elim_tattr ty with
        | TFun(x,ty') -> Id.typ x, ty'
        | _ ->
            Format.eprintf "ty: %a@." Print.typ ty;
            assert false
      in
      let e = effect_of_typ ty in
      flatten_sub env t1'.typ ty';
      flatten_sub env t2'.typ ty;
      set_effect e @@ {desc=TryWith(t1',t2'); typ=ty'; attr=t.attr}
  | Raise t1 ->
      let t1' = gen_constr env tenv t1 in
      let ty = make_template env t.typ in
      let e = effect_of_typ ty in
      env.constraints <- (EExcep, e) :: env.constraints;
      set_effect e @@ {desc=Raise t1'; typ=ty; attr=t.attr}
  | _ ->
      Format.eprintf "%a@." Print.term t;
      assert false



let rec solve env =
  let n = env.counter + 1 in
  let upper = Array.make n [] in
  let sol = Array.make n (EVar 0) in
  let init (conts,exceps) (x,y) =
    match x,y with
    | EVar i, EVar j -> upper.(i) <- j::upper.(i); conts, exceps
    | ENone, EVar _ -> conts, exceps
    | ECont, EVar i -> i::conts, exceps
    | EExcep, EVar i -> conts, i::exceps
    | _ -> assert false
  in
  let conts,exceps = List.fold_left init ([],[]) env.constraints in
  Debug.printf "conts: %a@." (List.print Format.pp_print_int) conts;
  Debug.printf "exceps: %a@." (List.print Format.pp_print_int) exceps;
  let set c xs = List.iter (fun y -> sol.(y) <- c) xs in
  let rec solve c rest =
    match rest with
    | [] -> ()
    | x::rest' ->
        let up = List.filter (fun y -> match sol.(y) with EVar _ -> true | _ -> false) upper.(x) in
        Debug.printf "up: %a@." (List.print Format.pp_print_int) up;
        set c up;
        solve c (up@rest')
  in
  solve EExcep exceps;
  set EExcep exceps;
  solve ECont conts;
  set ECont conts;
  Array.iteri (fun i x -> Debug.printf  "  e_%d := %a@." i print_effect x) sol;
  fun x ->
    if x < 0 || n < x then invalid_arg "solve";
    match sol.(x) with
    | EVar _ -> ENone
    | r -> r



let apply_sol =
  let app = make_trans2 () in
  let tr_attr sol attrs =
    match attrs with
    | AEffect (EVar x)::attrs' -> AEffect (sol x)::attrs'
    | AEffect e::attrs' -> AEffect e::attrs'
    | _ -> assert false
  in
  let tr_typ sol ty =
    match ty with
    | TAttr(attrs, ty1) ->
        let attrs' = List.map (function TAEffect(EVar x) -> TAEffect(sol x) | a -> a) attrs in
        let ty1' = app.tr2_typ sol ty1 in
        TAttr(attrs', ty1')
    | _ -> app.tr2_typ_rec sol ty
  in
  app.tr2_attr <- tr_attr;
  app.tr2_typ <- tr_typ;
  app.tr2_term



let infer ?(for_cps=false) t =
  let env = initial_env for_cps in
  let ext_funs =
    let eq x y = Id.(x = y) && (can_unify (Id.typ x) (Id.typ y) || Id.typ x = Id.typ y) in
    get_fv ~eq t
  in
  if List.length ext_funs <> List.length (List.unique ~eq:Id.eq ext_funs) then
    begin
      List.iter (fun x -> Format.eprintf "%a: %a@." Id.print x Print.typ (Id.typ x)) ext_funs;
      unsupported "polymorphic use of external functions";
    end;
  let tenv = List.map (Pair.add_right (Id.typ |- make_template env |- force_cont)) ext_funs in
  let t' = gen_constr env tenv t in
  Debug.printf "Add evar: %a@." Print.term' t';
  Debug.printf "CONSTRAINTS:@.";
  List.iter (fun (e1,e2) -> Debug.printf "  %a <: %a@." print_effect e1 print_effect e2) env.constraints;
  Debug.printf "@.";
  let sol = solve env in
  apply_sol sol t'





let mark =
  let tr = make_trans () in
  let rec can_have_pred ty =
    match decomp_tattr ty with
    | attrs, TBase _ -> List.for_all (function TARefPred _ -> false | _ -> true) attrs
    | _, TTuple xs -> List.exists (can_have_pred -| Id.typ) xs
    | _ -> false
  in
  let mark_id x =
    let ty = Id.typ x in
    let ty' =
      if is_base_typ (Id.typ x) then
        _TAttr [TARefPred(Id.new_var_id x, Term.true_)] ty
      else
        ty
    in
    Id.set_typ x ty'
  in
  let tr_typ ty =
    match ty with
    | _ when is_base_typ ty -> ty
    | TTuple xs ->
        let xs' = List.map tr.tr_var xs in
        let xs'' =
          let mark x acc =
            let x' =
              if can_have_pred (TTuple acc) then
                mark_id x
              else
                x
            in
            x'::acc
          in
          List.fold_right mark xs' []
        in
        TTuple xs''
    | TAttr(attr, TFun(x,ty2)) when List.mem TAPureFun attr ->
        let x' = mark_id @@ tr.tr_var x in
        TAttr(attr, TFun(x', tr.tr_typ ty2))
    | TFun(x,ty2) ->
        let ty2' = tr.tr_typ ty2 in
        let x' = tr.tr_var x in
        let x'' =
          if ENone = effect_of_typ ty2 && can_have_pred ty2' then
            mark_id x'
          else
            x'
        in
        TFun(x'', ty2')
    | _ -> tr.tr_typ_rec ty
  in
  tr.tr_typ <- tr_typ;
  tr.tr_term

let mark_safe_fun_arg t =
  t
  |> infer
  |@> Debug.printf "INFERRED: %a@." Print.term'
  |> mark
  |> Trans.remove_effect_attribute
  |> Trans.reconstruct
  |@> Debug.printf "MARKED: %a@." Print.term'
