open Utilities
open CEGAR_syntax
open CEGAR_type
open CEGAR_print

let const_of_bool b = if b then True else False



let apply_body_def f (g,xs,t1,e,t2) = g, xs, t1, e, f t2



let rec subst x t = function
    Const c -> Const c
  | Var y when x = y -> t
  | Var y -> Var y
  | App(t1,t2) -> App(subst x t t1, subst x t t2)
  | Let(y,t1,t2) when x = y -> Let(y, subst x t t1, t2)
  | Let(y,t1,t2) -> Let(y, subst x t t1, subst x t t2)
  | Fun(y,t1) when x = y -> Fun(y,t1)
  | Fun(y,t1) -> Fun(y, subst x t t1)

let rec subst_map map = function
    Const c -> Const c
  | Var x when List.mem_assoc x map -> List.assoc x map
  | Var x -> Var x
  | App(t1,t2) -> App(subst_map map t1, subst_map map t2)
  | Let(x,t1,t2) ->
      let map' = List.filter (fun (y,_) -> x <> y) map in
        Let(x, subst_map map' t1, subst_map map' t2)
  | Fun(x,t1) ->
      let map' = List.filter (fun (y,_) -> x <> y) map in
        Fun(x, subst_map map' t1)

let subst_def x t (f,xs,t1,t2) =
  f, xs, subst x t t1, subst x t t2

let rec subst_typ x t = function
    TBase(b,ps) ->
      (** ASSUME: y does not contain x **)
      let ps' y = List.map (subst x t) (ps y) in
        TBase(b, ps')
  | TFun(typ1,typ2) -> TFun (subst_typ x t typ1, fun y -> subst_typ x t (typ2 y))
  | _ -> assert false



let rec arg_num = function
    TBase _ -> 0
  | TFun(_,typ) -> 1 + arg_num (typ (Const Unit))
  | _ -> assert false




let rec pop_main ((env,defs,main):prog) =
  let compare_fun f g = compare (g = main, f) (f = main, g) in
  let compare_def (f,_,_,_,_) (g,_,_,_,_) = compare_fun f g in
  let compare_env (f,_) (g,_) = compare_fun f g in
  let env' = List.sort compare_env env in
  let defs' = List.sort compare_def defs in
    env', defs', main




let rec put_into_if_term = function
    Const c -> Const c
  | Var x -> Var x
  | App _ as t ->
      let t',ts = decomp_app t in
        if t' = Const If
        then
          match ts with
              t1::t2::t3::ts' ->
                let t1' = put_into_if_term t1 in
                let t2' = put_into_if_term t2 in
                let t3' = put_into_if_term t3 in
                let ts'' = List.map put_into_if_term ts' in
                make_if t1' (put_into_if_term (make_app t2' ts'')) (put_into_if_term (make_app t3' ts''))
            | _ -> assert false
        else
          let ts' = List.map put_into_if_term ts in
            make_app t' ts'
  | Fun(x,t) -> Fun(x, put_into_if_term t)
  | Let(x,t1,t2) -> Let(x, put_into_if_term t1, put_into_if_term t2)
let put_into_if ((env,defs,main):prog) : prog = env, List.map (apply_body_def put_into_if_term) defs, main





let eta_expand_def env (f,xs,t1,e,t2) =
  let d = arg_num (List.assoc f env) - List.length xs in
  let ys = Array.to_list (Array.init d (fun _ -> new_id "x")) in
  let t2' = List.fold_left (fun t x -> App(t, Var x)) t2 ys in
    f, xs@ys, t1, e, t2' (* put_into_term t2' *)

let eta_expand ((env,defs,main) : prog) : prog=
  env, List.map (eta_expand_def env) defs, main


let rec make_arg_let t =
  let desc =
    match t.Syntax.desc with
        Syntax.Unit -> Syntax.Unit
      | Syntax.True -> Syntax.True
      | Syntax.False -> Syntax.False
      | Syntax.Unknown -> assert false
      | Syntax.Int n -> Syntax.Int n
      | Syntax.Var x -> Syntax.Var x
      | Syntax.App(t, ts) ->
          let f = Id.new_var "f" (t.Syntax.typ) in
          let xts = List.map (fun t -> Id.new_var "x" (t.Syntax.typ), t) ts in
          let t' = {Syntax.desc=Syntax.App(Syntax.make_var f, List.map (fun (x,_) -> Syntax.make_var x) xts); Syntax.typ=Type.TUnknown} in
            (List.fold_left (fun t2 (x,t1) -> Syntax.make_let [x,[],t1] t2) t' ((f,t)::xts)).Syntax.desc
      | Syntax.If(t1, t2, t3) ->
          let t1' = make_arg_let t1 in
          let t2' = make_arg_let t2 in
          let t3' = make_arg_let t3 in
            Syntax.If(t1',t2',t3')
      | Syntax.Branch(t1, t2) -> assert false
      | Syntax.Let(flag,bindings,t2) ->
          let bindings' = List.map (fun (f,xs,t) -> f, xs, make_arg_let t) bindings in
          let t2' = make_arg_let t2 in
            Syntax.Let(flag,bindings',t2')
      | Syntax.BinOp(op, t1, t2) ->
          let t1' = make_arg_let t1 in
          let t2' = make_arg_let t2 in
            Syntax.BinOp(op, t1', t2')
      | Syntax.Not t -> Syntax.Not (make_arg_let t)
      | Syntax.Fun(x,t) -> assert false
      | Syntax.Event _ -> assert false
      | _ -> assert false
  in
    {Syntax.desc=desc; Syntax.typ=t.Syntax.typ}



let nil _ = []

let rec trans_typ = function
    Type.TUnit -> TBase(TUnit, nil)
  | Type.TBool -> TBase(TBool, fun x -> [x])
  | Type.TAbsBool -> assert false
  | Type.TInt _ -> TBase(TInt, nil)
  | Type.TRInt _  -> assert false
  | Type.TVar _  -> TBase(TUnit, nil)
  | Type.TFun(x,typ) -> TFun(trans_typ (Id.typ x), fun _ -> trans_typ typ)
  | Type.TList _ -> assert false
  | Type.TConstr("event",_) -> assert false
  | Type.TConstr _ -> assert false
  | Type.TUnknown -> assert false
  | Type.TPair _ -> assert false
  | _ -> assert false

let trans_var x = Id.to_string x

let rec trans_typ' = function
    Type.TUnit -> TBase(TUnit, nil)
  | Type.TBool -> TBase(TBool, nil)
  | Type.TAbsBool -> assert false
  | Type.TInt _ -> assert false
  | Type.TRInt _  -> assert false
  | Type.TVar _  -> assert false
  | Type.TFun({Id.typ=Type.TInt ps} as x,typ) ->
      let x' = trans_var x in
      let typ1 = TBase(TInt, fun z -> List.map (fun p -> subst "v_0" z (snd (trans_term [] [] p))) ps) in
      let typ2 = trans_typ' typ in
        TFun(typ1, fun y -> subst_typ x' y typ2)
  | Type.TFun(x,typ) ->
      let x' = trans_var x in
      let typ1 = trans_typ' (Id.typ x) in
      let typ2 = trans_typ' typ in
        TFun(typ1, fun y -> subst_typ x' y typ2)
  | Type.TList _ -> assert false
  | Type.TConstr("event",_) -> assert false
  | Type.TConstr _ -> assert false
  | Type.TUnknown -> assert false
  | Type.TPair _ -> assert false
  | _ -> assert false

and trans_binop = function
    Syntax.Eq -> assert false
  | Syntax.Lt -> Const Lt
  | Syntax.Gt -> Const Gt
  | Syntax.Leq -> Const Leq
  | Syntax.Geq -> Const Geq
  | Syntax.And -> Const And
  | Syntax.Or -> Const Or
  | Syntax.Add -> Const Add
  | Syntax.Sub -> Const Sub
  | Syntax.Mult -> Const Mul

and trans_term xs env t =
  match t.Syntax.desc with
      Syntax.Unit -> [], Const Unit
    | Syntax.True -> [], Const True
    | Syntax.False -> [], Const False
    | Syntax.Unknown -> assert false
    | Syntax.Int n -> [], Const (Int n)
    | Syntax.NInt _ -> assert false
    | Syntax.App({Syntax.desc=Syntax.RandInt false}, [{Syntax.desc=Syntax.Unit}]) ->
        let k = new_id "k" in
          [k, TFun(typ_int, fun _ -> typ_int), ["n"], Const True, [], Var "n"], App(Const RandInt, Var k)
    | Syntax.App({Syntax.desc=Syntax.RandInt true}, [t1;t2]) ->
        assert (t1 = Syntax.unit_term);
        let defs1,t1' = trans_term xs env t1 in
        let defs2,t2' = trans_term xs env t2 in
          defs1@defs2, App(Const RandInt, t2')
    | Syntax.RandInt _ -> assert false
    | Syntax.Var x ->
        let x' = trans_var x in
          [], Var x'
    | Syntax.App({Syntax.desc=Syntax.Event(s,false)}, [t]) ->
        let k = new_id "k" in
        let defs,t' = trans_term xs env t in
        let xs = diff (get_fv t') (List.map fst env) in
        let defs' = (k, TFun(typ_unit, fun _ -> typ_unit), xs@["u"], Const True, [], t')::defs in
          defs', App(Const (Temp s), make_app (Var k) (List.map (fun x -> Var x) xs))
    | Syntax.App({Syntax.desc=Syntax.Event(s,true)}, [t1;t2]) ->
        assert (t1 = Syntax.unit_term);
        let defs1,t1' = trans_term xs env t1 in
        let defs2,t2' = trans_term xs env t2 in
          defs1@defs2, App(Const (Temp s), App(t2', Const Unit))
    | Syntax.App(t, ts) ->
        let defs,t' = trans_term xs env t in
        let defss,ts' = List.split (List.map (trans_term xs env) ts) in
          defs @ (List.flatten defss), make_app t' ts'
    | Syntax.If(t1, t2, t3) ->
        let defs1,t1' = trans_term xs env t1 in
        let defs2,t2' = trans_term xs env t2 in
        let defs3,t3' = trans_term xs env t3 in
        let f = new_id "f" in
        let x = new_id "b" in
        let typs = typ_bool :: List.map (fun x -> List.assoc x env) xs in
        let typ = List.fold_right (fun typ1 typ2 -> TFun(typ1, fun _ -> typ2)) typs (trans_typ t2.Syntax.typ) in
        let def1 = f, typ, x::xs, Var x, [], t2' in
        let def2 = f, typ, x::xs, make_not (Var x), [], t3' in
        let t = List.fold_left (fun t x -> App(t,Var x)) (App(Var f,t1')) xs in
          def1::def2::defs1@defs2@defs3, t
    | Syntax.Let _ -> assert false
    | Syntax.BinOp(Syntax.Eq, t1, t2) ->
        let defs1,t1' = trans_term xs env t1 in
        let defs2,t2' = trans_term xs env t2 in
        let op =
          match t1.Syntax.typ with
              Type.TUnit -> EqUnit
            | Type.TBool -> EqBool
            | Type.TInt _ -> EqInt
            | _ -> assert false
        in
          defs1@defs2, make_app (Const op) [t1'; t2']
    | Syntax.BinOp(op, t1, t2) ->
        let defs1,t1' = trans_term xs env t1 in
        let defs2,t2' = trans_term xs env t2 in
          defs1@defs2, make_app (trans_binop op) [t1'; t2']
    | Syntax.Not t ->
        let defs,t' = trans_term xs env t in
          defs, App(Const Not, t')
    | Syntax.Fun _ -> assert false
    | Syntax.Event _ -> assert false
    | Syntax.Bottom -> [], Const Bottom
    | _ -> assert false

let rec formula_of t =
  match t.Syntax.desc with
      Syntax.Unit -> Const Unit
    | Syntax.True -> Const True
    | Syntax.False -> Const False
    | Syntax.Unknown -> assert false
    | Syntax.Int n -> Const (Int n)
    | Syntax.NInt _ -> assert false
    | Syntax.RandInt false -> raise Not_found
    | Syntax.Var x ->
        let x' = trans_var x in
          Var x'
    | Syntax.App(t, ts) -> raise Not_found
    | Syntax.If(t1, t2, t3) -> raise Not_found
    | Syntax.Let _ -> assert false
    | Syntax.BinOp(Syntax.Eq, t1, t2) ->
        let t1' = formula_of t1 in
        let t2' = formula_of t2 in
        let op =
          match t1.Syntax.typ with
              Type.TUnit -> EqUnit
            | Type.TBool -> EqBool
            | Type.TInt _ -> EqInt
            | _ -> assert false
        in
          make_app (Const op) [t1'; t2']
    | Syntax.BinOp(op, t1, t2) ->
        let t1' = formula_of t1 in
        let t2' = formula_of t2 in
          App(App(trans_binop op, t1'), t2')
    | Syntax.Not t ->
        let t' = formula_of t in
          App(Const Not, t')
    | Syntax.Fun _
    | Syntax.Event _ -> assert false
    | _ -> assert false

let trans_def (f,(xs,t)) =
  let xs' = List.map trans_var xs in
  let env = List.map2 (fun x' x -> x', trans_typ (Id.typ x)) xs' xs in
    try
      (match t.Syntax.desc with
	   Syntax.If(t1, t2, t3) ->
	     let t1' = formula_of t1 in
	     let defs2,t2' = trans_term xs' env t2 in
	     let defs3,t3' = trans_term xs' env t3 in
	       ((trans_var f, trans_typ (Id.typ f), xs', t1', [], t2')::defs2) @
		 ((trans_var f, trans_typ (Id.typ f), xs', make_not t1', [], t3')::defs3)
	 | _ -> raise Not_found)
    with Not_found ->
      let defs,t' = trans_term xs' env t in
	(trans_var f, trans_typ (Id.typ f), xs', Const True, [], t')::defs

let move_event (f,xs,t1,e,t2) =
  assert (e = []);
  let e',t2' =
    match t2 with
        App(Const (Temp s), t2') -> [Event s], t2'
      | _ -> [], t2
  in
    f, xs, t1, e', t2'

let rec uniq_env = function
    [] -> []
  | (f,typ)::env ->
      if List.exists (fun (g,_) -> f = g) env
      then uniq_env env
      else (f,typ) :: uniq_env env

let trans_prog t =
  let t = Trans.trans_let t in
  let () = if false then Format.printf "trans_let :@.%a\n\n@." (Syntax.print_term true) t in
  let main = new_id "main" in
  let defs,t = Trans.lift t in
  let defs_t,t' = trans_term [] [] t in
  let defs' =
    match !Flag.cegar with
        Flag.CEGAR_SizedType ->
          let typ = TFun(TBase(TUnit,fun _ -> []), fun _ -> TBase(TUnit,fun _ -> [])) in
            (main,typ,["u"],Const True,[],t') :: defs_t @ flatten_map trans_def defs
      | Flag.CEGAR_DependentType ->
          let typ = TBase(TUnit,fun _ -> []) in
            (main,typ,[],Const True,[],t') :: defs_t @ flatten_map trans_def defs
  in
  let env,defs'' = List.split (List.map (fun (f,typ,xs,t1,e,t2) -> (f,typ), (f,xs,t1,e,t2)) defs') in
  let env' = uniq_env env in
  let defs''' = List.map move_event defs'' in
  let prog = pop_main (eta_expand (env', defs''', main)) in

  let () = Id.clear_counter () in
  let map = rev_flatten_map (fun (f,xs,_,_,_) -> (f,rename_id f)::List.map (fun x -> x,rename_id x) xs)  (get_defs prog) in
  let map = uniq' (fun (x,_) (y,_) -> compare x y) map in
  let rename_var x = List.assoc x map in
  let rename_term t = subst_map (List.map (fun (x,x') -> x,Var x') map) t in
  let rename_def (f,xs,t1,e,t2) = rename_var f, List.map rename_var xs, rename_term t1, e, rename_term t2 in
  let env = List.map (fun (f,typ) -> rename_var f, typ) (get_env prog) in
  let defs = List.map rename_def (get_defs prog) in
  let main = rename_var (get_main prog) in
  let prog = env, defs, main in
  let _ = Typing.infer prog in
    if is_CPS prog then Flag.form := Flag.CPS :: !Flag.form;
    prog


let nil = fun _ -> []

exception TypeBottom


let rec get_const_typ = function
  | Unit _ -> TBase(TUnit, nil)
  | True _ -> typ_bool
  | False _ -> typ_bool
  | RandInt _ -> assert false
  | RandBool _ -> TBase(TBool,nil)
  | And -> TFun(typ_bool, fun x -> TFun(typ_bool, fun y -> typ_bool))
  | Or -> TFun(typ_bool, fun x -> TFun(typ_bool, fun y -> typ_bool))
  | Not -> TFun(TBase(TInt,nil), fun x -> typ_bool)
  | Lt -> TFun(TBase(TInt,nil), fun x -> TFun(TBase(TInt,nil), fun y -> typ_bool))
  | Gt -> TFun(TBase(TInt,nil), fun x -> TFun(TBase(TInt,nil), fun y -> typ_bool))
  | Leq -> TFun(TBase(TInt,nil), fun x -> TFun(TBase(TInt,nil), fun y -> typ_bool))
  | Geq -> TFun(TBase(TInt,nil), fun x -> TFun(TBase(TInt,nil), fun y -> typ_bool))
  | EqBool -> TFun(TBase(TBool,nil), fun x -> TFun(TBase(TBool,nil), fun y -> typ_bool))
  | EqInt -> TFun(TBase(TInt,nil), fun x -> TFun(TBase(TInt,nil), fun y -> typ_bool))
  | Int n -> TBase(TInt, fun x -> [make_eq_int x (Const (Int n))])
  | Add -> TFun(TBase(TInt,nil), fun x -> TFun(TBase(TInt,nil), fun y -> TBase(TInt,fun r -> [make_eq_int r (make_add x y)])))
  | Sub -> TFun(TBase(TInt,nil), fun x -> TFun(TBase(TInt,nil), fun y -> TBase(TInt,fun r -> [make_eq_int r (make_sub x y)])))
  | Mul -> TFun(TBase(TInt,nil), fun x -> TFun(TBase(TInt,nil), fun y -> TBase(TInt,fun r -> [make_eq_int r (make_mul x y)])))
  | Tuple _ -> assert false
  | Proj _ -> assert false
  | If _ -> assert false
  | Bottom -> raise TypeBottom
  | _ -> assert false


let rec get_typ env = function
    Const c -> get_const_typ c
  | Var x -> List.assoc x env
  | App(Const RandInt, t) ->
      let typ2 = match get_typ env t with TFun(_,typ) -> typ (Var "") | _ -> assert false in
        typ2
  | App(App(App(Const If, _), t1), t2) ->
      begin
        try
          get_typ env t1
        with TypeBottom -> get_typ env t2
      end
  | App(t1,t2) ->
      let typ2 = match get_typ env t1 with TFun(_,typ) -> typ t2 | _ -> assert false in
        typ2
  | Let(x,t1,t2) ->
      let typ = get_typ env t1 in
      let env' = (x,typ)::env in
        get_typ env' t2
  | Fun(x,t) ->
      let typ1 = List.assoc x env in
      let typ2 = get_typ env t in
        TFun(typ1, fun _ -> typ2)
      



let rec get_arg_num = function
    TFun(_,typ) -> 1 + get_arg_num (typ (Const Unit))
  | typ -> 0


let map_defs f defs =
  let aux (g,xs,t1,t2) =
    let defs1,t1' = f t1 in
    let defs2,t2' = f t2 in
      (g,xs,t1',t2')::defs1@@defs2
  in
    rev_map_flatten aux defs


let rec extract_temp_if = function
    Const If -> assert false
  | Const c -> [], Const c
  | Var x -> [], Var x
  | App(App(App(Const If, t1), t2), t3) ->
      let defs1,t1' = extract_temp_if t1 in
      let defs2,t2' = extract_temp_if t2 in
      let defs3,t3' = extract_temp_if t3 in
      let f = new_id "f" in
      let x = new_id "b" in
      let xs = uniq (get_fv t2 @@ get_fv t3) in
      let def1 = f, x::xs, Var x, t2 in
      let def2 = f, x::xs, make_not (Var x), t3 in
      let defs,t = [def1;def2], App(List.fold_left (fun t x -> App(t,Var x)) (Var f) xs, t1) in
        defs@@defs1@@defs2@@defs3, t
  | App(t1,t2) ->
      let defs1,t1' = extract_temp_if t1 in
      let defs2,t2' = extract_temp_if t2 in
        defs1@@defs2, App(t1',t2')
  | _ -> assert false
let extract_temp_if defs = map_defs extract_temp_if defs



let rec lift_term xs = function
    Const c -> [], Const c
  | Var x -> [], Var x
  | App(t1,t2) ->
      let defs1,t1' = lift_term xs t1 in
      let defs2,t2' = lift_term xs t2 in
        defs1@@defs2, App(t1',t2')
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
let lift_def (f,xs,t1,e,t2):fun_def list =
  let ys,t2' = decomp_fun t2 in
  let xs' = xs@ys in
  let defs1,t1' = lift_term xs t1 in
  let defs2,t2'' = lift_term xs' t2' in
    (f, xs', t1', e, t2'')::defs1@defs2
let lift ((_,defs,main):prog) : prog =
  let defs':fun_def list = rev_flatten_map lift_def defs in
    Typing.infer (([],defs',main):prog)




let rec lift_term2 xs = function
    Const c -> [], Const c
  | Var x -> [], Var x
  | App(t1,t2) ->
      let defs1,t1' = lift_term2 xs t1 in
      let defs2,t2' = lift_term2 xs t2 in
        defs1@@defs2, App(t1',t2')
  | Let(f,t1,t2) ->
      let ys,t1' = decomp_fun t1 in
      let fv = inter xs (diff (get_fv t1) ys) in
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
      let fv = inter xs (diff (get_fv t1) ys) in
      let fv' = List.map (fun x -> if List.mem x xs then rename_id x else x) fv in
      let ys' = fv' @ ys in
      let t1' = List.fold_left2 (fun t x x' -> subst x (Var x') t) t1 fv fv' in
      let f' = rename_id f in
      let f'' = make_app (Var f') (List.map (fun x -> Var x) fv) in
      let defs1,t1'' = lift_term2 ys' t1' in
        (f',ys',Const True,[],t1'') :: defs1, f''


let lift_def2 ((f,xs,t1,e,t2):fun_def) : fun_def list =
  let ys,t2' = decomp_fun t2 in
  let defs1,t1' = lift_term2 xs t1 in
  let defs2,t2'' = lift_term2 xs t2' in
    (f, xs@ys, t1', e, t2'')::defs1@defs2
let lift2 ((_,defs,main):prog) : prog =
  let defs = flatten_map lift_def2 defs in
  let () = if false then Format.printf "LIFTED:\n%a@." CEGAR_print.print_prog ([],defs,main) in
    Typing.infer ([],defs,main)


let rec get_env typ xs =
  match typ,xs with
      TFun(typ1,typ2), x::xs ->
        let typ2 = typ2 (Var x) in
          (x,typ1) :: get_env typ2 xs
    | _ -> []

    









exception EvalBottom

let eval_prog_cbn (env,defs,main) =
  let get_int_value = function
      Const (Int n) -> n
    | Const RandInt -> Random.int 100
    | Const Bottom -> raise EvalBottom
    | _ -> assert false
  in
  let rec step_eval = function
      Const c -> Const c
    | Var x ->
        let defs' = List.filter (fun (f,_,t1,_) -> f = x && eval t1 = Const True) defs in
        let _,_,_,t = List.nth defs' (Random.int (List.length defs')) in
        let () = if List.length defs' > 2 then Format.printf " *** non-deterministic branch ***@." in
          t
    | App(App(Const And, t1), t2) ->
        if eval t1 = Const False
        then Const False
        else eval t2
    | App(App(Const Or, t1), t2) ->
        if eval t1 = Const True
        then Const True
        else eval t2
    | App(Const Not, t) ->
        if eval t = Const True
        then Const False
        else Const True
    | App(App(Const Lt, t1), t2) ->
        let n1 = get_int_value (eval t1) in
        let n2 = get_int_value (eval t2) in
          Const (const_of_bool (n1 < n2))
    | App(App(Const Gt, t1), t2) ->
        let n1 = get_int_value (eval t1) in
        let n2 = get_int_value (eval t2) in
          Const (const_of_bool (n1 > n2))
    | App(App(Const Leq, t1), t2) ->
        let n1 = get_int_value (eval t1) in
        let n2 = get_int_value (eval t2) in
          Const (const_of_bool (n1 <= n2))
    | App(App(Const Geq, t1), t2) ->
        let n1 = get_int_value (eval t1) in
        let n2 = get_int_value (eval t2) in
          Const (const_of_bool (n1 >= n2))
    | App(App(Const EqInt, t1), t2) ->
        let n1 = get_int_value (eval t1) in
        let n2 = get_int_value (eval t2) in
          Const (const_of_bool (n1 > n2))
    | App(App(Const Add, t1), t2) ->
        let n1 = get_int_value (eval t1) in
        let n2 = get_int_value (eval t2) in
          Const (Int (n1 + n2))
    | App(App(Const Sub, t1), t2) ->
        let n1 = get_int_value (eval t1) in
        let n2 = get_int_value (eval t2) in
          Const (Int (n1 - n2))
    | App(App(Const Mul, t1), t2) ->
        let n1 = get_int_value (eval t1) in
        let n2 = get_int_value (eval t2) in
          Const (Int (n1 * n2))
    | App(App(App(Const If, t1), t2), t3) ->
        begin
          match eval t1 with
              Const True -> t2
            | Const False -> t3
            | _ -> assert false
        end
    | App(Fun(x,t1),t2) -> subst x t2 t1
    | App _ as t ->
        let t1,ts = decomp_app t in
        let f = match t1 with Var f -> f | _ -> assert false in
        let aux xs t =
          let n = List.length xs in
          let () = assert (n <= List.length ts) in
          let ts' = take ts n in
            List.fold_right2 subst xs ts' t
        in
        let defs' = List.filter (fun (g,xs,t1,_) -> g = f && eval (aux xs t1) = Const True) defs in
        let _,xs,_,t' = List.nth defs' (Random.int (List.length defs')) in
        let () = if List.length defs' > 2 then Format.printf " *** non-deterministic branch ***@." in
        let _,ts2 = take2 ts (List.length xs) in
          make_app (aux xs t') ts2
    | Let(x,t1,t2) -> subst x t1 t2
    | Fun(x,t) -> Fun(x,t)
  and eval t =
    let t' = step_eval t in
      if t = t'
      then t
      else eval t'
  in
  let rec eval_and_print t =
    let hd,b =
      match t with
          Const c -> Const c, false
        | Var x -> Var x, false
        | App _ -> fst (decomp_app t), true
        | _ -> assert false
    in
      Format.printf "%a%s ->@." print_term hd (if b then " ..." else "");
      let t' =
        try Some (step_eval t) with EvalBottom -> None
      in
      let rec aux = function
          None
        | Some (Const Bottom) -> Format.printf "diverge.\n@."
        | Some t' when t = t' -> Format.printf "terminated.\n@."
        | Some t' -> eval_and_print t'
      in
        aux t'
  in
    eval_and_print (Var main)
    


(*
let eval_prog (env,defs,main) =
  let eval = function
      Const c -> Const c
    | Var x -> assert false
    | App(t1, t2) as t ->
        let t1' = eval t1 in
          if t1 <> t1'
          then App(t1',t2)
          else
            let t2' = eval t2 in
              if t2 <> t2'
              then App(t1',t2')
              else
                let t1,ts = decomp_app t in
                let _,xs,cond,_ = List.find ()
  in
    Format.printf "%a ->@." print_term t;
*)


let rec has_bottom = function
    Var _ -> false
  | Const Bottom -> true
  | Const _ -> false
  | App(t1, t2) -> has_bottom t1 || has_bottom t2
  | _ -> assert false


