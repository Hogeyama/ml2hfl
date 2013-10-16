open Util
open CEGAR_syntax
open CEGAR_type

module S = Syntax
module U = Term_util

let const_of_bool b = if b then True else False



let apply_body_def f (g,xs,t1,e,t2) = g, xs, t1, e, f t2



let rec subst x t = function
    Const c -> Const c
  | Var y when x = y -> t
  | Var y -> Var y
  | App(t1,t2) -> App(subst x t t1, subst x t t2)
  | Let(y,t1,t2) when x = y -> Let(y, subst x t t1, t2)
  | Let(y,t1,t2) -> Let(y, subst x t t1, subst x t t2)
  | Fun(y,typ,t1) when x = y -> Fun(y, typ, t1)
  | Fun(y,typ,t1) -> Fun(y, typ, subst x t t1)

let rec subst_map map = function
    Const c -> Const c
  | Var x when List.mem_assoc x map -> List.assoc x map
  | Var x -> Var x
  | App(t1,t2) -> App(subst_map map t1, subst_map map t2)
  | Let(x,t1,t2) ->
      let map' = List.filter (fun (y,_) -> x <> y) map in
        Let(x, subst_map map' t1, subst_map map' t2)
  | Fun(x, typ,t1) ->
      let map' = List.filter (fun (y,_) -> x <> y) map in
        Fun(x, typ, subst_map map' t1)

let subst_def x t (f,xs,t1,t2) =
  f, xs, subst x t t1, subst x t t2

let rec subst_typ x t = function
    TBase(b,ps) ->
      (** ASSUME: y does not contain x **)
      let ps' y = List.map (subst x t) (ps y) in
        TBase(b, ps')
  | TFun(typ1,typ2) -> TFun(subst_typ x t typ1, fun y -> subst_typ x t (typ2 y))
  | _ -> assert false

let rec subst_typ_map map = function
    TBase(b,ps) ->
      (** ASSUME: y does not contain x **)
      let ps' y = List.map (subst_map map) (ps y) in
        TBase(b, ps')
  | TFun(typ1,typ2) -> TFun(subst_typ_map map typ1, fun y -> subst_typ_map map (typ2 y))
  | _ -> assert false



let rec arg_num = function
    TBase _ -> 0
  | TFun(_,typ) -> 1 + arg_num (typ (Const Unit))
  | _ -> assert false




let rec pop_main {env=env;defs=defs;main=main} =
  let compare_fun f g = compare (g = main, f) (f = main, g) in
  let compare_def (f,_,_,_,_) (g,_,_,_,_) = compare_fun f g in
  let compare_env (f,_) (g,_) = compare_fun f g in
  let env' = List.sort compare_env env in
  let defs' = List.sort compare_def defs in
    {env=env'; defs=defs'; main=main}




let rec get_arg_env typ xs =
  match typ,xs with
      TFun(typ1,typ2), x::xs ->
        let typ2 = typ2 (Var x) in
          (x,typ1) :: get_arg_env typ2 xs
    | _ -> []




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
            | _ -> Format.printf "%a@." CEGAR_print.term t; assert false
        else
          let ts' = List.map put_into_if_term ts in
            make_app t' ts'
  | Fun(x,typ,t) -> Fun(x, typ, put_into_if_term t)
  | Let(x,t1,t2) -> Let(x, put_into_if_term t1, put_into_if_term t2)
let put_into_if prog =
  {prog with defs=List.map (apply_body_def put_into_if_term) prog.defs}





let eta_expand_def env (f,xs,t1,e,t2) =
  let d = arg_num (List.assoc f env) - List.length xs in
  let ys = Array.to_list (Array.init d (fun _ -> new_id "x")) in
  let t2' = List.fold_left (fun t x -> App(t, Var x)) t2 ys in
    f, xs@ys, t1, e, t2' (* put_into_term t2' *)

let eta_expand prog =
  {prog with defs = List.map (eta_expand_def prog.env) prog.defs}


let rec make_arg_let t =
  let desc =
    match t.S.desc with
        S.Const c -> S.Const c
      | S.Unknown -> assert false
      | S.Var x -> S.Var x
      | S.App(t, ts) ->
          let f = Id.new_var "f__" (t.S.typ) in
          let xts = List.map (fun t -> Id.new_var "x" (t.S.typ), t) ts in
          let t' =
            {S.desc=S.App(U.make_var f, List.map (fun (x,_) -> U.make_var x) xts);
             S.typ=Type.typ_unknown}
          in
            (List.fold_left (fun t2 (x,t1) -> U.make_let [x,[],t1] t2) t' ((f,t)::xts)).S.desc
      | S.If(t1, t2, t3) ->
          let t1' = make_arg_let t1 in
          let t2' = make_arg_let t2 in
          let t3' = make_arg_let t3 in
            S.If(t1',t2',t3')
      | S.Branch(t1, t2) -> assert false
      | S.Let(flag,bindings,t2) ->
          let bindings' = List.map (fun (f,xs,t) -> f, xs, make_arg_let t) bindings in
          let t2' = make_arg_let t2 in
            S.Let(flag,bindings',t2')
      | S.BinOp(op, t1, t2) ->
          let t1' = make_arg_let t1 in
          let t2' = make_arg_let t2 in
            S.BinOp(op, t1', t2')
      | S.Not t -> S.Not (make_arg_let t)
      | S.Fun(x,t) -> assert false
      | S.Event _ -> assert false
      | _ -> assert false
  in
    {S.desc=desc; S.typ=t.S.typ}


let nil _ = []

let trans_var x = Id.to_string x
let trans_inv_var s = Id.from_string s Type.typ_unknown

let id_prog prog =
  let map = List.rev_map (fun (f,_) -> f, f) prog.env in
  let rmap = List.map (fun (f,f') -> f', trans_inv_var f) map in
  prog, map, rmap


(* for predicates *)
let rec trans_inv_term = function
    Const True -> U.true_term
  | Const False -> U.false_term
  | Const (Int n) -> U.make_int n
  | Var x -> U.make_var (trans_inv_var x)
  | App(App(Const And, t1), t2) ->
      U.make_and (trans_inv_term t1) (trans_inv_term t2)
  | App(App(Const Or, t1), t2) ->
      U.make_or (trans_inv_term t1) (trans_inv_term t2)
  | App(Const Not, t) ->
      U.make_not (trans_inv_term t)
  | App(App(Const Lt, t1), t2) ->
      U.make_lt (trans_inv_term t1) (trans_inv_term t2)
  | App(App(Const Gt, t1), t2) ->
      U.make_gt (trans_inv_term t1) (trans_inv_term t2)
  | App(App(Const Leq, t1), t2) ->
      U.make_leq (trans_inv_term t1) (trans_inv_term t2)
  | App(App(Const Geq, t1), t2) ->
      U.make_geq (trans_inv_term t1) (trans_inv_term t2)
  | App(App(Const (EqInt|EqBool), t1), t2) ->
      U.make_eq (trans_inv_term t1) (trans_inv_term t2)
  | App(App(Const Add, t1), t2) ->
      U.make_add (trans_inv_term t1) (trans_inv_term t2)
  | App(App(Const Sub, t1), t2) ->
      U.make_sub (trans_inv_term t1) (trans_inv_term t2)
  | App(App(Const Mul, t1), t2) ->
      U.make_mul (trans_inv_term t1) (trans_inv_term t2)
  | t -> Format.printf "%a@." CEGAR_print.term t; assert false



let rec trans_typ = function
    Type.TUnit -> TBase(TUnit, nil)
  | Type.TBool ->
      if !Flag.bool_init_empty
      then TBase(TBool, nil)
      else TBase(TBool, fun x -> [x])
  | Type.TAbsBool -> assert false
  | Type.TInt -> TBase(TInt, nil)
  | Type.TRInt _  -> assert false
  | Type.TVar{contents=None} -> assert false
  | Type.TVar{contents=Some typ} -> trans_typ typ
  | Type.TFun({Id.typ=Type.TBool|Type.TPred({Id.typ=Type.TBool},_)} as x,typ) ->
      let x' = trans_var x in
      let ps' =
        match Id.typ x with
          Type.TPred(y,ps) ->
            fun z ->
              List.map (fun p -> subst (trans_var y) z (snd (trans_term "" [] [] p))) ps
        | _ -> fun _ -> []
      in
      let ps'' =
        if !Flag.bool_init_empty
        then fun z -> ps' z
        else fun z -> z :: ps' z
      in
      let typ1 = TBase(TBool, ps'') in
      let typ2 = trans_typ typ in
        TFun(typ1, fun y -> subst_typ x' y typ2)
  | Type.TFun({Id.typ=Type.TInt|Type.TPred({Id.typ=Type.TInt},_)} as x,typ) ->
      let x' = trans_var x in
      let ps' =
        match Id.typ x with
          Type.TPred(y,ps) ->
            fun z ->
              List.map (fun p -> subst (trans_var y) z (snd (trans_term "" [] [] p))) ps
        | _ -> fun _ -> []
      in
      let typ1 = TBase(TInt, ps') in
      let typ2 = trans_typ typ in
        TFun(typ1, fun y -> subst_typ x' y typ2)
  | Type.TFun(x,typ) ->
      let typ1 = trans_typ (Id.typ x) in
      let typ2 = trans_typ typ in
        TFun(typ1, fun _ -> typ2)
  | Type.TList _ -> assert false
  | Type.TConstr(s, false) -> TBase(TAbst s, nil)
  | Type.TConstr _ -> assert false
  | Type.TPair _ -> assert false
  | Type.TPred(x,ps) -> trans_typ (Id.typ x)


and trans_binop = function
    S.Eq -> assert false
  | S.Lt -> Const Lt
  | S.Gt -> Const Gt
  | S.Leq -> Const Leq
  | S.Geq -> Const Geq
  | S.And -> Const And
  | S.Or -> Const Or
  | S.Add -> Const Add
  | S.Sub -> Const Sub
  | S.Mult -> Const Mul

and trans_const c typ =
  match c with
    S.Unit -> Unit
  | S.True -> True
  | S.False -> False
  | S.Int n -> Int n
  | S.Char c -> Char c
  | S.String s -> String s
  | S.Float s -> Float s
  | S.Int32 n -> Int32 n
  | S.Int64 n -> Int64 n
  | S.Nativeint n -> Nativeint n
  | S.CPS_result -> CPS_result

(** App(Temp e, t) denotes execution of App(t,Unit) after happening the event e *)
and trans_term post xs env t =
  match t.S.desc with
    | S.Const c -> [], Const (trans_const c t.S.typ)
    | S.Unknown -> assert false
    | S.App({S.desc=S.RandInt false}, [{S.desc=S.Const S.Unit}]) ->
        let k = new_id ("k" ^ post) in
          [k, TFun(typ_int, fun _ -> typ_int), ["n"], Const True, [], Var "n"], App(Const RandInt, Var k)
    | S.App({S.desc=S.RandInt true}, [t1;t2]) ->
        assert (t1 = U.unit_term);
        let defs1,t1' = trans_term post xs env t1 in
        let defs2,t2' = trans_term post xs env t2 in
          defs1@defs2, App(Const RandInt, t2')
    | S.RandInt _ -> assert false
    | S.App({S.desc=S.RandValue(Type.TConstr(s,false), true)}, [t1]) ->
        let defs1,t1' = trans_term post xs env t1 in
          defs1, App(t1', Const (RandVal s))
    | S.Var x ->
        let x' = trans_var x in
          [], Var x'
    | S.App({S.desc=S.Event(s,false)}, [t]) ->
        let k = new_id "k" in
        assert (t = U.unit_term);
        let ret_typ = if List.mem Flag.CPS !Flag.form then typ_result else typ_unit in
        let defs = [k, TFun(typ_unit, fun _ -> ret_typ), ["u"], Const True, [], Const CPS_result] in
        defs, App(Const (Temp s), Var k)
    | S.App({S.desc=S.Event(s,true)}, [t1;t2]) ->
        assert (t1 = U.unit_term);
        let defs1,t1' = trans_term post xs env t1 in
        let defs2,t2' = trans_term post xs env t2 in
          defs1@defs2, App(Const (Temp s), t2')
    | S.App(t, ts) ->
        let defs,t' = trans_term post xs env t in
        let defss,ts' = List.split (List.map (trans_term post xs env) ts) in
          defs @ (List.flatten defss), make_app t' ts'
    | S.If(t1, t2, t3) ->
        let defs1,t1' = trans_term post xs env t1 in
        let defs2,t2' = trans_term post xs env t2 in
        let defs3,t3' = trans_term post xs env t3 in
        let f = new_id ("br" ^ post) in
        let x = new_id "b" in
        let typ0 = trans_typ t2.S.typ in
        let aux x typ2 = TFun(List.assoc x env, fun y -> subst_typ x y typ2) in
        let typ = List.fold_right aux xs typ0 in
        let typ' = TFun(typ_bool(), fun _ -> typ) in
        let def1 = f, typ', x::xs, Var x, [], t2' in
        let def2 = f, typ', x::xs, make_not (Var x), [], t3' in
        let t = List.fold_left (fun t x -> App(t,Var x)) (App(Var f,t1')) xs in
          def1::def2::defs1@defs2@defs3, t
    | S.Let _ -> assert false
    | S.BinOp(S.Eq, t1, t2) ->
        let defs1,t1' = trans_term post xs env t1 in
        let defs2,t2' = trans_term post xs env t2 in
        let op =
          match Type.elim_tpred t1.S.typ with
            Type.TUnit -> EqUnit
          | Type.TBool -> EqBool
          | Type.TInt -> EqInt
          | Type.TConstr(typ, false) -> CmpPoly(typ, "=")
          | typ -> Format.printf "trans_term: %a@." S.print_typ typ; assert false
        in
        defs1@defs2, make_app (Const op) [t1'; t2']
    | S.BinOp(op, t1, t2) ->
        let defs1,t1' = trans_term post xs env t1 in
        let defs2,t2' = trans_term post xs env t2 in
        let op' =
          match t1.S.typ with
            Type.TConstr(typ, false) -> Const (CmpPoly(typ, S.string_of_binop op))
          | _ -> trans_binop op
        in
        defs1@defs2, make_app op' [t1'; t2']
    | S.Not t ->
        let defs,t' = trans_term post xs env t in
          defs, App(Const Not, t')
    | S.Fun _ -> assert false
    | S.Event _ -> assert false
    | S.Bottom -> [], Const Bottom
    | _ ->
        Format.printf "%a@." S.pp_print_term t;
        assert false

let rec formula_of t =
  match t.S.desc with
      S.Const c -> Const (trans_const c t.S.typ)
    | S.Unknown -> assert false
    | S.RandInt false -> raise Not_found
    | S.RandInt true -> assert false
    | S.Var x ->
        let x' = trans_var x in
          Var x'
    | S.App(t, ts) -> raise Not_found
    | S.If(t1, t2, t3) -> raise Not_found
    | S.Let _ -> assert false
    | S.BinOp(S.Eq, t1, t2) ->
        let t1' = formula_of t1 in
        let t2' = formula_of t2 in
        let op =
          match Type.elim_tpred t1.S.typ with
              Type.TUnit -> EqUnit
            | Type.TBool -> EqBool
            | Type.TInt -> EqInt
            | Type.TConstr(typ, false) -> CmpPoly(typ, "=")
            | _ -> Format.printf "%a@." S.print_typ t1.S.typ; assert false
        in
          make_app (Const op) [t1'; t2']
    | S.BinOp(op, t1, t2) ->
        let t1' = formula_of t1 in
        let t2' = formula_of t2 in
          App(App(trans_binop op, t1'), t2')
    | S.Not t ->
        let t' = formula_of t in
          App(Const Not, t')
    | S.Fun _ -> assert false
    | S.Event _ -> assert false
    | S.RandValue (_, _) -> assert false
    | S.Branch (_, _) -> assert false
    | S.Record _ -> assert false
    | S.Proj (_, _, _, _) -> assert false
    | S.SetField (_, _, _, _, _, _) -> assert false
    | S.Nil -> assert false
    | S.Cons (_, _) -> assert false
    | S.Constr (_, _) -> assert false
    | S.Match (_, _) -> assert false
    | S.Raise _ -> assert false
    | S.TryWith (_, _) -> assert false
    | S.Pair (_, _) -> assert false
    | S.Fst _ -> assert false
    | S.Snd _ -> assert false
    | S.Bottom -> assert false
    | S.Label (_, _) -> assert false

let trans_def (f,(xs,t)) =
  let f' = trans_var f in
  let post = "_" ^ Id.name f in
  let xs' = List.map trans_var xs in
  let path = ref [] in
  let aux x' x =
    let typ = trans_typ (Id.typ x) in
      path := 1::!path;
      x', typ
  in
  let env = List.map2 aux xs' xs in
    try
      (match t.S.desc with
	   S.If(t1, t2, t3) ->
	     let t1' = formula_of t1 in
	     let defs2,t2' = trans_term post xs' env t2 in
	     let defs3,t3' = trans_term post xs' env t3 in
             let typ' = trans_typ (Id.typ f) in
	       ((f', typ', xs', t1', [], t2')::defs2) @
		 ((f', typ', xs', make_not t1', [], t3')::defs3)
	 | _ -> raise Not_found)
    with Not_found ->
      let defs,t' = trans_term post xs' env t in
      let typ' = trans_typ (Id.typ f) in
	(f', typ', xs', Const True, [], t')::defs


let get_var_arity f env = get_typ_arity (List.assoc f env)

let rec is_CPS_value env = function
    Const _
  | Var _ -> true
  | App(App(Const And, t1), t2)
  | App(App(Const EqUnit, t1), t2)
  | App(App(Const EqInt, t1), t2)
  | App(App(Const EqBool, t1), t2)
  | App(App(Const (CmpPoly _), t1), t2)
  | App(App(Const Or, t1), t2)
  | App(App(Const Lt, t1), t2)
  | App(App(Const Gt, t1), t2)
  | App(App(Const Leq, t1), t2)
  | App(App(Const Geq, t1), t2)
  | App(App(Const Add, t1), t2)
  | App(App(Const Sub, t1), t2)
  | App(App(Const Mul, t1), t2) -> is_CPS_value env t1 && is_CPS_value env t2
  | App(Const Not, t) -> is_CPS_value env t
  | App _ as t ->
      let t1,ts = decomp_app t in
      let n = match t1 with Var f -> get_var_arity f env | _ -> 0 in
        n > List.length ts && List.for_all (is_CPS_value env) ts
  | Let _ -> assert false
  | Fun _ -> assert false
let is_CPS_def env (f,xs,cond,es,t) =
  let env' = get_arg_env (List.assoc f env) xs @@@ env in
  let b1 = is_CPS_value env' cond in
  let b2 =
    match t with
        Const _ -> true
      | Var _ -> true
      | App _ -> List.for_all (is_CPS_value env') (snd (decomp_app t))
      | Let _ -> assert false
      | Fun _ -> assert false
  in
    b1 && b2
let is_CPS {env=env;defs=defs} = List.for_all (is_CPS_def env) defs




let event_of_temp ({env=env;defs=defs;main=main} as _prog) =
  if List.mem Flag.CPS !Flag.form
  then
    let make_event (f,xs,t1,e,t2) =
      assert (e = []);
      match t2 with
          App(Const (Temp s), t2') when t1 = Const True ->
            [], [f, xs, t1, [Event s], App(t2', Const Unit)]
        | App(Const (Temp s), t2') ->
            let g = new_id s in
              [g, TFun(typ_bool(),fun _ -> TFun(TFun(typ_unit, fun _ -> typ_result), fun _ -> typ_result))],
              (* cannot refute if b is eliminated, because k can have no predicates in current impl. *)
              [g, ["b"; "k"], Const True, [Event s], App(Var "k", Const Unit);
               f, xs, t1, [], App(App(Var g, Const True), t2')]
        | _ -> [], [f, xs, t1, [], t2]
    in
    let envs,defss = List.split (List.map make_event defs) in
      {env=List.flatten envs @@@ env; defs=List.flatten defss; main=main}
  else
    let rec aux = function
        Const (Temp e) -> [e]
      | Const c -> []
      | Var x -> []
      | App(t1, t2) -> aux t1 @@@ aux t2
      | Fun _ -> assert false
      | Let _ -> assert false
    in
    let evts = uniq (rev_map_flatten (fun (_,_,_,_,t) -> aux t) defs) in
    let map = List.map (fun e -> e, new_id e) evts in
    let evt_env = List.map (fun (_,f) -> f, TFun(typ_unit, fun _ -> typ_unit)) map in
    let evt_defs = List.map (fun (e,f) -> f,["u"],Const True,[Event e],Const Unit) map in
    let rec aux = function
        Const c -> Const c
      | Var x -> Var x
      | App(Const (Temp e), t) -> App(t, App(Var (List.assoc e map), Const Unit))
      | App(t1, t2) -> App(aux t1, aux t2)
      | Fun _ -> assert false
      | Let _ -> assert false
    in
    let defs' = List.map (apply_body_def aux) defs in
      {env=evt_env@@@env; defs=evt_defs@@@defs'; main=main}


let rec uniq_env = function
    [] -> []
  | (f,typ)::env ->
      if List.exists (fun (g,_) -> f = g) env
      then uniq_env env
      else (f,typ) :: uniq_env env


let rename_prog prog =
  let () = Id.clear_counter () in
  let vars = List.map (fun (f,_,_,_,_) -> f) prog.defs in
  let var_names = List.rev_map id_name (uniq vars) in
  let rename_id' x var_names =
    let x_name = id_name x in
      if List.length (List.filter ((=) x_name) var_names) <= 1 &&
        x_name <> "l0" && x_name <> "l1" (* for labels in model-checking *)
      then x_name
      else rename_id x
  in
  let make_map_fun (f,_) =
    let f' = rename_id' f var_names in
      f, f'
  in
  let map = List.rev_map make_map_fun prog.env in
  let () =
    if !Flag.print_progress
    then
      (List.iter (fun (f,f') -> Format.printf "rename: %s ==> %s@." f f') map;
       Format.printf "@.")
  in
  let var_names' = List.map snd map in
  let rename_var map x = List.assoc x map in
  let rename_def (f,xs,t1,e,t2) =
    let counter = Id.get_counter () in
    let () = Id.clear_counter () in
    let var_names'' = List.rev_map id_name xs @@@ var_names' in
    let arg_map = List.map (fun x -> x, rename_id' x var_names'') xs in
    let arg_map = uniq ~cmp:(fun (x,_) (y,_) -> compare x y) arg_map in
    let smap = List.map (fun (x,x') -> x, Var x') (arg_map @@@ map) in
    let rename_term t = subst_map smap t in
    let def = rename_var map f, List.map (rename_var arg_map) xs, rename_term t1, e, rename_term t2 in
    if Id.get_counter () > counter then Id.save_counter ();
      def
  in
  let env = List.map (fun (f,typ) -> rename_var map f, typ) prog.env in
  let defs = List.map rename_def prog.defs in
  let main = rename_var map prog.main in
  let prog = {env=env; defs=defs; main=main} in
  let () = if false then Format.printf "@.PROG:@.%a@." CEGAR_print.prog_typ prog in
  let is_cps = List.mem Flag.CPS !Flag.form in
  let () = ignore (Typing.infer ~is_cps prog) in
  let rmap = List.map (fun (f,f') -> f', trans_inv_var f) map in
    prog, map, rmap

let id_prog prog =
  let map = List.rev_map (fun (f,_) -> f, f) prog.env in
  let rmap = List.map (fun (f,f') -> f', trans_inv_var f) map in
  prog, map, rmap



module CRT = CEGAR_ref_type
module RT = Ref_type

let rec trans_ref_type = function
    CRT.Base(b,x,p) ->
      let b' =
        match b with
            CRT.Unit -> RT.Unit
          | CRT.Bool -> RT.Bool
          | CRT.Int -> RT.Int
          | CRT.Abst s -> RT.Abst s
      in
        RT.Base(b', trans_inv_var x, trans_inv_term p)
  | CRT.Fun(x,typ1,typ2) ->
      RT.Fun(trans_inv_var x, trans_ref_type typ1, trans_ref_type typ2)
  | CRT.Inter typs ->
      RT.Inter (List.map trans_ref_type typs)


let trans_prog t =
  let debug = false in
  let ext_env = List.map (fun (x,typ) -> trans_var x, trans_typ typ) (Trans.make_ext_env t) in
  let () = if debug then Format.printf "BEFORE:@.%a@.@.@." S.pp_print_term t in
  let t = Trans.trans_let t in
  let () = if debug then Format.printf "AFTER:@.%a@.@.@." S.pp_print_term t in
  let main = new_id "main" in
  let (defs,t_main),get_rtyp = Lift.lift t in
  let defs_t,t_main' = trans_term "" [] [] t_main in
  let defs' =
    match !Flag.cegar with
        Flag.CEGAR_InteractionType ->
          let typ = TFun(typ_unit, fun _ -> typ_unit) in
            (main,typ,["u"],Const True,[],t_main') :: defs_t @ flatten_map trans_def defs
      | Flag.CEGAR_DependentType ->
          let typ = if List.mem Flag.CPS !Flag.form then typ_result else typ_unit in
            (main,typ,[],Const True,[],t_main') :: defs_t @ flatten_map trans_def defs
  in
  let env,defs'' = List.split (List.map (fun (f,typ,xs,t1,e,t2) -> (f,typ), (f,xs,t1,e,t2)) defs') in
  let env' = uniq_env (ext_env @@@ env) in
  let prog = {env=env'; defs=defs''; main=main} in
  let () = if debug then Format.printf "@.PROG:@.%a@." CEGAR_print.prog_typ prog in
  let prog = event_of_temp prog in
  let () = if debug then Format.printf "@.PROG:@.%a@." CEGAR_print.prog_typ prog in
  let prog = eta_expand prog in
  let () = if debug then Format.printf "@.PROG:@.%a@." CEGAR_print.prog_typ prog in
  let prog = pop_main prog in
  let () = if debug then Format.printf "@.PROG:@.%a@." CEGAR_print.prog_typ prog in
  let prog,map,rmap = id_prog prog in
  let get_rtyp f typ = get_rtyp f (trans_ref_type typ) in
    prog,map,rmap,get_rtyp


let nil = fun _ -> []

exception TypeBottom


let rec get_const_typ = function
  | Unit -> typ_unit
  | True -> typ_bool()
  | False -> typ_bool()
  | Char _ -> typ_abst "char"
  | String _ -> typ_abst "string"
  | Float _ -> typ_abst "float"
  | Int32 _ -> typ_abst "int32"
  | Int64 _ -> typ_abst "int64"
  | Nativeint _ -> typ_abst "nativeint"
  | RandInt -> TFun(TFun(TBase(TInt,nil), fun x -> typ_unit), fun x -> typ_unit)
  | RandBool -> TBase(TBool,nil)
  | RandVal s -> TBase(TAbst s,nil)
  | And -> TFun(typ_bool(), fun x -> TFun(typ_bool(), fun y -> typ_bool()))
  | Or -> TFun(typ_bool(), fun x -> TFun(typ_bool(), fun y -> typ_bool()))
  | Not -> TFun(TBase(TInt,nil), fun x -> typ_bool())
  | Lt -> TFun(TBase(TInt,nil), fun x -> TFun(TBase(TInt,nil), fun y -> typ_bool()))
  | Gt -> TFun(TBase(TInt,nil), fun x -> TFun(TBase(TInt,nil), fun y -> typ_bool()))
  | Leq -> TFun(TBase(TInt,nil), fun x -> TFun(TBase(TInt,nil), fun y -> typ_bool()))
  | Geq -> TFun(TBase(TInt,nil), fun x -> TFun(TBase(TInt,nil), fun y -> typ_bool()))
  | EqUnit -> TFun(TBase(TUnit,nil), fun x -> TFun(TBase(TUnit,nil), fun y -> typ_bool()))
  | EqBool -> TFun(TBase(TBool,nil), fun x -> TFun(TBase(TBool,nil), fun y -> typ_bool()))
  | EqInt -> TFun(TBase(TInt,nil), fun x -> TFun(TBase(TInt,nil), fun y -> typ_bool()))
  | CmpPoly(typ,_) -> TFun(TBase(TAbst typ,nil), fun x -> TFun(TBase(TAbst typ,nil), fun y -> typ_bool()))
  | Int n -> TBase(TInt, fun x -> [make_eq_int x (Const (Int n))])
  | Add -> TFun(TBase(TInt,nil), fun x ->
                TFun(TBase(TInt,nil), fun y ->
                     TBase(TInt,fun r -> [make_eq_int r (make_add x y)])))
  | Sub -> TFun(TBase(TInt,nil), fun x ->
                TFun(TBase(TInt,nil), fun y ->
                     TBase(TInt,fun r -> [make_eq_int r (make_sub x y)])))
  | Mul -> TFun(TBase(TInt,nil), fun x ->
                TFun(TBase(TInt,nil), fun y ->
                     TBase(TInt,fun r -> [make_eq_int r (make_mul x y)])))
  | Tuple _ -> assert false
  | Proj _ -> assert false
  | If -> assert false
  | Bottom -> raise TypeBottom
  | Label _ -> assert false
  | Temp _ -> assert false
  | CPS_result -> typ_result



let rec get_typ env = function
    Const c -> get_const_typ c
  | Var x -> List.assoc x env
  | App(Const (Label _), t) ->
      get_typ env t
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
  | Fun(x,_,t) -> assert false
(*
      let typ1 = List.assoc x env in
      let typ2 = get_typ env t in
        TFun(typ1, fun _ -> typ2)
*)




let rec get_arg_num = function
    TFun(_,typ) -> 1 + get_arg_num (typ (Const Unit))
  | typ -> 0


let map_defs f defs =
  let aux (g,xs,t1,t2) =
    let defs1,t1' = f t1 in
    let defs2,t2' = f t2 in
      (g,xs,t1',t2')::defs1@@@defs2
  in
    rev_map_flatten aux defs










let rec has_bottom = function
    Var _ -> false
  | Const Bottom -> true
  | Const _ -> false
  | App(t1, t2) -> has_bottom t1 || has_bottom t2
  | _ -> assert false




let rec normalize_bool_term ?(imply = fun _ _ -> false) = function
    Const c -> Const c
  | Var x -> Var x
  | App(Const Not, App(Const Not, t1)) -> normalize_bool_term ~imply t1
  | App(Const Not, App(App(Const (Lt|Gt|Leq|Geq as op), t1), t2)) ->
      let op' =
        match op with
            Lt -> Geq
          | Gt -> Leq
          | Leq -> Gt
          | Geq -> Lt
          | _ -> assert false
      in
        normalize_bool_term ~imply (App(App(Const op', t1), t2))
  | App(App(Const EqBool, Const True), t) -> normalize_bool_term ~imply t
  | App(App(Const And, _), _) as t ->
      let rec decomp = function
          App(App(Const And, t1), t2) -> decomp t1 @@@ decomp t2
        | t -> [normalize_bool_term ~imply t]
      in
      let rec aux ts1 = function
          [] -> List.rev ts1
        | t::ts2 ->
            if imply (ts1@@@ts2) t
            then aux ts1 ts2
            else aux (t::ts1) ts2
      in
      let ts' = aux [] (decomp t) in
      begin
        match ts' with
            [] -> Const True
          | t'::ts'' -> List.fold_left make_and t' ts''
      end
  | App(App(Const Or, _), _) as t ->
      let rec decomp = function
          App(App(Const Or, t1), t2) -> decomp t1 @@@ decomp t2
        | t -> [normalize_bool_term ~imply t]
      in
      let rec aux ts1 = function
          [] -> ts1
        | t::ts2 ->
            if imply (ts1@@@ts2) (make_not t)
            then aux ts1 ts2
            else aux (t::ts1) ts2
      in
      let ts' = aux [] (decomp t) in
      begin
        match ts' with
            [] -> Const False
          | t'::ts'' -> List.fold_left make_or t' ts''
      end
  | App(App(Const (EqInt|Lt|Gt|Leq|Geq) as op, t1), t2) ->
      let neg xs = List.map (fun (x,n) -> x,-n) xs in
      let rec decomp = function
          Const (Int n) -> [None, n]
        | Var x -> [Some (Var x), 1]
        | App(App(Const Add, t1), t2) ->
            decomp t1 @@@ decomp t2
        | App(App(Const Sub, t1), t2) ->
            decomp t1 @@@ neg (decomp t2)
        | App(App(Const Mul, t1), t2) ->
            let xns1 = decomp t1 in
            let xns2 = decomp t2 in
            let reduce xns = List.fold_left (fun acc (_,n) -> acc+n) 0 xns in
            let not_const xns = List.exists (fun (x,_) -> x <> None) xns in
              begin
                match not_const xns1, not_const xns2 with
                    true, true ->
                      Format.printf "Nonlinear expression not supported: %a@."
                        CEGAR_print.term (make_app op [t1;t2]);
                      assert false
                  | false, true ->
                      let k = reduce xns1 in
                        List.rev_map (fun (x,n) -> x,n*k) xns2
                  | true, false ->
                      let k = reduce xns2 in
                        List.rev_map (fun (x,n) -> x,n*k) xns1
                  | false, false ->
                      [None, reduce xns1 + reduce xns2]
              end
        | _ -> assert false
      in
      let xns1 = decomp t1 in
      let xns2 = decomp t2 in
      let compare (x1,_) (x2,_) =
        let aux = function
            None -> true, ""
          | Some (Var x) -> false, x
          | _ -> assert false
        in
          compare (aux x1) (aux x2)
      in
      let xns = List.sort compare (xns1 @@@ (neg xns2)) in
      let d = List.fold_left (fun d (_,n) -> gcd d (abs n)) 0 xns in
      let xns' = List.map (fun (x,n) -> assert (n mod d = 0); x, n/d) xns in
      let rec aux = function
          [] -> []
        | (x,n)::xns ->
            let xns1,xns2 = List.partition (fun (y,_) -> x=y) xns in
            let n' = List.fold_left (fun acc (_,n) -> acc+n) 0 ((x,n)::xns1) in
              (x,n') :: aux xns2
      in
      let xns'' = aux xns' in
      let xns''' = List.filter (fun (_,n) -> n<>0) xns'' in
      let x,n = List.hd xns''' in
      let xns = List.tl xns''' in
      let op',t1',t2' =
        let aux = function
            None, n -> Const (Int n)
          | Some x, 1 -> x
          | Some x, n -> make_mul (make_int n) x
        in
        let t1,xns',op' =
          if n<0
          then
            let op' =
              match op with
                  Const EqInt -> Const EqInt
                | Const Lt -> Const Gt
                | Const Gt -> Const Lt
                | Const Leq -> Const Geq
                | Const Geq -> Const Leq
                | _ -> assert false
            in
              aux (x,-n), xns, op'
          else
            aux (x,n), neg xns, op
        in
        let ts = List.map aux xns' in
        let make_add_sub t1 t2 =
          match t2 with
              Const (Int n) when n < 0 -> make_sub t1 (make_int (-n))
            | App(App(Const Mul, Const (Int n)), t2') when n < 0 -> make_sub t1 (make_mul (make_int (-n)) t2')
            | _ -> make_add t1 t2
        in
        let t2 =
          match ts with
              [] -> Const (Int 0)
            | t::ts' -> List.fold_left make_add_sub t ts'
        in
          op', t1, t2
      in
        make_app op' [t1'; t2']
  | App(t1, t2) -> App(normalize_bool_term ~imply t1, normalize_bool_term ~imply t2)
  | Let _ -> assert false
  | Fun _ -> assert false





let assoc_fun_def defs f =
  let make_fun xs t =
    let xs' = List.map rename_id xs in
    let map = List.map2 (fun x x' -> x, Var x') xs xs' in
      List.fold_right (fun x t -> Fun(x,None,t)) xs' (subst_map map t)
  in
  let defs' = List.filter (fun (g,_,_,_,_) -> f = g) defs in
    match defs' with
        [_,xs,Const True,_,t] -> make_fun xs t
      | [_] -> raise (Fatal "Not implemented: CEGAR_abst_CPS.assoc_fun_def")
      | [_,xs1,t11,_,t12; _,xs2,t21,_,t22] when make_not t11 = t21 ->
          assert (xs1 = xs2);
          make_fun xs1 (make_if t11 t12 t22)
      | [_,xs1,t11,_,t12; _,xs2,t21,_,t22] when t11 = make_not t21 ->
          assert (xs1 = xs2);
          make_fun xs1 (make_if t21 t22 t12)
      | _ -> assert false

let get_nonrec defs main orig_fun_list force =
  let check (f,xs,t1,e,t2) =
    let defs' = List.filter (fun (g,_,_,_,_) -> f = g) defs in
    let used = List.filter (fun (_,_,t1,_,t2) -> List.mem f (get_fv t1 @@@ get_fv t2)) defs in
      List.for_all (fun (_,_,_,e,_) -> e = []) defs' &&
        f <> main &&
        (List.for_all (fun (_,xs,t1,e,t2) -> subset (get_fv t1 @@@ get_fv t2) xs) defs' ||
         (1 >= List.length (uniq (List.map (fun (f,_,_,_,_) -> f) used)) || List.mem f force) &&
         2 >= List.length defs')
  in
  let defs' = List.filter check defs in
  let nonrec = List.map (fun (f,xs,_,_,t) -> f, assoc_fun_def defs f) defs' in
    if !Flag.expand_nonrec_init
    then nonrec
    else
      let orig_fun_list' = diff orig_fun_list force in
        List.filter (fun (f,_) -> not (List.mem f orig_fun_list')) nonrec


let print_prog_typ' orig_fun_list force fm {env=env;defs=defs;main=main} =
  let nonrec = get_nonrec defs main orig_fun_list force in
  let env' = List.filter (fun (f,_) -> not (List.mem_assoc f nonrec)) env in
    CEGAR_print.prog_typ fm {env=env';defs=defs;main=main}
