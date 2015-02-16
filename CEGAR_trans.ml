open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_print
open CEGAR_util

exception EvalFail
exception EvalValue
exception EvalSkip
exception EvalRestart
exception EvalTerminate

let debug () = List.mem "CEGAR_trans" !Flag.debug_module

let new_id' x = new_id (Format.sprintf "%s_%d" x !Flag.cegar_loop)

let rec merge_typ env typ typ' =
  match typ,typ' with
  | TBase(b1,ps1),TBase(b2,ps2) when b1 = b2 ->
      let x = new_id' "x" in
      let env' = (x,typ)::env in
      let ps1' = ps1 (Var x) in
      let ps2' = ps2 (Var x) in
      let equiv env t1 t2 =
        let t1' = FpatInterface.conv_formula t1 in
        let t2' = FpatInterface.conv_formula t2 in
        FpatInterface.implies [t1'] [t2'] &&
        FpatInterface.implies [t2'] [t1']
      in
      let add env ps p =
        if List.exists (equiv env p) ps
        then ps
        else normalize_bool_term p :: ps
      in
      let ps = List.fold_left (add env') ps1' ps2' in
      let ps t = List.map (subst x t) ps in
      TBase(b1, ps)
  | TFun(typ11,typ12), TFun(typ21,typ22) ->
      let x = new_id' "x" in
      let env' = (x,typ11)::env in
      let typ12 = typ12 (Var x) in
      let typ22 = typ22 (Var x) in
      let typ1 = merge_typ env typ11 typ21 in
      let typ2 = merge_typ env' typ12 typ22 in
      TFun(typ1, fun t -> subst_typ x t typ2)
  | TBase _, _
  | TFun _, _
  | TAbs _, _
  | TApp _, _ -> Format.printf "merge_typ: %a,%a@." CEGAR_print.typ typ CEGAR_print.typ typ'; assert false

let merge_typ typ1 typ2 =
  try
    merge_typ [] typ1 typ2
  with _ -> (Format.printf "Cannot merge@.  TYPE 1: %a@.  TYPE 2: %a@." CEGAR_print.typ typ1 CEGAR_print.typ typ2; assert false)


let nil_pred _ = []

let trans_var x = Id.to_string x
let trans_inv_var s = Id.from_string s Type.typ_unknown

let id_prog prog =
  let map = List.rev_map (fun (f,_) -> f, f) prog.env in
  let rmap = List.map (Pair.map_snd trans_inv_var) map in
  prog, map, rmap

(* for predicates *)
let rec trans_inv_term = function
    Const True -> Term_util.true_term
  | Const False -> Term_util.false_term
  | Const (Int n) -> Term_util.make_int n
  | Var x -> Term_util.make_var (trans_inv_var x)
  | App(App(Const And, t1), t2) ->
      Term_util.make_and (trans_inv_term t1) (trans_inv_term t2)
  | App(App(Const Or, t1), t2) ->
      Term_util.make_or (trans_inv_term t1) (trans_inv_term t2)
  | App(Const Not, t) ->
      Term_util.make_not (trans_inv_term t)
  | App(App(Const Lt, t1), t2) ->
      Term_util.make_lt (trans_inv_term t1) (trans_inv_term t2)
  | App(App(Const Gt, t1), t2) ->
      Term_util.make_gt (trans_inv_term t1) (trans_inv_term t2)
  | App(App(Const Leq, t1), t2) ->
      Term_util.make_leq (trans_inv_term t1) (trans_inv_term t2)
  | App(App(Const Geq, t1), t2) ->
      Term_util.make_geq (trans_inv_term t1) (trans_inv_term t2)
  | App(App(Const (EqInt|EqBool), t1), t2) ->
      Term_util.make_eq (trans_inv_term t1) (trans_inv_term t2)
  | App(App(Const Add, t1), t2) ->
      Term_util.make_add (trans_inv_term t1) (trans_inv_term t2)
  | App(App(Const Sub, t1), t2) ->
      Term_util.make_sub (trans_inv_term t1) (trans_inv_term t2)
  | App(App(Const Mul, t1), t2) ->
      Term_util.make_mul (trans_inv_term t1) (trans_inv_term t2)
  | t -> Format.printf "%a@." CEGAR_print.term t; assert false



let rec trans_typ = function
  | Type.TUnit -> typ_unit
  | Type.TBool -> typ_bool ()
  | Type.TInt -> typ_int
  | Type.TVar{contents=None} -> typ_int
  | Type.TVar{contents=Some typ} -> trans_typ typ
  | Type.TFun({Id.typ=Type.TBool|Type.TPred({Id.typ=Type.TBool},_)} as x,typ) ->
      let x' = trans_var x in
      let ps' =
        match Id.typ x with
        | Type.TPred(y,ps) ->
            fun z -> List.map (fun p -> subst (trans_var y) z (snd (trans_term "" [] [] p))) ps
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
        | Type.TPred(y,ps) ->
            let y' = trans_var y in
            let ps' = List.map (snd -| trans_term "" [] []) ps in
            fun z -> List.map (subst y' z) ps'
        | _ -> fun _ -> []
      in
      let typ1 = TBase(TInt, ps') in
      let typ2 = trans_typ typ in
      TFun(typ1, fun y -> subst_typ x' y typ2)
  | Type.TFun(x,typ) ->
      let typ1 = trans_typ @@ Id.typ x in
      let typ2 = trans_typ typ in
      TFun(typ1, fun _ -> typ2)
  | Type.TConstr(s, false) -> TBase(TAbst s, nil_pred)
  | Type.TPred(x,ps) ->
      begin
        let x' = trans_var x in
        let ps' = List.map (snd -| trans_term "" [] []) ps in
        match trans_typ @@ Id.typ x with
        | TBase(b, preds) ->
            let preds' y = List.map (subst x' y) ps' @ preds y in
            TBase(b, preds')
        | typ -> Format.printf "trans_typ[TPred]: %a@." CEGAR_print.typ typ; assert false
      end
  | Type.TTuple xs -> make_ttuple @@ List.map (trans_typ -| Id.typ) xs
  | typ -> Format.printf "trans_typ: %a@." Print.typ typ; assert false


and trans_binop = function
  | Syntax.Eq -> assert false
  | Syntax.Lt -> Const Lt
  | Syntax.Gt -> Const Gt
  | Syntax.Leq -> Const Leq
  | Syntax.Geq -> Const Geq
  | Syntax.And -> Const And
  | Syntax.Or -> Const Or
  | Syntax.Add -> Const Add
  | Syntax.Sub -> Const Sub
  | Syntax.Mult -> Const Mul

and trans_const c typ =
  match c with
  | Syntax.Unit -> Unit
  | Syntax.True -> True
  | Syntax.False -> False
  | Syntax.Int n -> Int n
  | Syntax.Char c -> Char c
  | Syntax.String s -> String s
  | Syntax.Float s -> Float s
  | Syntax.Int32 n -> Int32 n
  | Syntax.Int64 n -> Int64 n
  | Syntax.Nativeint n -> Nativeint n
  | Syntax.CPS_result -> CPS_result

(** App(Temp e, t) denotes execution of App(t,Unit) after happening the event e *)
and trans_term post xs env t =
  match t.Syntax.desc with
  | Syntax.Const(Syntax.RandInt _) -> assert false
  | Syntax.Const c -> [], Const (trans_const c t.Syntax.typ)
  | Syntax.App({Syntax.desc=Syntax.Const(Syntax.RandInt false); Syntax.attr}, [{Syntax.desc=Syntax.Const Syntax.Unit}]) ->
      let k = new_id ("k" ^ post) in
      [k, TFun(typ_int, fun _ -> typ_int), ["n"], Const True, [], Var "n"], App(Const (RandInt None), Var k)
  | Syntax.App({Syntax.desc=Syntax.Const(Syntax.RandInt true); Syntax.attr}, [t1;t2]) ->
      assert (t1 = Term_util.unit_term);
      let k = new_id ("k" ^ post) in
      let defs1,t1' = trans_term post xs env t1 in
      let defs2,t2' = trans_term post xs env t2 in
      defs1@defs2, App(Const (RandInt None), t2')
  | Syntax.App({Syntax.desc=Syntax.Const(Syntax.RandValue(Type.TInt, true)); Syntax.attr}, [t1;t2]) ->
      assert (t1 = Term_util.unit_term);
      let defs1,t1' = trans_term post xs env t1 in
      let defs2,t2' = trans_term post xs env t2 in
      defs1@defs2, App(Const (RandInt None), t2')
  | Syntax.App({Syntax.desc=Syntax.Const(Syntax.RandValue(Type.TConstr(s,false), true))}, [t1]) ->
      let defs1,t1' = trans_term post xs env t1 in
      defs1, App(t1', Const (RandVal s))
  | Syntax.Var x ->
      let x' = trans_var x in
      [], Var x'
  | Syntax.App({Syntax.desc=Syntax.Event(s,false)}, [t]) ->
      let k = new_id "k" in
      assert (t = Term_util.unit_term);
      let defs = [k, TFun(typ_unit, fun _ -> typ_unit), ["u"], Const True, [], Const Unit] in
      defs, App(Const (Temp s), Var k)
  | Syntax.App({Syntax.desc=Syntax.Event(s,true)}, [t1;t2]) ->
      assert (t1 = Term_util.unit_term);
      let defs1,t1' = trans_term post xs env t1 in
      let defs2,t2' = trans_term post xs env t2 in
      defs1@defs2, App(Const (Temp s), t2')
  | Syntax.App(t, ts) ->
      let defs,t' = trans_term post xs env t in
      let defss,ts' = List.split_map (trans_term post xs env) ts in
      defs @ (List.flatten defss), make_app t' ts'
  | Syntax.If(t1, t2, t3) ->
      let defs1,t1' = trans_term post xs env t1 in
      let defs2,t2' = trans_term post xs env t2 in
      let defs3,t3' = trans_term post xs env t3 in
      let f = new_id ("br" ^ post) in
      let x = new_id "b" in
      let typ0 = trans_typ t2.Syntax.typ in
      let aux x typ2 = TFun(List.assoc x env, fun y -> subst_typ x y typ2) in
      let typ = List.fold_right aux xs typ0 in
      let typ' = TFun(typ_bool(), fun _ -> typ) in
      let def1 = f, typ', x::xs, Var x, [], t2' in
      let def2 = f, typ', x::xs, make_not (Var x), [], t3' in
      let t = List.fold_left (fun t x -> App(t,Var x)) (App(Var f,t1')) xs in
      def1::def2::defs1@defs2@defs3, t
  | Syntax.Let _ -> assert false
  | Syntax.BinOp(Syntax.Eq, t1, t2) ->
      let defs1,t1' = trans_term post xs env t1 in
      let defs2,t2' = trans_term post xs env t2 in
      let op =
        match Type.elim_tpred t1.Syntax.typ with
        | Type.TUnit -> EqUnit
        | Type.TBool -> EqBool
        | Type.TInt -> EqInt
        | Type.TConstr(typ, false) -> CmpPoly(typ, "=")
        | typ -> Format.printf "trans_term: %a@." Print.typ typ; assert false
      in
      defs1@defs2, make_app (Const op) [t1'; t2']
  | Syntax.BinOp(op, t1, t2) ->
      let defs1,t1' = trans_term post xs env t1 in
      let defs2,t2' = trans_term post xs env t2 in
      let op' =
        match t1.Syntax.typ with
        | Type.TConstr(typ, false) -> Const (CmpPoly(typ, Print.string_of_binop op))
        | _ -> trans_binop op
      in
      defs1@defs2, make_app op' [t1'; t2']
  | Syntax.Not t ->
      let defs,t' = trans_term post xs env t in
      defs, App(Const Not, t')
  | Syntax.Fun _ -> assert false
  | Syntax.Event _ -> assert false
  | Syntax.Bottom -> [], Const Bottom
  | Syntax.Proj(i, t) ->
      let defs,t' = trans_term post xs env t in
      defs, make_proj (Option.get @@ Type.tuple_num t.typ) i t'
  | Syntax.Tuple ts ->
      let defss,ts' = List.split_map (trans_term post xs env) ts in
      List.flatten defss, make_tuple ts'
  | _ ->
      Format.printf "%a@." Print.term t;
      assert false

let rec formula_of t =
  match t.Syntax.desc with
  | Syntax.Const(Syntax.RandInt false) -> raise Not_found
  | Syntax.Const(Syntax.RandInt true) -> assert false
  | Syntax.Const c -> Const (trans_const c t.Syntax.typ)
  | Syntax.Var x -> Var (trans_var x)
  | Syntax.App(t, ts) -> raise Not_found
  | Syntax.If(t1, t2, t3) -> raise Not_found
  | Syntax.Let _ -> assert false
  | Syntax.BinOp(Syntax.Eq, t1, t2) ->
      let t1' = formula_of t1 in
      let t2' = formula_of t2 in
      let op =
        match Type.elim_tpred t1.Syntax.typ with
        | Type.TUnit -> EqUnit
        | Type.TBool -> EqBool
        | Type.TInt -> EqInt
        | Type.TConstr(typ, false) -> CmpPoly(typ, "=")
        | _ -> Format.printf "%a@." Print.typ t1.Syntax.typ; assert false
      in
      make_app (Const op) [t1'; t2']
  | Syntax.BinOp(op, t1, t2) ->
      let t1' = formula_of t1 in
      let t2' = formula_of t2 in
      App(App(trans_binop op, t1'), t2')
  | Syntax.Not t ->
      let t' = formula_of t in
      App(Const Not, t')
  | Syntax.Proj _ -> raise Not_found
  | Syntax.Tuple _ -> raise Not_found
  | _ -> Format.printf "formula_of: %a@." Print.constr t; assert false

let trans_def (f,(xs,t)) =
  let f' = trans_var f in
  let post = "_" ^ Id.name f in
  let xs' = List.map trans_var xs in
  let path = ref [] in
  let aux x' x =
    let typ = trans_typ @@ Id.typ x in
    path := 1::!path;
    x', typ
  in
  let env = List.map2 aux xs' xs in
  try
    (match t.Syntax.desc with
     | Syntax.If(t1, t2, t3) ->
	 let t1' = formula_of t1 in
	 let defs2,t2' = trans_term post xs' env t2 in
	 let defs3,t3' = trans_term post xs' env t3 in
         let typ' = trans_typ @@ Id.typ f in
	 ((f', typ', xs', t1', [], t2')::defs2) @
         ((f', typ', xs', make_not t1', [], t3')::defs3)
     | _ -> raise Not_found)
  with Not_found ->
    let defs,t' = trans_term post xs' env t in
    let typ' = trans_typ @@ Id.typ f in
    (f', typ', xs', Const True, [], t')::defs


let get_var_arity f env = get_typ_arity (List.assoc f env)

let rec is_CPS_value env = function
  | Const _
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




let event_of_temp {env;defs;main;attr} =
  if List.mem ACPS attr
  then
    let make_event (f,xs,t1,e,t2) =
      assert (e = []);
      match t2 with
      | App(Const (Temp s), t2') when t1 = Const True || not @@ List.mem ACPS attr ->
          [], [f, xs, t1, [Event s], App(t2', Const Unit)]
      | App(Const (Temp s), t2') ->
          let g = new_id s in
          [g, TFun(typ_bool(),fun _ -> TFun(TFun(typ_unit, fun _ -> typ_result), fun _ -> typ_result))],
          (* cannot refute if b is eliminated, because k cannot have predicates in current impl. *)
          [g, ["b"; "k"], Const True, [Event s], App(Var "k", Const Unit);
           f, xs, t1, [], App(App(Var g, Const True), t2')]
      | _ -> [], [f, xs, t1, [], t2]
    in
    let envs,defss = List.split_map make_event defs in
    {env=List.flatten envs @@@ env; defs=List.flatten defss; main; attr}
  else
    let rec aux = function
      | Const (Temp e) -> [e]
      | Const c -> []
      | Var x -> []
      | App(t1, t2) -> aux t1 @@@ aux t2
      | Fun _ -> assert false
      | Let _ -> assert false
    in
    let evts = List.unique @@ List.rev_map_flatten (fun (_,_,_,_,t) -> aux t) defs in
    let map = List.map (fun e -> e, new_id e) evts in
    let evt_env = List.map (fun (_,f) -> f, TFun(typ_unit, fun _ -> typ_unit)) map in
    let evt_defs = List.map (fun (e,f) -> f,["u"],Const True,[Event e],Const Unit) map in
    let rec aux = function
      | Const c -> Const c
      | Var x -> Var x
      | App(Const (Temp e), t) -> App(t, App(Var (List.assoc e map), Const Unit))
      | App(t1, t2) -> App(aux t1, aux t2)
      | Fun _ -> assert false
      | Let _ -> assert false
    in
    let defs' = List.map (map_body_def aux) defs in
    {env=evt_env@@@env; defs=evt_defs@@@defs'; main; attr}


let rec uniq_env = function
  | [] -> []
  | (f,typ)::env ->
      if List.exists (fun (g,_) -> f = g) env
      then uniq_env env
      else (f,typ) :: uniq_env env


let rename_prog prog =
  let counter1 = Id.get_counter () in
  Id.clear_counter ();
  let vars = List.map (fun (f,_,_,_,_) -> f) prog.defs in
  let var_names = List.rev_map id_name (List.unique vars) in
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
    if debug()
    then
      (List.iter (fun (f,f') -> Format.printf "rename: %s ==> %s@." f f') map;
       Format.printf "@.")
  in
  let var_names' = List.map snd map in
  let rename_var map x = List.assoc x map in
  let rename_def (f,xs,t1,e,t2) =
    let counter2 = Id.get_counter () in
    let () = Id.clear_counter () in
    let var_names'' = List.rev_map id_name xs @@@ var_names' in
    let arg_map = List.map (fun x -> x, rename_id' x var_names'') xs in
    let arg_map = List.unique ~cmp:(fun (x,_) (y,_) -> x = y) arg_map in
    let smap = List.map (fun (x,x') -> x, Var x') (arg_map @@@ map) in
    let rename_term t = subst_map smap t in
    let def = rename_var map f, List.map (rename_var arg_map) xs, rename_term t1, e, rename_term t2 in
    Id.set_counter counter2;
    def
  in
  let env = List.map (Pair.map_fst @@ rename_var map) prog.env in
  let defs = List.map rename_def prog.defs in
  let main = rename_var map prog.main in
  let prog = {env; defs; main; attr=prog.attr} in
  if false then Format.printf "@.PROG:@.%a@." CEGAR_print.prog_typ prog;
  ignore (Typing.infer prog);
  let rmap = List.map (Pair.map_snd trans_inv_var) map in
  Id.set_counter counter1;
  prog, map, rmap

let id_prog prog =
  let map = List.rev_map (fun (f,_) -> f, f) prog.env in
  let rmap = List.map (fun (f,f') -> f', trans_inv_var f) map in
  prog, map, rmap



module CRT = CEGAR_ref_type
module RT = Ref_type

let rec trans_ref_type = function
  | CRT.Base(b,x,p) ->
      let b' =
        match b with
        | CRT.Unit -> RT.Unit
        | CRT.Bool -> RT.Bool
        | CRT.Int -> RT.Int
        | CRT.Abst s -> RT.Abst s
      in
      RT.Base(b', trans_inv_var x, trans_inv_term p)
  | CRT.Fun(x,typ1,typ2) ->
      RT.Fun(trans_inv_var x, trans_ref_type typ1, trans_ref_type typ2)
  | CRT.Inter typs ->
      RT.Inter (List.map trans_ref_type typs)


let trans_term = trans_term "" [] []

let trans_prog ?(spec=[]) t =
  let pr p s t = if debug () then Format.printf "##[trans_prog] %s:@.%a@.@." s p t in
  let pr1 = pr Print.term' in
  let pr2 = pr CEGAR_print.prog_typ in
  let ext_env = List.map (Pair.map trans_var trans_typ) @@ Trans.make_ext_env t in
  pr1 "BEFORE" t;
  let t = Trans.trans_let t in
  pr1 "AFTER" t;
  let main = new_id "main" in
  let (defs,t_main),get_rtyp = Lift.lift t in
  let defs_t,t_main' = trans_term t_main in
  let is_cps = List.mem Syntax.ACPS t.Syntax.attr in
  let defs' =
    match !Flag.cegar with
    | Flag.CEGAR_InteractionType ->
        let typ = TFun(typ_unit, fun _ -> typ_unit) in
        (main,typ,["u"],Const True,[],t_main') :: defs_t @ List.flatten_map trans_def defs
    | Flag.CEGAR_DependentType ->
        let typ = if is_cps then typ_result else typ_unit in
        (main,typ,[],Const True,[],t_main') :: defs_t @ List.flatten_map trans_def defs
  in
  let env,defs'' = List.split_map (fun (f,typ,xs,t1,e,t2) -> (f,typ), (f,xs,t1,e,t2)) defs' in
  let env' =
    let spec' = List.map (Pair.map trans_var trans_typ) spec in
    let aux (f,typ) = try f, merge_typ typ @@ List.assoc f spec' with Not_found -> f,typ in
    uniq_env (ext_env @@@ List.map aux env)
  in
  let prog = {env=env'; defs=defs''; main; attr=if is_cps then [ACPS] else []} in
  pr2 "PROG_A" prog;
  let prog = event_of_temp prog in
  pr2 "PROG_B" prog;
  let prog = eta_expand prog in
  pr2 "PROG_C" prog;
  let prog = pop_main prog in
  pr2 "PROG_D" prog;
  let prog = assign_id_to_rand prog in
  pr2 "PROG_E" prog;
  let prog,map,rmap = id_prog prog in
  pr2 "PROG_F" prog;
  let get_rtyp f typ = get_rtyp f (trans_ref_type typ) in
  prog,map,rmap,get_rtyp


let assoc_def_aux defs n t =
  let defs' = List.filter (fun (f,_,_,_,_) -> Var f = t) defs in
    List.nth defs' n

let assoc_def labeled t ce defs =
  if List.exists (fun f -> Var f = t) labeled
  then List.tl ce, assoc_def_aux defs (List.hd ce) t
  else ce, assoc_def_aux defs 0 t


let rec is_value env = function
  | Const Bottom -> false
  | Const RandBool -> false
  | Const _ -> true
  | Var x -> get_arg_num (get_typ env (Var x)) > 0
  | App(App(App(Const If, _), _), _) -> false
  | App _ as t ->
      let t1,ts = decomp_app t in
        List.for_all (is_value env) (t1::ts) && get_arg_num (get_typ env t) = List.length ts
  | Let _ -> assert false
  | Fun _ -> assert false

let rec read_bool () =
  Format.printf "RandBool (t/f/r/s): @?";
  match read_line () with
  | "t" -> true
  | "f" -> false
  | "r" -> raise EvalRestart
  | "s" -> raise EvalSkip
  | s -> read_bool ()

let rec step_eval_abst_cbn ce labeled env_abst defs = function
  | Const Bottom -> raise TypeBottom
  | Const RandBool ->
      let t =
        if read_bool ()
        then Const True
        else Const False
      in
      ce, t
  | Const Unit -> ce, Const Unit
  | Var x ->
      let ce',(f,xs,tf1,es,tf2) = assoc_def labeled (Var x) ce defs in
      assert (tf1 = Const True);
      if List.mem (Event "fail") es then raise EvalFail;
      ce', tf2
  | App(App(App(Const If, Const True), t2), _) -> ce, t2
  | App(App(App(Const If, Const False), _), t3) -> ce, t3
  | App(App(App(Const If, t1), t2), t3) ->
      let ce',t1' = step_eval_abst_cbn ce labeled env_abst defs t1 in
      ce', App(App(App(Const If, t1'), t2), t3)
  | App(Const (Label n), t) ->
      step_eval_abst_cbn ce labeled env_abst defs t
  | App _ as t ->
      let t1,ts = decomp_app t in
      if t1 = Const If
      then
        match ts with
          t1::t2::t3::ts' ->
          let t2' = make_app t2 ts' in
          let t3' = make_app t3 ts' in
          step_eval_abst_cbn ce labeled env_abst defs (make_if t1 t2' t3')
        | _ -> assert false
      else
        let ce',(f,xs,tf1,es,tf2) = assoc_def labeled t1 ce defs in
        let ts1,ts2 = List.split_nth (List.length xs) ts in
        assert (tf1 = Const True);
        if List.mem (Event "fail") es then raise EvalFail;
        ce', make_app (List.fold_right2 subst xs ts1 tf2) ts2
  | _ -> assert false

let rec eval_abst_cbn prog labeled abst ce =
  Format.printf "Program with abstraction types::@.%a@." CEGAR_print.prog abst;
  Format.printf "CE: %a@." CEGAR_print.ce ce;
  let env_orig = prog.env in
  let env_abst = abst.env in
  let defs = abst.defs in
  let main = abst.main in
  let ce' = ce in
  let rec loop ce t =
    Format.printf "%a -->@\n" CEGAR_print.term t;
    assert (match get_typ env_abst t with TBase(TUnit,_) -> true | _ -> false);
    begin
      try
        match decomp_app t with
          Var x, _ ->
          Format.printf "  %s:: %a@\n" x CEGAR_print.typ (List.assoc x env_orig)
        | _ -> ()
      with Not_found -> ()
    end;
    let ce',t' = step_eval_abst_cbn ce labeled env_abst defs t in
    if t' = Const Unit then raise EvalTerminate;
    loop ce' t'
  in
  let rec confirm () =
    Format.printf "(s)kip/(r)estart: @?";
    match read_line () with
    | "s" -> ()
    | "r" -> eval_abst_cbn prog labeled abst ce
    | s -> confirm ()
  in
  Format.printf "Evaluation of abstracted program::@.";
  try
    loop ce' (Var main)
  with
  | EvalRestart -> eval_abst_cbn prog labeled abst ce
  | EvalFail ->
      Format.printf "ERROR!@.@.";
      confirm ()
  | EvalSkip -> ()
  | EvalTerminate ->
      Format.printf "TERMINATES!@.@.";
      confirm ()
  | TypeBottom ->
      Format.printf "DIVERGES!@.@.";
      confirm ()







let assoc_def labeled defs ce acc t =
  let f = match t with Var f -> f | _ -> assert false in
  let defs' = List.filter (fun (g,_,_,_,_) -> g = f) defs in
  if List.mem f labeled
  then
    let c = List.hd ce in
    let ce' = List.tl ce in
    let acc' = c::acc in
    let def = List.nth defs' c in
    ce', acc', def
  else
    let acc' = 0::acc in
    let def = List.hd defs' in
    assert (List.length defs' = 1);
    ce, acc', def

let init_cont _ acc _ = List.rev acc

let rec trans_ce_aux labeled ce acc defs t k =
  if false then Format.printf "trans_ce_aux[%d,%d]: %a@." (List.length ce) (List.length acc) CEGAR_print.term t;
  match t with
  | Const (RandInt _) -> assert false
  | Const c -> k ce acc (Const c)
  | Var x -> k ce acc (Var x)
  | App(Const Not, t) ->
      trans_ce_aux labeled ce acc defs t (fun ce acc t ->
      k ce acc (make_app (Const Not) [t]))
  | App(App(Const (And|Or|Lt|Gt|Leq|Geq|EqUnit|EqBool|EqInt|Add|Sub|Mul as op),t1),t2) ->
      trans_ce_aux labeled ce acc defs t1 (fun ce acc t1 ->
      trans_ce_aux labeled ce acc defs t2 (fun ce acc t2 ->
      k ce acc (make_app (Const op) [t1;t2])))
  | App(Const (RandInt _), t) ->
      let r = new_id "r" in
      trans_ce_aux labeled ce acc defs (App(t,Var r)) k
  | App(t1,t2) ->
      trans_ce_aux labeled ce acc defs t1 (fun ce acc t1 ->
      trans_ce_aux labeled ce acc defs t2 (fun ce acc t2 ->
      let t1',ts = decomp_app (App(t1,t2)) in
      let _,xs,_,_,_ = List.find (fun (f,_,_,_,_) -> Var f = t1') defs in
      if List.length xs > List.length ts
      then k ce acc (App(t1,t2))
      else
        let ce',acc',(f,xs,tf1,e,tf2) = assoc_def labeled defs ce acc t1' in
        let ts1,ts2 = List.split_nth (List.length xs) ts in
        let aux = List.fold_right2 subst xs ts1 in
        let tf2' = make_app (aux tf2) ts2 in
        assert (List.length xs = List.length ts);
        if e = [Event "fail"]
        then init_cont ce' acc' tf2'
        else trans_ce_aux labeled ce' acc' defs tf2' k))
  | Let _ -> assert false
  | Fun _ -> assert false

let trans_ce ce labeled {defs; main} =
  let _,_,_,_,t = List.find (fun (f,_,_,_,_) -> f = main) defs in
  let ce' = trans_ce_aux labeled ce [] defs t init_cont in
  assert (not (List.mem main labeled));
  0::ce'


let rec simplify_if_term t =
  match t with
  | Const c -> Const c
  | Var x -> Var x
  | App(App(App(Const If, t1), t2), t3) ->
      let t1' = simplify_if_term t1 in
      let t2' = simplify_if_term t2 in
      let t3' = simplify_if_term t3 in
      begin
        match normalize_bool_term t1' with
        | Const True -> t2'
        | Const False -> t3'
        | App(Const Not, t1'') -> make_if t1'' t3' t2'
        | t1'' -> make_if t1'' t2' t3'
      end
  | App(t1,t2) -> App(simplify_if_term t1, simplify_if_term t2)
  | Let _ -> assert false
  | Fun(x,typ,t) -> Fun(x, typ, simplify_if_term t)

let simplify_if {env; defs; main; attr} =
  let defs' = List.map (fun (f,xs,t1,e,t2) -> f, xs, simplify_if_term t1, e, simplify_if_term t2) defs in
  {env; defs=defs'; main; attr}
