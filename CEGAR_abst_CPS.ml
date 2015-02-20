open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_util
open CEGAR_abst_util

let debug () = List.mem "CEGAR_abst_CPS" !Flag.debug_module

let abst_arg x typ =
  if debug() then Format.printf "abst_arg: %a, %a;;@." CEGAR_print.var x CEGAR_print.typ typ;
  match typ with
  | TBase(_,ps) ->
      begin
        match ps (Var x) with
        | [] -> []
        | [_] -> [x]
        | ps -> List.mapi (fun i _ -> add_name x @@ string_of_int @@ i+1) ps
      end
  | _ -> [x]
let make_pts x typ =
  let xs = abst_arg x typ in
  let ps =
    match typ with
    | TBase(_,ps) -> ps (Var x)
    | _ -> [Const True]
  in
  List.filter (fun (p,_) -> p <> Const True) @@ List.map2 (fun p x -> p, Var x) ps xs


let rec beta_reduce_term = function
  | Const c -> Const c
  | Var x -> Var x
  | App(t1, t2) ->
      let t1' = beta_reduce_term t1 in
      let t2' = beta_reduce_term t2 in
      begin
        match t1' with
        | Fun(x,_,t1') -> beta_reduce_term (subst x t2' t1')
        | _ -> App(t1', t2')
      end
  | Fun(x, typ, t) -> Fun(x, typ, beta_reduce_term t)
  | Let _ -> assert false
let beta_reduce_def (f,xs,t1,e,t2) =
  f, xs, beta_reduce_term t1, e, beta_reduce_term t2

let rec expand_nonrec orig_fun_list force {env;defs;main;attr} =
  let nonrec = get_nonrec defs main orig_fun_list force in
  let aux (f,xs,t1,e,t2) = f, xs, subst_map nonrec t1, e, subst_map nonrec t2 in
  let rec loop defs =
    let defs' = List.map aux defs in
    if defs = defs'
    then defs
    else loop defs'
  in
  let defs' = List.filter (fun (f,_,_,_,_) -> not (List.mem_assoc f nonrec)) defs in
  let defs'' = loop defs' in
  let defs''' = List.map beta_reduce_def defs'' in
  {env; defs=defs'''; main; attr}





let rec trans_eager_bool f = function
  | Const True
  | Const False
  | Var _ as t -> App(Var f, t)
  | Const RandBool
  | App(App(App(Const If, Const RandBool), Const True), Const False) ->
      make_br (App(Var f, Const True)) (App(Var f, Const False))
  | App(App(Const Or, t1), t2) ->
      let x = new_id "b" in
      let f' = new_id "f" in
      let t1' = trans_eager_bool f' t1 in
      let t2' = trans_eager_bool f t2 in
      Let(f', Fun(x, None, make_if (Var x) (App(Var f, Const True)) t2'), t1')
  | App(App(Const And, t1), t2) ->
      let x = new_id "b" in
      let f' = new_id "f" in
      let t1' = trans_eager_bool f' t1 in
      let t2' = trans_eager_bool f t2 in
      Let(f', Fun(x, None, make_if (Var x) t2' (App(Var f, Const False))), t1')
  | App(Const Not, t) ->
      let x = new_id "b" in
      let f' = new_id "f" in
      let t' = trans_eager_bool f' t in
      Let(f', Fun(x, None, make_if (Var x) (App(Var f, Const False)) (App(Var f, Const True))), t')
  | App(App(App(Const If, t1), t2), t3) ->
      let x = new_id "b" in
      let f' = new_id "f" in
      let t1' = trans_eager_bool f' t1 in
      let t2' = trans_eager_bool f t2 in
      let t3' = trans_eager_bool f t3 in
      Let(f', Fun(x, None, make_if (Var x) t2' t3'), t1')
  | t -> Format.printf "trans_eager_bool: %a@." CEGAR_print.term t; assert false

let id x = x
let is_bool env t =
  try
    match get_typ env t with
    | TBase(TBool,_) -> true
    | _ -> false
  with TypeBottom -> false
let rec trans_eager_term env c t =
  match t with
  | App(App(Const And, _), _)
  | App(App(Const Or, _), _)
  | App(Const Not, _)
  | App(App(App(Const If, _), _), _) when is_bool env t ->
      let x = new_id "b" in
      let f = new_id "f" in
      begin
        match c (Var x) with
        | App(Var k, Var y) when x = y -> trans_eager_bool k t
        | t' -> Let(f, Fun(x, None, t'), trans_eager_bool f t)
      end
  | Const (RandBool | And | Or | Not | Lt | Gt | Leq | Geq | EqUnit | EqInt | EqBool) -> assert false
  | Const _
  | Var _ -> c t
  | Fun(x,_,t) -> c (Fun(x, None, trans_eager_term env id t))
  | App(App(App(Const If, t1), t2), t3) ->
      let x = new_id "b" in
      let f = new_id "f" in
      let t2' = trans_eager_term env id t2 in
      let t3' = trans_eager_term env id t3 in
      let t' = make_if (Var x) t2' t3' in
      let t1' = trans_eager_bool f t1 in
      Let(f, Fun(x, None, t'), c t1')
  | App(t1, t2) ->
      let t1' = trans_eager_term env id t1 in
      let c' x = c (App(t1', x)) in
      trans_eager_term env c' t2
  | Let(x, t1, t2) ->
      let f = new_id "f" in
      let t2' = trans_eager_term env id t2 in
      let test = new_id "test" in
      let t1' = trans_eager_term env (fun x -> App(Var test, x)) t1 in
      Format.printf "t1: %a@." CEGAR_print.term t1;
      Format.printf "t2: %a@." CEGAR_print.term t2;
      Format.printf "t1': %a@." CEGAR_print.term t1';
      assert false;
      c (Let(f, Fun(x, None, t2'), t1'))
let trans_eager_def env (f,xs,t1,e,t2) =
  let env' = get_arg_env (List.assoc f env) xs @@@ env in
    assert (t1 = Const True);
    f, xs, t1, e, trans_eager_term env' id t2

let trans_eager prog =
  let defs = List.map (trans_eager_def prog.env) prog.defs in
  {prog with defs = defs}


let rec eta_expand_term_aux env t typ =
  if false && debug() then Format.printf "ETA_AUX: %a: %a@." CEGAR_print.term t CEGAR_print.typ typ;
  match typ with
  | TBase _ -> t
  | TFun(typ1,typ2) ->
      let x = new_id "x_" in
      let typ2 = typ2 (Var x) in
      let env' = (x,typ1)::env in
      let typ1' =
        match get_typ env' t with
        | TFun(typ,_) -> typ
        | typ -> Format.printf "%a: %a@." CEGAR_print.term t CEGAR_print.typ typ; assert false
      in
      let t' = App(t, eta_expand_term_aux env' (Var x) typ1') in
      Fun(x, Some typ1, eta_expand_term_aux env' t' typ2)
  | _ -> assert false

let rec eta_expand_term env t typ =
  if false && debug() then Format.printf "ETA: %a: %a@." CEGAR_print.term t CEGAR_print.typ typ;
  match t with
  | Const Bottom
  | Const (RandInt _)
  | Const CPS_result -> t
  | (Var _ | Const _ | App _) when is_base_term env t -> t
  | Var x -> eta_expand_term_aux env t typ
  | App(App(App(Const If, t1), t2), t3) ->
      make_if t1 (eta_expand_term env t2 typ) (eta_expand_term env t3 typ)
  | App(Const (Label n), t) -> make_label n @@ eta_expand_term env t typ
  | App _ ->
      let t1,ts = decomp_app t in
      let rec aux ts typ =
        match ts,typ with
        | [], _ -> []
        | t::ts', TFun(typ1,typ2) ->
            let typ2 = typ2 t in
            let ts'' = aux ts' typ2 in
            eta_expand_term env t typ1 :: ts''
        | t::_, _ -> Format.printf "ERROR: %a, %a@." CEGAR_print.term t1 CEGAR_print.term t; assert false
      in
      let t' = make_app t1 (aux ts @@ get_typ env t1) in
      eta_expand_term_aux env t' typ
  | Const _ -> assert false
  | Let _ -> assert false
  | Fun(x,_,t') ->
      match typ with
      | TFun(typ1,typ2) ->
          let env' = (x,typ1)::env in
          let t'' = eta_expand_term env' t' (typ2 (Var x)) in
          Fun(x, Some typ1, t'')
      | _ -> Format.printf "%a@." CEGAR_print.term t; assert false
let eta_expand_def env (f,xs,t1,e,t2) =
  let rec decomp_typ typ xs =
    match xs with
        [] -> typ, []
      | x::xs' ->
          let typ1,typ2 = match typ with TFun(typ1,typ2) -> typ1,typ2 (Var x) | _ -> assert false in
          let typ',env' = decomp_typ typ2 xs' in
            typ', (x,typ1)::env'
  in
  let typ,env' = decomp_typ (List.assoc f env) xs in
  let env'' = env' @@@ env in
  let t2' = eta_expand_term env'' t2 typ in
  f, xs, t1, e, t2'
let eta_expand prog = {prog with defs = List.map (eta_expand_def prog.env) prog.defs}



let rec eta_reduce_term = function
  | Const _
  | Var _ as t -> t
  | App(t1,t2) -> App(eta_reduce_term t1, eta_reduce_term t2)
  | Let(x, t1, t2) -> Let(x, eta_reduce_term t1, eta_reduce_term t2)
  | Fun(x, typ, t) ->
      let t' = eta_reduce_term t in
      match t' with
      | App(App(App(Const If,_),_),_) -> Fun(x, typ, t')
      | App(t, Var y) when x = y && not (List.mem y (get_fv t)) -> t
      | _ -> Fun(x, typ, t')
(*
let eta_reduce_term t =
  Format.printf "REDUCE: [%a]@." print_term t;
  eta_reduce_term t
let eta_reduce_term t = t
*)

let print_env fm env =
  List.iter (fun (f,typ) -> Format.fprintf fm "%a:%a,@ " CEGAR_print.var f CEGAR_print.typ typ) env;
  Format.fprintf fm "@."

let rec propagate_fun_arg_typ typ t =
  match typ, t with
  | TFun(typ1, typ2), Fun(x,_,t') -> Fun(x, Some typ1, propagate_fun_arg_typ (typ2 @@ Var x) t')
  | _ -> t

(*
let rec abstract_rand_int must env cond pts preds t typ =
  let typ_arg = TBase(TInt, preds) in
  let typ' = TFun(typ_arg, fun _ -> typ) in
  let t' = hd @@ abstract_term must env cond pts (propagate_fun_arg_typ typ' t) typ' in
  Format.printf "typ_arg: @[%a@." CEGAR_print.typ typ_arg;
  Format.printf "  t: @[%a@." CEGAR_print.term t;
  Format.printf "  t': @[%a@." CEGAR_print.term t';
  let x = new_id "r" in
  let env' = (x,typ_int)::env in
  let preds' = preds (Var x) in
  let pts' = make_pts x typ_arg in
  let rec aux pts pts_new preds =
    match pts_new,preds with
    | [],[] -> make_app t' @@ List.map snd pts'
    | (t,y)::pts_new', pred::preds' ->
        let k = new_id "k" in
        let y' = match y with Var y' -> y' | _ -> assert false in
        let t1 = aux ((t,y)::pts) pts_new' preds' in
        let t2 = make_app (Var k) [abst_rand_int env' cond pts x pred] in
        Let(k, Fun(y', None, t1), t2)
    | _ -> assert false
  in
  aux pts pts' preds'
*)

let rec abstract_rand_int n must env cond pts xs t =
  let _,preds = decomp_rand_typ ~xs:(Some xs) @@ assoc_renv n env in
  let typ' = TFun(TBase(TInt, preds), fun _ -> typ_result) in
  let t' = hd @@ abstract_term must env cond pts t typ' in
  let x = new_id "r" in
  let env' = (x,typ_int)::env in
  let preds' = preds (Var x) in
  if !Flag.debug_level > 0 then Format.printf "preds': %a@." (List.print CEGAR_print.term) preds';
  let rec make_combs n =
    if n <= 0
    then [[]]
    else
      let combs = make_combs (n-1) in
      List.flatten_map (fun acc -> [true::acc; false::acc]) combs
  in
  let f = new_id "f" in
  let cond_combs = make_combs @@ List.length pts in
  let pred_combs = make_combs @@ List.length preds' in
  let cts =
    let aux cond_comb =
      let cond' = List.map2 (fun b (p,_) -> if b then p else make_not p) cond_comb pts @ cond in
      if check_aux env cond' (Const False)
      then None
      else
        let argss =
          let aux' pred_comb =
            let bs,ps = List.split @@ List.map2 (fun b p -> b, if b then p else make_not p) pred_comb preds' in
            if check_exist env cond' x @@ List.fold_right make_and ps (Const True)
            then Some (bs, List.map (fun b -> if b then Const True else Const False) bs)
            else None
          in
          List.filter_map aux' pred_combs
        in
        let cs = List.map2 (fun b (_,t) -> if b then t else make_not t) cond_comb pts in
        Some (List.fold_right make_and cs (Const True), make_br_exists n @@ List.map (Pair.map_snd @@ make_app (Var f)) argss)
    in
    List.filter_map aux cond_combs
  in
  Let(f, t', List.fold_right (fun (b,t) t' -> make_if b t t') cts (Const Bottom))


and abstract_term must env cond pts t typ =
  if false && debug() then Format.printf "abstract_term: %a: %a@." CEGAR_print.term t CEGAR_print.typ typ;
  match t with
  | Const CPS_result -> [Const Unit]
  | Const Bottom ->
      assert (is_typ_result typ);
      [Const Bottom]
  | (Var _ | Const _ | App _) when is_base_term env t ->
      let btyp,ps = decomp_tbase typ in
      if btyp = typ_result_base
      then [Const Unit]
      else List.map (abst env cond pts) (ps t)
  | Var x when congruent env cond (List.assoc x env) typ ->
      List.map (fun x -> Var x) (abst_arg x typ)
  | App(App(App(Const If, t1), t2), t3) ->
      let t1' = hd @@ abstract_term None env cond pts t1 typ_bool_id in
      let t2' = hd @@ abstract_term must env (t1::cond) pts t2 typ in
      let t3' = hd @@ abstract_term must env (make_not t1::cond) pts t3 typ in
      [make_if t1' t2' t3']
  | App(Const (Label n), t) -> [make_label n (hd @@ abstract_term must env cond pts t typ)]
  | App(Const (RandInt None), t) ->
      abstract_term must env cond pts t (TFun(typ_int, fun _ -> typ))
  | App _ when is_app_randint t ->
      let t',ts = decomp_app t in
      let n =
        match t' with
        | Const (RandInt (Some n)) -> n
        | _ -> assert false
      in
      let ts',t'' = List.decomp_snoc ts in
      [abstract_rand_int n must env cond pts ts' t'']
  | App _ when not !Flag.cartesian_abstraction ->
      let t1,ts = decomp_app t in
      let rec decomp_typ ts typ =
        match ts,typ with
        | [], _ when is_typ_result typ -> []
        | t2::ts', TFun(typ1,typ2) -> typ1 :: decomp_typ ts' (typ2 t2)
        | _,typ -> Format.printf "@.%a@.typ:%a@." CEGAR_print.term t CEGAR_print.typ typ; assert false
      in
      let typs = decomp_typ ts @@ get_typ env t1 in
      let aux env (defs,args,pts) t typ =
        let x = new_id "xnc" in
        let pts' = make_pts x typ @@@ pts in
        let xs' = abst_arg x typ in
        let ts' = abstract_term None env cond pts t typ in
        let add_flag =
          let b =
            match typ with
            | TBase _ -> false
            | _ -> true
          in
          List.map (Pair.pair b)
        in
        defs @ (add_flag @@ List.combine xs' ts'),
        args @ List.map _Var xs',
        pts'
      in
      let _Let' (b,(x,t)) t' = if b then subst x t t' else Let(x,t,t') in
      let defs,args,_ = List.fold_left2 (aux env) ([],[],pts) ts typs in
      [List.fold_right _Let' defs @@ make_app t1 args]
  | App _ ->
      let t1,ts = decomp_app t in
      let rec decomp_typ ts typ =
        match ts,typ with
        | [], _ when is_typ_result typ -> []
        | t2::ts', TFun(typ1,typ2) ->
            typ1 :: decomp_typ ts' (typ2 t2)
        | _,typ -> Format.printf "@.%a@.typ:%a@." CEGAR_print.term t CEGAR_print.typ typ; assert false
      in
      let typs = decomp_typ ts @@ get_typ env t1 in
      let t' = make_app t1 @@ List.flatten @@ List.map2 (abstract_term None env cond pts) ts typs in
      if !Flag.use_filter
      then [filter env cond pts must t']
      else [t']
  | Fun _ ->
      let env',t' = decomp_annot_fun t in
      let env' = List.map (Pair.map_snd Option.get) env' in
      let pts' = List.flatten_map (Fun.uncurry make_pts) env' in
      let pts'' = pts' @@@ pts in
      let xs' = List.flatten_map (Fun.uncurry abst_arg) env' in
      let env'' = env' @@@ env in
      let typ' = CEGAR_type.app typ (List.map (_Var -| fst) env') in
      let t'' = hd (abstract_term (Some pts') env'' cond pts'' t' typ') in
      let t''' =
        if !Flag.use_filter
        then filter env'' cond pts'' must t''
        else t''
      in
      [make_fun_temp xs' t''']
  | Var _ -> assert false
  | Const _ -> assert false
  | Let _ -> assert false



let rec abstract_typ = function
  | TBase(base,ps) when base = typ_result_base -> [typ_unit]
  | TBase(_,ps) -> List.map (fun _ -> typ_bool_empty) (ps (Const Unit))
  | TFun(typ1,typ2) ->
      let typ2 = typ2 (Const Unit) in
      let typs = abstract_typ typ1 in
      let aux typ1 typ2 = TFun(typ1, fun _ -> typ2) in
      [List.fold_right aux typs (hd (abstract_typ typ2))]
  | _ -> assert false

let abstract_typ typ = hd @@ abstract_typ typ


let abstract_def env (f,xs,t1,e,t2) =
  let rec decomp_typ typ xs =
    match xs with
    | [] -> typ, []
    | x::xs' ->
        let typ1,typ2 = match typ with TFun(typ1,typ2) -> typ1,typ2 (Var x) | _ -> assert false in
        let typ',env' = decomp_typ typ2 xs' in
        typ', (x,typ1)::env'
  in
  let typ,env' = decomp_typ (try List.assoc f env with Not_found -> assert false) xs in
  if debug() then Format.printf "%a: ENV: %a@." CEGAR_print.var f print_env env';
  let env'' = env' @@@ env in
  let pts = List.flatten_map (fun (x,typ) -> make_pts x typ) env' in
  let xs' = List.flatten_map (fun (x,typ) -> abst_arg x typ) env' in
  if debug() then Format.printf "%a: %a ===> %a@." CEGAR_print.var f CEGAR_print.term t2 CEGAR_print.term t2;
  if debug() then Flag.print_fun_arg_typ := true;
  if debug() then Format.printf "%s:: %a@." f CEGAR_print.term t2;
  let t2' = hd (abstract_term None env'' [t1] pts t2 typ) in
  let t2'' = eta_reduce_term t2' in
  if e <> [] && t1 <> Const True then
    let g = rename_id f in
    let fv = List.diff (get_fv t2'') (List.map fst env) in
    [g, fv, Const True, e, t2'';
     f, xs', Const True, [], assume env' [] pts t1 @@ make_app (Var g) (List.map _Var fv)]
  else
    [f, xs', Const True, e, assume env' [] pts t1 t2'']





let make_br' t1 t2 =
  if t1 = Const Unit then t2
  else if t2 = Const Unit then t1
  else make_br t1 t2

type typ_cps = X | TFun1 of typ_cps | TFun2 of typ_cps * typ_cps

let rec trans_typ typ =
  match decomp_tfun typ with
  | [], TBase(TUnit, _) -> X
  | [typ1], TBase(TUnit, _) -> TFun1 (trans_typ typ1)
  | [typ1;typ2], TBase(TUnit, _) -> TFun2(trans_typ typ1, trans_typ typ2)
  | typs,typ -> Format.printf "%a@." CEGAR_print.typ typ; assert false

(* Assume that
   - a continuation is in the last position of arguments
   - not the form of selective CPS
   - external functions have no predicates
*)

let rec make_arg ks = function
  | X -> []
  | TFun1 typ -> [make_ext_fun_cps ks typ]
  | TFun2(typ1,typ2) -> [make_ext_fun_cps ks typ1; make_ext_fun_cps ks typ2]

and add_ks k typ ks = if typ = X then k::ks else ks

and make_ext_fun_cps ks = function
  | X -> List.fold_left (fun t x -> make_br' t (Var x)) (Const Unit) ks
  | TFun1 typ ->
      let k = new_id "k" in
      let ks' = add_ks k typ ks in
      Fun(k, None, make_app (Var k) (make_arg ks' typ))
  | TFun2(typ1,typ2) ->
      let f = new_id "f" in
      let k = new_id "k" in
      let ks' = ks |> add_ks f typ1 |> add_ks k typ2 in
      let t1 = make_app (Var f) (make_arg ks' typ1) in
      let t2 = make_app (Var k) (make_arg ks' typ2) in
      Fun(f, None, Fun(k, None, make_br t1 t2))

let add_ext_funs_cps prog =
  let env = get_ext_fun_env prog in
  let defs = List.map (fun (f,typ) -> f, [], Const True, [], make_ext_fun_cps [] (trans_typ typ)) env in
  let defs' = defs @ prog.defs in
  ignore @@ Typing.infer {env=[]; defs=defs'; main=prog.main; attr=[]};
  {prog with defs=defs'}

let abstract_prog prog =
  let env = List.map (fun f -> f, abstract_typ @@ List.assoc f prog.env) @@ List.filter_out is_randint_var @@ get_ext_funs prog in
  let defs = List.flatten_map (abstract_def prog.env) prog.defs in
  let attr = List.remove_all prog.attr ACPS in
  {env; defs; main=prog.main; attr}

let abstract orig_fun_list force prog top_funs =
  let labeled,prog = add_label prog in
  prog
  |& !Flag.expand_nonrec &> expand_nonrec orig_fun_list force
  |> CEGAR_trans.simplify_if
  |@ (debug() && !Flag.expand_nonrec) &> Format.printf "EXPAND_NONREC:@\n%a@." CEGAR_print.prog
  |> eta_expand
  |@debug()&> Format.printf "ETA_EXPAND:@\n%a@." CEGAR_print.prog
  |> abstract_prog
  |@debug()&> Format.printf "ABST:@\n%a@." CEGAR_print.prog
  |> Typing.infer -| initialize_env
  |@(!Flag.debug_abst)&> eval_step_by_step
  |> CEGAR_lift.lift2
  |@debug()&> Format.printf "LIFT:@\n%a@." CEGAR_print.prog
  |> trans_eager
  |@debug()&> Format.printf "TRANS_EAGER:@\n%a@." CEGAR_print.prog
  |> put_into_if
  |@> Typing.infer
  |@debug()&> Format.printf "PUT_INTO_IF:@\n%a@." CEGAR_print.prog
  |> CEGAR_lift.lift2
  |> Pair.add_left @@ Fun.const labeled
