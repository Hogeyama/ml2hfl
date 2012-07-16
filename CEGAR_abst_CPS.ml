open Utilities
open CEGAR_syntax
open CEGAR_type
open CEGAR_util
open CEGAR_abst_util



let debug = false


let abst_arg x typ =
  if debug then Format.printf "abst_arg: %a, %a@." CEGAR_print.var x CEGAR_print.typ typ;
  match typ with
      TBase(_,ps) ->
        begin
          match ps (Var x) with
              [] -> []
            | [_] -> [x]
            | ps -> Utilities.mapi (fun i _ -> add_name x (string_of_int i)) ps
        end
    | _ -> [x]
let make_pts x typ =
  let xs = abst_arg x typ in
  let ps =
    match typ with
        TBase(_,ps) -> ps (Var x)
      | _ -> [Const True]
  in
    List.filter (fun (p,_) -> p <> Const True) (List.map2 (fun p x -> p, Var x) ps xs)






let rec beta_reduce_term = function
    Const c -> Const c
  | Var x -> Var x
  | App(t1, t2) ->
      let t1' = beta_reduce_term t1 in
      let t2' = beta_reduce_term t2 in
        begin
          match t1' with
              Fun(x,_,t1') -> beta_reduce_term (subst x t2' t1')
            | _ -> App(t1', t2')
        end
  | Fun(x, typ, t) -> Fun(x, typ, beta_reduce_term t)
  | Let _ -> assert false
let beta_reduce_def (f,xs,t1,e,t2) =
  f, xs, beta_reduce_term t1, e, beta_reduce_term t2

let rec expand_nonrec orig_fun_list force {env=env;defs=defs;main=main} =
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
    {env=env; defs=defs'''; main=main}





let rec trans_eager_bool f = function
    Const True
  | Const False
  | Var _ as t -> App(Var f, t)
  | Const RandInt -> assert false
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
  | App(App(App(Const If, Const RandBool), Const True), Const False) ->
      make_br (App(Var f, Const True)) (App(Var f, Const False))
  | App(App(App(Const If, t1), t2), t3) ->
      let x = new_id "b" in
      let f' = new_id "f" in
      let t1' = trans_eager_bool f' t1 in
      let t2' = trans_eager_bool f t2 in
      let t3' = trans_eager_bool f t3 in
        Let(f', Fun(x, None, make_if (Var x) t2' t3'), t1')
  | t -> Format.printf "trans_eager_bool: %a@." CEGAR_print.term t; assert false

let id x = x
let rec trans_eager_term env c t =
  let is_bool t =
    try
      match get_typ env t with
          TBase(TBool,_) -> true
        | _ -> false
    with TypeBottom -> false
  in
    match t with
        App(App(Const And, _), _)
      | App(App(Const Or, _), _)
      | App(Const Not, _)
      | App(App(App(Const If, _), _), _) when is_bool t ->
          let x = new_id "b" in
          let f = new_id "f" in
            begin
              match c (Var x) with
                  App(Var k, Var y) when x = y -> trans_eager_bool k t
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
            begin
              match make_if (Var x) t2' t3' with
                  App(Var k, Var y) when x=y ->
                    let t1' = trans_eager_bool k t1 in
                      c (Let(f, Var k, t1'))
                | t' ->
                    let t1' = trans_eager_bool f t1 in
                      c (Let(f, Fun(x, None, t'), t1'))
            end
      | App(t1, t2) ->
          let c' x = App(trans_eager_term env id t1, x) in
            c (trans_eager_term env c' t2)
      | Let(f, t1, t2) ->
          let t1' = trans_eager_term env id t1 in
          let t2' = trans_eager_term env id t2 in
            c (Let(f, t1', t2'))
let trans_eager_def env (f,xs,t1,e,t2) =
  let env' = get_arg_env (List.assoc f env) xs @@ env in
    assert (t1 = Const True);
    f, xs, t1, e, trans_eager_term env' id t2

let trans_eager prog = {prog with defs = List.map (trans_eager_def prog.env) prog.defs}


let rec eta_expand_term_aux env t typ =
  if debug then Format.printf "ETA_AUX: %a: %a@." CEGAR_print.term t CEGAR_print.typ typ;
  match typ with
      TBase _ -> t
    | TFun(typ1,typ2) ->
        let x = new_id "x" in
        let typ2 = typ2 (Var x) in
        let env' = (x,typ1)::env in
        let typ1' =
          match get_typ env' t with
              TFun(typ,_) -> typ
            | typ -> Format.printf "%a: %a@." CEGAR_print.term t CEGAR_print.typ typ; assert false
        in
        let t' = App(t, eta_expand_term_aux env' (Var x) typ1') in
          Fun(x, Some typ1, eta_expand_term_aux env' t' typ2)
    | _ -> assert false

let rec eta_expand_term env t typ =
  if debug then Format.printf "ETA: %a: %a@." CEGAR_print.term t CEGAR_print.typ typ;
  match t with
      Const Bottom
    | Const RandInt -> t
    | (Var _ | Const _ | App _) when is_base_term env t -> t
    | Var x -> eta_expand_term_aux env t typ
    | App(App(App(Const If, t1), t2), t3) ->
        make_if t1 (eta_expand_term env t2 typ) (eta_expand_term env t3 typ)
    | App(Const (Label n), t) -> make_label n (eta_expand_term env t typ)
    | App _ ->
        let rec aux ts typ =
          match ts,typ with
              [], _ -> []
            | t::ts', TFun(typ1,typ2) ->
                let typ2 = typ2 t in
                let ts'' = aux ts' typ2 in
                  eta_expand_term env t typ1 :: ts''
            | _ -> assert false
        in
        let t1,ts = decomp_app t in
        let t' = make_app t1 (aux ts (get_typ env t1)) in
          eta_expand_term_aux env t' typ
    | Const _ -> assert false
    | Let _ -> assert false
    | Fun(x,_,t') ->
        match typ with
            TFun(typ1,typ2) ->
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
  let env'' = env' @@ env in
  let t2' = eta_expand_term env'' t2 typ in
    f, xs, t1, e, t2'
let eta_expand prog = {prog with defs = List.map (eta_expand_def prog.env) prog.defs}


let rec eta_reduce_term = function
    Const _
  | Var _ as t -> t
  | App(t1,t2) -> App(eta_reduce_term t1, eta_reduce_term t2)
  | Let _ -> assert false
  | Fun(x, typ, t) ->
      let t' = eta_reduce_term t in
      match t' with
          App(App(App(Const If,_),_),_) -> Fun(x, typ, t')
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

let rec abstract_term top must env cond pts t typ =
  if debug then Format.printf "abstract_term: %a: %a@." CEGAR_print.term t CEGAR_print.typ typ;
  match t with
    | Const Bottom ->
        assert (fst (decomp_tbase typ) = TUnit); [Const Bottom]
    | Var x when congruent env cond (List.assoc x env) typ ->
        List.map (fun x -> Var x) (abst_arg x typ)
    | (Var _ | Const _ | App _) when is_base_term env t ->
        let btyp,ps = decomp_tbase typ in
          if top && btyp = TUnit && ps t = []
          then [Const Unit]
          else List.map (abst env cond pts) (ps t)
    | App(App(App(Const If, t1), t2), t3) ->
        let t1' = hd (abstract_term false None env cond pts t1 typ_bool2) in
        let t2' = hd (abstract_term top must env (t1::cond) pts t2 typ) in
        let t3' = hd (abstract_term top must env (make_not t1::cond) pts t3 typ) in
          [make_if t1' t2' t3']
    | App(Const (Label n), t) -> [make_label n (hd (abstract_term top must env cond pts t typ))]
    | App(Const RandInt, t) -> abstract_term false must env cond pts t (TFun(typ_int, fun _ -> typ))
    | App _ ->
        let t1,ts = decomp_app t in
        let rec aux ts typ =
          match ts,typ with
              [], _ when fst (decomp_tbase typ) = TUnit -> []
            | t::ts', TFun(typ1,typ2) ->
                abstract_term false None env cond pts t typ1 @ aux ts' (typ2 t)
            | _,typ -> Format.printf "@.%a@.typ:%a@." CEGAR_print.term t CEGAR_print.typ typ; assert false
        in
        let t' = make_app t1 (aux ts (get_typ env t1)) in
          if !Flag.use_filter
          then [filter env cond pts must t']
          else [t']
    | Fun _ ->
        let env',t' = decomp_annot_fun t in
        let env' = List.map (fun (x,typ) -> x, get_opt_val typ) env' in
        let pts' = flatten_map (fun (x,typ) -> make_pts x typ) env' in
        let pts'' = pts' @@ pts in
        let xs' = flatten_map (fun (x,typ) -> abst_arg x typ) env' in
        let env'' = env' @@ env in
        let typ' = CEGAR_type.app typ (List.map (fun (x,_) -> Var x) env') in
        let t'' = hd (abstract_term true (Some pts') env'' cond pts'' t' typ') in
          [make_fun_temp xs' t'']
    | Var _ -> assert false
    | Const _ -> assert false
    | Let _ -> assert false




(* for_debug *)
let rec abstract_typ = function
    TBase(TUnit,ps) when ps (Const Unit) = [] -> [TBase(TUnit,ps)]
  | TBase(_,ps) -> List.map (fun _ -> TBase(TBool,fun _ -> [])) (ps (Const Unit))
  | TFun(typ1,typ2) ->
      let typ2 = typ2 (Const Unit) in
      let typs = abstract_typ typ1 in
      let aux typ1 typ2 = TFun(typ1, fun _ -> typ2) in
        [List.fold_right aux typs (hd (abstract_typ typ2))]
  | _ -> assert false


let abstract_def env (f,xs,t1,e,t2) =
  let rec decomp_typ typ xs =
    match xs with
        [] -> typ, []
      | x::xs' ->
          let typ1,typ2 = match typ with TFun(typ1,typ2) -> typ1,typ2 (Var x) | _ -> assert false in
          let typ',env' = decomp_typ typ2 xs' in
            typ', (x,typ1)::env'
  in
  let typ,env' = decomp_typ (try List.assoc f env with Not_found -> assert false) xs in
  if debug then Format.printf "%a: ENV: %a@." CEGAR_print.var f print_env env';
  let env'' = env' @@ env in
  let pts = flatten_map (fun (x,typ) -> make_pts x typ) env' in
  let xs' = flatten_map (fun (x,typ) -> abst_arg x typ) env' in
  if debug then Format.printf "%a: %a ===> %a@." CEGAR_print.var f CEGAR_print.term t2 CEGAR_print.term t2;
  if debug then Flag.print_fun_arg_typ := true;
  if debug then Format.printf "%s:: %a@." f CEGAR_print.term t2;
  let t2' = hd (abstract_term true None env'' [t1] pts t2 typ) in
  let t2'' = eta_reduce_term t2' in
    if e <> [] && t1 <> Const True
    then
      let g = rename_id f in
      let fv = diff (get_fv t2'') (List.map fst env) in
        [g, fv, Const True, e, t2'';
         f, xs', Const True, [], assume env' [] pts t1 (make_app (Var g) (List.map (fun x -> Var x) fv))]
    else [f, xs', Const True, e, assume env' [] pts t1 t2'']





let abstract orig_fun_list force prog =
  let labeled,prog = add_label prog in
  let prog = if !Flag.expand_nonrec then expand_nonrec orig_fun_list force prog else prog in
  let () = if false && !Flag.expand_nonrec then Format.printf "EXPAND_NONREC:@\n%a@." CEGAR_print.prog prog in
  let prog = eta_expand prog in
  let () = if false then Format.printf "ETA_EXPAND:@\n%a@." CEGAR_print.prog prog in
  let defs = flatten_map (abstract_def prog.env) prog.defs in
  let prog = {env=[]; defs=defs; main=prog.main} in
  let () = if false then Format.printf "ABST:@\n%a@." CEGAR_print.prog prog in
  let prog = Typing.infer prog in
  let prog = lift2 prog in
  let () = if false then Format.printf "LIFT:@\n%a@." CEGAR_print.prog prog in
  let prog = trans_eager prog in
  let () = if false then Format.printf "TRANS_EAGER:@\n%a@." CEGAR_print.prog prog in
  let prog = put_into_if prog in
  let _ = Typing.infer prog in
  let () = if false then Format.printf "PUT_INTO_IF:@\n%a@." CEGAR_print.prog prog in
  let prog = lift2 prog in
    labeled, prog
