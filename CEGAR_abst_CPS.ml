open Utilities
open CEGAR_syntax
open CEGAR_type
open CEGAR_print
open CEGAR_util
open CEGAR_abst_util



let debug = false


let abst_arg x typ =
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
  | t -> Format.printf "trans_eager_bool: %a@." print_term t; assert false

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
      | Const (RandBool | And | Or | Not | Lt | Gt | Leq | Geq | EqInt | EqBool) -> assert false
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

let trans_eager (env,defs,main) =
  env, List.map (trans_eager_def env) defs, main




(*
let c = ref 0
let ni () = let n = !c in incr c; n

let rec coerce env cond pts t typ1 typ2 =
Format.printf "t:%a@.pts: %a@." CEGAR_print.term t print_pbs pts;
  match typ1,typ2 with
      _, TBase(TUnit,ps) when ps (Const Unit) = [] -> [t]
    | TBase _, TBase(_,ps2) ->
        List.map (abst env cond pts) (ps2 t)
    | TFun _, TFun _ when congruent env cond typ1 typ2 -> [t]
    | TFun(typ11,typ12), TFun(typ21,typ22) ->
        let x = new_id "x" in
        let typ12 = typ12 (Var x) in
        let typ22 = typ22 (Var x) in
        let env' = (x,typ21)::env in
          begin
            match typ11,typ21 with
                TBase(_,ps1), TBase(_,ps2) ->
                Format.printf "typ11,typ21: %a,%a@." CEGAR_print.typ typ11 CEGAR_print.typ typ21;
                  let xs = abst_arg x typ21 in
                  let pts' = make_pts x typ21 @@ pts in
                  let ts = coerce env' cond pts' (Var x) typ21 typ11 in
                  let t' = hd (coerce env' cond pts' (make_app t ts) typ12 typ22) in
                  let t'' = filter env' cond pts' t' in
                    [make_fun_temp xs t'']
              | TFun _, TFun _ ->
                  let x = new_id "f" in
                  let t' = App(t, hd (coerce env' cond pts (Var x) typ21 typ11)) in
                    [Fun(x, None, hd (coerce env' cond pts t' typ12 typ22))]
              | _ -> assert false
          end
    | _ -> Format.printf "coerce: %a, %a@." print_typ typ1 print_typ typ2; assert false

let coerce env cond pts t typ1 typ2 =
  let r = ni() in
    Format.printf "coerce%d: %a ==> %a@." r print_typ typ1 print_typ typ2;
  let t' = coerce env cond pts t typ1 typ2 in
    Format.printf "      %d  %a ==> [%a]@." r print_term t (print_list print_term ";" false) t';
    t'



let rec abstract_term env cond pts t typ =
  if true then Format.printf "abstract_term: %a: %a@." CEGAR_print.print_term t CEGAR_print.print_typ typ;
  match t with
      Var x -> coerce env cond pts t (List.assoc x env) typ
    | Const Bottom -> coerce env cond pts t typ typ
    | Const c -> coerce env cond pts t (get_const_typ c) typ
    | App _ when is_base_term env t ->
        let base = get_base typ in
          coerce env cond pts t (TBase(base,fun x -> [make_eq_int x t])) typ
    | App(Const RandInt, t) -> abstract_term env cond pts t (TFun(typ_int, fun _ -> typ))
    | App _ ->
        let t1,ts = decomp_app t in
        let rec aux ts typ =
          match ts,typ with
              [], _ -> [],typ
            | t::ts', TFun(typ1,typ2) ->
                let typ2 = typ2 t in
                let ts'',typ' = aux ts' typ2 in
                  abstract_term env cond pts t typ1 @ ts'', typ'
            | _ -> assert false
        in
        let ts',typ' = aux ts (get_typ env t1) in
          coerce env cond pts (make_app t1 ts') typ' typ
    | Let(x,t1,t2) -> assert false
    | Fun _ -> assert false
*)


let rec eta_expand_term_aux env t typ =
  if false then Format.printf "ETA_AUX: %a: %a@." print_term t print_typ typ;
  match typ with
      TBase _ -> t
    | TFun(typ1,typ2) ->
        let x = new_id "x" in
        let typ2 = typ2 (Var x) in
        let env' = (x,typ1)::env in
        let typ1' = match get_typ env' t with TFun(typ,_) -> typ | _ -> assert false in
        let t' = App(t, eta_expand_term_aux env' (Var x) typ1') in
          Fun(x, Some typ1, eta_expand_term_aux env' t' typ2)
    | _ -> assert false

let rec eta_expand_term env t typ =
  if debug then Format.printf "ETA: %a: %a@." print_term t print_typ typ;
  match t with
      Const Bottom
    | Const RandInt -> t
    | (Var _ | Const _ | App _) when is_base_term env t -> t
    | Var x -> eta_expand_term_aux env t typ
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
    | Fun _ -> assert false


let rec eta_reduce_term = function
    Const _
  | Var _ as t -> t
  | App(t1,t2) -> App(eta_reduce_term t1, eta_reduce_term t2)
  | Let _ -> assert false
  | Fun(x, typ, t) ->
      match eta_reduce_term t with
          App(t, Var y) when x = y && not (List.mem y (get_fv t)) -> t
        | t' -> Fun(x, typ, t')
(*
let eta_reduce_term t =
  Format.printf "REDUCE: [%a]@." print_term t;
  eta_reduce_term t
let eta_reduce_term t = t
*)

let print_env fm env =
  List.iter (fun (f,typ) -> Format.fprintf fm "%a:%a,@ " print_var f print_typ typ) env;
  Format.fprintf fm "@."

let rec abstract_term top env cond pts t typ =
  if debug then Format.printf "abstract_term: %a: %a@." CEGAR_print.print_term t CEGAR_print.print_typ typ;
  match t with
    | Const Bottom -> assert (fst (decomp_tbase typ) = TUnit); [Const Bottom]
    | (Var _ | Const _ | App _) when is_base_term env t ->
        let btyp,ps = decomp_tbase typ in
          if top && btyp = TUnit && ps t = []
          then [Const Unit]
          else List.map (abst env cond pts) (ps t)
    | App(Const RandInt, t) -> abstract_term false env cond pts t (TFun(typ_int, fun _ -> typ))
    | App _ ->
        let t1,ts = decomp_app t in
        let rec aux ts typ =
          match ts,typ with
              [], _ -> assert (fst (decomp_tbase typ) = TUnit); []
            | t::ts', TFun(typ1,typ2) ->
                abstract_term false env cond pts t typ1 @ aux ts' (typ2 t)
            | _ -> assert false
        in
        let t' = make_app t1 (aux ts (get_typ env t1)) in
          [filter env cond pts t']
    | Fun _ ->
        let env',t' = decomp_annot_fun t in
        let env' = List.map (fun (x,typ) -> x, get_opt_val typ) env' in
        let pts' = flatten_map (fun (x,typ) -> make_pts x typ) env' @@ pts in
        let xs' = flatten_map (fun (x,typ) -> abst_arg x typ) env' in
        let env'' = env' @@ env in
        let typ' = CEGAR_type.app typ (List.map (fun (x,_) -> Var x) env') in
        let t'' = hd (abstract_term false env'' cond pts' t' typ') in
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
Format.printf "%s@." f;
  let rec decomp_typ typ xs =
    match xs with
        [] -> typ, []
      | x::xs' ->
          let typ1,typ2 = match typ with TFun(typ1,typ2) -> typ1,typ2 (Var x) | _ -> assert false in
          let typ',env' = decomp_typ typ2 xs' in
            typ', (x,typ1)::env'
  in
  let typ,env' = decomp_typ (List.assoc f env) xs in
if debug then Format.printf "%a: ENV: %a@." CEGAR_print.var f print_env env';
  let env'' = env' @@ env in
  let pts = List.flatten (List.map (fun (x,typ) -> make_pts x typ) env') in
  let xs' = List.flatten (List.map (fun (x,typ) -> abst_arg x typ) env') in
  let t2' = eta_expand_term env'' t2 typ in
if debug then Format.printf "%a: %a ===> %a@." CEGAR_print.var f CEGAR_print.term t2 CEGAR_print.term t2';
if debug then Flag.print_fun_arg_typ := true;
if debug then Format.printf "%s:: %a@." f print_term t2';
  let t2'' = hd (abstract_term true env'' [t1] pts t2' typ) in
  let t2''' = eta_reduce_term t2'' in
    if e <> [] && t1 <> Const True
    then
      let g = rename_id f in
      let fv = diff (get_fv t2''') (List.map fst env) in
        [g, fv, Const True, e, t2''';
         f, xs', Const True, [], assume env' [] pts t1 (make_app (Var g) (List.map (fun x -> Var x) fv))]
    else [f, xs', Const True, e, assume env' [] pts t1 t2''']





let abstract ((env,defs,main):prog) : prog =
  let (env,defs,main) = add_label (env,defs,main) in
  let _ = Typing.infer (env,defs,main) in
  let defs = flatten_map (abstract_def env) defs in
  let () = if true then Format.printf "ABST:\n%a@." CEGAR_print.print_prog ([], defs, main) in
  let prog = Typing.infer ([], defs, main) in
  let prog = lift2 prog in
  let () = if false then Format.printf "LIFT:\n%a@." CEGAR_print.print_prog_typ prog in
  let prog = trans_eager prog in
  let () = if false then Format.printf "TRANS_EAGER:\n%a@." CEGAR_print.print_prog_typ prog in
  let prog = put_into_if prog in
  let _ = Typing.infer prog in
  let () = if false then Format.printf "PUT_INTO_IF:\n%a@." CEGAR_print.print_prog_typ prog in
  let prog = lift2 prog in
    prog
