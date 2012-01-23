open Utilities
open CEGAR_syntax
open CEGAR_type
open CEGAR_print
open CEGAR_util
open CEGAR_abst_util


let abst_arg x typ =
  match typ with
      TBase(TUnit,ps) when ps (Const Unit) = [] -> [x]
    | TBase(_,ps) ->
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
        TBase(TUnit,ps) when ps (Const Unit) = [] -> [Const True]
      | TBase(_,ps) -> ps (Var x)
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
        Let(f', Fun(x, make_if (Var x) (App(Var f, Const True)) t2'), t1')
  | App(App(Const And, t1), t2) ->
      let x = new_id "b" in
      let f' = new_id "f" in
      let t1' = trans_eager_bool f' t1 in
      let t2' = trans_eager_bool f t2 in
        Let(f', Fun(x, make_if (Var x) t2' (App(Var f, Const False))), t1')
  | App(Const Not, t) ->
      let x = new_id "b" in
      let f' = new_id "f" in
      let t' = trans_eager_bool f' t in
        Let(f', Fun(x, make_if (Var x) (App(Var f, Const False)) (App(Var f, Const True))), t')
  | App(App(App(Const If, Const RandBool), Const True), Const False) ->
      make_br (App(Var f, Const True)) (App(Var f, Const False))
  | App(App(App(Const If, t1), t2), t3) ->
      let x = new_id "b" in
      let f' = new_id "f" in
      let t1' = trans_eager_bool f' t1 in
      let t2' = trans_eager_bool f t2 in
      let t3' = trans_eager_bool f t3 in
        Let(f', Fun(x, make_if (Var x) t2' t3'), t1')
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
                | t' -> Let(f, Fun(x, t'), trans_eager_bool f t)
            end
      | Const (RandBool | And | Or | Not | Lt | Gt | Leq | Geq | EqInt | EqBool) -> assert false
      | Const _
      | Var _ -> c t
      | Fun(x,t) -> c (Fun(x, trans_eager_term env id t))
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
                      c (Let(f, Fun(x, t'), t1'))
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

let trans_eager ((env,defs,main):prog) : prog =
  env, List.map (trans_eager_def env) defs, main






let rec coerce env cond pts t typ1 typ2 =
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
                  let xs = abst_arg x typ21 in
                  let pts' = make_pts x typ21 @@ pts in
                  let ts = coerce env' cond pts' (Var x) typ21 typ11 in
                  let t' = hd (coerce env' cond pts' (make_app t ts) typ12 typ22) in
                  let t'' = filter env' cond pts' t' in
                    [make_fun_temp xs t'']
              | TFun _, TFun _ ->
                  let x = new_id "f" in
                  let t' = App(t, hd (coerce env' cond pts (Var x) typ21 typ11)) in
                    [Fun(x, hd (coerce env' cond pts t' typ12 typ22))]
              | _ -> assert false
          end
    | _ -> Format.printf "coerce: %a, %a@." print_typ typ1 print_typ typ2; assert false
(*
let coerce env cond pts t typ1 typ2 =
  let t' = coerce env cond pts t typ1 typ2 in
    Format.printf "coerce: %a ==> %a@." print_typ typ1 print_typ typ2;
    Format.printf "        %a ==> [%a]@." print_term t (print_list print_term ";" false) t';
    t'
*)


let rec abstract_term env cond pts t typ =
  if false then Format.printf "abstract_term: %a: %a@." CEGAR_print.print_term t CEGAR_print.print_typ typ;
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
  let typ,env' = decomp_typ (List.assoc f env) xs in
  let env'' = env' @@ env in
  let pts = List.flatten (List.map (fun (x,typ) -> make_pts x typ) env') in
  let xs' = List.flatten (List.map (fun (x,typ) -> abst_arg x typ) env') in
  let t2' = hd (abstract_term env'' [t1] pts t2 typ) in
    if e <> [] && t1 <> Const True
    then
      let g = rename_id f in
      let fv = diff (get_fv t2') (List.map fst env) in
        [g, fv, Const True, e, t2';
         f, xs', Const True, [], assume env' [] pts t1 (make_app (Var g) (List.map (fun x -> Var x) fv))]
    else [f, xs', Const True, e, assume env' [] pts t1 t2']





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
