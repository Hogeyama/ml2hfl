open Util
open Syntax
open Term_util
open Type
open Modular_syntax


let debug () = List.mem "Check_mod" !Flag.debug_module




let print_ce_set fm ce_set =
  let pr fm (f, ce) = Format.fprintf fm "%a: %a" Id.print f (List.print Format.pp_print_int) ce in
  Format.fprintf fm "%a" (print_list pr ",@ ") ce_set

let to_CEGAR_ref_type_base base =
  match base with
  | Ref_type.Unit -> CEGAR_ref_type.Unit
  | Ref_type.Bool -> CEGAR_ref_type.Bool
  | Ref_type.Int -> CEGAR_ref_type.Int
  | Ref_type.Abst s -> CEGAR_ref_type.Abst s
let rec to_CEGAR_ref_type typ =
  match typ with
  | Ref_type.Base(base, x, p) -> CEGAR_ref_type.Base(to_CEGAR_ref_type_base base, Id.to_string x, snd @@ CEGAR_trans.trans_term p)
  | Ref_type.Fun(x, typ1, typ2) -> CEGAR_ref_type.Fun(Id.to_string x, to_CEGAR_ref_type typ1, to_CEGAR_ref_type typ2)
  | Ref_type.Inter(styp, typs) -> CEGAR_ref_type.Inter(CEGAR_trans.trans_typ styp, List.map to_CEGAR_ref_type typs)
  | _ -> unsupported "Ref_type.to_CEGAR_ref_type"

let rec add_id_event t =
  match t.desc with
  | Let(flag, bindings, t') ->
      let bindings' = List.map (Triple.map_trd @@ make_seq (make_event_unit "id")) bindings in
      {t with desc=Let(flag, bindings', add_id_event t')}
  | _ -> t


let make_fix f xs t =
  make_letrec [f, xs, t] @@ make_var f
let decomp_fix t =
  match t.desc with
  | Let(Recursive, [f, xs, t'], {desc=Var g}) when f = g -> Some (f, xs, t')
  | _ -> None
let is_fix t = decomp_fix t <> None

let get_arg_num = List.length -| Triple.snd -| Option.get -| decomp_fix


let bool_of_term' t = Option.try_any (fun _ -> bool_of_term t)

let is_fail t =
  match t.desc with
  | App({desc=Event("fail",_)}, [_]) -> true
  | _ -> false

let exists_fail ts = List.exists is_fail ts

let append_path path rs =
  List.map (Triple.map_trd @@ (@) path) rs

let merge_path path1 path2 =
  let aux (x,ce) acc =
    if Id.mem_assoc x acc then
      List.map (fun (y,ce') -> let ce'' = if Id.same x y then ce@ce' else ce' in y, ce'') acc
    else
      (x,ce)::acc
  in
  List.fold_right aux path1 path2

type dir = Single_to_Modular | Modular_to_Single
type eval_result =
  | Single of (typed_term * (int * int list) list * int list) list
  | Modular of (typed_term * int list * (id * int list) list)

let init_result_of_dir dir t ce_env =
  match dir with
  | Modular_to_Single -> Single [t, ce_env, []]
  | Single_to_Modular -> Modular(t, snd @@ List.get ce_env, [])

let decomp_single r =
  match r with
  | Single rs -> rs
  | Modular _ -> assert false

let decomp_modular r =
  match r with
  | Single _ -> assert false
  | Modular r -> r

let eta_decomp_funs' t =
  match t.desc with
  | Event("fail",false) ->
      let u = Id.new_var TUnit in
      [u], make_app t [make_var u]
  | _ ->
      let xs,t' = decomp_funs t in
      let xs',_ = decomp_tfun t'.typ in
      let xs'' = List.map Id.new_var_id xs' in
      xs@xs'', make_app t' @@ List.map make_var xs''

let counter = Counter.create ()
let new_label () = Counter.gen counter

let add_label = make_trans2 ()
let add_label_term l t =
  let t' = add_label.tr2_term_rec l t in
  match t.desc with
  | If(t1, t2, t3) ->
      assert (Option.is_none @@ get_id_option t');
      add_id l t'
  | _ -> t'
let () = add_label.tr2_term <- add_label_term
let add_label = add_label.tr2_term
let get_label = get_id_option

(* ASSUME: Input must be normal form *)
(* In Single_to_Modular mode, ce_env must be of the form [0, ce] *)
let rec eval dir top_funs fun_env ce_set ce_env label_env t =
  let dbg = 0=0 && !!debug in
  if dbg then Format.printf"@[ORIG: %a@\n  @[" Print.term t;
  let r =
  (if dbg then match dir with Single_to_Modular -> Format.printf "Single to Modular.@\n" | Modular_to_Single -> Format.printf "Modular to Single.@\n");
  if dbg then Format.printf "TOP_FUNS: %a@\n" (List.print Id.print) top_funs;
  if dbg then Format.printf "Dom(FUN_ENV): %a@\n" (List.print Id.print) @@ List.map fst fun_env;
  if dbg then Format.printf "CE_SET: %a@\n" (List.print @@ Pair.print Id.print @@ List.print Format.pp_print_int) ce_set;
  if dbg then Format.printf "CE_ENV: %a@\n" (List.print @@ Pair.print Format.pp_print_int @@ List.print Format.pp_print_int) ce_env;
  if dbg then Format.printf "LABEL_ENV: %a@\n" (List.print @@ Pair.print Format.pp_print_int Id.print) label_env;
  match t.desc with
  | Const _
  | Var _
  | BinOp _
  | Not _
  | Fun _
  | Event _ -> init_result_of_dir dir t ce_env
  | _ when is_fail t -> init_result_of_dir dir t ce_env
  | Bottom ->
      assert (dir = Modular_to_Single);
      if dbg then Format.printf "BOTTOM@\n";
      Single []
  | App({desc=Const(RandValue _)}, [t2]) ->
      if dbg then Format.printf "Check_mod.eval APP1@\n";
      assert (t2 = unit_term);
      init_result_of_dir dir t ce_env
  | App({desc=Fun(x,t1)}, v::vs) when List.for_all is_value vs ->
      if dbg then Format.printf "Check_mod.eval APP2@\n";
      let t' = make_app (subst x v t1) vs in
      eval dir top_funs fun_env ce_set ce_env label_env t'
  | App(t1, ts) when is_fix t1 ->
      if dbg then Format.printf "Check_mod.eval APP3@\n";
      let n = get_arg_num t1 in
      if n < List.length ts then
        init_result_of_dir dir t ce_env
      else if n > List.length ts then
        unsupported "Check_mod.eval: App(fix, _)"
      else
        let f,xs,t1' = Option.get @@ decomp_fix t1 in
        let t1'' = subst f t1 t1' in
        let t' = List.fold_right2 subst xs ts t1'' in
        eval dir top_funs fun_env ce_set ce_env label_env t'
  | App({desc=Var f}, ts) when List.length ts > List.length @@ fst @@ Id.assoc f fun_env ->
      if dbg then Format.printf "Check_mod.eval APP4@\n";
      let n = List.length @@ fst @@ Id.assoc f fun_env in
      if dbg then Format.printf "Check_mod.eval App %a %d@\n" Id.print f n;
      if dbg then Format.printf "Check_mod.eval App %a@\n" Print.term  @@ snd @@ Id.assoc f fun_env;
      let ts1,ts2 = List.split_nth n ts in
      eval dir top_funs fun_env ce_set ce_env label_env @@ make_app_raw (make_app (make_var f) ts1) ts2
  | App({desc=Var f}, ts) ->
      if dbg then Format.printf "Check_mod.eval APP5@\n";
      if dbg then Format.printf "ASSOC: %a@\n" Id.print f;
      let ys,t_f = Id.assoc f fun_env in
      assert (List.length ts <= List.length ys);
      if List.length ts < List.length ys then
        init_result_of_dir dir t ce_env
      else
        begin
          match dir with
          | Modular_to_Single ->
              let label = new_label () in
              if dbg then Format.printf "LABEL: %a, %d@\n  @[" Id.print f label;
              let label_env' = (label,f)::label_env in
              let t_f' = if Id.mem f top_funs then add_label label t_f else t_f in
              let paths =
                if Id.mem f top_funs then
                  List.assoc_all ~cmp:Id.eq f ce_set
                else
                  [[]]
              in
              if dbg then Format.printf "PATHS: %a@\n" (List.print @@ List.print Format.pp_print_int) paths;
              if paths = [] then
                let xs,typ = Type.decomp_tfun t_f.typ in
                let t' = make_funs xs @@ unit_term in
                if dbg then Format.printf "typ: %a@\n" Print.typ typ;
                assert (can_unify typ TUnit);
                Single [t', ce_env, []]
              else
                let aux path =
                  if dbg then Format.printf "PATH: %d, %a@\n" label (List.print Format.pp_print_int) path;
                  let t' = make_app (make_fix f ys t_f') ts in
                  let ce_env' = (label,path)::ce_env in
                  decomp_single @@ eval dir top_funs fun_env ce_set ce_env' label_env' t'
                in
                let r = List.flatten_map aux paths in
                if dbg then Format.printf "@]";
                Single r
          | Single_to_Modular ->
              let t' = make_app (make_fix f ys t_f) ts in
              eval dir top_funs fun_env ce_set ce_env label_env t'
        end
  | App(t1, t2::ts) ->
      if dbg then Format.printf "Check_mod.eval APP6@\n";
      assert (not @@ is_value t1);
      assert (List.for_all is_value @@ t2::ts);
      let r = eval dir top_funs fun_env ce_set ce_env label_env t1 in
      begin
        match r with
        | Single rs ->
            let aux (v,ce_env,path) =
              if is_fail v then
                [fail_unit_term, ce_env, path]
              else
                match v.desc with
                | Fun(x,t1') -> decomp_single @@ eval dir top_funs fun_env ce_set ce_env label_env @@ make_app (subst x t2 t1') ts
                | _ -> assert false
            in
            let rs' = List.flatten_map aux rs in
            Single rs'
        | Modular(v, ce, paths) ->
            if is_fail v then
              Modular(fail_unit_term, ce, paths)
            else
              match v.desc with
              | Fun(x,t1') -> eval dir top_funs fun_env ce_set [0,ce] label_env @@ make_app (subst x t2 t1') ts
              | _ -> assert false
      end
  | If(_, t2, t3) ->
      begin
        match dir with
        | Modular_to_Single ->
            let label = get_label t in
            let r =
              match label with
              | None ->
                  let aux br =
                    if dbg then Format.printf "BRANCH2: %s@\n" (if br=0 then "then" else "else");
                    let t23 = if br = 0 then t2 else t3 in
                    append_path [br] @@ decomp_single @@ eval dir top_funs fun_env ce_set ce_env label_env t23
                  in
                  aux 1 @ aux 0
              | Some label ->
                  let ce,ce_env' = List.decomp_assoc label ce_env in
                  match ce with
                  | [] -> []
                  | br::ce' ->
                      if dbg then Format.printf "CE[%d]: %a@\n" label (List.print Format.pp_print_int) ce;
                      let t23 = if br = 0 then t2 else t3 in
                      let ce_env'' = (label,ce')::ce_env' in
                      if dbg then Format.printf "BRANCH1: %s@\n" (if br=0 then "then" else "else");
                      append_path [br] @@ decomp_single @@ eval dir top_funs fun_env ce_set ce_env'' label_env t23
            in
            Single r
        | Single_to_Modular ->
            let label = get_label t in
            let f,ce = List.get ce_env in
            match ce with
            | [] ->
                assert (label = None);
                assert false
            | br::ce' ->
                let t23 = if br = 0 then t2 else t3 in
                let v,ce_env,paths = decomp_modular @@ eval dir top_funs fun_env ce_set [f,ce'] label_env t23 in
                let paths' =
                  match label with
                  | None -> paths
                  | Some label ->
                      if dbg then Format.printf "LABEL: %d@\n" label;
                      if dbg then Format.printf "LABEL_ENV: %a@\n" (List.print @@ Pair.print Format.pp_print_int Id.print) label_env;
                      let f = List.assoc label label_env in
                      if Id.mem_assoc f paths then
                        List.map (fun (g,path) -> g, if Id.eq f g then br::path else path) paths
                      else
                        (f,[br])::paths
                in
                if dbg then Format.printf "PATHS': %a@\n" (List.print @@ Pair.print Id.print @@ List.print Format.pp_print_int) paths';
                Modular(v, ce_env, paths')
      end
  | Let _ when is_fix t -> init_result_of_dir dir t ce_env
  | Let(flag, [], t2) -> eval dir top_funs fun_env ce_set ce_env label_env t2
  | Let(Nonrecursive, [f,xs,t1], t2) ->
      if xs = [] then
        let fun_env' = if is_base_typ t1.typ then fun_env else (f, eta_decomp_funs' t1)::fun_env in
        match eval dir top_funs fun_env ce_set ce_env label_env t1 with
        | Single rs ->
            let aux (v,ce_env,path) =
              if is_fail v then
                [fail_unit_term, ce_env, path]
              else
                t2
                |> eval dir top_funs fun_env' ce_set ce_env label_env
                |> decomp_single
                |> append_path path
            in
            Single (List.flatten_map aux rs)
        | Modular(v, ce, path) ->
            if is_fail v then
              Modular(fail_unit_term, ce, path)
            else
              match eval dir top_funs fun_env' ce_set [0,ce] label_env t2 with
              | Single _ -> assert false
              | Modular(v', ce', path') -> Modular(v', ce', merge_path path path')
      else
        let t2' = subst f (make_funs xs t1) t2 in
        eval dir top_funs fun_env ce_set ce_env label_env t2'
  | Let(Recursive, [f,xs,t1], t2) ->
      assert (xs <> []);
      let t2' = subst f (make_fix f xs t1) t2 in
      eval dir top_funs fun_env ce_set ce_env label_env t2'
  | Raise t ->
      assert false
  | TryWith(t1, t2) ->
      begin
        match eval dir top_funs fun_env ce_set ce_env label_env t1 with
        | Single _ -> assert false
        | Modular({desc=Raise _}, ce, path) ->
            assert false (*TODO*)
        | Modular(v, ce, path) ->
            Modular(v, ce, path)
      end
  | _ ->
      Format.printf "@.%a@." Print.term t;
      unsupported "Check_mod.eval"
  in
  if dbg then
    if dir=Modular_to_Single then
      Format.printf"@]@\nRETURN %a@\n@]" (List.print @@ List.print Format.pp_print_int) @@ List.map Triple.trd @@ decomp_single r
    else
      Format.printf"@]@\nRETURN@\n@]";
  r

type result =
  | Typable of Ref_type.env
  | Untypable of (Syntax.id * int list) list

let add_context prog f typ =
  let {fun_typ_env=env; fun_def_env=fun_env} = prog in
  let dbg = 0=0 && !!debug in
  if dbg then Format.printf "ADD_CONTEXT prog: %a@." print_prog prog;
  if dbg then Format.printf "ADD_CONTEXT: %a :? %a@." Print.id f Ref_type.print typ;
  let fs =
    let xs,t = Id.assoc f fun_env in
    List.Set.diff ~cmp:Id.eq (get_fv t) (f::xs)
  in
  let label_env = List.mapi (fun i f -> f, i) fs in
  let label_env = [f, 0] in
  let t' =
    unit_term
    |> Trans.ref_to_assert @@ Ref_type.Env.of_list [f,typ]
    |@dbg&> Format.printf "ADD_CONTEXT t: %a@." Print.term
    |> normalize
    |@dbg&> Format.printf "ADD_CONTEXT t': %a@." Print.term
  in
  let fun_env' =
    env
    |> Ref_type.Env.filter_key (Id.mem -$- fs)
    |@dbg&> (fun x -> Format.printf "AAA@.")
    |> Ref_type.Env.to_list
    |@dbg&> (fun x -> Format.printf "AAA@.")
    |> List.map (Pair.map_snd @@ decomp_funs -| Triple.trd -| Ref_type.generate [] [])
    |@dbg&> Format.printf "ADD_CONTEXT fun_env': %a@." Modular_syntax.print_def_env
  in
  let fun_env'' =
    List.map (Pair.map_snd @@ Pair.map_snd normalize) fun_env' @
    [f, Pair.map_snd (add_label 0 -| normalize) @@ Id.assoc f fun_env]
  in
  t', fun_env'', List.map Pair.swap label_env

let check prog f typ =
  if !!debug then Format.printf "MAIN_LOOP prog: %a@." print_prog prog;
  let {fun_typ_env=env; fun_def_env=fun_env} = prog in
  let t,fun_env',label_env = add_context prog f typ in
  let top_funs = List.map fst fun_env' in
  if !!debug then Format.printf "  Check %a : %a@." Id.print f Ref_type.print typ;
  if !!debug then Format.printf "  t: %a@." Print.term_typ t;
  if !!debug then Format.printf "  t with def: %a@.@." Print.term @@ make_letrecs (List.map Triple.of_pair_r fun_env') t;
  let make_pps spec =
    let open Main_loop in
    preprocesses spec
    |> preprocess_and_after CPS
  in
  let (result, make_get_rtyp, set_target'), main, set_target =
    t
    |> make_letrecs (List.map Triple.of_pair_r fun_env')
    |@> Type_check.check -$- TUnit
    |> Trans.map_main (make_seq -$- unit_term)
    |@!!debug&> Format.printf "  Check: %a@." Print.term_typ
    |@> Type_check.check -$- TUnit
    |> Main_loop.verify ~make_pps:(Some(make_pps)) ~fun_list:(Some []) [] Spec.init
  in
  match result with
  | CEGAR.Safe env ->
      if !!debug then Format.printf "  Typable@.";
      if !!debug then Format.printf "  env: %a@." (List.print @@ Pair.print Format.pp_print_string CEGAR_ref_type.print) env;
      let env' = (f,typ) :: Main_loop.trans_env (List.map fst fun_env) make_get_rtyp env in
      if !!debug then Format.printf "  env': %a@." (List.print @@ Pair.print Id.print Ref_type.print) env';
      Typable (Ref_type.Env.of_list env')
  | CEGAR.Unsafe(sol, ModelCheck.CESafety ce) ->
      if !!debug then Format.printf "  Untypable@.@.";
      if !!debug then Format.printf "  CE_INIT: %a@\n" (List.print Format.pp_print_int) ce;
      let v,ce',paths = decomp_modular @@ eval Single_to_Modular top_funs fun_env' [] [0,ce] label_env t in
      if !!debug then Format.printf "  PATHS: %a@\n" (List.print @@ Pair.print Id.print @@ List.print Format.pp_print_int) paths;
      assert (v = fail_unit_term);
      assert (ce' = []);
      let paths' = List.filter_map (fun (f,_) -> if Id.mem_assoc f paths then None else Some(f,[])) fun_env @ paths in
      if !!debug then Format.printf "  PATHS': %a@\n" (List.print @@ Pair.print Id.print @@ List.print Format.pp_print_int) paths';
      Untypable paths'
  | CEGAR.Unsafe _ -> assert false
