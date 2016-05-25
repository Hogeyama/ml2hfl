open Util
open Syntax
open Term_util
open Type

let debug () = List.mem "Modular" !Flag.debug_module

module CT = Comp_tree

let remove_ext_def = make_trans2 ()

let remove_ext_def_desc ext desc =
  match desc with
  | Let(flag, bindings, t) ->
      let bindings' = List.filter_out (fun (f,_,_) -> Id.mem f ext) bindings in
      let t' = remove_ext_def.tr2_term ext t in
      if bindings' = [] then t'.desc else Let(flag, bindings', t')
  | _ -> remove_ext_def.tr2_desc_rec ext desc

let () = remove_ext_def.tr2_desc <- remove_ext_def_desc
let remove_ext_def = remove_ext_def.tr2_term

let divide spec t ref_env =
  if !!debug then Format.printf "PROGRAM: %a@." Print.term t;
  if !!debug then Format.printf "ORIG: %a@." (List.print Print.id) @@ get_top_funs t;
  let ext = List.map fst ref_env in
  let t_main = remove_ext_def ext t in
  if !!debug then Format.printf "MAIN: %a@." (List.print Print.id) @@ get_top_funs t_main;
  let make_spec f =
    let ref_env,ext_ref_env = List.partition (Id.eq f -| fst) ref_env in
    let aux (_,typ) =
      if not @@ Type.same_shape (Id.typ f) (Ref_type.to_simple typ) then
        begin
          Format.printf "VAR: %a@." Id.print f;
          Format.printf "  Prog: %a@." Print.typ @@ Id.typ f;
          Format.printf "  Spec: %a@." Ref_type.print typ;
          fatal @@ Format.sprintf "Type of %s in the specification is wrong?" @@ Id.name f
        end
    in
    List.iter aux ref_env;
    let spec' = {spec with Spec.ref_env; Spec.ext_ref_env = ext_ref_env @ spec.Spec.ext_ref_env} in
    if !!debug then Format.printf "SUB[%a]: %a@." Print.id f Spec.print spec';
    spec'
  in
  let targets = List.map (fun f -> Id.to_string f, make_spec f, t) ext in
  if !!debug then Format.printf "MAIN: %a@." Print.term t_main;
  ("MAIN", make_spec (Id.new_var ~name:"MAIN" TUnit), t_main)::targets


let main orig spec parsed =
  let verify (s,spec,t) =
    if !!debug then Format.printf "Start verification of %s:@.%a@." s Spec.print spec;
    s, Main_loop.run orig [] ~spec t
  in
  Spec.get_ref_env spec parsed
  |@(not !Flag.only_result)&> Spec.print_ref_env Format.std_formatter
  |> divide spec parsed
  |> List.map verify
  |@> Format.printf "RESULT: %a@." (List.print @@ Pair.print Format.pp_print_string Format.pp_print_bool)
  |> List.for_all snd





(************************************************************************************************************)
(************************************************************************************************************)
(************************************************************************************************************)
(************************************************************************************************************)
(************************************************************************************************************)
(************************************************************************************************************)




module RefTypInfer = struct
  open Fpat
  open Util
  open Combinator

  let infer_etrs fs is_cp prog etrs =
    etrs
    |> Fpat.RefTypInfer.infer_etrs fs is_cp prog
    |*@> Format.printf "refinement types:@,  %a@." Fpat.RefType.pr_env
(*
    |> List.map (Pair.map_snd Fpat.AbsType.of_refinement_type)
    |> Fpat.Util.List.classify (Fpat.Combinator.comp2 (=) fst fst)
    |> List.map
         (function
           | (f, sty) :: fstys ->
              f, Fpat.AbsType.merge (sty :: List.map snd fstys)
           | _ -> assert false)
*)

  let refine prog fs is_cp cexs feasible ext_cexs =
    let etrs =
      Fpat.Util.List.concat_map2
        (fun cex ext_cex ->
         let penv =
           List.map
             (fun (p, ps) ->
              let cnt = ref 0 in
              p,
              fun ts ->
              let (tenv, phi) = List.nth ps !cnt in
              let tenv = tenv @ [p, Type.mk_int] in
              cnt := !cnt + 1;
              Logger.debug_assert
                (fun () -> List.length tenv = List.length ts)
                ~on_failure:
                (fun () ->
                 Format.printf
                   "AbsTypInfer.refine: the lengths of %a and %a are different"
                   TypEnv.pr tenv
                   Term.pr_list ts);
              let tsub = List.map2 (fun (x, _) t -> x, t) tenv ts in
              let tts = List.map2 (fun (_, ty) t -> t, ty) tenv ts in
              Pva.make
                (Idnt.T(Idnt.T(p, !cnt, List.length tenv - 1), -1, 0))
                tts,
              Formula.subst tsub phi)
             ext_cex
         in
         CompTreeExpander.error_traces_of prog feasible penv [cex])
        cexs ext_cexs
    in
    infer_etrs fs is_cp prog etrs
end



type program = (id * typed_term) list

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
  | Ref_type.Inter typs -> CEGAR_ref_type.Inter (List.map to_CEGAR_ref_type typs)
  | _ -> unsupported "Ref_type.to_CEGAR_ref_type"

let rec add_id_event t =
  match t.desc with
  | Let(flag, bindings, t') ->
      let bindings' = List.map (Triple.map_trd @@ make_seq (make_event_unit "id")) bindings in
      {t with desc=Let(flag, bindings', add_id_event t')}
  | _ -> t

let infer_ref_type ces t =
  let dbg = 0=0 in
  if dbg && !!debug then Format.printf "infer_ref_type t: %a@." Print.term t;
  if dbg && !!debug then Format.printf "infer_ref_type ces: %a@." (List.print @@ List.print Format.pp_print_int) ces;
  Main_loop.init_typ_excep ();
  let t' = add_id_event t in
  if dbg && !!debug then Format.printf "infer_ref_type t': %a@." Print.term t';
  let prog, make_get_rtyp, info = Main_loop.preprocess t' Spec.init in
  if dbg && !!debug then Format.printf "prog: %a@." CEGAR_print.prog prog;
  FpatInterface.init prog;
  let labeled = CEGAR_abst_util.has_branch prog in
  let is_cp = FpatInterface.is_cp prog in
  let inlined_functions = CEGAR_util.inlined_functions info.CEGAR_syntax.orig_fun_list info.CEGAR_syntax.inlined prog in
  if dbg && !!debug then Format.printf "inliend_functions: [%s]@." @@ String.join "; " inlined_functions;
  let ces' = List.map (CEGAR_trans.trans_ce labeled prog) ces in
  let prog = FpatInterface.conv_prog prog in
  let env = RefTypInfer.refine prog inlined_functions is_cp ces' false (List.map (Fun.const []) ces) in
  if dbg && !!debug then Format.printf "ENV: @[%a@." Fpat.RefType.pr_env env;
  let get_rtyp f =
    List.assoc (FpatInterface.conv_var f) env
    |*@> Format.printf "typ1: %a@." Fpat.RefType.pr
    |> Ref_type.from_fpat
    |*@> Format.printf "typ2: %a@." Ref_type.print
    |> to_CEGAR_ref_type
    |*@> Format.printf "typ3: %a@." CEGAR_ref_type.print
  in
  make_get_rtyp get_rtyp

(*
let infer_abs_type spec ces parsed =
  assert (spec.Spec.ref_env <> []);
  let ref_env = Spec.get_ref_env spec parsed |@ not !Flag.only_result &> Spec.print_ref_env Format.std_formatter in
  let t,main = Trans.ref_to_assert ref_env parsed in
  Main_loop.init_typ_excep ();
  let prog, get_rtyp, info = Main_loop.preprocess t spec in
  FpatInterface.init prog;
  let labeled = CEGAR_abst_util.has_branch prog in
  let is_cp = FpatInterface.is_cp prog in
  let inlined_functions = CEGAR_util.inlined_functions info.CEGAR_syntax.orig_fun_list info.CEGAR_syntax.inlined prog in
  let ces' = List.map (CEGAR_trans.trans_ce labeled prog) ces in
  let prog = FpatInterface.conv_prog prog in
  let typ =
    RefTypInfer.refine prog inlined_functions is_cp ces' false (List.map (Fun.const []) ces)
    |@true&> Format.printf "ENV: @[%a@." Fpat.RefType.pr_env
  in
  typ, main
*)

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

let decomp_funs' t =
  match t.desc with
  | Event("fail",false) ->
      let u = Id.new_var TUnit in
      [u], make_app t [make_var u]
  | _ ->  decomp_funs t

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
let rec eval dir top_funs fun_env ce_set ce_env label_env t =
  let dbg = 0=9 in
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
      assert (t2 = unit_term);
      init_result_of_dir dir t ce_env
  | App({desc=Fun(x,t1)}, v::vs) when List.for_all is_value vs ->
      let t' = make_app (subst x v t1) vs in
      eval dir top_funs fun_env ce_set ce_env label_env t'
  | App(t1, ts) when is_fix t1 ->
      let n = get_arg_num t1 in
      if n < List.length ts then
        init_result_of_dir dir t ce_env
      else if n > List.length ts then
        unsupported "Modular.eval: App(fix, _)"
      else
        let f,xs,t1' = Option.get @@ decomp_fix t1 in
        let t1'' = subst f t1 t1' in
        let t' = List.fold_right2 subst xs ts t1'' in
        eval dir top_funs fun_env ce_set ce_env label_env t'
  | App({desc=Var f}, ts) when List.length ts > List.length @@ fst @@ Id.assoc f fun_env ->
      let n = List.length @@ fst @@ Id.assoc f fun_env in
      if dbg then Format.printf "Modular.eval App %a %d@\n" Id.print f n;
      if dbg then Format.printf "Modular.eval App %a@\n" Print.term  @@ snd @@ Id.assoc f fun_env;
      let ts1,ts2 = List.split_nth n ts in
      eval dir top_funs fun_env ce_set ce_env label_env @@ make_app_raw (make_app (make_var f) ts1) ts2
  | App({desc=Var f}, ts) ->
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
  | App(t1, [t2]) ->
      assert (not @@ is_value t1);
      assert (is_value t2);
      let r = eval dir top_funs fun_env ce_set ce_env label_env t1 in
      begin
        match r with
        | Single rs ->
            let aux (v,ce_env,path) =
              if is_fail v then
                [fail_unit_term, ce_env, path]
              else
                match v.desc with
                | Fun(x,t1') -> decomp_single @@ eval dir top_funs fun_env ce_set ce_env label_env @@ subst x t2 t1'
                | _ -> assert false
            in
            let rs' = List.flatten_map aux rs in
            Single rs'
        | Modular(v, ce, paths) ->
            if is_fail v then
              Modular(fail_unit_term, ce, paths)
            else
              match v.desc with
              | Fun(x,t1') -> eval dir top_funs fun_env ce_set ce_env label_env @@ subst x t2 t1'
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
            | [] -> assert false
            | br::ce' ->
                let t23 = if br = 0 then t2 else t3 in
                let v,ce_env,paths = decomp_modular @@ eval dir top_funs fun_env ce_set [f,ce'] label_env t23 in
                let paths' =
                  match label with
                  | None -> paths
                  | Some label ->
                      Format.printf "LABEL: %d@\n" label;
                      Format.printf "LABEL_ENV: %a@\n" (List.print @@ Pair.print Format.pp_print_int Id.print) label_env;
                      let f = List.assoc label label_env in
                      if Id.mem_assoc f paths then
                        List.map (fun (g,path) -> g, if Id.eq f g then br::path else path) paths
                      else
                        (f,[br])::paths
                in
                Format.printf "PATHS': %a@\n" (List.print @@ Pair.print Id.print @@ List.print Format.pp_print_int) paths';
                Modular(v, ce_env, paths')
      end
  | Let _ when is_fix t -> init_result_of_dir dir t ce_env
  | Let(flag, [], t2) -> eval dir top_funs fun_env ce_set ce_env label_env t2
  | Let(flag, [f,xs,t1], t2) ->
      assert (flag = Nonrecursive || not @@ Id.mem f @@ Term_util.get_fv t1);
      if xs = [] then
        let fun_env' = if is_base_typ t1.typ then fun_env else (f, decomp_funs' t1)::fun_env in
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
              eval dir top_funs fun_env' ce_set [0,ce] label_env t2
      else
        let t2' = subst f (make_funs xs t1) t2 in
        eval dir top_funs fun_env ce_set ce_env label_env t2'
  | _ ->
      Format.printf "@.%a@." Print.term t;
      unsupported "Modular.eval"
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

let add_context = Infer_mod.add_context

let infer env fun_env f typ ce_set =
  let t,top_funs,fun_env',label_env = add_context true env fun_env f typ in
  if !!debug then Format.printf "  FUN_ENV': %a@." (List.print @@ Pair.print Id.print Print.term) @@ List.map (Pair.map_snd snd) fun_env';
  let t' = make_letrecs (List.map Triple.of_pair_r fun_env') t in
  if !!debug then Format.printf "  Infer: %a : %a@." Print.term t Ref_type.print typ;
  let ce_set' = List.filter (fun (f,_) -> List.exists (fun (_,g) -> Id.eq f g) label_env) ce_set in
  if !!debug then Format.printf "  CE_SET': %a@." (List.print @@ Pair.print Id.print (List.print Format.pp_print_int)) ce_set';
  let rs = decomp_single @@ eval Modular_to_Single top_funs fun_env' ce_set' [] [] t in
  let rs' = List.filter (is_fail -| Triple.fst) rs in
  let ces = List.map (Triple.trd) rs' in
  if !!debug then Format.printf "@.  Counterexamples: %a@." (List.print @@ List.print Format.pp_print_int) ces;
  let get_rtyp = infer_ref_type ces t' in
  let funs = List.takewhile (not -| Id.eq f) @@ List.map fst fun_env in
  if !!debug then Format.printf "@.  funs: %a@," (List.print Id.print) funs;
  let cand_env = List.map (Pair.add_right get_rtyp) funs in (* TODO: fix *)
  if !!debug then Format.printf "@.  candidate refinement types:@,  %a@," (List.print @@ Pair.print Id.print Ref_type.print) cand_env;
  assert (not @@ List.exists (fun (f,typ) -> List.exists (fun (g,typ') -> Id.same f g && Ref_type.same typ typ') env) cand_env);
  cand_env

let check env f fun_env typ =
  let t,top_funs,fun_env',label_env = add_context false env fun_env f typ in
  if !Flag.print_progress then Format.printf "  Check %a : %a@." Id.print f Ref_type.print typ;
  let (result, make_get_rtyp, set_target'), main, set_target =
    t
    |> make_letrecs (List.map Triple.of_pair_r fun_env')
    |@ !!debug&> Format.printf "  Check: %a@." Print.term
    |> Trans.map_main (make_seq -$- unit_term)
    |> Main_loop.verify [] Spec.init
  in
  match result with
  | CEGAR.Safe env ->
      if !!debug then Format.printf "  Typable@.";
      if !!debug then Format.printf "  env: %a@." (List.print @@ Pair.print Format.pp_print_string CEGAR_ref_type.print) env;
      let env' = (f,typ) :: Main_loop.trans_env (List.map fst fun_env) make_get_rtyp env in
      if !!debug then Format.printf "  env': %a@." (List.print @@ Pair.print Id.print Ref_type.print) env';
      Typable env'
  | CEGAR.Unsafe(sol, ModelCheck.CESafety ce) ->
      if !!debug then Format.printf "  Untypable@.@.";
      if !!debug then Format.printf "  CE_INIT: %a@\n" (List.print Format.pp_print_int) ce;
      let v,ce',paths = decomp_modular @@ eval Single_to_Modular top_funs fun_env' [] [0,ce] label_env t in
      assert (v = fail_unit_term);
      assert (ce' = []);
      let paths' = List.filter_map (fun (f,_) -> if Id.mem_assoc f paths then None else Some(f,[])) fun_env @ paths in
      Untypable paths'
  | CEGAR.Unsafe _ -> assert false

let merge_tenv env' env =
  List.fold_left (fun acc (f,typ) -> if Id.mem_assoc f acc then acc else (f,typ)::acc) [] (env' @ env)

let merge_ce_set ce_set' ce_set =
  let ce_set'' = ce_set' @ ce_set in
  let fs = List.unique ~cmp:Id.eq @@ List.map fst ce_set'' in
  List.map (fun f -> f, List.unique @@ List.flatten @@ List.assoc_all f ce_set'') fs











let rec main_loop c orig env fun_env f typ ce_set =
  if !!debug then Format.printf "MAIN_LOOP[%a,%d]: %a :? %a@." Id.print f c Id.print f Ref_type.print typ;
  if !!debug then Format.printf "MAIN_LOOP[%a,%d] env: %a@." Id.print f c (List.print @@ Pair.print Id.print Ref_type.print) env;
  match check env f fun_env typ with
  | Typable env' ->
      if !!debug then Format.printf "TYPABLE: %a : %a@.@." Id.print f Ref_type.print typ;
      Typable (merge_tenv env' env)
  | Untypable ce_set' ->
      if !!debug then Format.printf "UNTYPABLE: %a : %a@." Id.print f Ref_type.print typ;
      if !!debug then Format.printf "UNTYPABLE ce_set': %a@.@." (List.print @@ Pair.print Id.print @@ List.print Format.pp_print_int) ce_set';
      let rec refine_loop ce_set'' =
        let candidate = Infer_mod.infer env fun_env f typ ce_set'' in
        let aux acc (g,typ') =
          if !!debug then Format.printf "CHECK CANDIDATE: %a :? %a@.@." Id.print g Ref_type.print typ';
          match acc with
          | Typable env' -> main_loop 0 orig env' fun_env g typ' ce_set''
          | Untypable ce_set -> Untypable ce_set
        in
        match List.fold_left aux (Typable env) candidate with
        | Typable env' ->
            if !!debug then Format.printf "ALL CANDIDATES ARE VALID@.@.";
            main_loop (c+1) orig env' fun_env f typ ce_set''
        | Untypable ce_set''' ->
            if !!debug then Format.printf "CANDIDATE IS INVALID@.@.";
            refine_loop (merge_ce_set ce_set''' ce_set'')
      in
      refine_loop (merge_ce_set ce_set' ce_set)
let main_loop orig env fun_env f typ = main_loop 0 orig env fun_env f typ []

let main orig spec parsed =
  if spec <> Spec.init then unsupported "Modular.main: spec";
  let normalized = Infer_mod.normalize parsed in
  if !!debug then Format.printf "NORM: %a@.@." Print.term normalized;
  let fbindings,body = decomp_prog normalized in
  assert (body.desc = Const Unit);
  List.iter (fun (flag,bindings) -> if flag=Recursive then assert (List.length bindings=1)) fbindings;
  let fun_env = List.flatten_map (fun (_,bindings) -> List.map Triple.to_pair_r bindings) fbindings in
  let _,(main,_) = List.decomp_snoc fun_env in
  let fun_env =
    let fs = List.flatten_map (snd |- List.map Triple.fst) fbindings in
    parsed
    |> Trans.ref_to_assert [main, Ref_type.from_simple @@ Id.typ main]
    |*@> Format.printf "parsed: %a@." Print.term
    |> CPS.trans
    |> fst
    |> Trans.direct_from_CPS
    |*@> Format.printf "CPS: %a@." Print.term
    |> decomp_prog
    |> fst
    |> List.flatten_map (snd |- List.map Triple.to_pair_r)
    |> List.filter (fst |- Id.mem -$- fs)
    |> List.map @@ Pair.map_snd @@ Pair.map_snd Infer_mod.normalize
  in
  if !!debug then Format.printf "FUN_ENV: %a@." (List.print @@ Pair.print Id.print Print.term) @@ List.map (Pair.map_snd @@ Fun.uncurry make_funs) fun_env;
  let typ = Ref_type.from_simple @@ Id.typ main in
  let env_init = List.map (fun (f,_) -> f, Ref_type.make_weakest @@ Trans.inst_tvar_tunit_typ @@ Id.typ f) fun_env in
  if !!debug then Format.printf "ENV_INIT: %a@." (List.print @@ Pair.print Id.print Ref_type.print) env_init;
  match main_loop parsed env_init fun_env main typ with
  | Typable _ -> true
  | Untypable _ -> false
