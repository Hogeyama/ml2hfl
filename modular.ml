open Util
open Syntax
open Term_util
open Type
open Modular_syntax

let debug () = List.mem "Modular" !Flag.debug_module

type result =
  | Typable
  | Untypable

exception NoProgress

let merge_tenv env' env = Ref_type.Env.merge env' env (*
  let aux acc (f,typ) =
(*
    Format.printf "MERGE_TENV acc: %a@\n" print_typ_env @@ Ref_type.Env.of_list acc;
 *)
    if Id.mem_assoc f acc then
      List.map (fun (g,typ') -> g, if Id.same f g then Ref_type.inter (Ref_type.to_simple typ) [typ;typ'] else typ') acc
    else
      (f,typ)::acc
  in
  Ref_type.Env.to_list env' @ Ref_type.Env.to_list env
  |> List.fold_left aux []
  |> Ref_type.Env.of_list*)
(*
let merge_tenv env1 env2 =
  Format.printf "MERGE_TENV env1: %a@\n" print_typ_env env1;
  Format.printf "MERGE_TENV env1: %a@\n" print_typ_env env2;
  let r = merge_tenv env1 env2 in
  Format.printf "MERGE_TENV r: %a@\n" print_typ_env r;
  r
 *)
let merge_tenv env' env = (* ??? *)
  Ref_type.Env.of_list @@ List.fold_right (fun (x,typ) acc -> if Id.mem_assoc x acc then acc else (x,typ)::acc) (Ref_type.Env.to_list env) (Ref_type.Env.to_list env')

let merge_ce_set ce_set' ce_set =
  let dbg = 0=1 in
  let cmp (x,ce) (x',ce') = Id.same x x' && ce = ce' in
  let ce_set'' = List.unique ~cmp @@ ce_set' @ ce_set in
  List.filter_out (fun (x,ce) -> ce = [] && 2 <= List.length @@ List.assoc_all ~cmp:Id.eq x ce_set'') ce_set''
  |@dbg&> Format.printf "MERGE_CE_SET: %a@." (List.print @@ Pair.print Id.print @@ List.print Format.pp_print_int)

let is_closed f def_env =
  let ys,t = Id.assoc f def_env in
  List.Set.subset ~cmp:Id.eq (get_fv t) (f::ys)

let rec main_loop c d prog cmp f typ ce_set =
  if !!debug then Format.printf "@[<hov 2>#[MAIN_LOOP]{%a,%d,%d} prog:@ %a@." Id.print f c d print_prog prog;
  if !!debug then Format.printf "@[<hov 2>#[MAIN_LOOP]{%a,%d,%d} ce_set:@ %a@." Id.print f c d (List.print @@ Pair.print Id.print @@ List.print Format.pp_print_int) ce_set;
  let {fun_typ_env=env; fun_def_env} = prog in
  if !!debug then Format.printf "@[<hov 2>#[MAIN_LOOP]{%a,%d,%d}:@ %a :? %a@." Id.print f c d Id.print f Ref_type.print typ;
  match Check_mod.check prog f typ with
  | Check_mod.Typable env' ->
      if !!debug then Format.printf "  @[<hov 2>#[MAIN_LOOP] TYPABLE{%a,%d,%d}: %a :@ %a@.@." Id.print f c d Id.print f Ref_type.print typ;
      Typable, merge_tenv env' env, ce_set
  | Check_mod.Untypable ce_set1 when is_closed f fun_def_env ->
      if !!debug then Format.printf "  @[<hov 2>#[MAIN_LOOP] UNTYPABLE{%a,%d,%d} (closed):@ %a : %a@.@." Id.print f c d Id.print f Ref_type.print typ;
      Untypable, prog.fun_typ_env, merge_ce_set ce_set ce_set1
  | Check_mod.Untypable ce_set1 ->
      if !!debug then Format.printf "  @[<hov 2>#[MAIN_LOOP] UNTYPABLE{%a,%d,%d}:@ %a : %a@." Id.print f c d Id.print f Ref_type.print typ;
      if !!debug then Format.printf "  @[<hov 2>#[MAIN_LOOP] UNTYPABLE{%a,%d,%d} ce_set1:@ %a@.@." Id.print f c d (List.print @@ Pair.print Id.print @@ List.print Format.pp_print_int) ce_set1;
      let rec refine_loop ce_set2 =
        if !!debug then Format.printf "@[<hov 2>#[MAIN_LOOP]{%a,%d,%d} ce_set2:@ %a@." Id.print f c d (List.print @@ Pair.print Id.print @@ List.print Format.pp_print_int) ce_set2;
        match Infer_mod.infer prog f typ ce_set2 with
        | None ->
            Untypable, prog.fun_typ_env, ce_set2
        | Some candidate ->
            let candidate' = List.sort ~cmp:(Compare.on ~cmp fst) @@ Ref_type.Env.to_list candidate in
            if !!debug then Format.printf "    @[<hov 2>#[MAIN_LOOP] CANDIDATES{%a,%d,%d}:@ %a@.@." Id.print f c d Ref_type.Env.print candidate;
            let aux (r,env',ce_set4) (g,typ') =
              main_loop 0 (d+1) {prog with fun_typ_env=env'} cmp g typ' ce_set4
            in
            let _,env',ce_set3 = List.fold_left aux (Typable, env, ce_set2) candidate' in
            if env' <> env then
              main_loop (c+1) d {prog with fun_typ_env=env'} cmp f typ ce_set3
            else if ce_set3 <> ce_set2 then
              refine_loop ce_set3
            else
              raise NoProgress
      in
      refine_loop (merge_ce_set ce_set1 ce_set)
let main_loop prog cmp f typ = main_loop 0 0 prog cmp f typ []

let report_safe env =
  Format.printf "Safe!@.@.";
  Format.printf "Refinement types: %a@.@." Ref_type.Env.print env

let report_unsafe ce_set =
  Format.printf "Unsafe!@.@.";
  Format.printf "Modular counterexamples: %a@.@." (List.print @@ Pair.print Id.print @@ List.print Format.pp_print_int) ce_set

let main _ spec parsed =
  if spec <> Spec.init then unsupported "Modular.main: spec";
  let fbindings,body =
    let pps =
      let open Main_loop in
      preprocesses spec
      |> preprocess_before CPS
      |> preprocess_filter_out [(*Encode_mutable_record; Encode_recdata; Encode_list;*) Beta_reduce_trivial]
    in
    Main_loop.run_preprocess pps parsed
    |> Main_loop.last_t
    |@!!debug&> Format.printf "INITIALIZED: %a@.@." Print.term'
    |> normalize
    |@!!debug&> Format.printf "NORMALIZED: %a@.@." Print.term
    |> decomp_prog
  in
  assert (body.desc = Const Unit);
  List.iter (fun (flag,bindings) -> if flag=Recursive then assert (List.length bindings=1)) fbindings;
  let fun_env = List.flatten_map (fun (_,bindings) -> List.map Triple.to_pair_r bindings) fbindings in
  let _,(main,_) = List.decomp_snoc fun_env in
  let typ = Ref_type.of_simple @@ Id.typ main in
  let prog =
    let fs =
      List.flatten_map (snd |- List.map Triple.fst) fbindings
      |> List.filter_out (Id.same main)
    in
    let env_init = Ref_type.Env.create fs (Ref_type.make_weakest -| Trans.inst_tvar_tunit_typ -| Id.typ) in
    if !!debug then Format.printf "ENV_INIT: %a@." Ref_type.Env.print env_init;
    {fun_typ_env=env_init; fun_def_env=fun_env}
  in
  let cmp =
    let map =
      let edges =
        fun_env
        |> List.map (fun (f,(xs,t)) -> f, List.Set.diff ~cmp:Id.eq (get_fv t) (f::xs))
        |> List.flatten_map (fun (f,fv) -> List.map (fun g -> g, f) fv)
      in
      edges
      |> topological_sort ~eq:Id.eq
      |> List.mapi (fun i x -> x, i)
    in
    Compare.on (Id.assoc -$- map)
  in
  let r, env, ce_set = main_loop prog cmp main typ in
  match r with
  | Typable ->
      report_safe env;
      true
  | Untypable ->
      report_unsafe ce_set;
      false
