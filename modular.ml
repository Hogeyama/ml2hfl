open Util
open Syntax
open Term_util
open Type
open Modular_syntax

let debug () = List.mem "Modular" !Flag.debug_module

type result =
  | Typable of Ref_type.Env.t
  | Untypable of (Syntax.id * int list) list

let merge_tenv env' env =
  Ref_type.Env.to_list env' @ Ref_type.Env.to_list env
  |> List.fold_left (fun acc (f,typ) -> if Id.mem_assoc f acc then acc else (f,typ)::acc) []
  |> Ref_type.Env.of_list

let merge_ce_set ce_set' ce_set =
  let ce_set'' = ce_set' @ ce_set in
  let fs = List.unique ~cmp:Id.eq @@ List.map fst ce_set'' in
  List.map (fun f -> f, List.unique @@ List.flatten @@ List.assoc_all f ce_set'') fs

let is_closed f def_env =
  let ys,t = Id.assoc f def_env in
  List.Set.subset ~cmp:Id.eq (get_fv t) (f::ys)

let rec main_loop c d prog cmp f typ ce_set =
  if !!debug then Format.printf "#[MAIN_LOOP]{%a,%d,%d} prog: %a@." Id.print f c d print_prog prog;
  if !!debug then Format.printf "#[MAIN_LOOP]{%a,%d,%d} ce_set: %a@." Id.print f c d (List.print @@ Pair.print Id.print @@ List.print Format.pp_print_int) ce_set;
  let {fun_typ_env=env; fun_def_env} = prog in
  if !!debug then Format.printf "#[MAIN_LOOP]{%a,%d,%d}: %a :? %a@." Id.print f c d Id.print f Ref_type.print typ;
  match Check_mod.check prog f typ with
  | Check_mod.Typable env' ->
      if !!debug then Format.printf "  #[MAIN_LOOP] TYPABLE{%a,%d,%d}: %a : %a@.@." Id.print f c d Id.print f Ref_type.print typ;
      Typable (merge_tenv env' env)
  | Check_mod.Untypable ce_set' when is_closed f fun_def_env ->
      if !!debug then Format.printf "  #[MAIN_LOOP] UNTYPABLE{%a,%d,%d} (closed): %a : %a@.@." Id.print f c d Id.print f Ref_type.print typ;
      Untypable (merge_ce_set ce_set ce_set')
  | Check_mod.Untypable ce_set' ->
      if !!debug then Format.printf "  #[MAIN_LOOP] UNTYPABLE{%a,%d,%d}: %a : %a@." Id.print f c d Id.print f Ref_type.print typ;
      if !!debug then Format.printf "  #[MAIN_LOOP] UNTYPABLE{%a,%d,%d}: fv: %a@." Id.print f c d (List.print Id.print) (get_fv @@ Fun.uncurry make_funs @@ Id.assoc f fun_def_env);
      if !!debug then Format.printf "  #[MAIN_LOOP] UNTYPABLE{%a,%d,%d} ce_set': %a@.@." Id.print f c d (List.print @@ Pair.print Id.print @@ List.print Format.pp_print_int) ce_set';
      let rec refine_loop ce_set'' =
        match Infer_mod.infer prog f typ ce_set'' with
        | None ->
            Untypable ce_set''
        | Some candidate ->
            let candidate' = List.sort ~cmp:(Compare.on ~cmp fst) @@ Ref_type.Env.to_list candidate in
            if !!debug then Format.printf "    #[MAIN_LOOP] CANDIDATES{%a,%d,%d}: %a@.@." Id.print f c d Ref_type.Env.print candidate;
            let aux acc (g,typ') =
              if !!debug then Format.printf "    #[MAIN_LOOP] CHECK CANDIDATE{%a,%d,%d}: %a :? %a@.@." Id.print f c d Id.print g Ref_type.print typ';
              match acc with
              | Typable env' -> main_loop 0 (d+1) {prog with fun_typ_env=env'} cmp g typ' ce_set''
              | Untypable ce_set -> Untypable ce_set
            in
            match List.fold_left aux (Typable env) candidate' with
            | Typable env' ->
                if !!debug then Format.printf "  #[MAIN_LOOP] ALL CANDIDATES ARE VALID{%a,%d,%d}@." Id.print f c d;
                if !!debug then Format.printf "    #[MAIN_LOOP] CANDIDATES{%a,%d,%d}: %a@.@." Id.print f c d Ref_type.Env.print candidate;
                main_loop (c+1) d {prog with fun_typ_env=env'} cmp f typ ce_set''
            | Untypable ce_set''' ->
                if !!debug then Format.printf "  #[MAIN_LOOP] CANDIDATE IS INVALID{%a,%d,%d}@.@." Id.print f c d;
                refine_loop (merge_ce_set ce_set''' ce_set'')
      in
      refine_loop (merge_ce_set ce_set' ce_set)
let main_loop prog cmp f typ = main_loop 0 0 prog cmp f typ []

let report_safe env =
  Format.printf "Safe!@.@.";
  Format.printf "Refinement types: %a@.@." Ref_type.Env.print env

let report_unsafe ce_set =
  Format.printf "Unsafe!@.@.";
  Format.printf "Modular counterexamples: %a@.@." (List.print @@ Pair.print Id.print @@ List.print Format.pp_print_int) ce_set

let main _ spec parsed =
  if spec <> Spec.init then unsupported "Modular.main: spec";
  let initialized =
    let pps =
      Main_loop.preprocess_before Main_loop.CPS spec
      |> List.filter_out (fst |- (=) Main_loop.Beta_reduce_trivial)
    in
    Main_loop.last_t @@ Main_loop.run_preprocess pps parsed
  in
  if !!debug then Format.printf "INITIALIZED: %a@.@." Print.term' initialized;
  let normalized = normalize initialized in
  if !!debug then Format.printf "NORM: %a@.@." Print.term normalized;
  let fbindings,body = decomp_prog normalized in
  assert (body.desc = Const Unit);
  List.iter (fun (flag,bindings) -> if flag=Recursive then assert (List.length bindings=1)) fbindings;
  let fun_env = List.flatten_map (fun (_,bindings) -> List.map Triple.to_pair_r bindings) fbindings in
  let _,(main,_) = List.decomp_snoc fun_env in
  let typ = Ref_type.from_simple @@ Id.typ main in
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
  match main_loop prog cmp main typ with
  | Typable env ->
      report_safe env;
      true
  | Untypable ce_set ->
      report_unsafe ce_set;
      false
