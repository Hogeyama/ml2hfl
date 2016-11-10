open Util
open Syntax
open Term_util
open Type
open Modular_common

module Debug = Debug.Make(struct let check = make_debug_check __MODULE__ end)

let num_tycheck = ref 0

exception NoProgress

let merge_neg_tenv env' env =
  Ref_type.NegEnv.normalize @@ Ref_type.NegEnv.merge env' env
let merge_tenv env' env =
  Ref_type.Env.normalize @@ Ref_type.Env.merge env' env (*
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
(*
let merge_tenv env' env = (* ??? *)
  Ref_type.Env.of_list @@ List.fold_right (fun (x,typ) acc -> if Id.mem_assoc x acc then acc else (x,typ)::acc) (Ref_type.Env.to_list env) (Ref_type.Env.to_list env')
 *)

let normalize_ce_set (ce_set:ce_set) =
  let prefix (f,ce1) (g,ce2) = List.is_prefix ce1 ce2 in
  List.remove_lower prefix ce_set


let is_closed f def_env =
  let ys,t = Id.assoc f def_env in
  List.Set.subset ~eq:Id.eq (get_fv t) (f::ys)

let report_safe env =
  Format.printf "Safe!@.@.";
  Format.printf "Refinement types: %a@.@." Ref_type.Env.print env

let report_unsafe neg_env ce_set =
  Format.printf "Unsafe!@.@.";
  Format.printf "Negative refinement types: %a@.@." Ref_type.NegEnv.print neg_env;
  Format.printf "Modular counterexamples: %a@.@." print_ce_set ce_set



let is_external_id f =
  let name = Id.name f in
  String.contains name '.' && is_uppercase name.[0]

let make_init_env cmp fbindings =
  let make f =
    Id.typ f
    |> Trans.inst_tvar_tunit_typ
    |> (if is_external_id f then Ref_type.of_simple else Ref_type.make_weakest)
  in
(*  if true then
    Modular_check.check prog f typ
  else*)
    List.flatten_map (snd |- List.map Triple.fst) fbindings
    |> Ref_type.Env.create make

let rec main_loop history c prog cmp f typ ce_set =
  let {fun_typ_env=env; fun_typ_neg_env=neg_env; fun_def_env} = prog in
  if Ref_type.subtype (Ref_type.Env.assoc f env) typ then
    `Typable, env, neg_env, ce_set
  else if false && Ref_type.suptype (Ref_type.NegEnv.assoc f neg_env) typ then
    `Untypable, env, neg_env, ce_set
  else
    let space = String.make (8*List.length history) ' ' in
    Debug.printf "%sTIME: %.3f@." space !!get_time;
    let pr f = Debug.printf ("%s%a@[<hov 2>#[MAIN_LOOP]%t" ^^ f ^^ "@.") space Color.set Color.Red Color.reset in
    pr " history: %a" (List.print @@ Pair.print Id.print Ref_type.print) history;
    pr "%a{%a,%d}%t env:@ %a" Color.set Color.Blue Id.print f c Color.reset Ref_type.Env.print @@ Ref_type.Env.filter_out (fun (f,_) -> is_external_id f) env;
    if false then pr "%a{%a,%d}%t neg_env:@ %a" Color.set Color.Blue Id.print f c Color.reset Ref_type.NegEnv.print neg_env;
    if false then pr "%a{%a,%d}%t ce_set:@ %a" Color.set Color.Blue Id.print f c Color.reset print_ce_set ce_set;
    pr "%a{%a,%d}%t:@ %a :? %a" Color.set Color.Blue Id.print f c Color.reset Id.print f Ref_type.print typ;
    incr num_tycheck;
    match Modular_check.check prog f typ with
    | Modular_check.Typable env' ->
        pr "%a{%a,%d}%t TYPABLE: %a :@ %a@." Color.set Color.Blue Id.print f c Color.reset Id.print f Ref_type.print typ;
        `Typable, merge_tenv env' env, neg_env, ce_set
    | Modular_check.Untypable ce when is_closed f fun_def_env ->
        pr "%a{%a,%d}%t UNTYPABLE (closed):@ %a : %a@." Color.set Color.Blue Id.print f c Color.reset Id.print f Ref_type.print typ;
        let neg_env' = merge_neg_tenv neg_env @@ Ref_type.NegEnv.of_list [f, typ] in
        `Untypable, env, neg_env', normalize_ce_set @@ (f,ce)::ce_set
    | Modular_check.Untypable ce ->
        pr "%a{%a,%d}%t UNTYPABLE:@ %a : %a@." Color.set Color.Blue Id.print f c Color.reset Id.print f Ref_type.print typ;
        let rec refine_loop infer_mode neg_env ce_set2 =
          if true then pr "%a{%a,%d}%t ce_set2:@ %a" Color.set Color.Blue Id.print f c Color.reset print_ce_set @@ List.filter_out (fst |- is_external_id) ce_set2;
          match Modular_infer.infer infer_mode prog f typ ce_set2 with
          | None ->
              pr "%a{%a,%d}%t THERE ARE NO CANDIDATES" Color.set Color.Blue Id.print f c Color.reset;
              let neg_env' = merge_neg_tenv neg_env @@ Ref_type.NegEnv.of_list [f, typ] in
              `Untypable, env, neg_env', ce_set2
          | Some candidate ->
              let candidate' =
                candidate
                |> Ref_type.Env.to_list
                |> List.filter_out (fun (g,_) -> Id.same f g)
                |> List.filter_out (fun (g,_) -> is_external_id g)
                |> List.sort ~cmp:(Compare.on ~cmp fst)
                |*> List.flatten_map (fun (g,typ) -> List.map (Pair.pair g) @@ Ref_type.decomp_inter typ)
              in
              pr "%a{%a,%d}%t CANDIDATES:@ %a" Color.set Color.Blue Id.print f c Color.reset Ref_type.Env.print @@ Ref_type.Env.of_list candidate';
              let aux (r,env',neg_env',ce_set4) (g,typ') =
                main_loop ((f,typ)::history) 0 {prog with fun_typ_env=env'; fun_typ_neg_env=neg_env'} cmp g typ' ce_set4
              in
              let _,env',neg_env',ce_set3 = List.fold_left aux (`Typable, env, neg_env, ce_set2) candidate' in
              if not @@ Ref_type.Env.eq env' env then
                (main_loop history (c+1) {prog with fun_typ_env=env'; fun_typ_neg_env=neg_env'} cmp f typ ce_set3)
              else if not @@ List.Set.eq ce_set3 ce_set2 then
                (refine_loop Modular_infer.init_mode neg_env' ce_set3)
              else if not @@ Modular_infer.is_last_mode infer_mode then
                (Debug.printf "%schange infer_mode@." space;
                 refine_loop (Modular_infer.next_mode infer_mode) neg_env' ce_set3)
              else
                raise NoProgress
        in
        refine_loop Modular_infer.init_mode neg_env (normalize_ce_set @@ (f,ce)::ce_set)
let main_loop prog cmp f typ = main_loop [] 0 prog cmp f typ []

let main _ spec parsed =
  Flag.print_only_if_id := true;
  if spec <> Spec.init then unsupported "Modular.main: spec";
  let fbindings,body =
    let pps =
      Preprocess.all spec
      |> Preprocess.before Preprocess.CPS
      |> Preprocess.filter_out [Preprocess.Beta_reduce_trivial]
    in
    parsed
    |@> Debug.printf "PARSED: %a@.@." Print.term'
    |> Preprocess.run pps
    |> Preprocess.last_t
    |@> Debug.printf "INITIALIZED: %a@.@." Print.term_typ
    |> normalize true
    |@> Debug.printf "NORMALIZED: %a@.@." Print.term
    |> decomp_prog
  in
  assert (body.desc = Const Unit);
  Debug.printf "TOP_FUNS: %a@." (print_list Print.id_typ "@\n") @@ List.flatten_map (snd |- List.map Triple.fst) fbindings;
  if List.exists (snd |- List.exists (Triple.fst |- is_fun_var |- not)) fbindings then
    unsupported "top-level let-bindings of non-functions";
  List.iter (fun (flag,bindings) -> if flag=Recursive then let ()=Format.printf "%a@." Id.print @@ Triple.fst @@List.hd bindings in assert (List.length bindings=1)) fbindings;
  let fun_env = List.flatten_map (fun (_,bindings) -> List.map Triple.to_pair_r bindings) fbindings in
  let _,(main,_) = List.decomp_snoc fun_env in
  let typ = Ref_type.of_simple @@ Id.typ main in
  let cmp =
    let edges = List.flatten_map (fun (f,(xs,t)) -> List.map (fun g -> g, f) @@ List.Set.diff ~eq:Id.eq (get_fv t) (f::xs)) fun_env in
    Compare.topological ~eq:Id.eq ~dom:(List.map fst fun_env) edges
  in
  let prog =
    let env_init = make_init_env cmp fbindings in
    Debug.printf "ENV_INIT: %a@." Ref_type.Env.print env_init;
    let fun_typ_neg_env =
      List.flatten_map (snd |- List.map Triple.fst) fbindings
      |> Ref_type.NegEnv.create (Ref_type.union -$- [] -| Id.typ)
    in
    let exn_decl =
      match find_exn_typ parsed with
      | None -> []
      | Some(Type(["exn", TVariant decl], "exn")) -> decl
      | Some _ -> assert false
    in
    {fun_typ_env=env_init; fun_typ_neg_env; fun_def_env=fun_env; exn_decl}
  in
  let r, env, neg_env, ce_set = main_loop prog cmp main typ in
  Format.printf "#tycheck: %n@." !num_tycheck;
  match r with
  | `Typable ->
      report_safe env;
      true
  | `Untypable ->
      report_unsafe neg_env ce_set;
      false
