open Util
open Syntax
open Term_util
open Type
open Modular_common

module Debug = Debug.Make(struct let check = make_debug_check __MODULE__ end)

let num_tycheck = ref 0
let time_check = ref 0.
let time_synthesize = ref 0.


exception NoProgress

let merge_neg_tenv env' env =
  Ref_type.NegEnv.normalize @@ Ref_type.NegEnv.merge env' env
let merge_tenv env' env =
  Ref_type.Env.normalize @@ Ref_type.Env.merge env' env

let normalize_ce_set (ce_set:ce_set) =
  let prefix (f,ce1) (g,ce2) = List.is_prefix ce1 ce2 in
  List.remove_lower prefix ce_set


let is_closed f def_env depth =
  let fs = take_funs_of_depth def_env f depth in
  let def_env' = List.filter (fst |- Id.mem -$- fs) def_env in
  let fv = List.flatten_map (snd |- snd |- get_fv) def_env' in
  let bv = List.flatten_map (fun (f,(xs,_)) -> f::xs) def_env' in
  List.Set.subset ~eq:Id.eq fv bv

let report_safe env =
  Format.printf "Safe!@.@.";
  Format.printf "Refinement types: %a@.@." Ref_type.Env.print env

let report_unsafe neg_env ce_set =
  Format.printf "Unsafe!@.@.";
  Format.printf "Negative refinement types: %a@.@." Ref_type.NegEnv.print neg_env;
  Format.printf "Modular counterexamples: %a@.@." print_ce_set ce_set



let make_init_env cmp bindings =
  let rec replace_with_top n typ =
    if n = 0 then
      typ
    else
      match typ with
      | Ref_type.Fun(x,typ1,typ2) -> Ref_type.Fun(x, Ref_type.top @@ Ref_type.to_simple typ1, replace_with_top (n-1) typ2)
      | _ -> assert false
  in
  let rec to_weak typ =
    match typ with
    | TFun(x,typ2) -> Ref_type.Fun(x, Ref_type.bottom @@ Id.typ x, to_weak typ2)
    | _ -> Ref_type.of_simple typ
  in
  let make (f,xs,_) =
    f,
    Id.typ f
    |> Trans.inst_tvar_tunit_typ
    |> (if Id.is_external f then Ref_type.of_simple else to_weak)
    |> replace_with_top (List.length xs - 1)
  in
  bindings
  |> List.flatten
  |> List.map make
  |> Ref_type.Env.of_list

let refine_init_env prog =
  let rec loop prog ce_set candidates =
    match candidates with
    | [] -> prog, ce_set
    | (f,typ)::candidates' ->
        let r = measure_and_add_time time_check (fun () -> Modular_check.check prog f typ 1) in
        let prog', ce_set'  =
          match r with
          | Modular_check.Typable env -> {prog with fun_typ_env=merge_tenv prog.fun_typ_env env}, ce_set
          | Modular_check.Untypable ce -> prog, normalize_ce_set @@ (f,ce)::ce_set
        in
        loop prog' ce_set' candidates'
  in
  Ref_type.Env.dom prog.fun_typ_env
  |> List.filter_out Id.is_external
  |> List.map (fun f -> f, Ref_type.of_simple @@ Id.typ f)
  |> loop prog []

let non_terminating_typ typ =
  let open Ref_type in
  let rec aux typ =
    match typ with
    | Fun(x,typ1,typ2) -> Fun(x, typ1, aux typ2)
    | Base(b,x,_) -> Base(b, x, false_term)
    | _ -> assert false
  in
  aux @@ of_simple @@ to_simple typ

let check prog f typ depth =
  measure_and_add_time time_check (fun () -> Modular_check.check prog f typ depth)

let infer prog f typ ce_set2 =
  measure_and_add_time time_synthesize (fun () -> Modular_infer.infer prog f typ ce_set2) !Flag.Modular.infer_merge

let rec main_loop_ind history c prog cmp dep f typ depth ce_set =
  let space = String.make (8*List.length history) ' ' in
  let pr f = MVerbose.printf ("%s%a@[<hov 2>#[MAIN_LOOP]%t" ^^ f ^^ "@.") space Color.set Color.Red Color.reset in
  let {fun_typ_env=env; fun_typ_neg_env=neg_env; fun_def_env} = prog in
  if Ref_type.subtype (Ref_type.Env.assoc f env) typ then
    (pr " TYPABLE (skip): %a :@ %a@." Id.print f Ref_type.print typ;
     `Typable, env, neg_env, ce_set)
  else if !Flag.Modular.use_neg_env && Ref_type.suptype (Ref_type.NegEnv.assoc f neg_env) typ then
    (pr " UNTYPABLE (skip): %a :@ %a@." Id.print f Ref_type.print typ;
     `Untypable, env, neg_env, ce_set)
  else
    let () = Debug.printf "%sTIME: %.3f@." space !!get_time in
    pr " history: %a" (List.print @@ Pair.print Id.print Ref_type.print) history;
    pr "%a{%a,%d}%t env:@ %a" Color.set Color.Blue Id.print f c Color.reset Ref_type.Env.print @@ Ref_type.Env.filter_out (fst |- Id.is_external) env;
    if !Flag.Modular.use_neg_env then pr "%a{%a,%d}%t neg_env:@ %a" Color.set Color.Blue Id.print f c Color.reset Ref_type.NegEnv.print neg_env;
    if false then pr "%a{%a,%d}%t ce_set:@ %a" Color.set Color.Blue Id.print f c Color.reset print_ce_set ce_set;
    pr "%a{%a,%d}%t:@ %a :? %a" Color.set Color.Blue Id.print f c Color.reset Id.print f Ref_type.print typ;
    incr num_tycheck;
    match check prog f typ depth with
    | Modular_check.Typable env' ->
        pr "%a{%a,%d}%t TYPABLE: %a :@ %a@." Color.set Color.Blue Id.print f c Color.reset Id.print f Ref_type.print typ;
        let env'' = merge_tenv env' env in
        let neg_env' =
          let aux (g,typ) =
            let typ' =
              if List.exists (fun (g',f') -> Id.same g g' && Id.same f f') dep then
                Ref_type.bottom @@ Id.typ g
              else
                typ
            in
            g, typ'
          in
          Ref_type.NegEnv.map aux neg_env
        in
        `Typable, env'', neg_env', ce_set
    | Modular_check.Untypable ce when is_closed f fun_def_env depth ->
        pr "%a{%a,%d}%t UNTYPABLE (closed):@ %a : %a@." Color.set Color.Blue Id.print f c Color.reset Id.print f Ref_type.print typ;
        let neg_env' = merge_neg_tenv neg_env @@ Ref_type.NegEnv.of_list [f, typ] in
        let ce_set' = normalize_ce_set @@ (f,ce)::ce_set in
        `Untypable, env, neg_env', ce_set'
    | Modular_check.Untypable ce ->
        pr "%a{%a,%d}%t UNTYPABLE:@ %a : %a@." Color.set Color.Blue Id.print f c Color.reset Id.print f Ref_type.print typ;
        let rec refine_loop neg_env ce_set2 prev_sol =
          if true then pr "%a{%a,%d}%t ce_set2:@ %a" Color.set Color.Blue Id.print f c Color.reset print_ce_set @@ List.filter_out (fst |- Id.is_external) ce_set2;
          let sol =
            match prev_sol with
            | None -> infer prog f typ ce_set2
            | Some sol -> sol
          in
          match measure_and_add_time time_synthesize (fun () -> sol Modular_infer.init_mode) with
          | None ->
              pr "%a{%a,%d}%t THERE ARE NO CANDIDATES" Color.set Color.Blue Id.print f c Color.reset;
              let neg_env' = merge_neg_tenv neg_env @@ Ref_type.NegEnv.of_list [f, typ] in
              `Untypable, env, neg_env', ce_set2
          | Some candidate ->
              let rec infer_mode_loop mode candidate =
                let candidate' =
                  candidate
                  |> Ref_type.Env.to_list
                  |> List.filter_out (fst |- Id.same f)
                  |> List.filter_out (fst |- Id.is_external)
                  |> List.sort ~cmp:(Compare.on ~cmp fst)
                in
                pr "%a{%a,%d}%t CANDIDATES:@ %a" Color.set Color.Blue Id.print f c Color.reset Ref_type.Env.print @@ Ref_type.Env.of_list candidate';
                let aux (r,env',neg_env',ce_set4) (g,typ') =
                  main_loop_ind ((f,typ)::history) 0 {prog with fun_typ_env=env'; fun_typ_neg_env=neg_env'} cmp dep g typ' depth ce_set4
                in
                let _,env',neg_env',ce_set3 = List.fold_left aux (`Typable, env, neg_env, ce_set2) candidate' in
                if not @@ Ref_type.Env.eq env' env then
                  main_loop_ind history (c+1) {prog with fun_typ_env=env'; fun_typ_neg_env=neg_env'} cmp dep f typ depth ce_set3
                else if not @@ List.Set.eq ce_set3 ce_set2 then
                  refine_loop neg_env' ce_set3 None
                else if not @@ Modular_infer.is_last_mode mode then
                  let mode' = Modular_infer.next_mode mode in
                  MVerbose.printf "%schange infer_mode %a => %a@." space Modular_infer.print_mode mode Modular_infer.print_mode mode';
                  infer_mode_loop mode' @@ Option.get @@ sol mode'(*
                else if true then
                  (MVerbose.printf "%sdepth := %d@." space (depth+1);
                   main_loop_ind history (c+1) prog cmp dep f typ (depth+1) ce_set3)*)
                else
                  raise NoProgress
(*
                  let typ' = non_terminating_typ typ in
                  ignore @@ read_line();
                  let _, env'', neg_env'', ce_set4 = main_loop_ind history (c+1) {prog with fun_typ_env=env'; fun_typ_neg_env=neg_env'} cmp dep f typ' depth ce_set3 in
                  ignore @@ read_line();
                  main_loop_ind history (c+1) {prog with fun_typ_env=env''; fun_typ_neg_env=neg_env''} cmp dep f typ depth ce_set4
 *)
              in
              infer_mode_loop Modular_infer.init_mode candidate
        in
        refine_loop neg_env (normalize_ce_set @@ (f,ce)::ce_set) None

let rec main_loop prog cmp candidates main typ infer_mode depth ce_set =
  let {fun_typ_env=env; fun_typ_neg_env=neg_env; fun_def_env} = prog in
  let pr f = MVerbose.printf ("%a@[<hov 2>#[MAIN_LOOP]%t " ^^ f ^^ "@.") Color.set Color.Red Color.reset in
  Debug.printf "TIME: %.3f@." !!get_time;
  let rec check env ce_set candidates =
    match candidates with
    | [] -> env, ce_set
    | (f,typ)::candidates' when Ref_type.subtype (Ref_type.Env.assoc f env) typ ->
        pr "TYPABLE (skip): %a :@ %a@." Id.print f Ref_type.print typ;
        check env ce_set candidates'
    | (f,typ)::candidates' ->
        pr "env:@ %a" Ref_type.Env.print @@ Ref_type.Env.filter_out (fst |- Id.is_external) env;
        pr "%a :? %a" Id.print f Ref_type.print typ;
        incr num_tycheck;
        let prog' = {prog with fun_typ_env=env} in
        let r = measure_and_add_time time_check (fun () -> Modular_check.check prog' f typ depth) in
        match r with
        | Modular_check.Typable env' ->
            pr "TYPABLE: %a :@ %a@." Id.print f Ref_type.print typ;
            let env'' = merge_tenv env' env in
            check env'' ce_set candidates'
        | Modular_check.Untypable ce ->
            pr "UNTYPABLE:@ %a : %a@." Id.print f Ref_type.print typ;
            let ce_set' = normalize_ce_set @@ (f,ce)::ce_set in
            check env ce_set' candidates'
  in
  let env',ce_set' = check env ce_set candidates in
  if Ref_type.subtype (Ref_type.Env.assoc main env') typ then
    `Typable, env', neg_env, ce_set'
  else
    let infer_mode' =
      if List.Set.eq ce_set ce_set' then
        (if Modular_infer.is_last_mode infer_mode then raise NoProgress;
         MVerbose.printf "change infer_mode@.";
         Modular_infer.next_mode infer_mode)
      else if not @@ Ref_type.Env.eq env' env then
        Modular_infer.init_mode
      else
        infer_mode
    in
    pr "ce_set':@ %a" print_ce_set @@ List.filter_out (fst |- Id.is_external) ce_set';
    match infer prog main typ ce_set' infer_mode' with
    | None ->
        pr "THERE ARE NO CANDIDATES";
        `Untypable, env, neg_env, ce_set'
    | Some candidate ->
        let candidate' =
          candidate
          |> Ref_type.Env.to_list
          |> List.filter_out (fst |- Id.same main)
          |> List.filter_out (fst |- Id.is_external)
          |> List.sort ~cmp:(Compare.on ~cmp fst)
          |*> List.flatten_map (fun (g,typ) -> List.map (Pair.pair g) @@ Ref_type.decomp_inter typ)
          |> List.snoc -$- (main,typ)
        in
        pr "CANDIDATES:@ %a@." Ref_type.Env.print @@ Ref_type.Env.of_list candidate';
        main_loop {prog with fun_typ_env=env'} cmp candidate' main typ infer_mode' depth ce_set'


let main_loop prog ce_set cmp dep f typ =
  if !Flag.Modular.infer_ind then
    main_loop_ind [] 0 prog cmp dep f typ 1 ce_set
  else
    main_loop prog cmp [f,typ] f typ Modular_infer.init_mode 1 ce_set

let rec last_def_to_fun t =
  match t.desc with
  | Let([f,xs,t1], ({desc=Const Unit} as t2)) ->
      let f',xs' =
        if xs = [] then
          let u = Id.new_var ~name:"u" TUnit in
          let typ = TFun(u, Id.typ f) in
          Id.set_typ f typ, [u]
        else
          f, xs
      in
      let desc = Let([f',xs',t1], t2) in
      {t with desc}
  | Let(_, {desc=Const Unit}) -> unsupported "last_def_to_fun"
  | Let(defs, t2) ->
      let t2' = last_def_to_fun t2 in
      {t with desc = Let(defs, t2')}
  | _ -> assert false

let assert_to_fun t =
  let rec loop t =
    match t.desc with
    | Let([u,[],t1], t2) when Id.typ u = TUnit ->
        let u' = Id.new_var ~name:"u" TUnit in
        let f = Id.new_var @@ TFun(u', TUnit) in
        let t1',t2' =
          match loop t2 with
          | `Unit, _ -> t1, t2
          | `Lifted, _ -> t, unit_term
          | `Other, _ -> unsupported "top-level let-bindings of non-functions"
        in
        `Lifted, make_let [f,[u],t1'] t2'
    | Let(bindings, t) ->
        `Other, make_let bindings @@ snd @@ loop t
    | Const Unit -> `Unit, t
    | _ -> assert false
  in
  snd @@ loop t

let main _ spec parsed =
  Flag.print_only_if_id := true;
  (*
  if spec <> Spec.init then unsupported "Modular.main: spec";
   *)
  let bindings,body =
    let pps =
      Preprocess.all spec
      |> Preprocess.before Preprocess.CPS
      |> Preprocess.filter_out [Preprocess.Beta_reduce_trivial]
    in
    let ref_env =
      Spec.get_ref_env spec parsed
      |@> Verbose.printf "%a@." Spec.print_ref_env
      |> Ref_type.Env.of_list
    in
    let pr s = Debug.printf "### %s:@.  %a@.@." s in
    parsed
    |@> pr "PARSED" Print.term
    |> Trans.ref_to_assert ref_env
    |@> pr "REF_TO_ASSERT" Print.term
    |> assert_to_fun
    |@> pr "ASSERT_TO_FUN" Print.term
    |> Preprocess.run pps
    |> Preprocess.last_t
    |> last_def_to_fun
    |@> pr "INITIALIZED" Print.term_typ
    |> normalize true
    |@> pr "NORMALIZED" Print.term
    |> decomp_prog
  in
  assert (body.desc = Const Unit);
  Debug.printf "TOP_FUNS: %a@." (print_list Print.id_typ "@\n") @@ List.flatten_map (List.map Triple.fst) bindings;
  if List.exists (List.exists (Triple.fst |- is_fun_var |- not)) bindings then
    unsupported "top-level let-bindings of non-functions";
  let fun_env = List.flatten_map (List.map Triple.to_pair_r) bindings in
  let _,(main,_) = List.decomp_snoc fun_env in
  let typ = Ref_type.of_simple @@ Id.typ main in
  let cmp,dep =
    let edges = List.flatten_map (fun (f,(xs,t)) -> List.map (fun g -> g, f) @@ List.Set.diff ~eq:Id.eq (get_fv t) (f::xs)) fun_env in
    Compare.topological ~eq:Id.eq ~dom:(List.map fst fun_env) edges,
    transitive_closure ~eq:Id.eq edges
  in
  let prog =
    let env_init = make_init_env cmp bindings in
    Debug.printf "ENV_INIT: %a@." Ref_type.Env.print env_init;
    let fun_typ_neg_env =
      List.flatten_map (List.map Triple.fst) bindings
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
  let prog',ce_set =
    if !Flag.Modular.refine_init then
      refine_init_env prog
    else
      prog, []
  in
  let r, env, neg_env, ce_set = main_loop prog' ce_set cmp dep main typ in
  Main_loop.print_result_delimiter ();
  match r with
  | `Typable ->
      report_safe env;
      Flag.result := "Safe";
      true
  | `Untypable ->
      report_unsafe neg_env ce_set;
      Flag.result := "Unsafe";
      false
