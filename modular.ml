open Util
open Syntax
open Term_util
open Type
open Modular_syntax

let debug () = List.mem "Modular" !Flag.debug_module

exception NoProgress

let merge_tenv env' env = Ref_type.Env.merge env' env

let merge_ce_set (ce_set':ce_set) (ce_set:ce_set) =
  let dbg = 0=1 in
  let prefix (f,ce1) (g,ce2) =
    Id.same f g && List.for_all (fun (f,path) -> List.is_prefix path (Id.assoc f ce2)) ce1
  in
  List.remove_lower prefix (ce_set' @ ce_set)
  |@dbg&> Format.printf "MERGE_CE_SET: %a@." print_ce_set

let is_closed f def_env =
  let ys,t = Id.assoc f def_env in
  List.Set.subset ~eq:Id.eq (get_fv t) (f::ys)

let report_safe env =
  Format.printf "Safe!@.@.";
  Format.printf "Refinement types: %a@.@." Ref_type.Env.print env

let report_unsafe ce_set =
  Format.printf "Unsafe!@.@.";
  Format.printf "Modular counterexamples: %a@.@." print_ce_set ce_set


let incr_extend f ce_set extend =
  let dbg = 0=1 && !!debug in
  assert (ce_set <> []);
  if dbg then Format.printf "IE f: %a@." Id.print f;
  if dbg then Format.printf "IE CE_SET: %a@." print_ce_set ce_set;
  if dbg then Format.printf "IE EXTEND: %a@." (List.print @@ Pair.print Id.print Format.pp_print_int) extend;
  let extend' =
    let aux g extend' =
      try
        snd @@ List.assoc_map ~eq:Id.eq g succ extend'
      with Not_found -> (g,1)::extend'
    in
    List.fold_left (fun extend' (g,_) -> aux g extend') extend @@ Id.assoc f ce_set
  in
  if dbg then Format.printf "IE EXTEND': %a@." (List.print @@ Pair.print Id.print Format.pp_print_int) extend';
  let new_ce_set =
    let aux ce =
      let rev xs =
        if xs = [] then
          []
        else
          let xs',last = List.decomp_snoc xs in
          xs' @ [1-last]
      in
      List.map (Pair.map_snd rev) ce
    in
    snd @@ List.assoc_map ~eq:Id.eq f aux ce_set
  in
  merge_ce_set new_ce_set ce_set, extend'

let rec main_loop history c prog cmp f typ ce_set extend =
  let space = String.make (8*List.length history) ' ' in
  let pr f = if !!debug then Format.printf ("%s%a@[<hov 2>#[MAIN_LOOP]%t" ^^ f ^^ "@.") space Color.set Color.Red Color.reset else Format.ifprintf Format.std_formatter f in
  pr " history: %a" (List.print @@ Pair.print Id.print Ref_type.print) history;
  pr "%a{%a,%d}%t env:@ %a" Color.set Color.Blue Id.print f c Color.reset Ref_type.Env.print prog.fun_typ_env;
  pr "%a{%a,%d}%t ce_set:@ %a" Color.set Color.Blue Id.print f c Color.reset print_ce_set ce_set;
  pr "%a{%a,%d}%t extend:@ %a" Color.set Color.Blue Id.print f c Color.reset (List.print @@ Pair.print Id.print Format.pp_print_int) extend;
  let {fun_typ_env=env; fun_def_env} = prog in
  pr "%a{%a,%d}%t:@ %a :? %a" Color.set Color.Blue Id.print f c Color.reset Id.print f Ref_type.print typ;
  match Check_mod.check prog f typ with
  | Check_mod.Typable env' ->
      pr "%a{%a,%d}%t TYPABLE: %a :@ %a@." Color.set Color.Blue Id.print f c Color.reset Id.print f Ref_type.print typ;
      `Typable, merge_tenv env' env, ce_set
  | Check_mod.Untypable ce_set1 when is_closed f fun_def_env ->
      pr "%a{%a,%d}%t UNTYPABLE (closed):@ %a : %a@." Color.set Color.Blue Id.print f c Color.reset Id.print f Ref_type.print typ;
      `Untypable, prog.fun_typ_env, merge_ce_set ce_set ce_set1
  | Check_mod.Untypable ce_set1 ->
      pr "%a{%a,%d}%t UNTYPABLE:@ %a : %a@." Color.set Color.Blue Id.print f c Color.reset Id.print f Ref_type.print typ;
      let rec refine_loop ce_set2 extend' =
        pr "%a{%a,%d}%t ce_set2:@ %a" Color.set Color.Blue Id.print f c Color.reset print_ce_set ce_set2;
        pr "%a{%a,%d}%t extend':@ %a" Color.set Color.Blue Id.print f c Color.reset (List.print @@ Pair.print Id.print Format.pp_print_int) extend';
        match Infer_mod.infer prog f typ ce_set2 extend' with
        | exception Not_found -> assert false
        | None ->
            pr "%a{%a,%d}%t THERE ARE NO CANDIDATES" Color.set Color.Blue Id.print f c Color.reset;
            `Untypable, prog.fun_typ_env, ce_set2
        | Some candidate ->
            pr "%a{%a,%d}%t CANDIDATES:@ %a" Color.set Color.Blue Id.print f c Color.reset Ref_type.Env.print candidate;
            let candidate' =
              candidate
              |> Ref_type.Env.to_list
              |> List.sort ~cmp:(Compare.on ~cmp fst)
              |*> List.flatten_map (fun (g,typ) -> List.map (Pair.pair g) @@ Ref_type.decomp_inter typ)
            in
            let aux (r,env',ce_set4) (g,typ') =
              if Ref_type.subtype (Ref_type.Env.assoc g env') typ' then
                (r,env',ce_set4)
              else
                main_loop ((f,typ)::history) 0 {prog with fun_typ_env=env'} cmp g typ' ce_set4 extend'
            in
            let _,env',ce_set3 = List.fold_left aux (`Typable, env, ce_set2) candidate' in
            if not @@ Ref_type.Env.eq env' env then
              (Infer_mod.infer_stronger := false;
               main_loop history (c+1) {prog with fun_typ_env=env'} cmp f typ ce_set3 extend')
            else if not @@ List.Set.eq ce_set3 ce_set2 then
              (Infer_mod.infer_stronger := false;
               refine_loop ce_set3 extend')
            else if not !Infer_mod.infer_stronger then
              (if !!debug then Format.printf "infer_stronger := true@.";
               Infer_mod.infer_stronger := true;
               refine_loop ce_set3 extend')
            else if true then
              (if !!debug then Format.printf "extend counterexample@.";
               Infer_mod.infer_stronger := false;
               let ce_set4,extend'' = List.fold_left (fun (cs,ex) g -> incr_extend g cs ex) (ce_set3,extend') (f :: (List.unique ~cmp:Id.eq @@ Ref_type.Env.dom candidate)) in
               refine_loop ce_set4 extend'')
            else
              raise NoProgress
      in
      refine_loop (merge_ce_set ce_set1 ce_set) extend
let main_loop prog cmp f typ = main_loop [] 0 prog cmp f typ [] []

let main _ spec parsed =
  if spec <> Spec.init then unsupported "Modular.main: spec";
  let (fbindings,body) =
    let aux t =
      t
      |@!!debug&> Format.printf "PARSED: %a@.@." Print.term'
      |> Main_loop.run_preprocess @@ Main_loop.preprocesses spec
      |> Main_loop.last_t
      |> Trans.direct_from_CPS
      |@!!debug&> Format.printf "INITIALIZED: %a@.@." Print.term'
      |> normalize
      |@!!debug&> Format.printf "NORMALIZED: %a@.@." Print.term
    in
    parsed
    |> Trans.add_main_and_trans aux
    |> decomp_prog
  in
  assert (body.desc = Const Unit);
  if !!debug then Format.printf "TOP_FUNS: %a@." (print_list Print.id_typ "@\n") @@ List.flatten_map (snd |- List.map Triple.fst) fbindings;
  if List.exists (snd |- List.exists (Triple.fst |- is_fun_var |- not)) fbindings then
    unsupported "top-level let-bindings of non-functions";
  List.iter (fun (flag,bindings) -> if flag=Recursive then assert (List.length bindings=1)) fbindings;
  let fun_env = List.flatten_map (fun (_,bindings) -> List.map Triple.to_pair_r bindings) fbindings in
  let _,(main,_) = List.decomp_snoc fun_env in
  let typ = Ref_type.of_simple @@ Id.typ main in
  let prog =
    let env_init =
      List.flatten_map (snd |- List.map Triple.fst) fbindings
      |> Ref_type.Env.create (Ref_type.make_weakest -| Trans.inst_tvar_tunit_typ -| Id.typ)
    in
    if !!debug then Format.printf "ENV_INIT: %a@." Ref_type.Env.print env_init;
    {fun_typ_env=env_init; fun_def_env=fun_env}
  in
  let cmp =
    let map =
      let edges =
        fun_env
        |> List.map (fun (f,(xs,t)) -> f, List.Set.diff ~eq:Id.eq (get_fv t) (f::xs))
        |> List.flatten_map (fun (f,fv) -> List.map (fun g -> g, f) fv)
      in
      let unused = List.filter_map (fun (f,_) -> if List.exists (fun (g,h) -> Id.same f g || Id.same f h) edges then None else Some f) fun_env in
      edges
      |> topological_sort ~eq:Id.eq
      |> (@) unused
      |> List.mapi (fun i x -> x, i)
    in
    Compare.on (Id.assoc -$- map)
  in
  let r, env, ce_set = main_loop prog cmp main typ in
  match r with
  | `Typable ->
      report_safe env;
      true
  | `Untypable ->
      report_unsafe ce_set;
      false
