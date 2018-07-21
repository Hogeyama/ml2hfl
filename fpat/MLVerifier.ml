open Combinator
open Util

(** Safety and termination verifier *)

(* non-termination verification mode *)
let nonterm_mode = ref false
(* termination verification mode *)
let termination_mode = ref false
(* induction-based Horn constraint solving mode *)
let induction_mode = ref false
(* refinement type optimization mode *)
let typeopt_mode = ref false

(* use simple refinement type templates with only postconditions*)
let use_simple_template = ref false
(* use refinement type templates with pre and postconditions *)
let use_precondition_template = ref false

(* enable precondition inference *)
let enable_infer_pre = ref false
(* enable weak precondition inference *)
let enable_infer_weak_pre = ref false
(* enable maximally-weak precondition inference *)
let enable_infer_maximally_weak_pre = ref false

(* enable size abstraction *)
let enable_sizeabs = ref true
(* convert HCCS over int to HCCS over real *)
let conv_real = ref false
(* save generated HCCS as graphviz *)
let save_graphviz = ref false
(* template type for Skolemization *)
let temp_type = ref ""
(* HCCS reduction *)
let reduce_hccs = ref true
let force_reduce = ref false
let complement_reduced_preds = ref false

let add_pre_pvar prog rtenv =
  let pvar_ref = ref None in
  let rtenv =
    List.map
      (function
        | RefTypEnv.Env(x, rty) when x = Idnt.make prog.Prog.main ->
          let args, ret = RefTyp.args_ret rty in
          let args, arg = List.initial_last args in
          let rty, pvar =
            RefTyp.add_pvar
              (List.map (Pair.map_snd RefTyp.to_simple_type) args)
              (snd arg)
          in
          pvar_ref := Some pvar;
          RefTypEnv.Env
            (x, RefTyp.mk_fun_args_ret_rty (args @ [fst arg, rty]) ret)
        | elem -> elem)
      rtenv
  in
  rtenv, !pvar_ref

(** Split prog.Prog.types into tenv for functions and for ADT,
    then make refinement type templates for functions.
    @note: give a simple type for prog.Prog.main
    and don't make templates for functions included in rtenv *)
let make_rtenv infer rtenv prog =
  prog.Prog.types
  |> List.partition (fst >> Prog.is_defined prog)
(*
  |> sef (Format.printf "@[<v> partitioned:%a@,@]@,"
            (Pair.pr
               (fun ppf a -> Format.fprintf ppf "defined: %a" TypEnv.pr a)
               (fun ppf a -> Format.fprintf ppf "undefined: %a" TypEnv.pr a)))
*)
  |> Pair.map_fst
    (List.filter (fst >> RefTypEnv.member rtenv >> not)
     >> List.partition (fst >> (=) (Idnt.make prog.Prog.main))
     >> Pair.map
       RefTypEnv.of_tenv
       ((if !use_simple_template
         then RefTypEnv.of_tenv_with_simple_template
         else if !use_precondition_template
         then RefTypEnv.of_tenv_with_precondition_template
         else RefTypEnv.of_tenv_with_template)
        >> (@) rtenv
        >> (@) (let obj_adt_types =
                  List.find_all (fun (a,b) -> a = Idnt.make "Obj")
                    prog.Prog.types
                in (* @todo avoid ad-hoc strategy *)
                if obj_adt_types = [] then []
                else
                  let RefTypEnv.Env(id,rty) =
                    RefTypEnv.of_tenv_with_precondition_template obj_adt_types
                    |> List.hd
                  in
                  let args, ret =
                    Pair.map_snd (RefTyp.to_simple_type
                                  >> RefTyp.of_simple_type)
                      (RefTyp.args_ret rty)
                  in
                  [ RefTypEnv.Env(id,RefTyp.mk_fun_args_ret_rty args ret) ] )
       )
     >> Pair.fold (@))
  |> (if_ (fun _ -> !nonterm_mode)
        (Pair.map_fst @@
         RefTypEnv.apply (Idnt.make prog.Prog.main)
           (RefTyp.set_phi_ret Formula.mk_false))
        id)
  |> (if_ (fun _ -> infer)
        (Pair.map_fst @@ add_pre_pvar prog)
        (Pair.map_fst @@ flip Pair.make None))
let make_rtenv ?(infer=false) ?(rtenv=[]) =
  Logger.log_block1 "MLVerifier.make_rtenv"
    ~after:(fun ((rtenv, _), tenv) ->
        Logger.printf2
          "output:@, rtenv: %a@, tenv: %a"
          RefTypEnv.pr rtenv TypEnv.pr tenv)
    (make_rtenv infer rtenv)

let gen_hccs wf strategies infer rtenv prog =
  let (rtenv, pvar_opt), ctenv = make_rtenv ~infer ~rtenv prog in
  Format.printf_force "Refinement Type Templates:@.  %a@.@." RefTypEnv.pr rtenv;
  RefTypCheck.init ();
  prog.Prog.fdefs
  |> List.map (RefTypCheck.tcheck_fdef ~wf strategies prog ctenv rtenv)
  |> ignore;
  !RefTypCheck.rev_consts
  |> List.map snd
  |> List.rev_map (fun phi -> HCCS.of_formula0 (Formula.fvs phi) phi)
  |> List.concat
  |> HCCS.simplify_light []
  |> SimTypInfer.infer_hccs prog.Prog.types
  |> Pair.map_snd @@ List.partition (HornClause.htenv_of >> (=) [])
  |> (fun (env, (h, e)) ->
      RefTypCheck.sync_wf_pvars h;
      rtenv, env, h @ e,
      !RefTypCheck.nondet_pvars,
      !RefTypCheck.wf_pvars,
      pvar_opt |> Option.fold [] List.return)
let gen_hccs ?(wf=false) ?(strategies=[]) ?(infer=false) ?(rtenv=[]) =
  Logger.log_block1 "MLVerifier.gen_hccs"
    (gen_hccs wf strategies infer rtenv)

let temp_gen =
  match !temp_type with
  | "eq" -> AEHCCSSolver.template_eq
  | "geq" -> AEHCCSSolver.template_geq
  | "leq" -> AEHCCSSolver.template_leq
  | "bound" -> AEHCCSSolver.template_bound
  | _ -> AEHCCSSolver.template_general

let preprocess hccs prog tenv0 sizes no_inline no_encode =
  let hccs =
    hccs
    |> (if !conv_real then HCCS.int_to_real else id)
    |> sef (Format.printf "@[<v>Generated HCCS:@,  %a@,@]@," HCCS.pr)
    |> (if no_encode then id else TupHCCSSolver.flatten)
    |> List.concat_map (AEHCCSSolver.skolemize temp_gen)
    |> SimTypInfer.infer_hccs prog.Prog.types |> snd (* need this? *)
    |> HCCS.simplify_light []
    |> List.unique (* need this? *)
    |> sef (Format.printf "@[<v>Skolemized HCCS:@,  %a@,@]@," HCCS.pr)
    |> (if no_encode then id else TupHCCSSolver.encode)
    |> HCCS.simplify_light []
    |> sef (Format.printf "@[<v>Tuple eliminated HCCS:@,  %a@,@]@," HCCS.pr)
  in
  let tenv =
    tenv0
    |> (if !conv_real then List.map (Pair.map_snd Type.int_to_real) else id)
    |> TupHCCSSolver.flatten_each_tenv
  in
  hccs
  |> HCCS.simplify_light []
  |> HCCS.simplify_full ~tenv []
  |> HCCS.simplify_lv2 ~tenv
  |> Pair.make tenv
  |> (if_ (const !enable_sizeabs)
        (ADTHCCSSolver.elim_adts ~sizes
         >> sef (Format.printf "@[<v>Size abstracted HCCS:@,  %a@,@]@," HCCS.pr))
        (ADTHCCSSolver.elim_accessors_recognizers
         >> sef (Format.printf
                   "@[<v>Accessors and recognizers eliminated HCCS:@,  %a@,@]@,"
                   HCCS.pr)))
  |> HCCS.elim_disj
  |> HCCS.simplify_light []
  |> HCCS.simplify_full ~tenv []
  |> HCCS.simplify_lv2 ~tenv
  (* @todo *)
  |> HCCS.map_phi (Formula.map_atom TupAtom.elim_teq_tneq)
  |> HCCS.simplify_full ~tenv []
  (* @todo *)
  |> sef (Format.printf "@[<v>Simplified HCCS:@,  %a@,@]@," HCCS.pr)
  |> (if_ (const (not !reduce_hccs))
        id
        (HCCS.simplify_trivial ~no_inline
         >> HCCS.reduce ~tenv no_inline
         >> HCCS.simplify_lv2 ~tenv
         >> HCCS.simplify_full ~tenv [](*@todo*)
         >> List.filter (HornClause.is_trivial >> not)
         >> sef (Format.printf "@[<v>Reduced HCCS:@,  %a@,@]@," HCCS.pr)
         >> if_ (const !save_graphviz)
           (sef (HCCS.save_graphviz (!Global.target_filename ^ ".dot")))
           id))
  |> if_ (const !conv_real) HCCS.int_to_real id
  (*|> HCCS.elim_ufuns*),
  tenv
(*
  |> ReduceHCCSSolver.solve_ehccs
    (fun _ -> false)
    !RHCCSSolver.ref_solver
    HCCSSolver.solve_dyn
*)

(** for maximally weak precondition inference *)
let improve =
  List.map (fun pvar ->
      let tenv = PredVar.args_of pvar in
      HornClause.mk_def
        ~tenv ~hphi:Formula.mk_true pvar [] Formula.mk_true)

let complement hcs sol =
  if !complement_reduced_preds then begin
    (* @todo suppress reduction below *)
    let prev_silent = !Global.silent in
    Global.silent := true;
    let sol =
      HCCS.subst sol hcs
      |> ComplementHCCSSolver.solve (!RHCCSSolver.ref_solver >> fst)
      |> (@) sol
    in
    Global.silent := prev_silent;
    sol
  end else ComplementHCCSSolver.complement hcs sol

let solve_safety infer rtenv sizes strategies hcs prog =
  if rtenv <> [] then Logger.printf "refinement types: %a@," RefTypEnv.pr rtenv;
  if sizes <> [] then Logger.printf "size functions: %a@," SizeFun.pr sizes;
  let rtenv, tenv0, hccs0, nds, [], pre_pvars =
    gen_hccs ~infer ~rtenv ~strategies prog
  in
  let hccs0 = hcs @ hccs0 @ improve pre_pvars in
  Format.printf_force "HCCS:@.  %a@.@." HCCS.pr hccs0;
  let hccs, tenv =
    let ps = nds @ List.map PredVar.idnt_of pre_pvars in
    preprocess hccs0 prog tenv0 sizes
      (fun x -> not !force_reduce && List.mem x ps) false
  in
  if !enable_infer_maximally_weak_pre && pre_pvars <> [] then
    RHCCSSolver.ref_solver :=
      !RHCCSSolver.ref_solver
      |> MultiObjectiveHCCSSolver.maximally_weak_pre (List.hd pre_pvars);
  let sol, coeff_sol =
    if !enable_infer_weak_pre && pre_pvars <> [] then
      UnusedHCCSSolver.solve_rec true
        (MultiObjectiveHCCSSolver.weak_pre_rechccssolver
           (List.hd pre_pvars) !RHCCSSolver.ref_solver)
        hccs
    else UnusedHCCSSolver.solve_rec true !RHCCSSolver.ref_solver hccs
  in
  let sol = sol |> TupHCCSSolver.unflatten tenv0 |> complement hccs0 in
  Format.printf_force "Solution of HCCS:@.  %a@.@." PredSubst.pr sol;
  RefTypEnv.subst coeff_sol (RefTypEnv.subst_pvars sol rtenv),
  List.filter_map
    (fun p ->
       try Some(p, List.assoc p sol) with Not_found -> None(*any pred is OK*))
    nds,
  coeff_sol,
  List.filter_map
    (fun pvar ->
       try let p = pvar |> PredVar.idnt_of in Some(p, List.assoc p sol)
       with Not_found -> None(*any pred is OK*))
    pre_pvars
let solve_safety
    ?(infer=false) ?(rtenv=[]) ?(sizes=[]) ?(strategies=[]) ?(hcs=[]) =
  Logger.log_block1 "MLVerifier.solve_safety"
    (solve_safety infer rtenv sizes strategies hcs)

let solve_safety_ind rtenv lemmas sizes strategies hcs prog =
  let rtenv, tenv, hccs, nds, [], pre_pvars =
    gen_hccs ~rtenv ~strategies prog
  in
  let hccs = hcs @ hccs in
  let old = !enable_sizeabs in
  enable_sizeabs := false;
  let hccs, tenv = preprocess hccs prog tenv [] (fun _ -> false) false in
  enable_sizeabs := old;
  hccs, InductHCCS.algo_solve hccs lemmas tenv
let solve_safety_ind
    ?(rtenv=[]) ?(lemmas=[]) ?(sizes=[]) ?(strategies=[]) ?(hcs=[]) =
  Logger.log_block1 "MLVerifier.solve_safety_ind"
    (solve_safety_ind rtenv lemmas sizes strategies hcs)

let solve_termination infer rtenv sizes ranks strategies hcs prog =
  if rtenv <> [] then Logger.printf "refinement types: %a@," RefTypEnv.pr rtenv;
  if sizes <> [] then Logger.printf "size functions: %a@," SizeFun.pr sizes;
  if ranks <> [] then Logger.printf "ranking functions: %a@," RankFun.pr ranks;
  let rtenv, tenv0, hccs0, nds, wfcs, pre_pvars =
    gen_hccs ~infer ~wf:true ~strategies ~rtenv prog
  in
  let hccs0 = hcs @ hccs0 @ improve pre_pvars in
  Format.printf_force "HCCS:@.  %a@.@." HCCS.pr hccs0;
  let hccs, tenv =
    let ps = nds @ List.map PredVar.idnt_of pre_pvars in
    preprocess hccs0 prog tenv0 sizes
      (fun x -> not !force_reduce && (RefTypCheck.is_wf_pvar x || List.mem x ps)) false
  in
  if !enable_infer_maximally_weak_pre && pre_pvars <> [] then
    RHCCSSolver.ref_solver :=
      !RHCCSSolver.ref_solver
      |> MultiObjectiveHCCSSolver.maximally_weak_pre (List.hd pre_pvars);
  let rankfuns, sol, coeff_sol =
    if !enable_infer_weak_pre && pre_pvars <> [] then
      MultiObjectiveHCCSSolver.weak_pre_termination
        (List.hd pre_pvars)
        (WFHCCSSolver.solve_hccs ~silent:true wfcs ranks)
        hccs
    else WFHCCSSolver.solve wfcs ranks hccs
  in
  let sol = sol |> TupHCCSSolver.unflatten tenv0 |> complement hccs0 in
  Format.printf_force "Solution of HCCS:@.  %a@.@." PredSubst.pr sol;
  RefTypEnv.subst coeff_sol (RefTypEnv.subst_pvars sol rtenv),
  List.filter_map
    (fun p ->
       try Some(p, List.assoc p sol) with Not_found -> None(*any pred is OK*))
    nds,
  coeff_sol,
  rankfuns,
  List.filter_map
    (fun pvar ->
       try let p = pvar |> PredVar.idnt_of in Some(p, List.assoc p sol)
       with Not_found -> None(*any pred is OK*))
    pre_pvars
let solve_termination
    ?(infer=false) ?(rtenv=[]) ?(sizes=[]) ?(ranks=[]) ?(strategies=[])
    ?(hcs=[]) =
  Logger.log_block1 "MLVerifier.solve_termination"
    (solve_termination infer rtenv sizes ranks strategies hcs)

let solve_multiobj
    rtenv sizes ranks strategies pvtempl hccs
    (pvpole:PredVarPoles.t) (pvprior:PredVarPriority.t) prog =
  let rtenv, tenv, hccs_aux, nds, wfcs, pre_pvars =
    gen_hccs ~wf:!termination_mode ~strategies ~rtenv prog
  in
  let hccs = hccs @ hccs_aux in
  let pvs = hccs |> HCCS.pvs |> List.unique in
  let pvpole = nds |> List.map (flip Pair.make false) |> (@) pvpole in
  let pvprior =
    let _pvs = PredVarPriority.list_of pvprior in
    nds
    |> List.filter (flip List.mem _pvs >> not)
    |> List.rev (* heuristic *)
    |> PredVarPriority.of_list
    |> (@) pvprior
    |> PredVarPriority.topological_sort pvs
  in
  let hccs, tenv =
    let ps = nds @ List.map PredVar.idnt_of pre_pvars in
    preprocess hccs prog tenv sizes
      (fun x -> RefTypCheck.is_wf_pvar x || List.mem x ps) false
  in
  let pvars =
    List.map
      (fun p ->
         try
           List.find_map
             (HornClause.hpv_of >> function
               | Some pv when p = PredVar.idnt_of pv -> Some pv
               | _ -> None)
             hccs
         with Not_found -> assert false)
      wfcs
  in
  let rankfuns =
    wfcs
    |> List.concat_map (WFHCCSSolver.info_ty_of hccs)
    |> WFHCCSSolver.unique
    |> List.map (RankFun.mk_templates ranks)
  in
  let hccs =
    if !termination_mode then
      List.map (WFHCCSSolver.wfrel_of_rankfuns rankfuns) pvars
      |> (@) hccs
      |> sef (Format.printf "@[<v>HCCS with WF constrs:@,  %a@,@]@," HCCS.pr)
    else hccs
  in
  hccs
  |> (MultiObjectiveHCCSSolver.optimize
        (MultiObjectiveHCCSSolver._solve ~pvtempl:pvtempl)
        pvpole pvprior)
  |> (fun s ->
      let sol = match s with
        | MultiObjectiveHCCSSolver.OptSol(sol) ->
          Format.printf
            "@.@[<v>Pareto Optimal Solutions:@, %a@]@."
            PredSubst.pr sol;
          Bench.additional_msg :=
            string_of_int !MultiObjectiveHCCSSolver.count ^ ",Opt";
          sol
        | MultiObjectiveHCCSSolver.Sol(sol) ->
          Format.printf
            "@.@[<v>Possibly Non-Pareto Optimal Solutions:@, %a@]@."
            PredSubst.pr sol;
          Bench.additional_msg :=
            string_of_int !MultiObjectiveHCCSSolver.count ^ ",non";
          sol
      in
      RefTypEnv.subst_pvars sol rtenv,
      List.filter_map
        (fun p ->
           try Some(p, List.assoc_fail p sol)
           with Not_found -> None(*any pred is OK*))
        nds)
let solve_multiobj
    ?(rtenv=[]) ?(sizes=[]) ?(ranks=[]) ?(strategies=[])
    ?(pvtempl=[]) ?(hcs=[]) =
  Logger.log_block1 "MLVerifier.solve_multiobj"
    (solve_multiobj rtenv sizes ranks strategies pvtempl hcs)
