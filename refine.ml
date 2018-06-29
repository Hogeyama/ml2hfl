open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_util
open CEGAR_trans

exception CannotRefute

module Debug = Debug.Make(struct let check = Flag.Debug.make_check __MODULE__ end)

let new_id' x = new_id (Format.sprintf "%s_%d" x !Flag.Log.cegar_loop)


let add_preds_env map env =
  let aux (f,typ1) =
    let typ' =
      List.assoc_option f map
      |> Option.map @@ merge_typ typ1
      |> Option.default typ1
    in
    f, typ'
  in
  List.map aux env

let add_renv map env =
  let aux (n, preds) = make_randint_name n, TBase(TInt, preds) in
  add_preds_env (List.map aux map) env

let rec negate_typ ty =
  match ty with
  | TBase(b,ps) ->
      let x = new_id' "x" in
      let preds = List.map make_not @@ ps (Var x) in
      let ps t = List.map (subst x t) preds in
      TBase(b, ps)
  | TFun(typ1,typ2) ->
      let x = new_id' "x" in
      let typ1 = negate_typ typ1 in
      let typ2 = negate_typ @@ typ2 (Var x) in
      TFun(typ1, subst_typ x -$- typ2)
  | TConstr _
  | TApp _ -> Format.eprintf "negate_typ: %a." CEGAR_print.typ ty; assert false

let add_nag_preds_renv env =
  let aux (f,typ) = if is_randint_var f then merge_typ typ (negate_typ typ) else typ in
  List.map aux env

let add_preds map prog =
  {prog with env = add_preds_env map prog.env}


let rec add_to_path path typ1 typ2 =
  match path,typ2 with
  | [],_ -> merge_typ typ1 typ2
  | 0::path',TFun(typ21,typ22) -> TFun(add_to_path path' typ1 typ21, typ22)
  | 1::path',TFun(typ21,typ22) -> TFun(typ21, add_to_path path' typ1 -| typ22)
  | _ -> Format.eprintf "%a@." CEGAR_print.typ typ2; assert false

let rec add_pred n path typ =
  match typ with
  | TBase _ -> assert false
  | TFun(typ1,typ2) when n=0 ->
      TFun(typ1, add_to_path (List.tl path) typ1 -| typ2)
  | TFun(typ1,typ2) ->
      assert (List.hd path = 1);
      TFun(typ1, add_pred (n-1) (List.tl path) -| typ2)
  | TConstr _ -> assert false
  | TApp _ -> assert false

let rec col_fix_pred path env_rev acc ty =
  match ty with
  | TBase _ -> acc
  | TApp(TConstr(TFixPred p), (TBase(b, _) as ty')) ->
      let x = new_id "x" in
      let env = List.rev @@ (x,ty')::env_rev in
      (path, env, p (Var x))::acc
  | TFun _ ->
      let args,ty' = decomp_tfun_env ty in
      assert (get_base ty' = typ_result_base);
      let aux (env_rev,i,acc) (x,ty1) =
        let env_rev' = if is_base ty1 then (x,ty1)::env_rev else env_rev in
        let acc' = col_fix_pred (i::path) env_rev acc ty1 in
        env_rev', i+1, acc'
      in
      Triple.trd @@ List.fold_left aux (env_rev,0,acc) args
  | _ -> assert false
let fix_pred_of f ty =
  ty
  |@> Debug.printf "[fix_pred_of] INPUT %s : %a@." f CEGAR_print.typ
  |> col_fix_pred [] [] []
  |@> Debug.printf "[fix_pred_of] OUTPUT %s : %a@." f Print.(list (triple (list int) __ CEGAR_print.term))
  |> List.map (fun (path,env,p) -> (FpatInterface.conv_var f,path), (env,p))

let instansiate_pred_by_env env c =
  Debug.printf "env: %a@." CEGAR_print.env env;
  let paths = List.flatten_map (Fun.uncurry fix_pred_of) env in
  Debug.printf "c: %a@." Fpat.HCCS.pr c;
  Debug.printf "paths: %a@." Print.(list (pair (pair Fpat.Idnt.pr (list int)) (pair __ CEGAR_print.term))) paths;
  let rec has_path x (y,path) =
    match x with
    | Fpat.Idnt.T(x', _, arg) ->
        begin
          match path with
          | [] -> false
          | i::path' -> i = arg && has_path x' (y,path')
        end
    | _ -> path = [] && x = y
  in
  let map =
    let aux (x,_) =
      try
        let (_,(args,t)) = List.find (has_path x -| fst) paths in
        let args = List.map FpatInterface.(Pair.map conv_var conv_typ) args in
        Some (x, (args, FpatInterface.conv_formula t))
      with Not_found -> None
    in
    Fpat.HCCS.tenv c
    |@> Debug.printf "tenv: %a@." Fpat.TypEnv.pr
    |> List.filter_map aux
    |@> Debug.printf "tenv': %a@." Print.(list (pair Fpat.Idnt.pr (pair Fpat.TypEnv.pr Fpat.Formula.pr)))
  in
  Fpat.HCCS.subst map c


let refine_post tmp =
  Fpat.SMTProver.finalize ();
  Fpat.SMTProver.initialize ();
  Time.add tmp Flag.Log.time_cegar

let refine labeled is_cp prefix ces ext_ces prog =
  let tmp = Time.get () in
  try
    Verbose.printf
      "%a(%d-4) Discovering predicates (infeasible case) ...%t @."
      Color.set Color.Green !Flag.Log.cegar_loop Color.reset;
    if Flag.Refine.use_prefix_trace then
      fatal "Not implemented: Flag.use_prefix_trace";
    let map =
      let ces,ext_ces =
        if !Flag.Refine.use_multiple_paths then
          ces, ext_ces
        else
          [List.hd ces], [List.hd ext_ces]
      in
      let solver orig c =
        let c' = instansiate_pred_by_env prog.env c in
        Verbose.printf "@[<v>";
        let r =
          if !Flag.Refine.use_rec_chc_solver then
            try
              Rec_CHC_solver.solve c'
            with
            | Rec_CHC_solver.TimeOut
            | Rec_CHC_solver.SolverAborted -> orig c'
          else
            orig c'
        in
        Verbose.printf "@]";
        r
      in
      FpatInterface.infer solver labeled is_cp ces ext_ces prog
    in
    let env =
      if !Flag.Refine.disable_predicate_accumulation then
        map
      else
        add_preds_env map prog.env
    in
    Verbose.printf "DONE!@.@.";
    refine_post tmp;
    map, {prog with env}
  with e ->
    refine_post tmp;
    raise e

let refine_with_ext labeled is_cp prefix ces ext_ces prog =
  let tmp = Time.get () in
  try
    if !Flag.Print.progress then
      Color.printf
        Color.Green
        "(%d-4) Discovering predicates (feasible case) ... @."
        !Flag.Log.cegar_loop;
    if Flag.Refine.use_prefix_trace then
      raise (Fatal "Not implemented: Flag.use_prefix_trace");
    Format.printf "@[<v>";
    let map = FpatInterface.infer_with_ext labeled is_cp ces ext_ces prog in
    Format.printf "@]";
    let env =
      if !Flag.Refine.disable_predicate_accumulation then
        map
      else
        add_preds_env map prog.env
    in
    if !Flag.Print.progress then Format.printf "DONE!@.@.";
    refine_post tmp;
    map, {prog with env}
  with e ->
    refine_post tmp;
    raise e

exception PostCondition of (Fpat.Idnt.t * Fpat.Type.t) list * Fpat.Formula.t * Fpat.Formula.t

let print_list fm = function
  | [] -> Format.fprintf fm "[]@."
  | x::xs ->
    let rec iter = function
      | [] -> ""
      | y::ys -> ", " ^ string_of_int y ^ iter ys
    in
    Format.fprintf fm "[%d%s]@." x (iter xs)

let refine_rank_fun ce ex_ce prog =
  let tmp = Time.get () in
  try
    (*Format.printf "(%d)[refine_rank_fun] %a @." !Flag.cegar_loop print_list ce;
      Format.printf "    %a@." (print_prog_typ' [] []) { env=env; defs=defs; main=main };*)
    if !Flag.Print.progress then Format.printf "(%d-4) Discovering ranking function ... @." !Flag.Log.cegar_loop;
    let env, spc =
      Format.printf "@[<v>";
      let env, spc = FpatInterface.compute_strongest_post prog ce ex_ce in
      Format.printf "@]";
      env, spc
    in

    let spcWithExparam =
      if !Flag.Termination.add_closure_exparam
      then
        let progWithExparam = Option.get prog.info.exparam_orig in
        Debug.printf "REFINE: %a@." CEGAR_print.prog @@ Option.get prog.info.exparam_orig;
        Format.printf "@[<v>";
        let _, spcWithExparam = FpatInterface.compute_strongest_post progWithExparam ce ex_ce in
        Format.printf "@]";
        Debug.printf "REFINE: %a@." Fpat.Formula.pr spcWithExparam;
        spcWithExparam
      else spc (* dummy *)
    in

    (* TEMPORARY *)
    (*Format.printf "[exparam]@.%a@." FpatInterface.Formula.pr spcWithExparam;
      Format.printf "[instantiated]@.%a@." FpatInterface.Formula.pr spc;*)

    raise (PostCondition (env, spc, spcWithExparam))
  with e ->
    refine_post tmp;
    raise e
