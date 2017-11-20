open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_util
open CEGAR_trans

exception CannotRefute

module Debug = Debug.Make(struct let check = make_debug_check __MODULE__ end)

let new_id' x = new_id (Format.sprintf "%s_%d" x !Flag.cegar_loop)

let decomp_path p =
  match decomp_app p with
  | Const (Temp "path"), p'::path ->
      p', List.map (function Const (Int n) -> n | _ -> assert false) path
  | _ -> assert false

let compose_path path p =
  make_app (Const (Temp "path")) (p :: List.map make_int path)

let rec copy_attr ty1 ty2 =
  match ty1,ty2 with
  | TBase _, TBase _ -> ty2
  | TFun(TApp(TConstr TAssumeTrue, ty11),ty12), TFun(ty21,ty22) ->
      begin
        match copy_attr (TFun(ty11,ty12)) (TFun(ty21,ty22)) with
        | TFun(ty1',ty2') -> TFun(TApp(TConstr TAssumeTrue,ty1'), ty2')
        | _ -> assert false
      end
  | TFun(ty11,ty12), TFun(ty21,ty22) ->
      let x = new_id' "x" in
      let ty2' = copy_attr (ty12 (Var x)) (ty22 (Var x)) in
      TFun(copy_attr ty11 ty21, subst_typ x -$- ty2')
  | TBase _, _
  | TFun _, _
  | TConstr _, _
  | TApp _, _ ->
      Format.printf "Refine.copy_attr: %a, %a@." CEGAR_print.typ ty1 CEGAR_print.typ ty2;
      assert false

let rec can_move ty =
  match ty with
  | TBase _ -> true
  | TFun(TApp(TConstr TAssumeTrue,TBase _),_) -> true
  | TFun(TBase _,ty2) -> true
  | TFun(TApp(TConstr TAssumeTrue,TFun _),ty2) -> can_move @@ ty2 (Var "d")
  | TFun(TFun(TBase _,ty12),ty2) when is_typ_result @@ ty12 (Var "d") && is_typ_result @@ ty2 (Var "d") -> true
  | TFun(TFun _,ty2) -> false
  | _ -> assert false

let rec move_arg_pred ty =
  match ty with
  | TBase _ -> ty
  | TFun(TApp(TConstr TAssumeTrue,TBase(b,ps)),ty2) when can_move @@ ty2 (Var "d") ->
      let x = new_id' "x" in
      let ps_x = ps (Var x) in
      let ty1' = TBase(b,fun _ -> []) in
      let ty2' =
        let add b' ps' =
          let y = new_id' "y" in
          let ps'_y = ps' (Var y) in
          let aux p1 p2 =
            let p1',path1 = decomp_path p1 in
            let p2',path2 = decomp_path p2 in
            let path1',c = List.decomp_snoc path1 in
            assert (c = 0);
            if p1' = Const True then
              p2
            else
              if List.is_prefix path1' path2 then
                if p2' = Const True then
                  make_not_s p1' |> compose_path path2
                else
                  make_imply p1' p2' |> compose_path path2
              else
                p2
          in
          let ps'_y' = List.map (List.fold_right aux ps_x) ps'_y in
          TBase(b', fun t -> List.map (subst y t) ps'_y')
        in
        map_base add @@ move_arg_pred @@ ty2 (Var x)
      in
      TFun(ty1', subst_typ x -$- ty2')
  | TFun(TApp(TConstr TAssumeTrue,ty1),ty2) -> move_arg_pred @@ TFun(ty1, ty2)
  | TFun(ty1,ty2) -> TFun(move_arg_pred ty1, move_arg_pred -| ty2)
  | TConstr _ -> unsupported "Refine"
  | TApp _ -> unsupported "Refine"
let move_arg_pred typ =
  Debug.printf "[MOVE_PRED] input: %a@." CEGAR_print.typ typ;
  let r = move_arg_pred typ in
  Debug.printf "[MOVE_PRED] output: %a@." CEGAR_print.typ r;
  r

let rec remove_arg_pred ty =
  match ty with
  | TBase(b,ps) ->
      let x = new_id' "x" in
      let ps_x = ps (Var x) in
      let ps_x' =
        let aux t =
          let t',_ = decomp_path t in
          if t' = Const True then
            []
          else
            [t']
        in
        List.flatten_map aux ps_x
      in
      TBase(b, fun t -> List.map (subst x t) ps_x')
  | TFun(ty1,ty2) -> TFun(remove_arg_pred ty1, remove_arg_pred -| ty2)
  | TApp(TConstr TAssumeTrue, ty2) -> remove_arg_pred ty2
  | TApp _ -> assert false
  | TConstr _ -> assert false

let rec remove_redundant_pred ty =
  match ty with
  | TBase(b,ps) ->
      let x = new_id' "x" in
      let ps_x = ps (Var x) in
      let ps_x' =
        let rec aux ps =
          match ps with
          | [] -> []
          | p::ps' ->
              if List.exists ((=) p) ps' then
                aux ps'
              else
                p :: aux ps'
        in
        aux ps_x
      in
      TBase(b, fun t -> List.map (subst x t) ps_x')
  | TFun(ty1,ty2) -> TFun(remove_redundant_pred ty1, remove_redundant_pred -| ty2)
  | TApp(TConstr TAssumeTrue, ty2) -> remove_redundant_pred ty2
  | TApp _ -> assert false
  | TConstr _ -> assert false

let add_preds_env map env =
  let aux (f,typ1) =
    let typ' =
      try
        let typ2 =
          List.assoc f map
          |@> Debug.printf "INPUT: %a@." CEGAR_print.typ
          |> copy_attr typ1
          |@> Debug.printf "COPY: %a@." CEGAR_print.typ
          |> move_arg_pred
          |@> Debug.printf "MOVE: %a@.@." CEGAR_print.typ
          |> remove_arg_pred
          |> remove_redundant_pred
        in
        merge_typ typ1 typ2
      with Not_found -> typ1
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
  | TApp _ -> Format.printf "negate_typ: %a." CEGAR_print.typ ty; assert false

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
  | _ -> Format.printf "%a@." CEGAR_print.typ typ2; assert false

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



let refine_post tmp =
  Fpat.SMTProver.finalize ();
  Fpat.SMTProver.initialize ();
  Time.add tmp Flag.time_cegar

let refine labeled is_cp prefix ces ext_ces prog =
  let tmp = Time.get () in
  try
    if !Flag.print_progress then
      Color.printf
        Color.Green
        "(%d-4) Discovering predicates (infeasible case) ... @."
        !Flag.cegar_loop;
    if Flag.use_prefix_trace then
      fatal "Not implemented: Flag.use_prefix_trace";
    let map =
      let ces,ext_ces =
        if !Flag.use_multiple_paths then
          ces, ext_ces
        else
          [List.hd ces], [List.hd ext_ces]
      in
      Format.printf "@[<v>";
      let map = FpatInterface.infer labeled is_cp ces ext_ces prog in
      Format.printf "@]";
      map
    in
    let env =
      if !Flag.disable_predicate_accumulation then
        map
      else
        add_preds_env map prog.env
    in
    if !Flag.print_progress then Format.printf "DONE!@.@.";
    refine_post tmp;
    map, {prog with env}
  with e ->
    refine_post tmp;
    raise e

let refine_with_ext labeled is_cp prefix ces ext_ces prog =
  let tmp = Time.get () in
  try
    if !Flag.print_progress then
      Color.printf
        Color.Green
        "(%d-4) Discovering predicates (feasible case) ... @."
        !Flag.cegar_loop;
    if Flag.use_prefix_trace then
      raise (Fatal "Not implemented: Flag.use_prefix_trace");
    Format.printf "@[<v>";
    let map = FpatInterface.infer_with_ext labeled is_cp ces ext_ces prog in
    Format.printf "@]";
    let env =
      if !Flag.disable_predicate_accumulation then
        map
      else
        add_preds_env map prog.env
    in
    if !Flag.print_progress then Format.printf "DONE!@.@.";
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
    if !Flag.print_progress then Format.printf "(%d-4) Discovering ranking function ... @." !Flag.cegar_loop;
    let env, spc =
      Format.printf "@[<v>";
      let env, spc = FpatInterface.compute_strongest_post prog ce ex_ce in
      Format.printf "@]";
      env, spc
    in

    let spcWithExparam =
      if !Flag.add_closure_exparam
      then
        let progWithExparam = Option.get prog.info.exparam_orig in
        if false then Format.printf "REFINE: %a@." CEGAR_print.prog @@ Option.get prog.info.exparam_orig;
        Format.printf "@[<v>";
        let _, spcWithExparam = FpatInterface.compute_strongest_post progWithExparam ce ex_ce in
        Format.printf "@]";
        if false then Format.printf "REFINE: %a@." Fpat.Formula.pr spcWithExparam;
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
