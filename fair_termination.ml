open Util
open Syntax
open Term_util
open Type
open Fair_termination_type
open Fair_termination_util

exception FailedToFindRF

let debug () = List.mem "Fair_termination" !Flag.debug_module


type trans_env = {target:id; target_xs:id list; target_result_typ:typ; fairness:fairness; s:id; set_flag:id; ps:id list}

let rec subst_state fairness s q b =
  let aux q' i j = if q = q' then make_bool b else make_proj j @@ make_proj i @@ make_var s in
  make_tuple @@ List.mapi (fun i (q1,q2) -> make_pair (aux q1 i 0) (aux q2 i 1)) fairness

let make_is_fair s =
  let aux i j = make_proj j @@ make_proj i @@ make_var s in
  List.fold_left make_and true_term @@ List.init (Option.get @@ tuple_num @@ Id.typ s) (fun i -> make_imply (aux i 0) (aux i 1))

let make_extra_vars fairness args =
  let s = Id.new_var ~name:"s" (make_s_init fairness).typ in
  let flag = Id.new_var ~name:"set_flag" TBool in
  let args' = List.filter (is_ground_typ -| Id.typ) args in
  let ps = List.map (Id.new_var_id |- Id.add_name_before "p_") args' in
  s, flag, ps

let join x y =
  match x,y with
  | None, _ -> y
  | _, None -> x
  | Some _, Some _ -> assert false

let trans = make_tr_col2 None join

let trans_value env v =
  let _,typ = trans.tr_col2_typ env v.typ in
  match v.desc with
  | Var x -> make_var @@ Id.set_typ x typ
  | _ -> {v with typ}

let trans_typ env typ =
  match typ with
  | TFun _ ->
      let xs,typ1 = decomp_tfun typ in
      let xs' = List.map (Id.map_typ @@ snd -| trans.tr_col2_typ env) xs in
      let _,typ1' = trans.tr_col2_typ env typ1 in
      let aux x typ' =
        let zs = [env.s; env.set_flag] @ env.ps @ [x] in
        List.fold_right _TFun zs @@ make_tpair (Id.typ env.s) typ'
      in
      None, List.fold_right aux xs' typ1'
  | _ -> trans.tr_col2_typ_rec env typ

(* Assume that input is in normal form *)
let trans_term env t =
  match t.desc with
  | _ when is_value t -> None, make_pair (make_var env.s) @@ trans_value env t
  | _ when t.desc = randint_unit_term.desc -> None, make_pair (make_var env.s) @@ trans_value env t
  | App({desc=Event(q, _)}, [_]) ->
      None, make_pair (subst_state env.fairness env.s q true) unit_term
  | App(t1, [_]) when t1.desc = fail_term.desc -> None, make_fail @@ make_tpair (Id.typ env.s) t.typ
  | App(v1, vs) ->
      let v1' = trans_value env v1 in
      if false then Format.printf "%a@." Print.term' v1';
      let vs' = List.map (trans_value env) vs in
      None, make_app v1' @@ List.flatten_map (List.snoc @@ List.map make_var (env.s::env.set_flag::env.ps)) vs'
  | If(v1, t2, t3) ->
      let vs2,t2' = trans.tr_col2_term env t2 in
      let vs3,t3' = trans.tr_col2_term env t3 in
      join vs2 vs3, make_if v1 t2' t3'
  | Let(_, [x,[],t1], t2) when not @@ Id.same x env.target ->
      if false then Format.printf "t1: %a@." Print.term t1;
      let vs1,t1' = trans.tr_col2_term env t1 in
      if false then Format.printf "t1': %a@." Print.term t1';
      let sx = Id.set_typ (Id.add_name_before "s__" @@ new_var_of_term t1) t1'.typ in
      let s' = Id.new_var_id env.s in
      let vs2,t2' = trans.tr_col2_term {env with s=s'} t2 in
      let _,x' = trans.tr_col2_var env x in
      join vs1 vs2, make_lets [sx,[],t1'; s',[],make_fst(make_var sx); x',[],make_snd(make_var sx)] t2'
  | Let(flag, bindings, t2) ->
      let aux (g,xs,t1) =
        if xs = [] then unsupported @@ Format.asprintf "fair termination!? %a" Print.id g;
        let args = List.map (fun _ -> make_extra_vars env.fairness env.target_xs) xs in
        let s',set_flag',ps' = List.last args in
        let vs, t1' =
          if Id.same g env.target
          then
            let b = Id.new_var TBool in
            let s'' = Id.new_var_id s' in
            let set_flag'' = Id.new_var_id set_flag' in
            Format.printf "set_flag': %a@." Id.print set_flag';
            Format.printf "set_flag'': %a@." Id.print set_flag'';
            let ps'' = List.map Id.new_var_id ps' in
            let xs' = List.filter (is_ground_typ -| Id.typ) xs in
            let rank_var = Id.new_var ~name:"rank" @@ List.fold_right _TFun (ps'@xs') TBool in
            let t1'' =
                let bindings' =
                  (s'', [], make_if (make_var b) (make_s_init env.fairness) (make_var s')) ::
                  (set_flag'', [], true_term) ::
                  List.map3 (fun x p' p'' -> p'', [], make_if (make_var b) (make_var x) (make_var p')) xs' ps' ps''
                in
                let t_rank = make_app (make_var rank_var) @@ List.map make_var (ps'@xs') in
                let t_check = make_if (make_is_fair s') (make_assert @@ make_imply (make_var set_flag') t_rank) unit_term in
                let vs,t1''' = trans.tr_col2_term {env with s=s''; ps=ps''; set_flag=set_flag''} t1 in
                assert (vs = None);
                make_lets bindings' @@ make_seq t_check t1'''
            in
            let t1''' =
              if !Flag.expand_nondet_branch
              then make_if randbool_unit_term (subst b true_term t1'') (subst b false_term t1'')
              else make_let [b,[],randbool_unit_term] t1''
            in
            Some (rank_var, ps', xs'), t1'''
          else
            trans.tr_col2_term {env with s=s'; set_flag=set_flag'} t1
        in
        let _,g' = trans.tr_col2_var env g in
        if false then Format.printf "g'[%d]: %a@." (List.length env.ps) Print.id_typ g';
        let xs'' = List.map (snd -| trans.tr_col2_var env) xs in
        let aux (s,set_flag,ps) x (first,t) =
          let t' = if first then t else make_pair (make_var s) t in
          false, make_funs (s::set_flag::ps@[x]) t'
        in
        vs, (g', [], snd @@ List.fold_right2 aux args xs'' (true, t1'))
      in
      let vss,bindings' = List.split_map aux bindings in
      let vs2,t2' = trans.tr_col2_term env t2 in
      List.fold_left join vs2 vss, make_let_f flag bindings' t2'
  | _ ->
      if !!debug then Format.printf "%a@." Print.term t;
      unsupported "Fair termination"

let () = trans.tr_col2_typ <- trans_typ
let () = trans.tr_col2_term <- trans_term

let rec get_top_fun_typ f t =
  match t.desc with
  | Let(_, bindings, t1) ->
      begin
        try
          let _,xs,t = List.find (fun (g,_,_) -> Id.same f g) bindings in
          xs, t.typ
        with Not_found -> get_top_fun_typ f t1
      end
  | _ -> invalid_argument "get_top_fun_typ"

let trans target fairness t =
  let target_xs,target_result_typ = get_top_fun_typ target t in
  let s, set_flag, ps = make_extra_vars fairness target_xs in
  if false then Format.printf "ps: %a@." (List.print Print.id) ps;
  let vs,t' = trans.tr_col2_term {target; target_xs; target_result_typ; fairness; s; set_flag; ps} t in
  let bindings =
    (s, [], make_s_init fairness) ::
    (set_flag, [], false_term) ::
    List.map (fun p -> p, [], make_term @@ Id.typ p) ps
  in
  vs,make_lets bindings t'


let verify_with rank_var rank_funs prev_vars arg_vars t =
  let ps = List.map Id.new_var_id prev_vars in
  let xs = List.map Id.new_var_id arg_vars in
  let t' = make_let [rank_var, ps@xs, make_check_rank ps xs rank_funs] t in
  Main_loop.run [] t'

let rec main_loop rank_var rank_funs prev_vars arg_vars preds_info(*need?*) t =
  try
    let result =
      if !Flag.separate_pred then
        unsupported "separte_pred"
      else if !Flag.split_callsite then
        unsupported "split_callsite"
      else
        verify_with rank_var rank_funs prev_vars arg_vars t
    in
    result
  with
  | Refine.PostCondition(env, spc, spcWithExparam) ->
      let rank_funs' =
        let aux = Fpat.Idnt.make -| Id.to_string in
        let arg_vars' = List.map aux arg_vars in
        let prev_vars' = List.map aux prev_vars in
        try
          List.map (fun (coeffs,const) -> {coeffs; const}) @@ Fpat.RankFunInfer.lrf spc arg_vars' prev_vars'
        with Not_found -> raise FailedToFindRF (* need fix Fpat.RankFunInfer.lrf to raise an appropriate exception *)
      in
      if !!debug then List.iter (Format.printf "Found ranking function: %a@.@." @@ print_rank_fun arg_vars) rank_funs';
      let preds_info' = (rank_funs',spc)::preds_info in
      let rank_funs'' = rank_funs' @ rank_funs in
      main_loop rank_var rank_funs'' prev_vars arg_vars preds_info' t





let pr ?(check_typ=Some TUnit) s t =
  if !!debug then
    begin
      Format.printf "##[%aFair_termination%t] %a:@.%a@.@." Color.set Color.Yellow Color.reset Color.s_red s Print.term_typ t;
      Option.iter (Type_check.check t) check_typ
    end

let run spec t =
  let {Spec.fairness} = spec in
  Format.printf "FAIRNESS: %a@.@." print_fairness fairness;
  let has_call = List.exists (fun (p,q) -> p = "Call" || q = "Call") fairness in
  let t' =
    t
    |> Trans.copy_poly_funs
    |@> pr "copy poly. funs."
    |> remove_and_replace_event
    |@> pr "remove_and_replace_event"
    |> normalize
    |@> pr "normalize"
    |&has_call&> add_call
    |@has_call&> pr "add event Call"
  in
  let top_funs = List.rev @@ get_top_rec_funs t' in
  let verify f =
    let rank_funs = [] in
    let vs,t'' = trans f fairness t' in
    let rank_var, prev_vars, arg_vars = Option.get vs in
    let t''' =
      t''
      |> Trans.replace_main ~force:true unit_term
      |@> pr @@ Format.asprintf "trans for %a" Print.id f
      |> Trans.flatten_let
      |@> pr "flatten let"
      |> Trans.null_tuple_to_unit
    in
    let result = main_loop rank_var rank_funs prev_vars arg_vars [] t''' in
    if !!debug then
      if result
      then Format.printf "%a is fair terminating.@.@." Id.print f
      else Format.printf "%a is possibly non fair terminating.@.@." Id.print f;
    result
  in
  try
    List.for_all verify top_funs
  with FailedToFindRF -> false
