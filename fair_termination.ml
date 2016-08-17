open Util
open Syntax
open Term_util
open Type
open Fair_termination_type
open Fair_termination_util

exception FailedToFindRF

module Debug = Debug.Make(struct let check () = List.mem "Fair_termination" !Flag.debug_module end)


type trans_env =
    {target: id;
     target_xs: id list;
     target_result_typ: typ;
     states: state list;
     fairness: fairness;
     s: id;
     set_flag: id;
     ps: id list}

let make_s_init (states:state list) =
  if false then Format.printf "LEN: %d@." (List.length states);
  make_tuple @@ List.make (List.length states) false_term

let rec subst_state states s q b =
  let aux i q' = if q = q' then make_bool b else make_proj i @@ make_var s in
  make_tuple @@ List.mapi aux states

let index_of states q =
  List.assoc_option q @@ List.mapi Pair.pair_rev states

let make_is_fair states fairness s =
  let aux p =
    if p = "Always"
    then true_term
    else
      match index_of states p with
      | None -> false_term
      | Some i -> make_proj i @@ make_var s
  in
  List.fold_left make_and true_term @@ List.map (fun (p,q) -> make_imply (aux p) (aux q)) fairness

let make_extra_vars states args =
  let s = Id.new_var ~name:"s" (make_s_init states).typ in
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

let rec trans_value env v =
  let _,typ = trans.tr_col2_typ env v.typ in
  match v.desc with
  | Var x -> make_var @@ Id.set_typ x typ
  | BinOp(op, v1, v2) -> {v with desc=BinOp(op, trans_value env v1, trans_value env v2); typ}
  | Not v -> {v with desc=Not (trans_value env v); typ}
  | Tuple vs -> {v with desc=Tuple(List.map (trans_value env) vs); typ}
  | Proj(i,v) -> {v with desc=Proj(i, trans_value env v); typ}
  | _ -> {v with typ}

let trans_typ env typ =
  match typ with
  | TFun(x,typ) -> trans.tr_col2_typ env (TFuns([x],typ))
  | TFuns(xs,typ1) ->
      let xs' = List.map (Id.map_typ @@ snd -| trans.tr_col2_typ env) xs in
      let _,typ1' = trans.tr_col2_typ env typ1 in
      let xs'',x = List.decomp_snoc xs' in
      None, TFuns(xs'' @ [env.s; env.set_flag] @ env.ps @ [x], make_tpair (Id.typ env.s) typ1')
  | _ -> trans.tr_col2_typ_rec env typ

(* Assume that input is in normal form *)
let trans_term env t =
  match t.desc with
  | _ when is_value t ->
      if false then Format.printf "VALUE: %a@." Print.term t;
      None, make_pair (make_var env.s) @@ trans_value env t
  | _ when t.desc = randint_unit_term.desc -> None, make_pair (make_var env.s) @@ trans_value env t
  | App({desc=Event(q, _)}, [_]) ->
      None, make_pair (subst_state env.states env.s q true) unit_term
  | App(t1, [_]) when t1.desc = fail_term.desc -> None, make_fail @@ make_tpair (Id.typ env.s) t.typ
  | App(v1, vs) ->
      let v1' = trans_value env v1 in
      if false then Format.printf "%a@." Print.term' v1';
      let vs' = List.map (trans_value env) vs in
      let rec aux head typ vs =
        if false then Format.printf "v1: %a@." Print.term v1;
        if false then Format.printf "typ: %a@." Print.typ typ;
        if false then Format.printf "vs: %a@.@." (List.print Print.term) vs;
        assert (vs <> []);
        match typ with
        | TFuns(xs, typ') ->
            let vs1,vs2 = List.split_nth (List.length xs) vs in
            if false then Format.printf "|xs|: %d@." @@(List.length xs);
            let vs1',v = List.decomp_snoc vs1 in
            let head' = make_app head (vs1' @ (List.map make_var @@ env.s::env.set_flag::env.ps) @ [v]) in
            if vs2 = []
            then head'
            else aux (make_snd head') typ' vs2
        | _ -> assert false
      in
      None, aux v1' v1.typ vs'
  | If(v1, t2, t3) ->
      let vs2,t2' = trans.tr_col2_term env t2 in
      let vs3,t3' = trans.tr_col2_term env t3 in
      if false then Format.printf "t2': %a@." Print.term t2';
      if false then Format.printf "t3': %a@." Print.term t3';
      join vs2 vs3, make_if v1 t2' t3'
  | Let(_, [x,[],t1], t2) when not @@ Id.same x env.target ->
      if false then Format.printf "START@.";
      if false then Format.printf "t1: %a@." Print.term t1;
      let vs1,t1' = trans.tr_col2_term env t1 in
      if false then Format.printf "t1': %a@." Print.term t1';
      let sx = Id.set_typ (Id.add_name_before "s__" @@ new_var_of_term t1) t1'.typ in
      let s' = Id.new_var_id env.s in
      let vs2,t2' = trans.tr_col2_term {env with s=s'} t2 in
      let _,x' = trans.tr_col2_var env x in
      if false then Format.printf "sx: %a@." Print.id_typ sx;
      join vs1 vs2, make_lets [sx,[],t1'; s',[],make_fst(make_var sx); x',[],make_snd(make_var sx)] t2'
  | Let(flag, bindings, t2) ->
      let aux (g,xs,t1) =
        if xs = [] then unsupported @@ Format.asprintf "fair termination!? %a" Print.id g;
        let _,g' = trans.tr_col2_var env g in
        let g_true = Id.new_var_id g' in
        let xss =
          let rec aux typ xs =
            if xs = []
            then []
            else
              match typ with
              | TFuns(ys, typ') ->
                  let xs1,xs2 = List.split_nth (List.length ys) xs in
                  xs1 :: aux typ' xs2
              | _ -> assert false
          in
          if false then Format.printf "g[%d]: %a@." (List.length xs) Print.id_typ g;
          aux (Id.typ g) xs
        in
        let args = List.map (fun _ -> make_extra_vars env.states env.target_xs) xss in
        let s',set_flag',ps' = List.last args in
        let vs, t1' =
          if Id.same g env.target
          then
            let b = Id.new_var TBool in
            let s'' = Id.new_var_id s' in
            let set_flag'' = Id.new_var_id set_flag' in
            let ps'' = List.map Id.new_var_id ps' in
            let xs' = List.filter (is_ground_typ -| Id.typ) xs in
            let rank_var = Id.new_var ~name:"rank" @@ List.fold_right _TFun (ps'@xs') TBool in
            let t_rank = make_app (make_var rank_var) @@ List.map make_var (ps'@xs') in
            let t_check = make_if (make_is_fair env.states env.fairness s') (make_assert t_rank) unit_term in
            let t_b = make_if (make_var set_flag') (make_seq t_check randbool_unit_term) true_term in
            let t_sp =
              let t_s1 = make_s_init env.states in
              let t_s2 = make_var s' in
              let t_p1 = make_tuple @@ List.map make_var xs' in
              let t_p2 = make_tuple @@ List.map make_var ps' in
              make_if (make_var b) (make_pair t_s1 t_p1) (make_pair t_s2 t_p2)
            in
            let sp = Id.new_var ~name:"sp" t_sp.typ in
            let t1'' =
              let bindings' =
                (sp, [], t_sp) ::
                  (s'', [], make_fst @@ make_var sp) ::
                    (set_flag'', [], true_term) ::
                      List.mapi (fun i p'' -> p'', [], make_proj i @@ make_snd @@ make_var sp) ps''
              in
              if false then Format.printf "t1: %a@." Print.term t1;
              let vs,t1''' = trans.tr_col2_term {env with s=s''; ps=ps''; set_flag=set_flag''} t1 in
              assert (vs = None);
              make_lets bindings' t1'''
            in
            let t1''' = make_let [b,[],t_b] t1'' in
            let t1'''' =
              if !Flag.expand_set_flag
              then make_if (make_var set_flag')
                           (subst set_flag' true_term t1''')
                           (Trans.alpha_rename @@ subst set_flag' false_term t1''')
              else t1'''
            in
            Some (rank_var, ps', xs'), t1''''
          else
            trans.tr_col2_term {env with s=s'; set_flag=set_flag'} t1
        in
        if false then Format.printf "g'[%d]: %a@." (List.length env.ps) Print.id_typ g';
        let xss' = List.map (List.map @@ snd -| trans.tr_col2_var env) xss in
        let aux (s,set_flag,ps) xs (first,t) =
          let t' = if first then t else make_pair (make_var s) t in
          let xs',x = List.decomp_snoc xs in
          false, make_funs (xs'@s::set_flag::ps@[x]) t'
        in
        let aux1 (s,set_flag,ps) xs (first,t) =
          let t' = if first then t else make_pair (make_var s) t in
          let xs',x = List.decomp_snoc xs in
          let t'' = subst set_flag true_term @@ subst_var g' g_true t' in
          false, make_funs (xs'@s::set_flag::ps@[x]) @@ subst_var g' g_true t''
        in
        let aux2 (s,set_flag,ps) xs (first,t) =
          let t' = if first then t else make_pair (make_var s) t in
          let xs',x = List.decomp_snoc xs in
          false, make_funs (xs'@s::set_flag::ps@[x]) t'
        in
        if false && Id.same g env.target
        then
          vs, [g', [], Trans.alpha_rename @@ snd @@ List.fold_right2 aux1 args xss' (true, t1');
               g_true, [], snd @@ List.fold_right2 aux2 args xss' (true, t1')]
        else vs, [g', [], snd @@ List.fold_right2 aux args xss' (true, t1')]
      in
      let vss,bindingss = List.split_map aux bindings in
      let bindings' = List.flatten bindingss in
      let vs2,t2' = trans.tr_col2_term env t2 in
      List.fold_left join vs2 vss, make_let_f flag bindings' t2'
  | _ ->
      Debug.printf "%a@." Print.term t;
      unsupported @@ Format.asprintf "Fair termination [%a]" Print.constr t

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
  | _ -> invalid_arg "get_top_fun_typ"

let trans target fairness t =
  let states = List.unique @@ List.Set.inter (get_states t) @@ List.flatten_map Pair.to_list fairness in
  if false then Format.printf "STATES: %a@." (List.print Format.pp_print_string) states;
  let target_xs,target_result_typ = get_top_fun_typ target t in
  let s, set_flag, ps = make_extra_vars states target_xs in
  if false then Format.printf "ps: %a@." (List.print Print.id) ps;
  let env = {target; target_xs; target_result_typ; states; fairness; s; set_flag; ps} in
  let vs,t' = trans.tr_col2_term env t in
  let bindings =
    (s, [], make_s_init states) ::
    (set_flag, [], false_term) ::
    List.map (fun p -> p, [], make_term @@ Id.typ p) ps
  in
  vs,make_lets bindings t'


let verify_with rank_var rank_funs prev_vars arg_vars exparam_sol t =
  if true && !Flag.add_closure_exparam
  then Format.printf "EXPARAM: %a@." (List.print @@ Pair.print Print.id Format.pp_print_int) exparam_sol;
  let ps = List.map Id.new_var_id prev_vars in
  let xs = List.map Id.new_var_id arg_vars in
  (*
  let rank_funs = if read_int () = 0 then [{coeffs=[-1;1];const=0}] else rank_funs in (* koskinen-2 *)
   *)
  let _t' = make_let [rank_var, ps@xs, make_check_rank ps xs rank_funs] t in
  if false then Format.printf "INPUT: %a@." Print.term t;
  let t'' = Trans.inline_specified (rank_var, ps@xs, make_check_rank ps xs rank_funs) t in
  let t''' = Trans.split_assert t'' in
  let ts =
    let paths = Trans.search_fail t''' in
    List.map (Trans.screen_fail -$- t''') paths
  in
  if false then Format.printf "BEFORE:@.%a@." Print.term t''';
  if false then Format.printf "HEAD:@.%a@." Print.term @@ List.hd ts;
  List.for_all (fun t -> incr Flag.fair_term_loop_count; Main_loop.run [] exparam_sol t) ts

let rec main_loop rank_var rank_funs prev_vars arg_vars exparam_sol spcs spcWithExparams preds_info(*need?*) t =
  try
    let result =
      if !Flag.separate_pred then
        unsupported "separte_pred"
      else if !Flag.split_callsite then
        unsupported "split_callsite"
      else
        verify_with rank_var rank_funs prev_vars arg_vars exparam_sol t
    in
    result
  with
  | Refine.PostCondition(env, spc, spcWithExparam) ->
      let spcs' = spc::spcs in
      let spcWithExparams' = spcWithExparam::spcWithExparams in
      let solution =
        let _all_vars = List.map fst env in
        let aux = Fpat.Idnt.make -| Id.to_string in
        let arg_vars' = List.map aux arg_vars in
        let prev_vars' = List.map aux prev_vars in
        if false then Format.printf "spc: %a@." Fpat.Formula.pr spc;
        if false then Format.printf "spcWithExparam: %a@." Fpat.Formula.pr spcWithExparam;
        Fpat.RankFunInfer.lrf !Flag.add_closure_exparam spcs' spcWithExparams' (*all_vars*) arg_vars' prev_vars'
      in
      let rank_funs' = List.map (fun (coeffs,const) -> {coeffs; const}) @@ fst solution in
      let exparam_sol' = List.map (Pair.map_fst (Id.from_string -$- TInt)) @@ snd solution in
      let exparam_sol'' = List.map (fun (x,n) -> x, List.assoc_default n x exparam_sol') exparam_sol in
      if false && !Flag.add_closure_exparam
      then Format.printf "SOLUTION: %a@." (List.print @@ Pair.print Print.id Format.pp_print_int) exparam_sol'';
      List.iter (Debug.printf "Found ranking function: %a@.@." @@ print_rank_fun arg_vars) rank_funs';
      let preds_info' = (rank_funs',spc)::preds_info in
      let rank_funs'' = rank_funs' @ rank_funs in
      if !Flag.print_progress then Format.printf "DONE!@.@.";
      main_loop rank_var rank_funs'' prev_vars arg_vars exparam_sol'' spcs' spcWithExparams' preds_info' t





let pr ?(check_typ=Some TUnit) s t =
  if !!Debug.check then
    begin
      Format.printf "##[%aFair_termination%t] %a:@.%a@.@." Color.set Color.Yellow Color.reset Color.s_red s Print.term_typ t;
      Option.iter (Type_check.check t) check_typ
    end

let rec run spec t =
  let {Spec.fairness} = spec in
  Format.printf "FAIRNESS: %a@.@." print_fairness fairness;
  let t' =
    t
    |> Trans.copy_poly_funs
    |> fst
    |@> pr "copy poly. funs."
    |> remove_and_replace_event
    |@> pr "remove_and_replace_event"
    |> Encode_rec.trans
    |&!Flag.add_closure_exparam&> insert_extra_param
    |@!Flag.add_closure_exparam&> pr "insert extra parameters"
    |> normalize
    |@> pr "normalize"
    |> set_main
    |@> pr "set_main"
    |> Uncurry.to_tfuns
    |@> pr ~check_typ:None "to_tfuns"
  in
  let main = Option.get @@ get_last_definition t' in
  if false then Format.printf "MAIN: %a@." Id.print main;
  let top_funs = List.filter_out (Id.same main) @@ get_top_funs t' in
  let top_funs' = List.filter (is_fun_typ -| Id.typ) top_funs in
  if false then Format.printf "TOP_FUNS: %a@." (List.print Id.print) top_funs';
  let verify f =
    let rank_funs = [] in
    let vs,t'' = trans f fairness t' in
    let rank_var, prev_vars, arg_vars = Option.get vs in
    if false then Format.printf "prev_vars: %a@." (List.print Print.id) prev_vars;
    if false then Format.printf "arg_vars: %a@." (List.print Print.id) arg_vars;
    let t''' =
      t''
      |> Trans.replace_main ~force:true unit_term
      |> Trans.tfuns_to_tfun
      |@> pr @@ Format.asprintf "trans for %a" Print.id f
      |> Trans.flatten_let
      |> Trans.simplify_if_cond
      |@> pr "simplify"
      |> Trans.null_tuple_to_unit
    in
    let fv = List.remove_all (get_fv t''') rank_var in
    if false then Format.printf "%a@." (List.print Print.id) fv;
    assert (List.for_all is_extra_coeff fv);
    let init_sol = List.map (fun x -> x, 1) fv in
    let result = main_loop rank_var rank_funs prev_vars arg_vars init_sol [] [] [] t''' in
    if !!Debug.check then
      if result
      then Format.printf "%a is fair terminating.@.@." Id.print f
      else Format.printf "%a is possibly non fair terminating.@.@." Id.print f;
    result
  in
  try
    List.for_all verify top_funs'
  with Fpat.RankFunInfer.LRFNotFound when !Flag.add_closure_exparam -> false
     | Fpat.RankFunInfer.LRFNotFound -> Flag.add_closure_exparam := true; run spec t
     | Fpat.RankFunInfer.LLRFNotFound -> assert false
