open Combinator
open Util

(** Reduction from AFMC model checking to
    weak recurrence game solving to
    safety/liveness game solving *)

let main_t = "main"
let angel = Idnt.make "angel"
let demon = Idnt.make "demon"
let proponent g = if Game.is_angel g then angel else demon
let opponent g = if Game.is_angel g then demon else angel

let solvable ord sol p =
  List.for_all
    (fun (p1, p2) -> if p2 = p && p1 <> p2 then List.mem_assoc p1 sol else true)
    ord

let process_nondet g fdef =
  if Idnt.make fdef.Fdef.name = proponent g then
    Fdef.map_body
      (MLExp.map_const
         (function
           | Const.RandBool -> Const.ReadBool(Idnt.new_var (), [](*@todo*))
           | Const.RandInt -> Const.ReadInt(Idnt.new_var (), [](*@todo*))
           | Const.RandReal -> Const.ReadReal(Idnt.new_var (), [](*@todo*))
           | c -> c))
      fdef
  else if Idnt.make fdef.Fdef.name = opponent g then
    Fdef.map_body
      (MLExp.map_const
         (function
           | Const.ReadBool(_, _) -> Const.RandBool
           | Const.ReadInt(_, _) -> Const.RandInt
           | Const.ReadReal(_, _) -> Const.RandReal
           | c -> c))
      fdef
  else fdef
let process_nondet =
  Logger.log_block2 "GameSolver.process_nondet" process_nondet

let rec specialize tenv phi subs =
  MLExp.visit
    (object
      method fvar = MLExp.mk_var
      method fcon = MLExp.mk_const
      method fif ty e1 e2 e3 =
        if Term.is_rand e1 || Term.is_read e1 then
          MLExp.mk_if ty e1
            (specialize tenv phi subs e2)
            (specialize tenv phi subs e3)
        else
          let phi1 = e1 |> Formula.of_term in
          let phi2 = Formula.mk_and phi phi1 in
          let phi3 = Formula.mk_and phi (phi1 |> Formula.bnot) in
          let phi1 =
            List.fold_left (fun phi sub -> Formula.subst [sub] phi) phi1 subs
          in
          let phi =
            List.fold_left (fun phi sub -> Formula.subst [sub] phi) phi subs
          in
          (*Format.printf "phi: %a@.phi1: %a@." Formula.pr phi Formula.pr phi1;*)
          try
            if SMTProver.implies_dyn ~tenv [phi] [phi1] then
              specialize tenv phi2 subs e2
            else if SMTProver.implies_dyn ~tenv
                [phi] [phi1 |> Formula.bnot] then
              specialize tenv phi3 subs e3
            else
              MLExp.mk_if ty e1
                (specialize tenv phi2 subs e2)
                (specialize tenv phi3 subs e3)
          with _ ->
            MLExp.mk_if ty e1
              (specialize tenv phi2 subs e2)
              (specialize tenv phi3 subs e3)
      method flet ty (p, ty) e1 e2 =
        match p with
        | Pattern.V(x) when not (Term.is_rand e1 || Term.is_read e1) (* @todo *) ->
          MLExp.mk_let_pat ty (p, ty) e1
            (specialize tenv phi ((x, e1) :: subs) e2)
        | _ ->
          MLExp.mk_let_pat ty (p, ty) e1
            (specialize tenv phi subs e2)
      method fletrec = MLExp.mk_letrec
      method fevent = MLExp.mk_event
      method fapp = MLExp.mk_app
      method ffun xty = MLExp.mk_fun [xty]
      method ftuple = TupTerm.make
      method fkon = ADTTerm.mk_kon
      method ffix = MLExp.mk_fix
      method fcls = MLExp.mk_closure
      method farray = ArrayTerm.mk_array
      method faget = ArrayTerm.mk_aget
      method faset = ArrayTerm.mk_aset
    end)
let specialize ?(tenv=[]) =
  Logger.log_block2 "GameSolver.specialize" (specialize tenv)


let check_term_cond = ref false
let check_at_call st_typ term_cond loop_cond loop_call t =
  if !check_term_cond then
    MLExp.fold
      (object
        method fvar = MLExp.mk_var
        method fcon = MLExp.mk_const
        method fif = MLExp.mk_if
        method flet = MLExp.mk_let_pat
        method fletrec = MLExp.mk_letrec
        method fevent = MLExp.mk_event
        method fapp e1 es =
          match e1, es with
          | Term.Var(x), [e2] when x = angel || x = demon ->
            let args = TupTerm.elements_of (e2, st_typ) in
            MLExp.mk_if Type.mk_unit
              (Pred.apply term_cond args |> Formula.term_of)
              UnitTerm.make
              (MLExp.mk_if Type.mk_unit
                 (Pred.apply loop_cond args |> Formula.term_of)
                 loop_call
                 (MLExp.mk_app e1 es))
          | _ -> MLExp.mk_app e1 es
        method ffun xty = MLExp.mk_fun [xty]
        method ftuple = TupTerm.make
        method fkon = ADTTerm.mk_kon
        method ffix = MLExp.mk_fix
        method fcls = MLExp.mk_closure
        method farray = ArrayTerm.mk_array
        method faget = ArrayTerm.mk_aget
        method faset = ArrayTerm.mk_aset
      end)
      t
  else t

let inline = ref true
let inline_op op fdef_op st_typ term_cond loop_cond loop_call t =
  if !inline then
    MLExp.fold
      (object
        method fvar = MLExp.mk_var
        method fcon = MLExp.mk_const
        method fif = MLExp.mk_if
        method flet = MLExp.mk_let_pat
        method fletrec = MLExp.mk_letrec
        method fevent = MLExp.mk_event
        method fapp e1 es =
          match e1, es with
          | Term.Var(x), [e2] when x = op ->
            let args = TupTerm.elements_of (e2, st_typ) in
            MLExp.mk_if Type.mk_unit
              (Pred.apply term_cond args |> Formula.term_of)
              UnitTerm.make
              (MLExp.mk_if Type.mk_unit
                 (Pred.apply loop_cond args |> Formula.term_of)
                 loop_call
                 (Fdef.apply fdef_op [e2]))
          | _ -> MLExp.mk_app e1 es
        method ffun xty = MLExp.mk_fun [xty]
        method ftuple = TupTerm.make
        method fkon = ADTTerm.mk_kon
        method ffix = MLExp.mk_fix
        method fcls = MLExp.mk_closure
        method farray = ArrayTerm.mk_array
        method faget = ArrayTerm.mk_aget
        method faset = ArrayTerm.mk_aset
      end)
      t
  else t


(* angelic nondeterministic boolean generation is supported only for
   safety game solving *)
let construct_prog safety fdefs tenv g =
  let init_st = Idnt.new_var () in
  let [st_typ] = Type.args_of (TypEnv.lookup tenv angel) in
  let fdefs = List.map (process_nondet g) fdefs in
  let proponent_body, opponent_body =
    let component_phi =
      match Game.get_comp_pred g with
      | None -> Formula.mk_true
      | Some(pred, _) ->
        Formula.subst
          (Fdef.pat_match
             (fst pred |> List.map (fst >> Pattern.mk_var) |> Pattern.of_list)
             (Term.mk_var init_st))
          (snd pred)
    in
    let term_cond =
      if safety
      then Game.get_pred_must_win g |> Pred.bnot
      else Game.get_pred_must_win g
    in
    let loop_cond =
      if safety
      then Game.get_pred_may_lose g |> Pred.bnot
      else Game.get_pred_may_lose g
    in
    let opponent_def =
      List.find (fun fdef -> Idnt.make fdef.Fdef.name = opponent g) fdefs
    in
    let opponent_loop_call =
      Term.mk_app (Term.mk_var (opponent g)) [Term.mk_var init_st]
    in
    let opponent_move =
      Fdef.apply opponent_def [Term.mk_var init_st]
      |> check_at_call st_typ term_cond loop_cond opponent_loop_call
      |> specialize ~tenv component_phi []
    in
    let proponent_def =
      List.find (fun fdef -> Idnt.make fdef.Fdef.name = proponent g) fdefs
    in
    let proponent_loop_call =
      Term.mk_app (Term.mk_var (proponent g)) [Term.mk_var init_st]
    in
    let proponent_move =
      let ts =
        proponent_def.Fdef.body
        |> MLExp.boolean_angelic_nondet_ids
        |> List.map (fun b -> [b, true; b, false])
        |> Vector.product id
        |> List.map
          (fun m ->
             Fdef.apply proponent_def [Term.mk_var init_st]
             |> inline_op (opponent g) opponent_def
               st_typ term_cond loop_cond proponent_loop_call
             |> check_at_call st_typ term_cond loop_cond proponent_loop_call
             |> specialize ~tenv component_phi []
             |> MLExp.determinize m)
        |> List.unique
      in
      List.fold_right MLExp.seq ts UnitTerm.make
    in
    let args = TupTerm.elements_of (Term.mk_var init_st, st_typ) in
    MLExp.mk_if Type.mk_unit
      (Pred.apply term_cond args |> Formula.term_of)
      UnitTerm.make
      (MLExp.mk_if Type.mk_unit
         (Pred.apply loop_cond args |> Formula.term_of)
         proponent_loop_call
         proponent_move)
    |> specialize ~tenv Formula.mk_true [],
    MLExp.mk_if Type.mk_unit
      (Pred.apply term_cond (TupTerm.elements_of (Term.mk_var init_st, st_typ))
       |> Formula.term_of)
      UnitTerm.make
      (MLExp.mk_if Type.mk_unit
         (Pred.apply loop_cond args |> Formula.term_of)
         opponent_loop_call
         opponent_move)
    |> specialize ~tenv Formula.mk_true []
  in
  { Prog.fdefs =
      List.filter
        (fun fdef -> Idnt.make fdef.Fdef.name <> proponent g
                     && Idnt.make fdef.Fdef.name <> opponent g)
        fdefs
      @ [Fdef.make
           (Idnt.string_of (proponent g)) [Pattern.mk_var init_st]
           Formula.mk_true
           proponent_body;
         Fdef.make
           (Idnt.string_of (opponent g)) [Pattern.mk_var init_st]
           Formula.mk_true
           opponent_body];
    Prog.types =
      (List.filter (fun (x, _) -> x <> proponent g && x <> opponent g) tenv)
      @ [proponent g, Type.mk_fun [st_typ; Type.mk_unit];
         opponent g, Type.mk_fun [st_typ; Type.mk_unit]];
    Prog.main = main_t }
let construct_prog =
  Logger.log_block3 "GameSolver.construct_prog" construct_prog

let construct_rtenv ?(read_bool=false) tenv rtenv g =
  let ret_phi =
    if Game.is_safety g then Formula.mk_false else Formula.mk_true
  in
  let rtenv =
    tenv
    |> List.filter (fun (x, _) -> x = angel || x = demon)
    |> RefTypEnv.of_tenv_with_template
    |> (if (*Game.is_demon g &&*) read_bool then id
        else RefTypEnv.apply angel (RefTyp.set_phi_ret ret_phi))
    |> (if (*Game.is_angel g &&*) read_bool then id
        else RefTypEnv.apply demon (RefTyp.set_phi_ret ret_phi))
    |> (@) rtenv
  in
  let args, ret = TypEnv.lookup tenv (Idnt.make main_t) |> Type.args_ret in
  (*assert (Type.is_unit ret);*)
  (if RefTypEnv.member rtenv (Idnt.make main_t) then
     []
   else
     let ret = RefTyp.mk_base (Idnt.new_var ()) Type.mk_unit ret_phi in
     let rty =
       match Game.get_comp_pred g with
       | None ->
         let args = List.map RefTyp.of_simple_type args in
         RefTyp.mk_fun (List.map (fun ty -> Idnt.new_var (), ty) (args @ [ret]))
       | Some(_, pred) ->
         let [arg] = args in
         let arg =
           RefTyp.set_pred
             (fun tts -> Pred.apply pred tts)
             (RefTyp.of_simple_type arg)
         in
         RefTyp.mk_fun [Idnt.new_var (), arg; Idnt.new_var (), ret]
     in
     [RefTypEnv.Env (Idnt.make main_t, rty)])
  @ rtenv

let rec solve fdefs tenv rtenv ranks strategies g =
  Format.printf_force "Solving %a@." Game.pr g;
  match g with
  | Game.AFMC(phi) ->
    let waa = WeakSymAltAutomaton.of_modal_mu phi in
    let fdefs, tenv, wrg = WeakSymAltAutomaton.product waa (tenv, fdefs) in
    (* @todo [rtenv] and [ranks] should also be transformed *)
    solve fdefs tenv rtenv ranks strategies wrg
  | Game.WeakRecurrence(is_angel, cs, rset, ord, infer) ->
    let pre_tenv = cs |> List.hd |> snd |> fst |> fst (*@todo*) in
    solve_weak_recurrence_game
      infer pre_tenv fdefs tenv rtenv ranks is_angel cs rset ord strategies
  | Game.Safety(_, (q, _), _, _) ->
    let pre_tenv = q |> fst (*@todo*) in
    solve_safety_game pre_tenv fdefs tenv rtenv strategies g
  | Game.Liveness(_, (q, _), _, _) ->
    let pre_tenv = q |> fst (*@todo*) in
    solve_liveness_game pre_tenv fdefs tenv rtenv ranks strategies g
and solve_weak_recurrence_game
    infer pre_tenv fdefs tenv rtenv ranks is_angel cs rset ord strategies =
  let cnt = ref 0 in (* count for rankfun annotation *)
  let rec aux cids1 cids2 sol =
    let cids21, cids22 = List.partition (solvable ord sol) cids2 in
    (*Format.printf "cids21: %a@." (List.pr Idnt.pr ",") cids21;*)
    let cids1 = cids21 @ cids1 in
    let cids2 = cids22 in
    match cids1 with
    | [] ->
      assert (cids2 = []);
      if List.for_all
          (fun (cid, b) -> not (PartOrd.is_initial ord cid) || Pred.is_true b)
          sol then begin
        Format.printf_force
          "For the weak recurrence game, %a has a winning strategy for all the initial components@."
          Idnt.pr (if is_angel then angel else demon);
        Pred.make pre_tenv Formula.mk_true
      end else begin
        Format.printf_force
          "For the weak recurrence game, %a may not have a winning strategy for some initial component@."
          Idnt.pr (if is_angel then angel else demon);
        Pred.make pre_tenv Formula.mk_false
      end
    | cid :: cids1' ->
      if PartOrd.is_initial ord cid then
        Format.printf_force "@.Solving the initial component %a@." Idnt.pr cid
      else
        Format.printf_force "@.Solving the component %a@." Idnt.pr cid;
      let b =
        let infer =
          infer && not (PartOrd.is_initial ord cid)
        in
        let is_winning = List.mem cid rset in
        let cids_win, cids_unknown, cids_cond_winning =
          PartOrd.preds ord cid
          |> List.partition3_map (fun cid ->
              if Pred.is_true (List.assocF cid sol)
              then `L(cid)
              else if Pred.is_false (List.assocF cid sol)
              then `C(cid)
              else `R(cid))
        in
        let comp_pred = List.assocF cid cs in
        let may_lose =
          List.map (fun cid -> List.assocF cid cs |> snd) cids_unknown @
          List.map (fun cid ->
              Pred.band [List.assocF cid cs |> snd;
                         List.assocF cid sol |> Pred.bnot])
            cids_cond_winning
        in
        let must_win =
          List.map (fun cid -> List.assocF cid cs |> snd) cids_win @
          List.map (fun cid ->
              Pred.band [List.assocF cid cs |> snd;
                         List.assocF cid sol])
            cids_cond_winning
        in
        if is_winning then
          (* solve a safety game *)
          if may_lose = [] then begin
            (* proponent is always winning *)
            Format.printf_force
              "For the component %a, %a has a winning strategy@."
              Idnt.pr cid
              Idnt.pr (if is_angel then angel else demon);
            Pred.make pre_tenv Formula.mk_true
          end else begin
            assert (List.for_all (fun fdef -> fdef.Fdef.name <> main_t) fdefs);
            let fdefs =
              let init_st = Idnt.new_var () in
              fdefs @
              [Fdef.make
                 main_t [Pattern.mk_var init_st]
                 Formula.mk_true
                 (MLExp.mk_if Type.mk_unit
                    MLExp.mk_rand_bool
                    (Term.mk_app (Term.mk_var angel) [Term.mk_var init_st])
                    (Term.mk_app (Term.mk_var demon) [Term.mk_var init_st]))]
            in
            let tenv =
              let [st_typ] = Type.args_of (TypEnv.lookup tenv angel) in
              tenv @ [Idnt.make main_t, Type.mk_fun [st_typ; Type.mk_unit]]
            in
            let pred1 = may_lose |> Pred.bor |> Pred.bnot in
            let pred2 =
              if must_win = []
              then Pred.mk_top (Pred.type_of pred1)
              else must_win |> Pred.bor |> Pred.bnot
            in
            Game.Safety (is_angel, (pred1, pred2), infer, Some comp_pred)
            |> solve fdefs tenv rtenv ranks strategies
          end
        else begin
          (* solve a liveness game *)
          let ranks =
            List.filter
              (function
                | RankFun.Rank_game(n, _, _) when n <> !cnt -> false
                | _ -> true)
              ranks
          in
          if ranks <> [] then Format.printf "rank funs: %a@." RankFun.pr ranks;
          cnt := 1 + !cnt;
          if must_win = [] then begin
            (* proponent is always losing *)
            Format.printf_force
              "For the component %a, %a has a winning strategy@."
              Idnt.pr cid
              Idnt.pr (if is_angel then demon else angel);
            Pred.make pre_tenv Formula.mk_false
          end else begin
            assert (List.for_all (fun fdef -> fdef.Fdef.name <> main_t) fdefs);
            let fdefs =
              let init_st = Idnt.new_var () in
              fdefs @
              [Fdef.make
                 main_t [Pattern.mk_var init_st]
                 Formula.mk_true
                 (MLExp.mk_if Type.mk_unit
                    MLExp.mk_rand_bool
                    (Term.mk_app (Term.mk_var angel) [Term.mk_var init_st])
                    (Term.mk_app (Term.mk_var demon) [Term.mk_var init_st]))]
            in
            let tenv =
              let [st_typ] = Type.args_of (TypEnv.lookup tenv angel) in
              tenv @ [Idnt.make main_t, Type.mk_fun [st_typ; Type.mk_unit]]
            in
            let pred1 = must_win |> Pred.bor in
            let pred2 =
              if may_lose = []
              then Pred.mk_bot (Pred.type_of pred1)
              else may_lose |> Pred.bor
            in
            Game.Liveness (is_angel, (pred1, pred2), infer, Some comp_pred)
            |> solve fdefs tenv rtenv ranks strategies
          end
        end
      in
      aux cids1' cids2 ((cid, b) :: sol)
  in
  aux [] (List.map fst cs) []
and solve_safety_game pre_tenv fdefs tenv0 rtenv strategies g =
  let prog = construct_prog true fdefs tenv0 g in
  Format.printf_force
    "Reduced to non-termination verification of the program:@.  %a@."
    Prog.pr prog;
  let read_bool =
    fdefs |> List.map (process_nondet g) |> List.exists Fdef.has_read_bool
  in
  let rtenv = construct_rtenv ~read_bool prog.Prog.types rtenv g in
  try
    let rtenv, nondet_sol, coeff_sol, pre_sol =
      MLVerifier.solve_safety ~infer:(Game.infer g) ~rtenv ~strategies prog
    in
    let pre =
      match pre_sol with
      | [] -> Pred.make pre_tenv Formula.mk_true
      | [p, (tenv, phi)] ->
        Pred.make ((Idnt.make "q", Type.mk_int)(*@todo*) :: tenv) phi
      (*Formula.subst
          (Fdef.pat_match
             (tenv |> List.map (fst >> Pattern.mk_var) |> Pattern.of_list)
             (Term.mk_var (Idnt.make "s")))
          phi*)
      | _ -> assert false
    in
    Format.printf_force
      "For the safety game, %a has a winning strategy for the states:@.  %a@."
      Idnt.pr (proponent g)
      Pred.pr pre;
    Format.printf_force "Inferred Refinement Types:@.  %a@." RefTypEnv.pr rtenv;
    if nondet_sol <> [] then
      Format.printf_force
        "Strategy for non-deterministic choices:@.  %a@."
        PredSubst.pr nondet_sol;
    pre
  with
  | HCCSSolver.NoSolution ->
    Format.printf_force
      "For the safety game, %a does not have a winning strategy@."
      Idnt.pr (proponent g);
    Pred.make pre_tenv Formula.mk_false(*@todo*)
  | HCCSSolver.Unknown ->
    Format.printf_force
      "For the safety game, %a may not have a winning strategy@."
      Idnt.pr (proponent g);
    Pred.make pre_tenv Formula.mk_false(*@todo*)
and solve_liveness_game pre_tenv fdefs tenv0 rtenv ranks strategies g =
  let prog = construct_prog false fdefs tenv0 g in
  Format.printf_force
    "Reduced to termination verification of the program:@.  %a@."
    Prog.pr prog;
  assert (not (Prog.has_read_bool prog));
  let rtenv = construct_rtenv prog.Prog.types rtenv g in
  try
    RefTypCheck.game_solving := true;
    let rtenv, nondet_sol, coeff_sol, rankfuns, pre_sol =
      MLVerifier.solve_termination
        ~infer:(Game.infer g) ~rtenv ~ranks ~strategies prog
    in
    let pre =
      match pre_sol with
      | [] -> Pred.make pre_tenv Formula.mk_true
      | [p, (tenv, phi)] ->
        Pred.make ((Idnt.make "q", Type.mk_int)(*@todo*) :: tenv) phi
          (*Formula.subst
            (Fdef.pat_match
               (tenv |> List.map (fst >> Pattern.mk_var) |> Pattern.of_list)
               (Term.mk_var (Idnt.make "s")))
            phi*)
      | _ -> assert false
    in
    RefTypCheck.game_solving := false;
    Format.printf_force
      "For the liveness game, %a has a winning strategy for the states:@.  %a@."
      Idnt.pr (proponent g)
      Pred.pr pre;
    Format.printf_force "Inferred Refinement Types:@.  %a@." RefTypEnv.pr rtenv;
    Format.printf_force "Ranking Functions:  %a@." RankFun.pr rankfuns;
    if nondet_sol <> [] then
      Format.printf_force
        "Strategy for non-deterministic choices:@.  %a@."
        PredSubst.pr nondet_sol;
    pre
  with
  | HCCSSolver.NoSolution ->
    Format.printf_force
      "For the liveness game, %a does not have a winning strategy@."
      Idnt.pr (proponent g);
    Pred.make pre_tenv Formula.mk_false
  | HCCSSolver.Unknown ->
    Format.printf_force
      "For the liveness game, %a may not have a winning strategy@."
      Idnt.pr (proponent g);
    Pred.make pre_tenv Formula.mk_false
