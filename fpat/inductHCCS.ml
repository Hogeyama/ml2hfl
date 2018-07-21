(*./bench.rb --bin './Induc.byte' --args '-bench' --bench './benchmarks/induction/IsaPlanner/' > /dev/null  *)
open Util
open Combinator
open ProofTree

exception Partial

type result = Valid | Unknown

let select_unfolded = ref false

(** printer *)
let logprint_pva_with_mark (pva, m) =
  Logger.printf "(%a * " Pva.pr pva;
  Logger.printf0 "{";
  List.iter (Logger.printf "_a%a, " Integer.pr) m;
  Logger.printf0 "})"

let logprint_pv_with_mark (pv, m) =
  Logger.printf "(%a * " PredVar.pr pv;
  Logger.printf0 "{";
  List.iter (Logger.printf "_a%a, " Integer.pr) m;
  Logger.printf0 "})"

let rec logprint_atoms = function
  | [] -> Logger.printf0 "@."
  | a::rest ->
    logprint_pva_with_mark a;
    Logger.printf0 ";@.";
    logprint_atoms rest

let logprint_phi phi =
  Logger.printf "%a@." Formula.pr phi

let logprint_h = function
  | Bot -> Logger.printf0 "false@."
  | P pva -> Logger.printf "%a@." Pva.pr pva

let logprint_guard = function
  | None -> ()
  | Some (pv, m) ->
    Logger.printf "(_a%a * " Integer.pr m;
    Logger.printf "%a)" PredVar.pr pv

let logprint_gam (atms, g, phi, h) =
  begin
    match g with
    | None -> ()
    | Some (pv, m) ->
      logprint_guard g;
      Logger.printf0 "@.&&@, "
  end;
  begin
    match atms with
    | [] -> ()
    | _ -> List.iter (Logger.printf "%a@, && " PredVar.pr) atms
  end;
  logprint_phi phi;
  Logger.printf0 "=> ";
  logprint_h h

let rec logprint_gamma = function
  | [] -> Logger.printf0 "[]@.@."
  | [gam] -> logprint_gam gam; Logger.printf0 "@."
  | gam::rest -> logprint_gam gam; logprint_gamma rest

let logprint_tuple (dcs, gamma, atms, phi, h) =
  Logger.printf0 "----------------------------@.";
  Logger.printf "[D]: @,%a@." HCCS.pr dcs;
  Logger.printf0 "@.[Gamma]:  @,";
  logprint_gamma gamma;
  Logger.printf0 "[A]: @,";
  logprint_atoms atms;
  Logger.printf0 "[phi]: @,";
  logprint_phi phi;
  Logger.printf0 "@.[h]: @,";
  logprint_h h;
  Logger.printf0 "----------------------------@."

let valid_pt = function
  | (_, _, _, phi, Bot) -> Valid_Bot (Formula.bnot phi)
  | (_, _, atms, phi, h) -> Valid_P (atms, phi, h)

let forall_elim not_elim_fvs phi h =
  let cs = Formula.conjuncts_of phi in
  let f (r1, r2) phi =
    let (c, ts) = Formula.term_of phi |> Term.fun_args in
    if Term.is_const c then
      Term.let_const c (function
          | Const.Eq _ ->
            let vs, ts' = List.partition Term.is_var ts in
            begin
              match vs, ts' with
              | [v], [t] ->
                if (Set_.subset vs ts'
                    || Set_.subset [Term.var_of v] not_elim_fvs)
                then (r1, phi :: r2)
                else ((Term.var_of v, t) :: r1, r2)
              | [v1; v2], [] -> begin
                  match
                    Set_.subset [Term.var_of v1] not_elim_fvs,
                    Set_.subset [Term.var_of v2] not_elim_fvs
                  with
                  | true, false -> (Term.var_of v2, v1) :: r1, r2
                  | false, true -> (Term.var_of v1, v2) :: r1, r2
                  | false, false -> (Term.var_of v1, v2) :: r1, r2
                  | _ -> r1, phi :: r2
                end
              | _ -> (r1, phi::r2)
            end
          | _ -> (r1, phi::r2))
    else (r1, phi :: r2)
  in
  let tsub, cs' = List.fold_left f ([], []) cs in
  let tsub, phis = Formula.resolve_duplicates_in_tsub tsub in
  let tsub = TermSubst.normalize tsub in
  let newphi =
    List.map (Formula.subst tsub) cs'
    |> List.rev
    |> (@) (List.map (Formula.subst tsub) phis)
    |> Formula.band
  in
  let newh =
    match h with
    | Bot -> Bot
    | P pva -> P (Pva.subst tsub pva)
  in
  newphi, newh
(*
let forall_elim =
  Logger.log_block3
    "InductHCCS.forall_elim"
    ~before:(fun _ x _ -> Logger.printf "input: %a@," Formula.pr x)
    ~after:(fun (x, _) -> Logger.printf "output: %a" Formula.pr x)
    forall_elim
*)

let forall_elim2 not_elim_fvs phi h =
  let cs = Formula.conjuncts_of phi in
  let fvslist = not_elim_fvs :: List.map Formula.fvs cs in
  let f r phi =
    let (c, ts) = Formula.term_of phi |> Term.fun_args in
    let rec ff (c, ts) =
      if Term.is_var c &&
         List.filter (List.mem (Term.var_of c)) fvslist
         |> List.length
         |> (=) 1 then r
      else if Term.is_const c then
        Term.let_const c (function
            (* | Const.Not -> Term.fun_args (List.hd ts) |> ff *)
            | Const.Neq _ | Const.Eq _
            | Const.Leq _ | Const.Geq _
            | Const.Lt _ | Const.Gt _
              (* | Const.UFun _*) ->
              if List.concat_map Term.fvs ts
                 |> List.exists
                   ((fun v -> List.filter (List.mem v) fvslist
                              |> List.length
                              |> (=) 1))
              then r
              else phi :: r
            (*
            | Const.UFun (ty, _) when ty = Type.mk_fun [Type.mk_int; Type.mk_bool] ->
              let vs, ts' = List.partition Term.is_var ts in
              begin
                match vs, ts' with
                | [v], [] ->
                  if List.find_all (List.mem (Term.var_of v)) fvslist
                     |> List.length
                     |> (=) 1
                  then r
                  else phi::r
                | _ -> phi::r
              end
              *)
            | _ -> phi::r)
      else phi :: r
    in
    ff (c, ts)
  in
  let newphi =
    List.fold_left f [] cs
    |> List.rev
    |> Formula.band
  in
  newphi, h

let replace_accessor tenv not_elim_fvs phi h =
  let cs = (* CNF.of_formula phi |> CNF.to_formula *)
    Formula.conjuncts_of phi
  in
  let rec loop v t =
    let (c, ts) = Term.fun_args t in
    if Term.is_const c then
      Term.let_const c (function
          | Const.Con(_, constr_id) ->
            let f i t =
              let access =
                Idnt.make @@ "_get_" ^ (Idnt.base constr_id)
                             ^ "_" ^ (string_of_int i) in
              let ty = ADTFormula.lookup access tenv in
              if Term.is_var t then
                [Term.var_of t, ADTTerm.mk_accessor ty constr_id i v]
              else
                let newv = ADTTerm.mk_accessor ty constr_id i v in
                loop newv t
            in
            List.concat_mapi f ts
          | _ -> [])
    else []
  in
  let mk_tsub phi =
    let (c, ts) = Formula.term_of phi |> Term.fun_args in
    if Term.is_const c then
      Term.let_const c (function
          | Const.Eq _ ->
            let vs, ts' = List.partition Term.is_var ts in
            begin
              match vs, ts' with
              | [v], [t] -> loop v t
              | _ -> []
            end
          | _ -> [])
    else []
  in
  let tsub =
    List.concat_map mk_tsub cs
    |> List.filter (fst >> flip List.mem not_elim_fvs >> not)
    |> List.unique ~cmp:(fun x y -> fst x = fst y)
  in
  let newphi = List.map (Formula.subst tsub) cs |> Formula.band in
  let newh = match h with Bot -> Bot | P pva -> P (Pva.subst tsub pva) in
  newphi, newh

let rec fa_elim tenv xs phi h =
  let phi', h' =
    forall_elim xs phi h
    |> Pair.fold (replace_accessor tenv xs)
    |> Pair.fold (forall_elim2 xs)
    |> Pair.map_fst FormulaSimplifier.simplify
    |> Pair.fold (replace_accessor tenv xs)
  in
  if (phi, h) = (phi', h') then begin
    (* Logger.printf "fa_elim(after): %a @." Formula.pr phi'; *)
    phi', h'
  end else fa_elim tenv xs phi' h'

(** induction *)

(* markers *)
let m_plus (pva, m) i = pva, i :: m
let m_plus_set atms i = List.map (flip m_plus i) atms
let map_empty = List.map (Pair.map_snd (fun _ -> []))
let m_filter atms f = List.filter (snd >> f) atms

let a_match tenv phi atms1 atms2 =
  atms2
  |> List.map fst
  |> List.for_all
    (fun pva2 ->
       atms1
       |> List.map fst
       |> List.filter (Pva.idnt_of >> (=) (Pva.idnt_of pva2))
       |> List.exists
         (fun pva1 ->
            List.for_all2
              (fun (t1, ty1) (t2, ty2) ->
                 (* (&&) (Type.equiv ty1 ty2)*)
                 (try Formula.eq ty1 t1 t2
                      |> FormulaSimplifier.simplify
                      |> List.return
                      |> SMTProver.implies_dyn ~tenv [phi]
                  with SMTProver.Unknown -> false))
              (pva1 |> Pva.args_of)
              (pva2 |> Pva.args_of)))

(** check the consistency of the given substitution *)
let rec is_consistent tenv phi = function
  | [] -> true
  | (x, t) :: rest ->
    try
      SMTProver.implies_dyn
        ~tenv
        [phi]
        [Formula.eq Type.mk_unknown t (List.assoc x rest)
         |> FormulaSimplifier.simplify]
      && is_consistent tenv phi rest
    with Not_found -> is_consistent tenv phi rest
       | SMTProver.Unknown ->
         Logger.printf "failed to check: %a@." Formula.pr phi;
         false(*assert false*)

let mk_sigmas atms1 pvs2 =
  List.fold_right
    (fun pv2 sigs ->
       atms1
       |> List.map fst
       |> List.filter (Pva.idnt_of >> (=) (PredVar.idnt_of pv2))
       |> List.map (Pva.pat_match pv2)
       |> flip List.cons sigs)
    pvs2 []

let rec pick_pvam unfolded atms_rest pvams =
  if !select_unfolded then begin
    Format.printf "Select unfolded atom:@.";
    List.iteri
      (fun i a ->
         Format.printf "@,%a: " Integer.pr i;
         print_pva_with_mark a)
      pvams;
    Format.printf "@,Unfold No(auto=-1): @.";
    let num = try read_int () with Failure "int_of_string" -> -1 in
    if num < 0 then pick_pvam unfolded atms_rest pvams
    else
      let pvam = List.nth pvams num in
      let rest = List.remove pvams pvam in
      pvam :: unfolded, rest, pvam
  end else
    match pvams with
    | [] -> (*pick_pvam [] [] unfolded*) failwith "cannot_pick_pvam"
    | pvam :: rest ->
      if List.mem pvam unfolded
      then pick_pvam unfolded (pvam :: atms_rest) rest
      else pvam :: unfolded, atms_rest @ rest, pvam
let pick_pvam unfolded = pick_pvam unfolded []

let get_conjectures dcs gcs lem =
  let lem_sep (pvs, None, phi, h) =
    let atms = List.map (Pva.of_pvar >> flip Pair.make []) pvs in
    dcs, [], atms, phi, h
  in
  let sep lem goal =
    let gamma = lem in
    let atms = HornClause.bpvas_of goal |> List.map (flip Pair.make []) in
    let phi = HornClause.bphi_of goal in
    (*let h = HornClause.head_of goal in*)
    let h = Bot in
    dcs, gamma, atms, phi, h
  in
  List.map lem_sep lem @ List.map (sep lem) gcs

let cmp_env = comp2 (=) fst fst

(* rule *)
let valid tenv = function
  | (_, _, _, phi, Bot) ->
    (* bot case *)
    let nphi = Formula.bnot phi |> CunFormula.simplify in
    if (try SMTProver.is_valid_dyn ~tenv nphi
        with SMTProver.Unknown -> false) then begin
      Logger.printf0 "<rule(valid_Bot)>:@.";
      Logger.printf "%a@." Formula.pr nphi;
      Logger.printf0 "is Valid!(end)@,@.";
      Valid
    end else begin
    (*
      Logger.printf "nphi =@,";
      print_phi nphi;
    *)
      Unknown
    end
  | (_, _, atms, phi, P pva) ->
    (* P case *)
    if a_match tenv phi atms [pva, []] then begin
      Logger.printf0 "<rule(valid_P)>:@.";
      Logger.printf0 "Valid!(end)@,@.";
      Valid
    end else Unknown

let rec apply_dcs tenv atms phi pva added_atms = function
  | [] -> added_atms
  | dc :: rest ->
    let hpv = HornClause.hpv_of dc |> Option.elem_of in
    if Pva.idnt_of pva <> PredVar.idnt_of hpv
    then apply_dcs tenv atms phi pva added_atms rest
    else begin
      let atms' = HornClause.bpvas_of dc in
      let phi' = HornClause.bphi_of dc in
      let hpva = Pva.of_pvar hpv in
      let aside_sigmas =
        mk_sigmas atms (List.map (Pva.pvar_of >> fst(*@todo OK?*)) atms')
      in
      let sub', exists_vars =
        Set_.diff ~cmp:cmp_env
          (Formula.fvs_ty phi') (List.concat_map Pva.fvs_ty (hpva :: atms'))
        |> (List.filter_map
              (fun (x, ty) ->
                 if Type.is_fun ty then None
                 (* to avoid ufuns from being quantified *)
                 else
                   let newx = Idnt.new_var () in
                   Some ((x, Term.mk_var newx), (newx, ty))))
        |> List.split
      in
      let sigma =
        try Pva.pat_match hpv pva
        with Not_found ->
          Format.printf "%a and %a do not match@." PredVar.pr hpv Pva.pr pva;
          assert false
      in
      Logger.printf "@.sigma:@,%a@." TermSubst.pr sigma;
      aside_sigmas
      |> Vector.product List.concat
      |> List.map ((@) sigma)
      |> List.filter (is_consistent tenv phi)
      |> List.map ((@) sub')
      |> (function
          | [] -> apply_dcs tenv atms phi pva added_atms rest
          | sigmas ->
            try
              let sigma =
                List.find
                  (flip Formula.subst phi'
                   >> Formula.exists exists_vars
                   (* >> Qelim.integer_qelim_dyn *)
                   >> List.return
                   >> (try_ (SMTProver.implies_dyn ~tenv [phi])
                         (fun SMTProver.Unknown _ -> false)))
                  sigmas
              in
              Pva.subst sigma hpva :: added_atms(* one such sigma is enough *)
            with Not_found -> apply_dcs tenv atms phi pva added_atms rest)
    end

(* rule *)
let rec fold tenv unfolded alpha (dcs, gamma, atms', phi, h) op_pvam =
  let atms = match op_pvam with
    | None -> atms'
    | Some pvam -> pvam :: atms'
  in
  Logger.printf0 "<rule(fold)>:@.";
  logprint_tuple (dcs, gamma, atms, phi, h);

  let added_atms =
    match h with
    | Bot -> []
    | P pva ->
      apply_dcs tenv atms phi pva [] dcs
      |> List.unique
      |> List.map (flip Pair.make [])
  in
  if added_atms = [] then
    match op_pvam with
    | None -> assert false
    | Some pvam -> unfold tenv unfolded alpha (dcs, gamma, atms', phi, h) pvam
  else begin
    Logger.printf0 "added pvas:@.";
    logprint_atoms added_atms;
    let tuple = (dcs, gamma, atms @ added_atms, phi, h) in
    logprint_tuple tuple;
    match valid tenv tuple with
    | Valid -> Fold (added_atms, Node (tuple, valid_pt tuple))
    | Unknown -> assert false
  end

(* rule *)
(** @param dcs definite clauses
    @param pvam pva to unfold
    @param unfolded already unfolded atoms
  *)
and unfold tenv unfolded alpha (dcs, gamma, atms, phi, h) pvam =
  Logger.printf0 "<rule(unfold)>:@.";
  Logger.printf0 "Unfolded pva: ";
  logprint_pva_with_mark pvam;
  let matchdefs = HornClause.defmatch dcs pvam in
  let atms_emp, atmsphis =
    Set_.diff atms unfolded |> map_empty |> List.split_map (fst >> Pva.pvar_of)
  in
  let pva' = m_plus pvam alpha in
  let pv', phi' = Pva.pvar_of (fst pva') in
  let not_elim_fvs =
    let fvs' = List.concat_map PredVar.fvs (pv' :: atms_emp) |> List.unique in
    List.concat_map (fst >> Pva.fvs) (pva' :: Set_.diff atms unfolded)
    |> (@) fvs'
    |> List.unique
  in
  Logger.printf "not_elim_fvs: %a@." Idnt.pr_list not_elim_fvs;
  let gam =
    let gamphi, h =
      Formula.band ([phi; phi'] @ atmsphis)
      |> FormulaSimplifier.simplify
      |> Logger.pprintf "gamphi %a" Formula.pr
      |> flip (fa_elim tenv not_elim_fvs) h
      |> (fun (x, y) -> Logger.printf "gamphi %a" Formula.pr x; x, y)
      |> Pair.map_fst (CunFormula.simplify >> ADTFormula.eval_accessor)

    in
    atms_emp, Some (pv', alpha), FormulaSimplifier.simplify gamphi, h
  in
  Logger.printf0 "@.added hypothesis: @,";
  logprint_gam gam;
  Logger.printf0 "@.";
  let gamma' = gam :: gamma in
  let make_pt i (atms_i, phi_i) =
    let atms_i' = m_plus_set atms_i alpha in
    let newphi = Formula.mk_and phi phi_i |> FormulaSimplifier.simplify in
    let tuple = (dcs, gamma', pvam :: atms @ atms_i', newphi, h) in
    Logger.printf "Unfold(Case%a): " Integer.pr i;
    logprint_pva_with_mark pvam;
    Logger.printf0 "@.";
    logprint_tuple tuple;
    match valid tenv tuple with
    | Valid -> Node (tuple, valid_pt tuple)
    | Unknown -> Node (tuple, apply(*fold*) tenv unfolded alpha tuple)
  in
  let rec make_pts i = function
    | [] -> []
    | x :: xs ->
      let pt = make_pt i x in
      if is_valid pt then pt :: make_pts (i + 1) xs else [pt]
  in
  Unfold (pvam, gam, make_pts 0 matchdefs)

and applyBot tenv atms phi = function
  | [] -> phi
  | (_, _, _, P _) :: rest -> applyBot tenv atms phi rest
  | (atms', guard, phi', Bot) :: rest ->
    (* A side sigma candidates list *)
    let aside_sigmas = mk_sigmas atms atms' in
    (* P side sigma candidates list *)
    let atms_g, pside_sigmas, tenv_phi' =
      match guard with
      | None ->
        atms,
        [[]],
        Set_.diff ~cmp:cmp_env
          (Formula.fvs_ty phi') (List.concat_map PredVar.fvs_ty atms')
      | Some (pv, m) ->
        let atms_g = m_filter atms (List.mem m) in
        atms_g,
        mk_sigmas atms_g [pv] |> List.hd,
        Set_.diff ~cmp:cmp_env
          (Formula.fvs_ty phi') (List.concat_map PredVar.fvs_ty (pv :: atms'))
    in
    let sub', exists_vars =
      tenv_phi'
      |> List.split_map
        (fun (x, ty) ->
           let newx = Idnt.new_var () in
           (x, Term.mk_var newx), (newx, ty))
      |> Pair.map_snd (List.partition (snd >> Type.is_fun) >> uncurry2 (@))
    in
    pside_sigmas :: aside_sigmas
    |> Vector.product List.concat
    |> List.filter (is_consistent tenv phi)
    |> List.map ((@) sub')
    |> (function
        | [] -> applyBot tenv atms phi rest
        | sigmas ->
          let nsigphi' =
            sigmas
            |> List.map
              (flip Formula.subst phi'
               >> Formula.exists exists_vars
               >> Formula.bnot
               >> FormulaSimplifier.simplify)
            |> Formula.band
            |> FormulaSimplifier.simplify
            |> Formula.conjuncts_of
            |> List.filter
              (fun phi ->
                 if Formula.is_forall phi &&
                    Formula.let_forall phi (fun _ ty _ -> Type.is_fun ty) then
                   begin
                     Logger.printf "filtered: %a@," Formula.pr phi;
                     false
                   end
                 else true)
            |> Conjunction.formula_of
          in
          let newphi =
            Formula.mk_and phi nsigphi'
            (*|> Qelim.integer_qelim_dyn*)
            |> FormulaSimplifier.simplify
          in
          if (try SMTProver.is_valid_dyn ~tenv (Formula.mk_iff phi newphi)
              with SMTProver.Unknown -> false)
          then applyBot tenv atms phi rest
          else begin
            Logger.printf0 "g: ";
            logprint_guard guard;
            Logger.printf0 "@.";
            Logger.printf0 "A_g:@.";
            logprint_atoms atms_g;
            Logger.printf0 "added phi:@.";
            logprint_phi nsigphi';
            Logger.printf0 "newphi:@.";
            logprint_phi newphi;
            applyBot tenv atms newphi rest
          end)
and applyP tenv atms phi = function
  | [] -> atms
  | (_, _, _, Bot) :: rest -> applyP tenv atms phi rest
  | (atms', guard, phi', P pva) :: rest ->
    (* A side sigma candidates list *)
    let aside_sigmas = mk_sigmas atms atms' in
    (* P side sigma candidates list *)
    let atms_g, pside_sigmas, tenv_phi' =
      match guard with
      | None ->
        atms,
        [[]],
        Set_.diff ~cmp:cmp_env
          (Formula.fvs_ty phi') (List.concat_map PredVar.fvs_ty atms')
      | Some (pv, m) ->
        let atms_g = m_filter atms (List.mem m) in
        atms_g,
        mk_sigmas atms_g [pv] |> List.hd,
        Set_.diff ~cmp:cmp_env
          (Formula.fvs_ty phi') (List.concat_map PredVar.fvs_ty (pv :: atms'))
    in
    let sub', exists_vars =
      tenv_phi'
      |> (List.filter_map
            (fun (x, ty) ->
               if Type.is_fun ty then None
               (* to avoid ufuns from being quantified *)
               else
                 let newx = Idnt.new_var () in
                 Some ((x, Term.mk_var newx), (newx, ty))))
      |> List.split
    in
    pside_sigmas :: aside_sigmas
    |> Vector.product List.concat
    |> List.filter (is_consistent tenv phi)
    |> List.map ((@) sub')
    |> (function
        | [] -> applyP tenv atms phi rest
        | sigmas ->
          let sigmas' =
            List.filter
              (flip Formula.subst phi'
               >> Formula.exists exists_vars
               (* >> Qelim.integer_qelim_dyn *)
               >> List.return
               >> (try_ (SMTProver.implies_dyn ~tenv [phi])
                     (fun SMTProver.Unknown _ -> false)))
              sigmas
          in
          Logger.printf0 "g: ";
          logprint_guard guard;
          Logger.printf0 "@.";
          Logger.printf0 "A_g:@.";
          logprint_atoms atms_g;

          let sigP's =
            List.map
              (flip Pva.subst pva >> flip Pair.make [])
              sigmas'
          in

          Logger.printf0 "added pvas:@.";
          logprint_atoms sigP's;
          applyP tenv (atms @ sigP's) phi rest)
(* rule *)
(** @param dcs definite clauses *)
and apply tenv unfolded alpha (dcs, gamma, atms, phi, h) =
  let do_fold = true in
  Logger.printf0 "<rule(apply_P)>:@.";
  let newatms = applyP tenv atms phi gamma in
  Logger.printf0 "<rule(apply_Bot)>:@.";
  let newphi = applyBot tenv newatms phi gamma in
  if phi = newphi && Set_.equiv atms newatms then
    try
      let unfolded', atms_rest, pvam = pick_pvam unfolded atms in
      if do_fold
      then fold tenv unfolded' (alpha + 1) (dcs, gamma, atms_rest, phi, h) (Some pvam)
      else unfold tenv unfolded' (alpha + 1) (dcs, gamma, atms_rest, phi, h) pvam
    with Failure "cannot_pick_pvam" -> begin
        match h with
        | Bot ->
          InValid
            (try SMTProver.solve_dyn ~tenv phi
             with SMTProver.Unknown -> assert false)
        | _ -> fold tenv unfolded alpha (dcs, gamma, atms, phi, h) None
      end
  else
    let tuple = (dcs, gamma, newatms, newphi, h) in
    logprint_tuple tuple;
    let added_atms = Set_.diff newatms atms in
    let pt =
      match valid tenv tuple with
      | Valid -> Node (tuple, valid_pt tuple)
      | Unknown ->
        try
          (* note: newatms is not used by hypothesis *)
          let unfolded', atms_rest, pvam = pick_pvam unfolded atms(*newatms*) in
          let unfolded' = unfolded' @ added_atms in
          let atms_rest = atms_rest @ added_atms in
          let pt =
            if do_fold
            then fold tenv unfolded' (alpha + 1) (dcs, gamma, atms_rest, newphi, h) (Some pvam)
            else unfold tenv unfolded' (alpha + 1) (dcs, gamma, atms_rest, newphi, h) pvam
          in
          Node (tuple, pt)
        with Failure "cannot_pick_pvam" -> begin
            match h with
            | Bot ->
              InValid
                (try SMTProver.solve_dyn ~tenv phi
                 with SMTProver.Unknown ->
                   logprint_tuple tuple;
                   assert false)
            | _ ->
              logprint_tuple tuple;
              assert false (* fold unfolded alpha tuple *)
          end
    in
    if added_atms = [] then Apply_Bot (newphi, pt)
    else if phi = newphi then Apply_P (added_atms, pt)
    else Apply_P (added_atms, Apply_Bot (newphi, pt))

let algo_solve hcs lemmas tenv =
  let hcs = hcs |> HCCS.normalize2 ~force:false in
  Logger.printf "HCCS:@, %a@." HCCS.pr hcs;
  get_conjectures (HCCS.defs_of hcs) (HCCS.goals_of hcs) lemmas
  |> List.map (fun conj ->
      logprint_tuple conj;
      let pt =
        match valid tenv conj with
        | Valid -> valid_pt conj
        | Unknown -> apply(*fold*) tenv [] 0 conj
      in
      Node (conj, pt))
let algo_solve = Logger.log_block3 "InductHCCS.algo_solve" algo_solve
