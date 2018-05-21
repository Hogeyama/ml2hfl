open Util
open Combinator

(** An HCCS solver over tuples *)

(*val elim_proj : HCCS.t -> HCCS.t*)
let elim_proj hcs =
  List.concat_map
    (HornClause.formula_of
     >> Formula.map_atom TupFormula.normalize_proj
     >> Logger.pprintf "after normalize_proj: %a@," Formula.pr
     >> TupFormula.elim_proj
     >> Logger.pprintf "after elim_proj: %a@," Formula.pr
     >> HCCS.of_formula0 (HCCS.pvs hcs))
    hcs
let elim_proj =
  Logger.log_block1 "TupHCCSSolver.elim_proj"
    ~before:(Logger.printf "input:@,  %a@," HCCS.pr)
    ~after:(Logger.printf "output:@,  %a@," HCCS.pr)
    elim_proj

let encode = elim_proj >> HCCS.map_phi (Formula.map_atom TupAtom.elim_teq_tneq)
let encode = Logger.log_block1 "TupHCCSSolver.encode" encode

let rec flatten_tenv = function
  | [] -> [], Formula.mk_true
  | (x, ty) :: tenv ->
    match Type.fun_args ty with
    | Type.Const(TypConst.Tuple(_)), tys ->
      let tenv' = List.map (fun ty -> Idnt.new_var (), ty) tys in
      let phi =
        Formula.eq
          ty
          (Term.mk_var x)
          (TupTerm.make tys (List.map (fst >> Term.mk_var) tenv'))
      in
      let tenv'', phi' = flatten_tenv (tenv' @ tenv) in
      tenv'', Formula.mk_and phi phi'
    | _ ->
      let tenv', phi = flatten_tenv tenv in
      (x, ty) :: tenv', phi

let flatten_pv pv =
  let p = PredVar.idnt_of pv in
  let tenv, phi = PredVar.args_of pv |> flatten_tenv in
  PredVar.make p tenv, phi

let rec flatten_tts = function
  | [] -> [], Formula.mk_true
  | (t, ty) :: tts ->
    match Type.fun_args ty with
    | Type.Const(TypConst.Tuple(_)), tys ->
      let tts' = List.map (fun ty -> Idnt.new_var () |> Term.mk_var, ty) tys in
      let phi = Formula.eq ty t (TupTerm.make tys (List.map fst tts')) in
      let tts'', phi' = flatten_tts (tts' @ tts) in
      tts'', Formula.mk_and phi phi'
    | _ ->
      let tts', phi = flatten_tts tts in
      (t, ty) :: tts', phi

let flatten_pva pva =
  let p = Pva.idnt_of pva in
  let tts, phi = Pva.args_of pva |> flatten_tts in
  Pva.make p tts, phi

let flatten_hc =
  HornClause.ae_fold
    (fun htenv hpv hphi bpvas bphi ->
       let pvas', phis = List.map flatten_pva bpvas |> List.split in
       match hpv with
       | None ->
         HornClause.make
           (HornClause.mk_head ~tenv:htenv ~phi:hphi None)
           (HornClause.mk_body pvas' (Formula.band (bphi :: phis)))
       | Some(pv) ->
         let pv', phi' = flatten_pv pv in
         HornClause.make
           (HornClause.mk_head ~tenv:htenv ~phi:hphi (Some pv'))
           (HornClause.mk_body pvas' (Formula.band (bphi :: phi' :: phis))))

let flatten = List.map flatten_hc

let rec flatten_tys = function
  | [] -> []
  | ty :: tys ->
    match Type.fun_args ty with
    | Type.Const(TypConst.Tuple(_)), tys' -> flatten_tys (tys' @ tys)
    | _ -> ty :: flatten_tys tys

let flatten_each_tenv tenv =
  List.map
    (Pair.map_snd
       (fun ty ->
          let args, ret = Type.args_ret ty in
          if Type.is_bool ret
          (*@todo we here want to flatten only types of predicate
            variables *) then
            Type.mk_fun_args_ret (flatten_tys args) ret
          else ty))
    tenv

let rec sub_of tenv_from tenv_to =
  match tenv_to with
  | [] -> assert (tenv_from = []); []
  | (t, ty) :: tenv_to' ->
    match Type.fun_args ty with
    | Type.Const(TypConst.Tuple(n)), tys ->
      sub_of
        tenv_from
        (List.mapi (fun i ty -> TupTerm.mk_proj tys i t, ty) tys
         |> flip (@) tenv_to')
    | _ ->
      if tenv_from = [] then begin
        Format.printf "ty: %a@." Type.pr ty;
        assert false
      end;
      let (x, ty') = List.hd tenv_from in
      (* assert ty = ty' *)
      (x, t) :: sub_of (List.tl tenv_from) tenv_to'

let unflatten tenv sol =
  List.map
    (fun (p, (p_tenv, phi)) ->
       p,
       try
         let ty = List.assoc p tenv in
         let args, ret(* this must be bool *) = Type.args_ret ty in
         let p_tenv' = List.map (fun ty -> Idnt.new_var (), ty) args in
         (*Format.printf_force "ty: %a@.p_tenv: %a" Type.pr ty TypEnv.pr p_tenv;*)
         let sub = sub_of p_tenv (List.map (Pair.map_fst Term.mk_var) p_tenv') in
         Pred.make p_tenv' (Formula.subst sub phi)
       with Not_found -> (p_tenv, phi))
    sol

let solve (solver : HCCSSolver.t) = encode >> solver
let solve = Logger.log_block2 "TupHCCSSolver.solve" solve
