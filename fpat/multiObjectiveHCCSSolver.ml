open Util
open Combinator

(** An HCCS solver with multi-objective optimization based on template based synthesis *)

type solution = OptSol of PredSubst.t | Sol of PredSubst.t

(* flags *)
let typeopt_mode = ref false
let optimality_checking = ref false
let heuristic_timeout = ref false
let template_expansion = ref true
let threshold = ref 10
let count = ref 1

let templates = ref []
let phi_wo_approx = ref Formula.mk_true (* use this for checking optimality *)

let template_of ?(eheads=[]) hcs pvtempl =
  Template.nonzero_const := [];
  hcs
  |> HCCS.tenv
  |> List.map
    (fun (pid, ty) ->
       if Idnt.base pid = "count" then (* @todo *)
         (PredVar.of_type pid ty
          |> PredVar.args_of
          |> Pair.unfold id (Template.make_bounds)
          |> Pair.make pid)
       else if Idnt.base pid = "countc" then (* make 0 <= c <= k *)(* @todo *)
	 (PredVar.of_type pid ty
          |> PredVar.args_of
          |> Pair.unfold id (Template.make_bounds ~use_init:false)
          |> Pair.make pid)
       else
         (* @todo can be reduced (or moved)?*)
         let conj, disj =
           if List.mem_assoc pid pvtempl then List.assoc pid pvtempl else !Template.num_conj, !Template.num_disj
         in
	 let etenv =
	   if eheads <> [] then
	     try
	       eheads
	       |> List.find
		 (fun head ->
		    match HornClause.pv_of_head head with
		    | Some (pvar) -> PredVar.idnt_of pvar = pid)
	       |> (fun head ->
		   let bound_tenv = head |> HornClause.tenv_of_head in
		   let pv_tenv =
		     head
		     |> HornClause.pv_of_head
		     |> (function Some(pvar) -> PredVar.args_of pvar)
		   in
		   pv_tenv
		   |> List.map
		     (fun elem ->
			List.mem_assoc (fst elem) bound_tenv))
	     with
	     | Not_found -> []
	   else
	     []
	 in
         PredVar.of_type pid ty
         |> PredVar.args_of
         |> Pair.unfold id (Template.make ~etenv:etenv conj disj 1)
         |> Pair.make pid)

(** check if the constraints that solver returns NoSolution is actual NoSolution in Integer
    by checking validity for the constraints without heauristic approximation.
    If is_sat returns false, we can find it is actually NoSolution and so returns true.
   @require: an HCCS that is substituted template formulas,
             and formulas for improving solution *)
let is_actual_NoSol psub hccs prior_phi =
  let phi =
    hccs
    |> HCCS.subst psub
    |> FwHCCSSolver.formula_of_forward
    |> Formula.bnot
    |> FormulaSimplifier.simplify
    |> Formula.mk_and prior_phi
    |> (fun x -> Formula.forall (Formula.fvs_ty x) x)
  in
  try
    Timer.start ();
    let m = !SMTProver.z3#solve [] phi in
    let time = Timer.stop () *. 1000.0 |> int_of_float in
    Logger.printf "checked SAT in %a ms@." Integer.pr time;
    let tsub =
      m
      |> Pair.unfold
        id
        (Map_.dom
         >> Set_.diff (PredSubst.coeffs psub)
         >> List.map (flip Pair.make IntTerm.zero))
      |> uncurry2 (@)
    in
    psub
    |> List.map @@ Pair.map_snd @@ Pair.map_snd
      (Formula.subst tsub >> FormulaSimplifier.simplify)
  with
  | SMTProver.Unsat ->
    let time = Timer.stop () *. 1000.0 |> int_of_float in
    Logger.printf "checked in %a ms@." Integer.pr time;
    raise HCCSSolver.NoSolution
  | SMTProver.Unknown ->
    Format.printf "Raised SMTProver.Unknown@.";
    let initial_time = Timer.stop () *. 1000.0 |> int_of_float in
    Logger.printf "checked Unknown in %a ms@." Integer.pr initial_time;
    raise HCCSSolver.Unknown
let is_actual_NoSol =
  Logger.log_block3
    "MultiObjectiveHCCSSolver.is_actual_NoSol"
    is_actual_NoSol

(** check whether an obtained solution is trivial solution or not *)
let is_trivial pvpole sol =
  List.exists
    (fun (id, subst) ->
       if List.mem_assoc id pvpole |> not then
         false
       else
         begin
           if List.assoc id pvpole then
             begin (* minimize *)
               snd subst
               |> (=) Formula.mk_true
             end
           else
             begin (* maximize *)
               snd subst
               |> (=) Formula.mk_false
             end
         end)
    sol
let is_trivial pvpole sol =
  Logger.log_block2
    "MultiObjectiveHCCSSolver.is_trivial"
    ~after:(Logger.printf "output: %a" Bool.pr)
    is_trivial pvpole sol

let set_timeout pvpole sol time =
  if !heuristic_timeout && (is_trivial pvpole sol |> not) then
    let time = time + (100 * List.length pvpole) in
    Format.printf "set timeout_z3 := %a@." Integer.pr time;
    Global.timeout_z3 := time;
    heuristic_timeout := false

let rec _solve pvtempl phi hccs =
  let psub =
    if !templates = [] then
      template_of hccs pvtempl
    else !templates
  in
  templates := psub;
  Logger.printf "psub: %a@," PredSubst.pr psub;
  try
    TemplateBasedHCCSSolver.solve ~psub:psub ~phi_opt:(Some phi) hccs
  with
  | HCCSSolver.NoSolution ->
    (* if NoSolution returned in first time,
       solve again with more expressible template just one more time *)
    if phi = Formula.mk_true && !template_expansion then
      begin
        let pvtempl =
          HCCS.pvs hccs
          |> List.filter (flip List.mem_assoc pvtempl >> not)
          |> List.map
            (fun id -> id, (2, 1))
          |> (@) (List.map
                    (fun (id, (conj, disj)) ->
                       if conj <= disj then
                         id, (conj+1, disj)
                       else id, (conj, disj+1))
                    pvtempl)
        in
        templates := [];
        (* template_expansion := false; *)
        Format.printf "solve again@.";
        _solve pvtempl phi hccs
      end
    else if !optimality_checking then
      begin
        Format.printf "Optimality check@.";
        is_actual_NoSol !templates hccs !phi_wo_approx
      end
    else
      raise HCCSSolver.Unknown
  | HCCSSolver.Unknown -> raise HCCSSolver.Unknown
(* let _solve pvtempl phi = *)
(*   try *)
(*     AEHCCSSolver.solve (_solve pvtempl phi) *)
(*   with *)
(*   | EAHCCSSolver.NoSolution *)
(*   | EAHCCSSolver.Unknown -> *)
(*     if !optimality_checking then *)
(*       (Format.printf "Optimality check@,"; *)
(*        if is_actual_NoSol !phi_wo_approx then *)
(*          raise HCCSSolver.NoSolution *)
(*        else *)
(*          raise HCCSSolver.Unknown) *)
(*     else *)
(*       raise HCCSSolver.Unknown *)
let _solve ?(pvtempl=[]) =
  Logger.log_block2
    "MultiObjectiveHCCSSolver._solve"
    ~before:(fun phi hcs ->
        Logger.printf2 "input:@, phi: %a@, hccs: %a@," Formula.pr phi HCCS.pr hcs)
    ~after:(Logger.printf "output: %a" PredSubst.pr)
    (_solve pvtempl)

let improve pvpole pvprior theta =
  (phi_wo_approx := PredVarPriority.whole_constr_wo_approximation_opt
       theta !templates pvpole pvprior);
  PredVarPriority.whole_constr_opt theta !templates pvpole pvprior
  |> Logger.pprintf "constr: %a@," Formula.pr
let improve =
  Logger.log_block3
    "MultiObjectiveHCCSSolver.improve"
    improve

let optimize solver pvpole pvprior (hccs:HCCS.t) : solution =
  let pvprior = (* assume that pvprior is already topological sorted *)
    (pvprior
     |> List.filter (flip List.mem_assoc pvpole))
    (* ignore nodes without specification about min/max *)
  in
  Format.printf "Optimization objectives:@.  Priorities: %a@.  Directions: %a@.@."
    (List.pr Idnt.pr " ⊏ ") pvprior PredVarPoles.pr pvpole;
  Timer.start ();
  let theta_0 = solver Formula.mk_true hccs in
  let initial_time = Timer.stop () *. 1000.0 |> int_of_float in
  Format.printf "initial solution: %a@.in %a ms@." PredSubst.pr theta_0 Integer.pr initial_time;
  set_timeout pvpole theta_0 initial_time;
  let theta_prev = ref theta_0 in
  let improve_phi_prev = ref Formula.mk_true in
  let rec iter () =
    if !count > !threshold then raise HCCSSolver.Unknown
    else
      begin
        Format.printf "iteration <%d>: @." !count;
        let improve_phi = improve pvpole pvprior !theta_prev in
        if improve_phi = !improve_phi_prev then
          begin
            Format.printf "Nothing to improve@.";
            raise HCCSSolver.Unknown
          end
        else
          begin
            Timer.start ();
            let theta = solver improve_phi hccs in
            let time = Timer.stop () *. 1000.0 |> int_of_float in
            Format.printf "solved in %a ms@." Integer.pr time;
            Format.printf " %a@." PredSubst.pr theta;
            set_timeout pvpole theta time;
            theta_prev := theta;
            improve_phi_prev := improve_phi;
            count := 1 + !count;
            iter ()
          end
      end
  in
  try
    iter ()
  with
  | HCCSSolver.NoSolution -> OptSol(!theta_prev)
  | _ -> Sol(!theta_prev)
let optimize =
  Logger.log_block4
    "MultiObjectiveHCCSSolver.optimize"
    optimize

(** {6 Solvers for weak precondtion finding } *)

(** find a maximally-weak precondition for pre_pvar
    by repeatedly weakening the previous precondition *)
let maximally_weak_pre pre_pvar solver hccs =
  let pre_id = PredVar.idnt_of pre_pvar in
  Timer.start ();
  let theta_0 = solver hccs in
  let initial_time = Timer.stop () *. 1000.0 |> int_of_float in
  Format.printf "initial solution: %a@. %a@.in %a ms@."
    PredSubst.pr (fst theta_0) TermSubst.pr (snd theta_0) Integer.pr initial_time;
  (* set_timeout [pre_id, false] theta_0 initial_time; *)
  let theta_prev = ref theta_0 in
  let improve_hccs_prev = ref [] in
  let rec iter () =
    if !count > !threshold then raise HCCSSolver.Unknown
    else
      begin
        Format.printf "iteration <%d>: @." !count;
        let improve = PredVarPriority.weaken (fst !theta_prev) pre_id in
        Format.printf "  improve: %a@," HCCS.pr improve;
        if improve = !improve_hccs_prev then
          begin
            Format.printf "Nothing to improve@.";
            raise HCCSSolver.Unknown
          end
        else
          begin
            Timer.start ();
            let theta = solver (improve @ hccs) in
            let time = Timer.stop () *. 1000.0 |> int_of_float in
            Format.printf "solved in %a ms@." Integer.pr time;
            Format.printf " %a@. %a@."
              PredSubst.pr (fst theta) TermSubst.pr (snd theta);
            set_timeout [pre_id, false] (fst theta) time;
            theta_prev := theta;
            improve_hccs_prev := improve;
            count := 1 + !count;
            iter ()
          end
      end
  in
  try
    iter ()
  with
  | HCCSSolver.NoSolution -> !theta_prev
  | _ -> !theta_prev
let maximally_weak_pre =
  Logger.log_block3
    "MultiObjectiveHCCSSolver.maximally_weak_pre"
    maximally_weak_pre

(** maximally-weak precondition inference for HCCSSolver.t *)
let maximally_weak_pre_hccssolver pre_pvar (solver:HCCSSolver.t) hccs =
  let solver = solver >> (fun t -> t, []) in
  maximally_weak_pre pre_pvar solver hccs
  |> fst

(** find a weak precondition represented as
    disjunctions of disjoint preconditions *)
let weak_pre pre_pvar solver hccs =
  let pre_pva = Pva.of_pvar pre_pvar in
  let precond = ref Formula.mk_false in
  let count = ref 0 in
  let rec aux () =
    let hccs =
      (HornClause.mk_goal [pre_pva] !precond) :: hccs
    in
    Format.printf "@[<hov2>Solve<%a>:@\n@," Integer.pr !count;
    let psub = solver hccs in
    precond := psub
               |> flip PredSubst.lookup pre_pva
               |> Formula.mk_or !precond
               |> FormulaSimplifier.simplify;
    Format.printf "  Solutions: %a@," PredSubst.pr psub;
    Format.printf "  Precondition: %a@]@." Formula.pr !precond;
    count := 1 + !count;
    aux ()
  in
  try
    aux ()
  with
  | e ->
    Format.printf "Failed to solve <%a>: raised %s@]@."
      Integer.pr !count
      (Printexc.to_string e);
    [PredSubst.mk_elem
       (PredVar.idnt_of pre_pvar)
       (PredVar.args_of pre_pvar)
       !precond]
let weak_pre = Logger.log_block3 "MultiObjectiveHCCSSolver.weak_pre" weak_pre

(** disjunctive weak precondition inference for termination *)
let weak_pre_termination pre_pvar solver hccs =
  weak_pre pre_pvar solver hccs
  |> (fun psub -> RankFun.mk_ranks [], psub, [])

(** disjunctive weak precondition inference for RHCCSSolver.t *)
let weak_pre_rechccssolver pre_pvar (solver:RHCCSSolver.t) hccs =
  let solver = solver >> fst in
  weak_pre pre_pvar solver hccs
  |> (fun psub -> psub, [])

