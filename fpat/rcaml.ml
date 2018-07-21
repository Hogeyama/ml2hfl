open Fpat
open Util
open Combinator

let get_commit_hash () =
  try
    let cin = open_in "COMMIT" in
    let fpat = input_line cin in
    close_in cin;
    fpat
  with Sys_error _ | End_of_file -> ""

let print_env args =
  let fpat = get_commit_hash () in
  Format.printf
    "RCaml: Refinement Type Checking and Inference Tool for OCaml@.";
  if fpat <> "" then Format.printf "  Build: %s@." fpat;
  Format.printf "  OCaml version: %s@." Sys.ocaml_version;
  let args =
    List.map
      (fun s ->
         if String.contains s ' ' then Format.sprintf "'%s'" s else s)
      args
  in
  Format.printf
    "  Command: %a@.@." (List.pr Format.pp_print_string " ") args

let parse p filename =
  let inchan = filename |> open_in in
  let lexbuf = inchan |> Lexing.from_channel in
  let res =
    lexbuf.Lexing.lex_curr_p <-
      { Lexing.pos_fname = (*Filename.basename*)filename;
        Lexing.pos_lnum = 1;
        Lexing.pos_bol = 0;
        Lexing.pos_cnum = 0 };
    lexbuf |> p
  in
  close_in inchan;
  res

let pr_adt ppf (id, env) =
  Format.fprintf ppf "%a = @[%a@]" Idnt.pr id TypEnv.pr env

let print_result ?(nondet_preconds=[]) ?(coeff_sol=[]) ?(rankfuns=[]) rtenv =
  if rankfuns = []
  then Format.printf_force "Safe!@."
  else Format.printf_force "Safe and Terminating!@.";
  Format.printf_force
    "Inferred Refinement Types:@.  @[%a@]@.@." RefTypEnv.pr rtenv;
  if rankfuns <> [] then
    Format.printf_force
      "Inferred Ranking Functions:@.  @[%a@]@.@." RankFun.pr rankfuns;
  if coeff_sol <> [] && !SizeFun.sizes_to_infer <> [] then begin
    let sizefuns = SizeFun.subst coeff_sol !SizeFun.sizes_to_infer in
    Format.printf_force
      "Inferred Size Functions:@.  @[%a@]@." SizeFun.pr sizefuns
  end;
  if nondet_preconds <> [] then
    Format.printf_force
      "Inferred Conditions on Angelic Nondeterminism:@.  @[%a@]@."
      PredSubst.pr nondet_preconds

(* @todo use option and MLVerifier *)
let load_d filename =
  let ctenv, fdefs, rtenv, pvpole, pvprior, pvtempl, _, hccs =
    filename |> parse (MLRefparser.main MLReflexer.token)
  in
  let ctenv, hccs = SimTypInfer.infer_hccs ctenv hccs in
  Format.printf "Predicates:@.  %a@." HCCS.pr hccs;
  Format.printf "ML types:@.  %a@." TypEnv.pr ctenv;
  Format.printf "Refinement types:@.  %a@.@." RefTypEnv.pr rtenv;
  Format.printf "Add templates:@. %a@." RefTypEnv.pr
    (List.map
       (function
         | RefTypEnv.Env(x, rty) ->
           RefTypEnv.Env (x, RefTyp.mk_template x [] rty)
         | RefTypEnv.EFormula p -> RefTypEnv.EFormula p)
       rtenv);
  fdefs
  |> List.map
    (RefTypCheck.tcheck_fdef []
       (Prog.make fdefs [] ""(*@todo*)) ctenv rtenv)
  |> ignore;
  !RefTypCheck.rev_consts
  |> List.classify (fun (f1, _) (f2, _) -> f1 = f2)
  |> List.iter
    (fun xs ->
       let fdef = List.hd xs |> fst in
       Format.printf "@[<v>Type checking:@.  %a@." Fdef.pr fdef;
       let phis = List.map snd xs in
       if List.for_all (Unwinding.is_valid ctenv hccs 10) phis
       then Format.printf "  Success!@]@.@."
       else Format.printf "  Failure...@]@.@.")

let make_ctenv =
  List.concat_map snd
  >> (fun tenv -> tenv @ ADTFormula.mk_accessors_recognizers tenv)

let load_ml filename =
  let typing_hccs tenv =
    List.concat_map
      (HornClause.formula_of
       >> Formula.term_of
       >> ADTFormula.typing_term tenv
       >> Formula.of_term
       >> (fun phi -> HCCS.of_formula0 (Formula.fvs phi) phi))
  in
  let (rtenv, constr), (progs, dtyps) =
    OCamlParser.from_file filename
  in
  if dtyps <> [] then
    Format.printf_force
      "datatypes:@.  @[%a@]@." (List.pr pr_adt ".@.") dtyps;
  let ctenv = make_ctenv dtyps in
  let fdefs, tenv = Fdef.make_fdefs progs ctenv in
  let tenv = ctenv @ tenv @ !OCamlParser.tenv in
  let rtenv = rtenv @ !OCamlParser.rtenv in
  let prog =
    { Prog.fdefs = fdefs; Prog.types = tenv; Prog.main = "main" }
  in
  if !MLVerifier.termination_mode && not !MLVerifier.typeopt_mode then
    (* termination verification *)
    try
      let rtenv, nondet_preconds, coeff_sol, rankfuns, (*preconds*)_ =
        MLVerifier.solve_termination
          ~rtenv
          ~infer:(!MLVerifier.enable_infer_pre)
          ~sizes:(OCamlParser.size_of constr)
          ~ranks:(OCamlParser.rank_of constr)
          ~strategies:(OCamlParser.strategies_of constr)
          ~hcs:(OCamlParser.hcs_of constr)
          prog
      in
      print_result ~nondet_preconds ~coeff_sol ~rankfuns rtenv
    with
    | HCCSSolver.NoSolution ->
      Format.printf_force "Unsafe or Non-terminating!@."
    | HCCSSolver.Unknown ->
      Format.printf_force "Unknown@."
  else if !MLVerifier.induction_mode then begin
    (* induction-based Horn constraint solving *)
    let lemmas = OCamlParser.lemma_of constr in
    let hcs = OCamlParser.hcs_of constr |> typing_hccs tenv in
    let hccs, ptrees =
      MLVerifier.solve_safety_ind ~rtenv ~lemmas ~hcs prog
    in
    if List.for_all ProofTree.is_valid ptrees then begin
      Format.printf_force "Safe!@.";
      Format.printf_force "[Proof log]@.";
    end else begin
      Format.printf_force "Unsafe!@.";
      Format.printf_force "[Disproof log]@.";
    end;
    Format.printf_force "HCCS:@.  %a@.@." HCCS.pr hccs;
    if lemmas <> [] then begin
      Format.printf_force "Lemma:@, ";
      Global.silent := false;
      ProofTree.print_gamma lemmas;
      Global.silent := true;
      Format.printf_force
        "@,Number of Lemmas: %a@."
        Integer.pr (List.length lemmas)
    end;
    Format.printf_force
      "@.Number of Goal Clauses: %a@."
      Integer.pr (HCCS.goals_of hccs |> List.length);
    Global.silent := false;
    let print_func =
      if !ProofTree.print_proof_tree then
        fun a b -> ProofTree.treeprints a b
      else
        fun a b -> ProofTree.prettyprints a b
    in
    print_func
      (List.map
         (function
           | (pvs, _, phi, ProofTree.Bot) ->
             HornClause.mk_goal (List.map Pva.of_pvar pvs) phi
           | (pvs, _, phi, ProofTree.P pva) ->
             let pv, phi' = Pva.pvar_of pva in
             HornClause.mk_def
               pv
               (List.map Pva.of_pvar pvs)
               (Formula.mk_and phi phi'))
         lemmas
       @ HCCS.goals_of hccs)
      ptrees;
    Global.silent := true
  end else if !MLVerifier.typeopt_mode then
    (* refinement type optimization *)
    try
      let rtenv, nondet_preconds =
        MLVerifier.solve_multiobj
          ~rtenv
          ~sizes:(OCamlParser.size_of constr)
          ~ranks:(OCamlParser.rank_of constr)
          ~strategies:(OCamlParser.strategies_of constr)
          ~pvtempl:(OCamlParser.templsize_of constr)
          ~hcs:(OCamlParser.hcs_of constr)
          (OCamlParser.dir_of constr)
          (OCamlParser.prior_of constr)
          prog
      in
      print_result ~nondet_preconds rtenv
    with
    | HCCSSolver.NoSolution -> Format.printf_force "Unsafe!@."
    | HCCSSolver.Unknown -> Format.printf_force "Unknown@."
  else
    (* safety verification *)
    try
      let rtenv, nondet_preconds, coeff_sol, _ =
        MLVerifier.solve_safety
          ~rtenv
          ~infer:(!MLVerifier.enable_infer_pre)
          ~sizes:(OCamlParser.size_of constr)
          ~strategies:(OCamlParser.strategies_of constr)
          ~hcs:(OCamlParser.hcs_of constr)
          prog
      in
      print_result ~nondet_preconds ~coeff_sol rtenv
    with
    | HCCSSolver.NoSolution -> Format.printf_force "Unsafe!@."
    | HCCSSolver.Unknown -> Format.printf_force "Unknown@."

let weak_games_load_ml filename =
  let (rtenv, constr), (progs, dtyps) = OCamlParser.from_file filename in
  if dtyps <> [] then
    Format.printf "datatypes:@.  @[%a@]@." (List.pr pr_adt ".@.") dtyps;
  let ctenv = make_ctenv dtyps in
  let (fdefs, tenv) =
    progs
    |> (List.map @@ Pair.map_fst @@
        (Fdef.of_mlexp_with_adt ctenv >> List.map Fdef.alpha_body))
    |> List.split
    |> Pair.map List.concat List.concat
  in
  let _ =
    List.map
      (GameSolver.solve
         fdefs
         (ctenv @ tenv @ !OCamlParser.tenv)
         (rtenv @ !OCamlParser.rtenv)
         (OCamlParser.rank_of constr)
         (OCamlParser.strategies_of constr))
      (OCamlParser.games_of constr)
  in
  ()

let load_fml filename =
  let (phis, term_opt) =
    parse (MLRefparser.fml MLReflexer.token) filename
  in
(*
  let phis =
    List.map
      (Formula.map_atom
         (CunAtom.fold
            (object
              method fvar = Formula.mk_var
              method feq ty = Formula.eq Type.mk_real
              method fneq ty = Formula.neq Type.mk_real
              method flt ty = NumFormula.lt Type.mk_real
              method fgt ty t1 t2 = NumFormula.lt Type.mk_real t2 t1
              method fleq ty = NumFormula.leq Type.mk_real
              method fgeq ty = NumFormula.geq Type.mk_real
              method fdivides = IntFormula.divides
              method frecognizer = ADTFormula.mk_recognizer
              method fsmem = SetFormula.mk_mem
              method fssubset = SetFormula.mk_subset
              method fterm = Formula.mk_atom
            end)))
      phis
  in
  let phis =
    List.map
      (Formula.term_of >>
       Term.fold
         (object
           method fvar = Term.mk_var
           method fcon = function
             | Const.Int(n) ->
               Term.mk_const (Const.Real(float_of_int n))
             | c -> Term.mk_const c
           method fapp r1 r2 = Term.mk_app r1 [r2]
           method fbin = Term.mk_binder
         end)
       >> Formula.of_term)
      phis
  in
*)
  match term_opt with
  | (None, None) ->
    let [phi1; phi2] = phis in
    Format.printf
      "Find an interpolant for:@.  input1: %a@.  input2: %a@.@."
      Formula.pr phi1 Formula.pr phi2;
    let res = InterpProver.interpolate_dyn (fun _ -> true) phi1 phi2 in
    Format.printf "result: %a@." Formula.pr res
  | (Some(phi_max), None) -> (* currently for testing z3opt (nuZ) *)
    Format.printf
      "Test for finding a miximized solution by nuZ for:@. %a@."
      Formula.pr_list phis;
    let phi = Formula.band phis in
    let tenv = Formula.fvs_ty phi in
    Logger.printf "tenv:@,  %a@," TypEnv.pr tenv;
    let res, _ = SMTProver.solve_opt ~tenv phi (SMTProver.Max phi_max) in
    Format.printf "obtain: %a@." TermSubst.pr res
  | (None, Some(phi_min)) -> (* currently for testing z3opt (nuZ) *)
    Format.printf
      "Test for finding a minimized solution by nuZ for:@. %a@."
      Formula.pr_list phis;
    let phi = Formula.band phis in
    let tenv = Formula.fvs_ty phi in
    Logger.printf "tenv:@,  %a@," TypEnv.pr tenv;
    let res, _ = SMTProver.solve_opt ~tenv phi (SMTProver.Min phi_min) in
    Format.printf "obtain: %a@." TermSubst.pr res
  | _ -> assert false

let load_fml_is_sat filename =
  let (phis, _) = parse (MLRefparser.fml MLReflexer.token) filename in
  match phis with
  | [phi] ->
    Format.printf "check is_sat for: %a@." Formula.pr phi;
    let res = SMTProver.is_sat_dyn phi in
    Format.printf "result: %b" res
  | [phi1;phi2] ->
    Format.printf
      "check if %a implies %a@." Formula.pr phi1 Formula.pr phi2;
    let res = SMTProver.implies_dyn [phi1] [phi2] in
    Format.printf "result: %b" res
  | _ -> assert false

let load_hcs = parse (HCCSParser.parser_main HCCSLexer.token)
let load_smt = parse (SexpParser.parser_main SexpLexer.token) >> HCCS.of_smtlib1
let load_smt2 = parse (SexpParser.sexps SexpLexer.token) >> HCCS.of_smtlib2

let solve_hcs hcs =
  try
    hcs
    |> HCCS.map_phi (Formula.map_atom CunAtom.elim_beq_bneq)
    |> (fun hcs ->
        Format.printf_force "HCCS:@.  %a@." HCCS.pr_verbose hcs; hcs)
    |> Pair.unfold HCCS.tenv_of_constructors id
    |> uncurry2 SimTypInfer.infer_hccs
    |> (fun (env, hcs) ->
        Unwinding.ctenv0 := env;
        Logger.printf2
          "types:@,  %a@,typed HCCS:@,  %a@,"
          TypEnv.pr env
          HCCS.pr_verbose hcs;
        hcs)
    |> HCCS.simplify_light []
    |> Logger.pprintf "light:@,  %a@," HCCS.pr
    |> HCCS.simplify_full []
    |> Logger.pprintf "full:@,  %a@," HCCS.pr
    |> HCCS.simplify_lv2
    |> Logger.pprintf "lv2:@.  %a@." HCCS.pr
    |> HCCS.map_phi CunFormula.elim_unit
    |> Logger.pprintf "elim_unit:@.  %a@." HCCS.pr

    |> HCCSSolver.solve_dyn
    |> PredSubst.normalize
    |> Format.printf_force "solution:@.  %a@." PredSubst.pr;
  with
  | HCCSSolver.NoSolution -> Format.printf "no solution.@."
  | HCCSSolver.Unknown -> Format.printf "unknown.@."

let load_fj filename =
  (* (rtenv,constr),(progs,dtyps) = (type template, annotations), (functions, ADT definitions) *)
  let (rtenv, constr), (_    , _    ) =
    OCamlParser.from_file (filename ^ ".annot")
  in
  let (_    , _     ), (progs, dtyps) =
    parse (JavaParser.prog JavaLexer.token) filename |> Fj2ml.fj2ml
  in
  let (rtenv, constr), (progs, dtyps) =
    RefTypEnv.update_base_types_of_templates
      ((rtenv, constr), (progs, dtyps))
  in
  (* Format.printf
     "progs:@.  @[%a@]@."
     (List.pr
       (fun ppf (t, arglist) ->
          Format.fprintf ppf "term=%a @. (id,type) = %a"
            Term.pr t
            (List.pr (fun ppf (id,ty) ->
                 Format.fprintf ppf "(%a,%a)" Idnt.pr id Type.pr ty)
                ".@.")
            arglist)
       ".@.")
     progs; *)
  if dtyps <> [] then
    Format.printf "datatypes:@.  @[%a@]@." (List.pr pr_adt "@,") dtyps;
  let ctenv = make_ctenv dtyps in
  let fdefs, tenv = Fdef.make_fdefs progs ctenv in
  let tenv = ctenv @ tenv in
  let prog =
    { Prog.fdefs = fdefs; Prog.types = tenv; Prog.main = "main" }
  in
  let rtenv, nondet_preconds, coeff_sol, _ =
    MLVerifier.solve_safety
      ~rtenv
      ~infer:(!MLVerifier.enable_infer_pre)
      ~sizes:(OCamlParser.size_of constr)
      ~strategies:(OCamlParser.strategies_of constr)
      ~hcs:(OCamlParser.hcs_of constr)
      prog
  in
  print_result ~nondet_preconds ~coeff_sol rtenv

let load_tml filename =
  let open Parsetree in
  let open Asttypes in
  print_string "temporal contracts compiler";
  let asts = 
    Parse.implementation @@
      Lexing.from_channel @@ open_in filename 
  in
  let contracts = 
    List.filter
      (fun item ->
        match item.pstr_desc with
        | Pstr_attribute ({txt="tcontracts"}, _) -> true 
        | _ -> false
      ) asts
  in
  let programs =
    List.filter
      (fun item ->
        match item.pstr_desc with
        | Pstr_extension _ | Pstr_attribute _ -> false
        | _ -> true
      ) asts
  in
  (*print_string "\nPrograms....\n";
  Printast.structure 1 Format.std_formatter programs;
  print_string "\nContracts....\n";
  Printast.implementation Format.std_formatter contracts;*)
  let open DisneyTemporal in
  print_string "\n(* Results *)\n";
  Pprintast.structure
    Format.std_formatter @@ monitoring contracts programs

let main filename =
  Global.target_filename := filename;
  Logger.initialize (filename ^ ".log");
  SMTProver.initialize ();
  try
    begin
      Timer.start_interval ();
      match Filename.get_extension filename with
      | ".fml" ->
        InterpProver.csisat_use_integer_heuristic := true;
        InterpProver.csisat_use_theory := false;
        InterpProver.csisat_int_mode := false;
        load_fml filename
      | ".fml_sat" -> load_fml_is_sat filename
      | ".hcs" -> load_hcs filename |> solve_hcs
      | ".smt" -> load_smt filename |> solve_hcs
      | ".smt2" -> load_smt2 filename |> solve_hcs
      | ".d" -> load_d filename
      | ".java" -> load_fj filename
      | ".tml" -> load_tml filename (* for Disney's temporal contracts *)
      | ".ml" | _ ->
        InterpProver.csisat_use_integer_heuristic := true;
        InterpProver.csisat_use_theory := false;
        InterpProver.csisat_int_mode := true;
        if !Global.game_mode
        then weak_games_load_ml filename
        else load_ml filename
    end;
    SMTProver.finalize ();
    Logger.finalize ()
  with
  | Global.Syntax_error(msg) -> failwith msg
  | e ->
    if FPATConfig.is_fpat_exception e
    then Logger.printf "%a@," FPATConfig.pr_exception e
    else Logger.printf0 "uncaught exception!@,";
    SMTProver.finalize ();
    Logger.finalize ();
    raise e (* @todo *)

let () =
  FPATConfig.set_default ();
  Arg.parse
    (Arg.align FPATConfig.arg_spec)
    (fun filename ->
       (*Repl.main ()*)
       print_env (Array.to_list Sys.argv);
       if !Global.bench_mode
       then Bench.bench_file ~timeout:(!Global.timeout) main filename
       else main filename)
    FPATConfig.usage
