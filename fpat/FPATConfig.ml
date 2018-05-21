open Util
open Combinator

(** FPAT Configuration *)

let _ =
  HCCSSolver.ext_of_string :=
    (function
      (* FLOPS 2008 *)
      | "qe" -> BwQEHCCSSolver.solve
      (* PPDP 2009 *)
      | "it" -> BwIPHCCSSolver.solve
      (* PEPM 2013 *)
      | "git" -> GenHCCSSolver.solve (CHGenInterpProver.interpolate false)
      | "gch" -> GenHCCSSolver.solve (CHGenInterpProver.interpolate true)
      | "gtb" -> GenHCCSSolver.solve
                   (TemplateBasedGenInterpProver.interpolate)
      (* TACAS 2015 *)
      | "sb0" -> BeautifulHCCSSolver.solve
      | "sb" -> DecoHCCSSolver.solve
                |> PartHCCSSolver.solve
                |> SampHCCSSolver.solve
      | "dex" -> ExpandDagHCCSSolver.solve BwIPHCCSSolver.solve
      | "sbe" -> SampHCCSSolver.solve_sbe
      (* ESOP 2015 *)
      | "rs1" -> HCCSSolver.solve_rsrefine_rs1
      | "rs4" -> HCCSSolver.solve_rsrefine_rs4
      | "rs8" -> HCCSSolver.solve_rsrefine_rs8
      | "itm" -> HCCSSolver.solve_rsrefine_itm
      | "exl" -> HCCSSolver.solve_rsrefine_exl
      | "rse" -> HCCSSolver.solve_rsrefine_rse
      (* SAS 2016 or FM 2016? *)
      | "expsb" -> PointSampleHCCSSolver.solve ~ex:true
      | "psb" -> PointSampleHCCSSolver.solve ~ex:false
      (* others *)
      | "lb" -> FwHCCSSolver.solve_simp
      | "ai" -> FwWidenHCCSSolver.solve ~wide:true
      | "tb" -> fun hcs -> TemplateBasedHCCSSolver.solve hcs
      | "uoa" -> Unwinding.solve []
      | "pdr" -> HCCSSolver.solve_pdr
      | "dual" -> HCCSSolver.solve_duality
      | _ -> raise Not_found);
  RHCCSSolver.ext_of_string :=
    (function
      | "itunw" -> RecHCCSSolver.solve ~auto:true
      | "expsb" -> PointSampleHCCSSolver.solve_rhccs ~ex:true
      | "psb" -> PointSampleHCCSSolver.solve_rhccs ~ex:false
      | "pdr" -> HCCSSolver.solve_pdr >> flip Pair.make []
      | _ -> raise Not_found)

let usage =
  (*"FPAT: A Framework for Program Analysis and Transformation\n\n" ^*)
  "Usage: "
  ^ Sys.executable_name
  ^ " <options> <files>\noptions are:"
let arg_spec =
  ["-I", Arg.String (fun dir -> Config.load_path := dir :: !Config.load_path),
   "<dir> Add <dir> to the list of include directories";
   "-silent", Arg.Set Global.silent, " Enable silent mode";
   "-bench", Arg.Set Global.bench_mode, " Enable benchmark mode";
   "-result-filename", Arg.String (fun filename -> Bench.result_filename := filename),
   "<s> result filename for benchmark mode";
   "-timeout", Arg.Int (fun n -> Global.timeout := n), "<n> Set timeout (sec)";
   (* debugging *)
   "-debug", Arg.Set Global.debug, " Enable debug mode";
   "-debug-regexp", Arg.String (fun str -> Global.debug := true;
                                 Logger.filter_regexp := Str.regexp str),
   "<regexp> Enable debug mode (only log calls that match <regexp> and their ancestor calls)";
   "-debug-regexp-exact", Arg.String (fun str -> Global.debug := true;
                                       Logger.filter_exact := true;
                                       Logger.filter_regexp := Str.regexp str),
   "<regexp> Enable debug mode (only log calls that match <regexp>)";
   "-debug-call-id", Arg.Int (fun n -> Global.debug := true;
                               Logger.filter_call_id := n),
   "<n> Enable debug mode (only log calls that match call_id <n> and their ancestor calls)";
   "-debug-call-id-exact", Arg.Int (fun n -> Global.debug := true;
                                     Logger.filter_exact := true;
                                     Logger.filter_call_id := n),
   "<n> Enable debug mode (only log calls that match call_id <n>)";
   "-debug-level", Arg.Int (fun n -> Global.debug_level := n),
   "<n> Set debug level";
   "-profile", Arg.Unit (fun () -> Global.debug := true;
                          Logger.filter_kinds := [Logger.Trace]),
   " Enable profiling mode";
   "-print-log", Arg.Set Global.print_log,
   " Print log messages to the standard output";
   "-disable-print-callstack-exception",
   Arg.Clear Logger.print_call_stack_on_exception,
   " Disable printing the call stack when an exception is occurred";
   (* options for program verification *)
   (* preprocessing of ML programs *)
   "-no-anf", Arg.Clear Fdef.use_anf, " Disable ANF transformation";
   (* constraint generation *)
   "-disable-ho-ufuns", Arg.Clear RefTypCheck.ho_ufuns, " Disable using uninterpreted functions for function arguments";
   (* HCCS solvers *)
   "-hccs", Arg.String (fun str ->
       try HCCSSolver.of_string_dyn str |> HCCSSolver.link_dyn
       with Not_found -> raise (Arg.Bad "invalid argument for -hccs")),
   "<n> Use an HCCS solver based on\n"
   (* FLOPS 2008 *)
   ^ "\t\t\t\tqe: top-down iterative quantifier elimination\n"
   (* PPDP 2009 *)
   ^ "\t\t\t\tit: top-down iterative interpolation\n"
   (* PEPM 2013 *)
   ^ "\t\t\t\tgit: generalized interpolation\n"
   ^ "\t\t\t\tgch: generalized interpolation using convex hull\n"
   ^ "\t\t\t\tgtb: generalized interpolation using template-based synthesis\n"
   (* TACAS 2015 *)
   ^ "\t\t\t\tsb0: polytope sampling (old implementation)\n"
   ^ "\t\t\t\tsb: polytope sampling\n"
   ^ "\t\t\t\tdex: dag expansion + top-down iterative interpolation\n"
   ^ "\t\t\t\tsbe: TACAS 2015 experiments\n"
   (* ESOP 2015 *)
   ^ "\t\t\t\trs1: relaxed stratification (with cut_len=1)\n"
   ^ "\t\t\t\trs4: relaxed stratification (with cut_len=4)\n"
   ^ "\t\t\t\trs8: relaxed stratification (with cut_len=8)\n"
   ^ "\t\t\t\titm: top-down iterative interpolation by MathSAT 5\n"
   ^ "\t\t\t\texl: exact L-restriction\n"
   ^ "\t\t\t\trse: ESOP 2015 experiments\n"
   (* SAS 2016 or FM 2016? *)
   ^ "\t\t\t\texpsb: extremal point sampling\n"
   ^ "\t\t\t\tsb: point sampling\n"
   (* others *)
   ^ "\t\t\t\tlb: bottom-up iterative propagation\n"
   ^ "\t\t\t\tai: bottom-up abstract interpretation with widening\n"
   ^ "\t\t\t\ttb: template-based synthesis\n"
   ^ "\t\t\t\tuoa: unwinding + iterative under/over approximation\n"
   ^ "\t\t\t\tpdr: PDR on Z3 (muZ)\n"
   ^ "\t\t\t\tdual: duality on Z3 (muZ)";
   (* options for HCCS solvers *)
   "-save-graphviz", Arg.Set MLVerifier.save_graphviz, " Genarate HCCS tree in *.dot file";
   "-l2r", Arg.Set BwIPHCCSSolver.solve_body_left_to_right,
   " Incrementally solve the body of each HCCS from left to right (it)";
   "-disable-interp-simp", Arg.Clear InterpProver.interp_simplify,
   " Disable simplification of interpolants and HCCS solutions (it)";
   "-fol-bw", Arg.Set FwHCCSSolver.fol_backward,
   " use backward transformation of HCCSs to FOL formulas (lb)";
   "-threshold", Arg.Set_int FwWidenHCCSSolver.threshold,
   " Set the threshold of widening (ai)";
   "-numconj", Arg.Set_int Template.num_conj,
   " The number of conjuncts (tb)";
   "-numdisj", Arg.Set_int Template.num_disj,
   " The number of disjuncts (tb)";
   "-lin-template", Arg.Set Template.linear_farkas,
   " Enable linear constraint solving (tb)";
   "-ucore", Arg.Set DecoHCCSSolver.use_unsat_core,
   " Use unsat core for decomposition refinement (sb)";
   "-intcex", Arg.Set PointSampleHCCSSolver.use_integer_counterexamples,
   " Use integer counterexamples (expsb)";
   "-disable-ray", Arg.Set Polyhedron.disable_ray,
   " Disable ray counterexample sampling (expsb)";
   "-enable-pol", Arg.Set Polyhedron.enable_pol,
   " Enable polytope counterexample sampling instead of point (expsb)";
   "-disable-conj", Arg.Set HornClause.disable_conj,
   " Disable conj (expsb)";
   "-ray-convex", Arg.Set PointSampleHCCSSolver.ray_convex,
   " Enable incomplete constraint generation for ray samples (expsb)";
   "-cegsp", Arg.Set PointSampleHCCSSolver.ceg_samp_part,
   " Enable counterexample guided sample set partitioning (expsb)";
   "-cegde", Arg.Set PointSampleHCCSSolver.ceg_dag_expand,
   " Enable counterexample guided DAG expansion (expsb)";
   "-cegru", Arg.Set PointSampleHCCSSolver.ceg_rec_unwind,
   " Enable counterexample guided recursion unwinding (expsb)";
   "-itr", Arg.Clear PointSampleHCCSSolver.inc_temp_refine,
   " Enable incrememtal template refinement (expsb)";
   "-nlin-psb", Arg.Clear PointSampleHCCSSolver.linear_farkas,
   " Enable non-linear constraint solving for polytope samples (expsb)";
   "-enable-comp", Arg.Set PointSampleHCCSSolver.complete,
   " Enable relatively complete constraint solving (expsb)";
   (* HCCS simplification *)
   "-no-reduce", Arg.Clear MLVerifier.reduce_hccs, " Disable HCCS reduction";
   "-force-reduce", Arg.Set MLVerifier.force_reduce, " Force HCCS reduction";
   "-res-inc", Arg.Set HCCS.resolve_inc, " resolve HCCSs incrementally";
   "-disable-pred-share1", Arg.Set PvaCube.disable_pred_sharing1,
   " an option for HCCS simplification?";
   "-enable-pred-share2", Arg.Set PvaCube.enable_pred_sharing2,
   " an option for HCCS simplification?";
   "-no-flatten", Arg.Clear WFHCCSSolver.flatten,
   " Disable flattening goal clauses";
   "-conv-real", Arg.Set MLVerifier.conv_real,
   " Convert integers in HCCS into reals";
   "-disable-elim-lt-gt", Arg.Set CunAtom.disable_elim_lt_gt,
   " Disable elimination of LessThan and GreaterThan for QFLIA";
   (* HCCS solving over booleans *)
   "-boolenc", Arg.Int (function
       | 0 -> HCCSSolver.ext_solve_bool := id
       | 1 -> HCCSSolver.ext_solve_bool := EncBoolHCCSSolver.solve false
       | 2 -> HCCSSolver.ext_solve_bool := EncBoolHCCSSolver.solve true
       | 3 -> HCCSSolver.ext_solve_bool :=
           SplitBoolHCCSSolver.solve BoolHCCSSolver.solve
       | _ -> raise (Arg.Bad "invalid argument for -boolenc")),
   "<n> Use a boolean HCCS solving method based on\n"
   ^ "\t\t\t\t0: no preprocessing\n"
   ^ "\t\t\t\t1: integer encoding (true -> 1, false -> 0)\n"
   ^ "\t\t\t\t2: integer encoding (true -> pos. int., false -> non-pos. int.)\n"
   ^ "\t\t\t\t3: splitting into integer and boolean HCCSs";
   "-use-gch-for-bool", Arg.Set AtomHCCSSolver.use_pepm_for_bool,
   "use gch for Boolean HCCS solving";
   (* HCCS solving over ADTs *)
   "-default-size-cata", Arg.Set SizeFun.use_default_size,
   " Use default size functions for ADTs (deprecated)";
   "-default-size", Arg.Set SizeFun.use_default_size,
   " Use default size functions for ADTs";
   "-no-sizeabs", Arg.Clear MLVerifier.enable_sizeabs, " Disable size abstraction";
   (* interpolating provers *)
   "-interp", Arg.String ((function
       | "csisat" -> InterpProver.interpolate_csisat_dyn
       | "csisatgen" -> InterpProver.interpolate_csisat_gen_dyn
       | "aisat" -> InterpProver.interpolate_aisat_dyn
       | "z3" -> InterpProver.interpolate_z3_dyn
       | "tb" -> TemplateBasedInterpProver.interpolate
                   QelimBody.elim_int_vars_full
       | "expsb" -> HCCSInterpProver.interpolate
                      (PointSampleHCCSSolver.solve ~ex:true)
       (*PointSampleInterpProver.interpolate ~ex:true
         (QelimBody.elim_int_vars_full [])*)
       | "psb" -> HCCSInterpProver.interpolate
                    (PointSampleHCCSSolver.solve ~ex:false)
       (*PointSampleInterpProver.interpolate ~ex:false
         (QelimBody.elim_int_vars_full [])*)
       | _ -> raise (Arg.Bad "invalid argument for -interp"))
      >> InterpProver.link_dyn),
   "<n> Use an interpolating prover based on\n"
   ^ "\t\t\t\tcsisat: CSIsat\n"
   ^ "\t\t\t\tcsisatgen: CSIsat + generalization heuristics\n"
   ^ "\t\t\t\tz3: Z3\n"
   ^ "\t\t\t\ttb: templated-based synthesis\n"
   ^ "\t\t\t\texpsb: extremal point sampling\n"
   ^ "\t\t\t\tpsb: point sampling";
   (* options for interpolating provers *)
   "-degree", Arg.Int (fun d -> InterpProver.degree := d; Template.degree := d),
   " Set the degree of interpolants";
   "-use-mosek", Arg.Set InterpProver.use_mosek,
   " Use Mosek for SDP solving";
   "-truncate", Arg.Float (fun t -> InterpProver.truncate := Some t),
   " Set truncation threshold";
   "-round", Arg.Int (fun bits -> InterpProver.round_bits := bits),
   " Set bits for rounding";
   "-aisat-debug", Arg.Set InterpProver.aisat_debug,
   " Enable debug mode of aiSat";
   "-nocomb", Arg.Set InterpProver.nocomb,
   " Use only at most one polynomial in the cone part of a template";
   "-grad", Arg.String (function s -> InterpProver.grad := Some s),
   " Make objective function with the gradient calculated by the degree of each monomial (prop or fact)";
   (* SMT solvers *)
   "-smt", Arg.String (function
       | "z3" -> SMTProver.init_z3 ()
       | "cvc3" -> SMTProver.init_cvc3 ()
       | _ -> raise (Arg.Bad "invalid argument for -smt")),
   "<n> Specify an SMT solver\n"
   ^ "\t\t\t\tz3: Z3\n"
   ^ "\t\t\t\tcvc3: CVC3";
   (* options for SMT solvers *)
   "-print-z3", Arg.Set SMTProver.print_z3, " Print Z3 input";
   "-timeout-z3", Arg.Int (fun n -> Global.timeout_z3 := n * 1000),
   "<n> Set timeout for Z3 (sec) (default: 60 sec)";
   (* polynomial constraint solvers *)
   "-pc", Arg.String (function
       | "z3" -> PolyConstrSolver.ext_solve := PolyConstrSolver.solve_z3
       | "cvc3" -> PolyConstrSolver.ext_solve := PolyConstrSolver.solve_cvc3
       | "glpk" -> PolyConstrSolver.ext_solve := PolyConstrSolver.solve_glpk
       (* use PolyConstrSolver.gen_coeff_constr ~pos:true ~linear:true *)
       | "gsl0" ->
         PolyConstrSolver.cqp_mode := 0;
         PolyConstrSolver.ext_solve := PolyConstrSolver.solve_gsl
       (* use PolyConstrSolver.gen_coeff_constr ~pos:false ~linear:true *)
       | "gsl1" ->
         PolyConstrSolver.cqp_mode := 1;
         PolyConstrSolver.ext_solve := PolyConstrSolver.solve_gsl
       (* use PolyConstrSolver.gen_coeff_constr ~pos:false ~linear:true *)
       | "gsl2" ->
         PolyConstrSolver.cqp_mode := 2;
         PolyConstrSolver.ext_solve := PolyConstrSolver.solve_gsl
       (* use PolyConstrSolver.gen_coeff_constr ~pos:true ~linear:true *)
       | "gsl3" ->
         PolyConstrSolver.cqp_mode := 3;
         PolyConstrSolver.ext_solve := PolyConstrSolver.solve_gsl
       (* use PolyConstrSolver.gen_coeff_constr ~pos:true ~linear:true *)
       | "bb" -> PolyConstrSolver.ext_solve := BvPolyConstrSolver.solve
       (* use PolyConstrSolver.gen_coeff_constr ~pos:true *)
       | "cad" -> PolyConstrSolver.ext_solve := CadPolyConstrSolver.solve
       | _ -> raise (Arg.Bad "invalid argument for -template")),
   "<n> Specify a polynomial constraint solver\n"
   ^ "\t\t\t\tz3: Z3\n"
   ^ "\t\t\t\tcvc3: CVC3\n"
   ^ "\t\t\t\tglpk: GLPK MIP\n"
   ^ "\t\t\t\tgsl0: GSL CQP (mode 1)\n"
   ^ "\t\t\t\tgsl1: GSL CQP (mode 2)\n"
   ^ "\t\t\t\tgsl2: GSL CQP (mode 3)\n"
   ^ "\t\t\t\tgsl3: GSL CQP (mode 4)\n"
   ^ "\t\t\t\tbb: bit-blasting\n"
   ^ "\t\t\t\tcad: QECAD";
   (* options for polynomial constraint solvers *)
   "-enable-lang-restrict", Arg.Set PolyConstrSolver.enable_lang_restrict,
   " Enable L-restriction";
   "-use-ineq", Arg.Set PolyConstrSolver.lang_restrict_use_inequality,
   " Use inequalities in L-restriction";
   "-tlr", Arg.Set_int PolyConstrSolver.lang_restrict_threshold,
   " Threshold of the absolute value of coefficients in L-restriction";
   "-tbit", Arg.Set_int BvPolyConstrSolver.bits_threshold,
   " Threshold of the size of bit-vectors (bb)";
   (* RHCCS solvers *)
   "-rhccs", Arg.String (function str ->
     try RHCCSSolver.ref_solver := RHCCSSolver.of_string_dyn str
     with Not_found -> raise (Arg.Bad "invalid argument for -rhccs")),
   "<n> Use an RHCCS solver based on\n"
   ^ "\t\t\t\titunw: iterative unwinding\n"
   ^ "\t\t\t\texpsb: extremal point sampling\n"
   ^ "\t\t\t\tpsb: point sampling";
   (* options for RHCCS solvers *)
   "-recsep", Arg.Set RecHCCSSolver.separation,
   " solve goal clauses separately";
   "-disable-cover-check", Arg.Set RecHCCSSolver.disable_cover_check,
   " disable cover check";
   "-save-unwound-hccs-dag", Arg.Set RecHCCSSolver.save_unwound_hccs_dag,
   " save the dag of unwound HCCS";
   (* options for RHCWFCS solver *)
   "-wfsep", Arg.Set WFHCCSSolver.separation,
   " solve goal clauses separately";
   (* options for forall-exists HCCS solvers *)
   "-skolem-type", Arg.String (fun t -> MLVerifier.temp_type := t),
   " specify the type of templates used in Skolemization";
   (* options for exists-forall HCCS solvers *)
   "-lin-eahccs", Arg.Set EAHCCSSolver.linear_farkas,
   " Enable linear constraint solving for EHCCS solving";
   "-aec", Arg.Set EAHCCSSolver.accumulate_ext_constrs,
   " Accumulate the constraints in EAHCCS solving";
   "-no-cmask", Arg.Clear PolyConstrSolver.mask_coeffs,
   " Not to mask coefficients in EAHCCS solving";
   (* options for ranking function synthesis *)
   "-rank-widen", Arg.Set RankFunInfer.rank_widen,
   " Use widening for ranking function synthesis";
   "-lex", Arg.Int (fun n -> RankFun.num_rankfuns := n),
   " Number of ranking functions (deprecated)";
   "-llrf", Arg.Int (fun n -> RankFun.num_rankfuns := n),
   "<n> Use lexicographic linear ranking functions";
   "-plrf", Arg.Set RankFun.piecewise_linear,
   "<n> Use piecewise linear ranking functions";
   (* refinement type inference *)
   "-disable-inlining", Arg.Set RefTypInfer.no_inlining,
   " Disable HCCS inlining";
   "-cpo", Arg.Set RefTypInfer.cut_points_only,
   " Find good predicates for cut-points";
   "-simple-template", Arg.Set MLVerifier.use_simple_template,
   " Use simple refinement templates";
   "-precond-template", Arg.Set MLVerifier.use_precondition_template,
   " Use simple precondition refinement templates";
   "-cred", Arg.Set MLVerifier.complement_reduced_preds,
   " Find a solution for the eliminated predicate variables";
   (* relatively-complete refinemenet type inference *)
   "-nex", Arg.Set_int RefTypInfer.number_of_extra_params,
   " Number of inserted extra parameters for each functional argument";
   "-cc", Arg.Set RefTypInfer.enable_coeff_const,
   " Enable constant terms of extra parameters";
   "-flag-coeff", Arg.Set RefTypInfer.flag_coeff,
   " an option for EAHCCS generation?";
   (* abstraction type inference *)
   "-split-eq", Arg.Set AbsType.split_equalities,
   " Split equalities";
   "-eap", Arg.Set AbsType.extract_atomic_predicates,
   " Extract atomic predicates";
   (* predicate abstraction *)
   "-use-cfp", Arg.Set PredAbst.use_cfp,
   " Use constraint fixpoint computation for predicate abstraction";
   "-wp-max", Arg.Set_int PredAbst.wp_max_num,
   "<n> The maximum dimension of hypercubes in predicate abstraction";
   "-neg-pred", Arg.Set PredAbst.use_neg_pred,
   " Use negative predicates for abstraction";
   "-no-opt", Arg.Clear PredAbst.incomplete_opt,
   " Disable an (incomplete) optimization of predicate abstraction";
   (* predicate set *)
   "-pset", Arg.Int (fun n -> CFPPAHCCSSolver.pset_opt := n),
   "<n> predicate set making (CFPPAHCCSSolver)\n"
   ^ "\t\t\t\t0: use atom\n"
   ^ "\t\t\t\t1: use raw formula";
   (* verification modes *)
   "-termination", Arg.Set MLVerifier.termination_mode,
   " Perform termination verification";
   "-nonterm", Arg.Set MLVerifier.nonterm_mode,
   " Perform non-termination verification";
   "-game", Arg.Set Global.game_mode,
   " Perform program games solving";
   (* game solving mode *)
   "-chktc", Arg.Set GameSolver.check_term_cond,
   " Check the termination condition at recursive calls";
   "-noinline", Arg.Clear GameSolver.inline,
   " Disable inlining in game solving";
   (* induction mode *)
   "-induction", Arg.Set MLVerifier.induction_mode,
   " Induction based Horn constraint solving";
   "-select", Arg.Set InductHCCS.select_unfolded, " Select unfolded pva";
   (* precondition inference *)
   "-weakpre", Arg.Unit (fun () ->
       MLVerifier.enable_infer_weak_pre := true;
       MLVerifier.enable_infer_pre := true),
   " Enable (disjunctive) weak precondition inference";
   "-maximally-weakpre",
   Arg.Unit (fun () ->
       MLVerifier.enable_infer_maximally_weak_pre := true;
       MLVerifier.enable_infer_pre := true),
   " Enable maximally-weak precondition inference by type optimization";
   (* type optimization mode *)
   "-typeopt",
   Arg.Int
     (function
       | 0 -> MLVerifier.typeopt_mode := true
       | 1 -> MLVerifier.typeopt_mode := true;
         MultiObjectiveHCCSSolver.optimality_checking := true
       | 2 -> MLVerifier.typeopt_mode := true;
         MultiObjectiveHCCSSolver.optimality_checking := true;
         MultiObjectiveHCCSSolver.heuristic_timeout := true
       | _ -> raise (Arg.Bad "invalid argument for -typeopt")),
   "<n> Specify a type optimization mode option\n"
   ^ "\t\t\t\t0: Type optimization mode\n"
   ^ "\t\t\t\t1: Type optimization mode with Pareto-optimality checking\n"
   ^ "\t\t\t\t2: Type optimization mode with Pareto-optimality checking and heuristic timeout";
   "-threshold-typeopt",
   Arg.Int (fun n -> MultiObjectiveHCCSSolver.threshold := n),
   "<n> Set threshold to a number of iteration";
   "-print-proof-tree",
   Arg.String (fun name -> 
                ProofTree.print_proof_tree := true;
                ProofTree.output_file_name := name),
   "<n> Set filename where is saved proof tree (tex-format).";
  ]



let set_default () =
  (* default SMT solver *)
  SMTProver.init_z3 ();
  (* default polynomial constraint solver *)
  PolyConstrSolver.ext_solve := PolyConstrSolver.solve_z3;
  (* default interpolating prover *)
  InterpProver.link_dyn InterpProver.interpolate_csisat_dyn;
  (* default HCCS solver *)
  HCCSSolver.link_dyn BwIPHCCSSolver.solve;
  (* default RHCCS solver *)
  RHCCSSolver.ref_solver := RecHCCSSolver.solve ~auto:true;
  (* default unit, bool, and tuple HCCS encoders *)
  HCCSSolver.ext_solve_unit := UnitHCCSSolver.solve;
  HCCSSolver.ext_solve_bool := id;
  HCCSSolver.ext_solve_tuple := TupHCCSSolver.solve
let set_default =
  (* this is not printed because !Global.debug is false *)
  Logger.log_block1 "FPATConfig.set_default" set_default

let is_fpat_exception = function
  | Global.NotImplemented _
  | Global.NoMatch _
  (*| SMTProver.Unsat*)
  | SMTProver.Unknown
  (*| InterpProver.NoInterpolant*)
  | InterpProver.Fail
  | InterpProver.Unknown
  (*| PolyConstrSolver.NoSolution*)
  | PolyConstrSolver.Unknown
  | RefTypInfer.FailedToRefineTypes
  | RefTypInfer.FailedToRefineInputTypes
  | RefTypInfer.FailedToRefineExtraParameters -> true
  | _ -> false

let pr_exception ppf = function
  | Global.NotImplemented msg -> Format.fprintf ppf "Not implemented: %s" msg
  | Global.NoMatch msg -> Format.fprintf ppf "Not matched: %s" msg
  | SMTProver.Unknown -> Format.fprintf ppf "Failure of SMT prover"
  | InterpProver.Fail ->
    Format.fprintf ppf
      "Failure of interpolating prover (integer domain not fully supported)"
  | InterpProver.Unknown ->
    Format.fprintf ppf "Failure of interpolating prover"
  | PolyConstrSolver.Unknown ->
    Format.fprintf ppf "Failure of polynomial constraint solver"
  | RefTypInfer.FailedToRefineTypes ->
    Format.fprintf ppf "Failure of abstraction type refinement"
  | RefTypInfer.FailedToRefineInputTypes ->
    Format.fprintf ppf
      "Failure of abstraction type refinement of external inputs"
  | RefTypInfer.FailedToRefineExtraParameters ->
    Format.fprintf ppf "Failure of parameter substitution refinement"
  | _ -> raise Not_found

let string_of_fpat_exception = Printer.string_of pr_exception
