let horsat = ref Environment.horsat
let horsatp = ref Environment.horsatp
let omega = ref Environment.omega
let trecs = ref Environment.trecs
let cvc3 = ref Environment.cvc3

let filename = ref ""
let spec_file = ref ""

type mode = Reachability | FileAccess | Termination | NonTermination | FairTermination | FairNonTermination
type model_checker = TRecS | HorSat | HorSatP

(* TRecS option *)
let trecs_param1 = ref 1000
let trecs_param2 = ref 10

(* debug option *)
let check_fun_arg_typ = false
let check_typ = true
let debug_module : string list ref = ref []
let debug_abst = ref false
let print_ref_typ_debug () = List.mem "Ref_type" !debug_module

(* method option *)
let mode = ref Reachability
let mc = ref TRecS
let input_cegar = ref false
let nondet = ref false (* eager evaluation for branch *)
let use_prefix_trace = false
let use_nint = ref false
let use_part_eval = true
let check_sat = true
let gen_int = true
let merge_counterexample = ref false
let use_multiple_paths = ref false
let split_free_var = ref false
let filter_forward = ref true
let use_unknown = ref false
let church_encode = ref false
let beta_reduce = false (* do beta reduction before model checking *)
let useless_elim = false
let lift_fv_only = ref false
let relative_complete = ref false
let never_use_relative_complete = ref true
let no_exparam = ref true
let cps_simpl = ref false
let bool_init_empty = ref false
let insert_param_funarg = ref false
let split_assert = ref false
let encode_list_opt = ref false
let tupling = ref false
let elim_same_arg = ref false
let base_to_int = ref false
let exists_unknown_false = true
let replace_const = ref false
let use_spec = ref false
let comment_spec = ref true
let cartesian_abstraction = ref true
let modular = ref false
let verify_ref_typ = ref false
let ignore_non_termination = ref false
let fail_as_exception = ref false



(* print option *)
let print_source = true
let print_cps = true
let print_abst = true
let print_abst_eager = true
let print_type = true
let print_hors = false
let print_trecs_output = false
let print_trace = false
let print_interpolant = true
let print_progress = ref true
let print_constraints = true
let print_lower_bound = true
let print_refine_log = true
let print_eval_abst = ref false
let print_fun_arg_typ = ref true
let print_rd_constraints = ref true
let print_abst_typ = ref false
let print_as_ocaml = ref false

let web = ref false

(* variables for log *)
let time_abstraction = ref 0.
let time_mc = ref 0.
let time_cegar = ref 0.
let time_interpolant = ref 0.
let time_parameter_inference = ref 0.
let result = ref ""

let cegar_loop = ref 1
let time_limit = ref 0
let max_input_size = 65536
let args = ref [""] (* command-line options *)


(* mode option *)
let only_result = ref false
let exp = ref false
let exp2 = ref false
let ignore_conf = ref false
let init_trans = ref true
let just_print_non_CPS_abst = ref false
let trans_to_CPS = ref true

(* predicate abstraction option *)
let use_filter = ref false
let disable_predicate_accumulation = ref false
let never_use_neg_pred = ref false
let wp_max_max = 8
let remove_false = ref false (* remove false from pbs/pts in CEGAR_abst_util *)
let assume = ref false (* use strongest post condition in if-term *)
let assume_if = ref false (* whether replace if-term to branch or not (this flag is used only when !assume = true) *)
let expand_nonrec = ref true
let expand_nonrec_init = ref true
let decomp_pred = ref false

(* pretty printer option *)
let () = Format.set_margin 120
let color = ref false
let color_always = ref false

let write_annot = ref true

(* termination-mode option *)
let disjunctive = ref false
let separate_pred = ref false
let split_callsite = ref false
let add_closure_depth = ref false
let add_closure_exparam = ref false

(* non-termination verification *)
let merge_paths_of_same_branch = ref false
let randint_refinement_log = ref false
let use_omega = ref true
let use_omega_first = ref false

(* fair-termination-mode option *)
let expand_set_flag = ref false
let fair_term_loop_count = ref 0

(* fair-non-termination-mode option *)
let expand_ce_iter_init = ref 5
let break_expansion_ref = ref false
