
let trecs = ref Environment.trecs
let cvc3 = ref Environment.cvc3

let filename = ref ""
let spec_file = ref ""

type mode = Reachability | FileAccess
type cegar = CEGAR_InteractionType | CEGAR_DependentType
type pred_abst = PredAbst | PredAbstCPS
type model_check = ModelCheck | ModelCheckCPS
type refine = RefineRefType of int
type form = CPS

(* TRecS option *)
let trecs_param1 = ref 1000
let trecs_param2 = ref 10

(* debug option *)
let check_fun_arg_typ = false
let check_typ = true

(* method option *)
let wp_max_num = ref 3
let assume = ref false (* use strongest post condition in if-term *)
let assume_if = ref false (* replace if-term to branch or not, when !assume = true *)
let nondet = ref false (* eager evaluation for branch *)
let use_dor = true
let use_prefix_trace = false
let use_nint = ref false
let use_subterm = false
let use_neg_pred = ref false
let never_use_neg_pred = ref false
let use_part_eval = true
let check_sat = true
let gen_int = true
let merge_counterexample = ref false
let split_free_var = ref false
let filter_forward = ref true
let use_unknown = ref false
let church_encode = false
let beta_reduce = false (* do beta reduction before model checking *)
let useless_elim = false
let lift_fv_only = ref false
let use_filter = ref false
let disable_predicate_accumulation = ref false
let relative_complete = ref false
let disable_relatively_complete_verification = ref false
let expand_nonrec = ref true
let expand_nonrec_init = ref true
let cps_simpl = ref false
let bool_init_empty = ref false
let insert_param_funarg = ref false
let split_assert = ref false
let encode_list_opt = ref false
let tupling = ref false
let elim_same_arg = ref false
let base_to_int = ref false

let mode = ref Reachability
let init_trans = ref true
let cegar = ref CEGAR_DependentType
let pred_abst = ref PredAbstCPS
let model_check = ref ModelCheckCPS
let refine = ref (RefineRefType 0)
let form : form list ref = ref []
let use_spec = ref false
let comment_spec = ref true





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
let print_ref_typ = ref false
let print_ref_typ_debug = false

let web = ref false

(* variables for log *)
let time_abstraction = ref 0.
let time_mc = ref 0.
let time_cegar = ref 0.
let time_interpolant = ref 0.
let time_parameter_inference = ref 0.
let result = ref ""

let cegar_loop = ref 0
let time_limit = ref 0
let max_input_size = 65536


(* mode option *)
let debug_level = ref 1
let only_result = ref false
let exp = ref false
let ignore_conf = ref false
let args = ref [""]


(* pretty printer's option *)
let () = Format.set_margin 120
let color = ref false
let color_always = ref false

let extpar_header = "_ep"

let write_annot = ref true

(* termination-mode option *)
let termination = ref false

let disjunctive = ref false
let separate_pred = ref false
let split_callsite = ref false
let add_closure_depth = ref false
let add_closure_exparam = ref false
