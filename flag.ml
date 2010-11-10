
let log_dir = "/home/ryosuke/web/cegar/log/"
let trecs = "./trecs"
let cvc3 = "./cvc3"

let merge_counterexample = ref false
let split_free_var = ref false
let filter_forward = ref true

let wp_depth = ref 0
let wp_max_num = ref 3

let use_unknown = ref false
let use_old_partition = ref false

let assume = ref false (* use strongest post condition in if-term *)
let assume_if = ref false (* replace if-term to branch or not, when !assume = true *)
let nondet = ref false (* eager evaluation for branch *)

let check_fun_arg_typ = false

(* display or not *)
let print_source = true
let print_cps = false
let print_abst = true
let print_abst_eager = false
let print_type = true
let print_hors = false
let print_trecs_output = true
let print_trace = false
let print_interpolant = true
let print_progress = false
let print_constraints = true
let print_lower_bound = true
let print_cvc3 = false

let time_abstraction = ref 0.
let time_mc = ref 0.
let time_cegar = ref 0.
let time_interpolant = ref 0.

let cegar_loop = ref 0
let time_limit = 0
let string_for_result = "__ce__"
let max_input_size = 65536


let web = false
let debug = true


let use_nint = ref false
let use_subterm = false
let use_neg_pred = true
let use_part_eval = true
let check_sat = true
