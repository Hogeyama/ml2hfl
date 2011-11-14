
let ocaml_lib = ["/usr/lib/ocaml"]
let log_dir = "./log"
let trecs = "./trecs -d2 -p 1000 100"
let trecs = "./trecs"
let cvc3 = "./cvc3"
let trecs_log = "log.hors"


type rec_flag = Nonrecursive | Recursive
type mutable_flag = Immutable | Mutable

type mode = Reachability | FileAccess
type refine = RefineSizedType | RefineDependentType
type cegar = CEGAR_SizedType | CEGAR_DependentType


(* debug option *)
let check_fun_arg_typ = false
let check_typ = false

(* method option *)
let wp_max_num = ref 3
let assume = ref false (* use strongest post condition in if-term *)
let assume_if = ref false (* replace if-term to branch or not, when !assume = true *)
let nondet = ref false (* eager evaluation for branch *)
let use_dor = true
let use_prefix_trace = false
let use_nint = ref false
let use_subterm = false
let use_neg_pred = true
let use_part_eval = true
let check_sat = true
let gen_int = true
let merge_counterexample = ref false
let split_free_var = ref false
let filter_forward = ref true
let use_unknown = ref false

let mode = ref Reachability
let refine = ref RefineDependentType
let cegar = ref CEGAR_DependentType
let init_trans = ref true
let cps_excep = ref false



(* display or not *)
let print_source = true
let print_cps = true
let print_abst = true
let print_abst_eager = true
let print_type = true
let print_hors = false
let print_trecs_output = false
let print_trace = false
let print_interpolant = false
let print_progress = true
let print_constraints = false
let print_lower_bound = false
let print_cvc3 = false
let print_refine_log = false


(* variables for log *)
let time_abstraction = ref 0.
let time_mc = ref 0.
let time_cegar = ref 0.
let time_interpolant = ref 0.

let cegar_loop = ref 0
let time_limit = 0
let max_input_size = 65536


(* mode option *)
let web = ref false
let debug = true


(* pretty printer's option *)
let () = Format.set_margin 120
