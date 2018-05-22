let omega = ref Mconfig.omega
let cvc3 = ref Mconfig.cvc3

let filenames : string list ref = ref []
let mainfile () =
  match !filenames with
  | filename::_ -> filename
  | _ -> invalid_arg "Flag.mainfile"
let spec_file = ref ""

let time_limit = ref 0

let use_abst : string list ref = ref []
let add_use_abst s = if not @@ List.mem s !use_abst then use_abst := s :: !use_abst
let pp : string option ref = ref None

module TRecS = struct
  let param1 = ref 1000
  let param2 = ref 10
end

module Method = struct
  type mode = Reachability | FileAccess | Termination | NonTermination | FairTermination | FairNonTermination | PrintRefConstr
  let mode = ref Reachability
  let input_cegar = ref false
  let nondet = ref false (* eager evaluation for branch *)
  let use_nint = ref false
  let use_part_eval = true
  let check_sat = true
  let gen_int = true
  let split_free_var = ref false
  let filter_forward = ref true
  let use_unknown = ref false
  let lift_fv_only = ref false
  let relative_complete = ref false
  let never_use_relative_complete = ref true
  let no_exparam = ref true
  let cps_simpl = ref false
  let bool_init_empty = ref false
  let insert_param_funarg = ref false
  let split_assert = ref false
  let encode_list_opt = ref false
  let abst_list_eq = ref true
  let abst_literal = ref (-1)
  let tupling = ref false
  let elim_redundant_arg = ref true
  let elim_same_arg = ref false
  let base_to_int = ref false
  let exists_unknown_false = true
  let replace_const = ref false
  let use_spec = ref false
  let comment_spec = ref true
  let modular = ref false
  let verify_ref_typ = ref false
  let ignore_non_termination = ref false
  let fail_as_exception = ref false
  let ignore_exn_arg = ref false
  let data_to_int = ref false
  let quick_check = ref false
end

module Print = struct
  let source = true
  let cps = true
  let abst = true
  let abst_eager = true
  let typ = true
  let hors = false
  let trecs_output = false
  let trace = false
  let interpolant = true
  let progress = ref true
  let modular_progress = ref true
  let constraints = true
  let lower_bound = true
  let refine_log = true
  let eval_abst = ref false
  let fun_arg_typ = ref true
  let rd_constraints = ref true
  let abst_typ = ref false
  let as_ocaml = ref false
  let only_if_id = ref false
  let unused_arg = ref false
  let result = ref true
  let certificate = ref false
end

module Log = struct
  let time_abstraction = ref 0.
  let time_mc = ref 0.
  let time_cegar = ref 0.
  let time_interpolant = ref 0.
  let time_parameter_inference = ref 0.
  let result = ref ""

  let cegar_loop = ref 1
  let args = ref [""] (* command-line options *)

  let output_csv : string option ref = ref None
  let output_json : string option ref = ref None
end

(** TODO merge with Method *)
module Mode = struct
  let ignore_conf = ref false
  let init_trans = ref true
  let just_print_non_CPS_abst = ref false
  let trans_to_CPS = ref true
  let module_mode = ref false
end

module PredAbst = struct
  let use_filter = ref false
  let never_use_neg_pred = ref false
  let wp_max_max = 8
  let remove_false = ref false (* remove false from pbs/pts in CEGAR_abst_util *)
  let assume = ref false (* use strongest post condition in if-term *)
  let assume_if = ref false (* whether replace if-term to branch or not (this flag is used only when !assume = true) *)
  let expand_non_rec = ref true
  let expand_non_rec_init = ref true
  let decomp_pred = ref false
  let decomp_eq_pred = ref false
  let no_simplification = ref false
  let cartesian = ref true
  let shift_pred = ref false
end

module Refine = struct
  let use_prefix_trace = false
  let merge_counterexample = ref false
  let use_multiple_paths = ref false
  let disable_predicate_accumulation = ref false
  let use_rec_hccs_solver = ref false
  type solver = Hoice | Z3 | Z3_spacer
  let solver = ref Hoice
  let solver_timelimit = ref 5 (* seconds *)
  let hoice = ref Mconfig.hoice
  let z3 = ref Mconfig.z3
  let z3_spacer = ref (Mconfig.z3 ^ " fixedpoint.engine=spacer")
end

module ModelCheck = struct
  let trecs = ref Mconfig.trecs
  let horsat = ref Mconfig.horsat
  let horsat2 = ref Mconfig.horsat2
  let horsatp = ref Mconfig.horsatp

  type model_checker = TRecS | HorSat | HorSat2 | HorSatP
  let mc = ref (if Mconfig.horsat2_available then HorSat2 else if Mconfig.horsat_available then HorSat else TRecS)

  let church_encode = ref false
  let beta_reduce = false
  let useless_elim = false
  let rename_hors = ref false
end

module PrettyPrinter = struct
  let () = Format.set_margin 120
  let color = ref false
  let color_always = ref false
  let write_annot = ref true
  let web = ref false
end

module Termination = struct
  let disjunctive = ref false
  let separate_pred = ref false
  let split_callsite = ref false
  let add_closure_depth = ref false
  let add_closure_exparam = ref false
end

module NonTermination = struct
  let merge_paths_of_same_branch = ref false
  let randint_refinement_log = ref false
  let use_omega = ref true
  let use_omega_first = ref false
end

module FairTermination = struct
  let expand_set_flag = ref false
  let loop_count = ref 0
end

module FairNonTermination = struct
  let expand_ce_iter_init = ref 5
  let break_expansion_ref = ref false
end

module Modular = struct
  let use_ref_typ_gen_method_in_esop2017 = ref false
  let infer_ind = ref false
  let refine_init = ref false
  let use_neg_env = ref true
  let infer_merge = ref false
  let check_simple = ref false
end

module Debug = struct
  let check_fun_arg_typ = false
  let check_typ = true
  let debuggable_modules : string list ref = ref []
  let debug_module : string list ref = ref []
  let abst = ref false

  let print_ref_typ () = List.mem "Ref_type" !debug_module
  let make_check s =
    debuggable_modules := s::!debuggable_modules;
    fun () -> List.mem s !debug_module
  let set_debug_modules mods =
    let modules = BatString.nsplit mods "," in
    let check m =
      if not @@ List.mem m !debuggable_modules then
        (Format.printf "Module \"%s\" is not registered for debug@." m;
         exit 1)
    in
    List.iter check modules;
    debug_module := modules @ !debug_module
end
