
open Utilities

exception TimeOut
exception LongInput
exception CannotDiscoverPredicate

let () =
  if Flag.for_paper
  then
    begin
      Flag.only_result := true;
      Flag.debug_level := 0;
      Flag.print_progress := false
    end


let print_info () =
  if Flag.for_paper
  then Format.printf " %2d, %7.2f  &@?" !Flag.cegar_loop (get_time())
  else
    begin
      Format.printf "cycle: %d\n" !Flag.cegar_loop;
      Format.printf "total: %.3f sec\n" (get_time());
      Format.printf "  abst: %.3f sec\n" !Flag.time_abstraction;
      Format.printf "  mc: %.3f sec\n" !Flag.time_mc;
      Format.printf "  refine: %.3f sec\n" !Flag.time_cegar;
      if false && Flag.debug then Format.printf "IP: %.3f sec\n" !Flag.time_interpolant;
      Format.printf "    exparam: %.3f sec\n" !Flag.time_parameter_inference;
      Format.pp_print_flush Format.std_formatter ()
    end



let spec_file = ref ""

let init () =
  Id.clear_counter ();
  Syntax.typ_excep := Type.TConstr("exn",true)

let preprocess t spec =
  let fun_list,t,get_rtyp =
    if !Flag.init_trans
    then
      let t' = Trans.copy_poly_funs t in
      let fun_list = Syntax.get_top_funs t' in
      let () =
        if true && !Flag.debug_level > 0 && t <> t'
        then Format.printf "copy_poly::@. @[%a@.@." Syntax.pp_print_term_typ t' in
      let t = t' in
      let spec' = Spec.rename spec t in
      let () = Spec.print spec' in
      let t' = Trans.replace_typ spec'.Spec.abst_env t in
      let () =
        if true && !Flag.debug_level > 0 && spec <> Spec.init
        then Format.printf "add_preds::@. @[%a@.@." Syntax.pp_print_term_typ t' in
      let t = t' in
      let t' = Abstract.abstract_recdata t in
      let () =
        if true && !Flag.debug_level > 0 && t <> t'
        then Format.printf "abst_recdata::@. @[%a@.@." Syntax.pp_print_term t' in
      let t = t' in
      let t',get_rtyp_list = Abstract.abstract_list t in
      let () =
        if true && !Flag.debug_level > 0 && t <> t'
        then Format.printf "abst_list::@. @[%a@.@." Syntax.pp_print_term t' in
      let t = t' in
      let get_rtyp = get_rtyp_list in
      let t' = Trans.inlined_f spec'.Spec.inlined_f t in
      let () =
        if true && !Flag.debug_level > 0 && t <> t'
        then Format.printf "inlined::@. @[%a@.@." Syntax.pp_print_term_typ t' in
      let t = t' in
      let t =
	if (match !Flag.refine with Flag.RefineRefType(_) -> true | _ -> false) && !Flag.relative_complete then
	  let t = Trans.lift_fst_snd t in
	  let t = VhornInterface.insert_extra_param t in (* THERE IS A BUG *)
	    if true && !Flag.debug_level > 0 then Format.printf "insert_extra_param (%d added)::@. @[%a@.@.%a@.@."
	      (List.length !VhornInterface.params) Syntax.pp_print_term t Syntax.pp_print_term' t;
	    t
	else
	  t
      in
      let t',get_rtyp_cps_trans = CPS.trans t in
      let () =
        if true && !Flag.debug_level > 0 && t <> t'
        then Format.printf "CPS::@. @[%a@.@." Syntax.pp_print_term_typ t' in
      let t = t' in
      let get_rtyp f typ = get_rtyp f (get_rtyp_cps_trans f typ) in
      let t',get_rtyp_remove_pair = CPS.remove_pair t in
      let () =
        if true && !Flag.debug_level > 0 && t <> t'
        then Format.printf "remove_pair::@. @[%a@.@." Syntax.pp_print_term t' in
      let get_rtyp f typ = get_rtyp f (get_rtyp_remove_pair f typ) in
      let t = t' in
      let t' = if !Flag.insert_param_funarg then Trans.insert_param_funarg t else t in
      let () =
        if !Flag.debug_level > 0 && t <> t'
        then Format.printf "insert unit param::@. @[%a@.@." Syntax.pp_print_term t'
      in
	fun_list, t', get_rtyp
    else Syntax.get_top_funs t, t, fun _ typ -> typ
  in

  let () = Type_check.check t Type.TUnit in
  let prog,map,rmap,get_rtyp_trans = CEGAR_util.trans_prog t in
  let get_rtyp f typ = get_rtyp f (get_rtyp_trans f typ) in

  let info =
    let fun_list =
      let aux x =
        try [List.assoc (CEGAR_util.trans_var x) map] with Not_found -> []
      in
        rev_flatten_map aux fun_list
    in
    let inlined = List.map CEGAR_util.trans_var spec.Spec.inlined in
      {CEGAR.orig_fun_list=fun_list; CEGAR.inlined=inlined}
  in
    prog, rmap, get_rtyp, info

let rec main_loop orig parsed =
  let () = init () in
  let t = parsed in
  let spec = Spec.parse Spec_parser.spec Spec_lexer.token !spec_file in
  let () = Spec.print spec in
  let main_fun,arg_num,t = if !Flag.cegar = Flag.CEGAR_DependentType then Trans.set_target t else "",0,t in
  let set_target = t in
  let () =
    if true && !Flag.debug_level > 0
    then Format.printf "set_target::@. @[%a@.@." Syntax.pp_print_term t
  in
  let prog, rmap, get_rtyp, info = preprocess t spec in
    match !Flag.cegar with
        Flag.CEGAR_InteractionType ->
          VhornInterface.verify [] prog
      | Flag.CEGAR_DependentType ->
          try
            match CEGAR.cegar prog info with
                prog', CEGAR.Safe env ->
                  let env' =
                    let aux (f,rtyp) : (Syntax.id * Ref_type.t) list =
                      try
                        let f' = List.assoc f rmap in
                          [f', Ref_type.rename (get_rtyp f' rtyp)]
                      with
                          Not_found -> []
                        | _ -> Format.printf "unimplemented or bug@.@."; []
                    in
                      rev_map_flatten aux env
                  in
                  let () =
                    if !Flag.write_annot
                    then
                      let env'' = List.map (fun (id, typ) -> Id.name id, typ) env' in
                        WriteAnnot.f !Flag.filename orig env''
                  in
                  let pr (f,typ) =
                    Format.printf "%s: %a@." (Id.name f) Ref_type.print typ
                  in
                    if not Flag.for_paper
                    then
                      begin
                        Format.printf "Safe!@.@.";
                        if env' <> [] then Format.printf "Refinement Types:@.";
                        List.iter pr env';
                        if env' <> [] then Format.printf "@."
                      end
              | _, CEGAR.Unsafe ce ->
                  if not Flag.for_paper
                  then
                    begin
                      Format.printf "Unsafe!@.@.";
                      if main_fun <> ""
                      then
                        Format.printf "Input for %s:@.  %a@." main_fun
                          (print_list Format.pp_print_int "; " false) (take ce arg_num);
                      Format.printf "@[<v 2>Error trace:%a@."  Eval.print (ce,set_target)
                    end
          with
              AbsTypeInfer.FailedToRefineTypes when not !Flag.relative_complete ->
                Format.printf "@.REFINEMENT FAILED!@.";
                Format.printf "Restart with relative_complete := true@.@.";
                Flag.relative_complete := true;
                incr Flag.cegar_loop;
                main_loop orig parsed
            | AbsTypeInfer.FailedToRefineTypes
            | Assert_failure("hcSolve.ml", 329, 12) when not !Flag.insert_param_funarg ->
                Flag.insert_param_funarg := true;
                incr Flag.cegar_loop;
                main_loop orig parsed
            | AbsTypeInfer.FailedToRefineTypes ->
                raise AbsTypeInfer.FailedToRefineTypes;
            | ParamSubstInfer.FailedToRefineExtraParameters ->
                VhornInterface.params := [];
                ParamSubstInfer.ext_coeffs := [];
                ParamSubstInfer.ext_constrs := [];
                incr Global.number_of_extra_params;
                incr Flag.cegar_loop;
                main_loop orig parsed


let main in_channel =
  let input_string =
    let s = String.create Flag.max_input_size in
    let n = my_input in_channel s 0 Flag.max_input_size in
      if n = Flag.max_input_size then raise LongInput;
      String.sub s 0 n
  in

  let lb = Lexing.from_string input_string in
  let () = lb.Lexing.lex_curr_p <-
    {Lexing.pos_fname = Filename.basename !Flag.filename;
     Lexing.pos_lnum = 1;
     Lexing.pos_cnum = 0;
     Lexing.pos_bol = 0};
  in
  let orig = Parse.use_file lb in
  let parsed = Parser_wrapper.from_use_file orig in
  let () =
    if true && !Flag.debug_level > 0
    then Format.printf "parsed::@. @[%a@.@." Syntax.pp_print_term parsed
  in
    main_loop orig parsed


let usage =  "Usage: " ^ Sys.executable_name ^ " [options] file\noptions are:"
let arg_spec =
  ["-I", Arg.String (fun dir -> Config.load_path := dir::!Config.load_path),
         "<dir>  Add <dir> to the list of include directories";
   "-margin", Arg.Int Format.set_margin, "<n>  Set pretty printing margin";
   "-only-result",
     Arg.Unit (fun () ->
                 Flag.only_result := true;
                 Flag.debug_level := 0;
                 Flag.print_progress := false),
     " Show only result";
   (* preprocessing *)
   "-na", Arg.Clear Flag.init_trans, " Disable encoding of recursive data structures";
   "-lift-fv", Arg.Unit (fun _ -> Flag.lift_fv_only := true), " Lift variables which occur in a body";
   "-cps-naive", Arg.Set Flag.cps_simpl, " Use naive CPS transformation";
   "-ins-param-funarg", Arg.Set Flag.insert_param_funarg, " Insert an extra param for functions with function arguments";
   (* verifier *)
   "-it", Arg.Unit (fun _ -> Flag.cegar := Flag.CEGAR_InteractionType), " Interaction type based verifier";
   "-spec", Arg.String (fun file -> spec_file := file), "<filename>  use <filename> as a specification";
   (* CEGAR *)
   "-dpa", Arg.Set Flag.disable_predicate_accumulation, " Disable predicate accumulation";
   "-no-enr", Arg.Clear Flag.expand_nonrec, " Do not expand non-recursive functions";
   "-enr", Arg.Set Flag.expand_nonrec, " Expand non-recursive functions";
   "-enr2", Arg.Unit (fun _ -> Flag.expand_nonrec := true; Flag.expand_nonrec_init := false),
            " Expand non-recursive functions except those in the original program";
   (* relatively complete verification *)
   "-rc", Arg.Set Flag.relative_complete, " Enable relatively complete verification";
   "-nex", Arg.Int (fun n -> Global.number_of_extra_params := n),
          " Number of inserted extra parameters for each functional argument";
   "-tbit", Arg.Int (fun n -> Global.bits_threshold := n),
          " Threshold on the number of bits used in the bit-vector modeling";
   "-cc", Arg.Set Global.enable_coeff_const,
          " Disable constant terms of extra parameters";
   "-aec", Arg.Set Global.accumulate_ext_constrs,
          " Accumulate constraints on the coefficients of extra parameters";
   "-dph", Arg.Set Global.disable_parameter_inference_heuristics,
          " Disable heuristics of instantiation parameter inference";
   (* predicate abstraction *)
   "-abs-filter", Arg.Set Flag.use_filter, " Turn on the abstraction-filter option";
   "-neg-pred-on", Arg.Set Flag.use_neg_pred, " Use negative predicates for abstraction";
   "-neg-pred-off", Arg.Unit (fun _ -> Flag.use_neg_pred := false; Flag.never_use_neg_pred := true),
                    " Never use negative predicates for abstraction";
   (* higher-order model checking *)
   "-trecs", Arg.String (fun cmd -> Flag.trecs := cmd),
             Format.sprintf "<cmd>  Change trecs command to <cmd> (default: \"%s\")" !Flag.trecs;
   "-old-trecs", Arg.Clear Flag.use_new_trecs, " Use old trecs (temporary option)";
   "-ea", Arg.Unit (fun _ -> Flag.print_eval_abst := true), " Print evaluation of abstacted program";
   (* predicate discovery *)
   "-bool-init-empty", Arg.Set Flag.bool_init_empty,
                      " Use an empty set as the initial sets of predicates for booleans";
   "-rs", Arg.Unit (fun _ -> Flag.refine := Flag.RefineRefType(0)),
          " Use refinement type based predicate discovery (same as -rsn 0)";
   "-rsn", Arg.Int (fun n -> Flag.refine := Flag.RefineRefType(n)),
          "<num>  Use refinement type based predicate discovery";
   "-rd", Arg.Unit (fun _ -> Flag.refine := Flag.RefineRefTypeOld),
          " Use refinement type based predicate discovery (obsolete)";
   "-eap", Arg.Set Global.extract_atomic_predicates, " Extract atomic predicates";
   "-mp", Arg.Set Global.use_multiple_paths, " Use multiple infeasible error paths for predicate discovery";
   "-gi", Arg.Unit (fun _ -> Global.predicate_discovery := Global.GenInterpolation),
     " Generalize constraints of multiple function calls by interpolation";
   "-gchi", Arg.Unit (fun _ -> Global.predicate_discovery := Global.GenConvexHullInterpolation),
     " Generalize constraints of multiple function calls by convex hull and interpolation";
   "-gtc", Arg.Unit (fun _ -> Global.predicate_discovery := Global.GenTemplateBasedConstraintSolving),
     " Generalize constraints of multiple function calls by template-based constraint solving";
   (* interpolating prover *)
   "-yint", Arg.Unit (fun _ -> Global.interp_prover := Global.Yint),
          " Use Yint interpolating prover";
  ]


let () =
  if !Sys.interactive
  then ()
  else
    try
      let set_file name =
        if !Flag.filename <> "" (* case of "./mochi.opt file1 file2" *)
        then (Arg.usage arg_spec usage; exit 1);
        Flag.filename := name
      in
      let () = Arg.parse arg_spec set_file usage in
      let cin =
        match !Flag.filename with
            "" | "-" -> Flag.filename := "stdin"; stdin
          | _ -> open_in !Flag.filename
      in
        Wrapper.open_cvc3 ();
        Wrapper2.open_cvc3 ();
        Cvc3Interface.open_cvc3 ();
        Sys.set_signal Sys.sigalrm
          (Sys.Signal_handle (fun _ ->
                                if Flag.for_paper
                                then Format.printf "           - \\&@?";
                                raise TimeOut));
        ignore (Unix.alarm Flag.time_limit);
        main cin;
        Cvc3Interface.close_cvc3 ();
        Wrapper2.close_cvc3 ();
        Wrapper.close_cvc3 ();
        print_info ()
    with
        Syntaxerr.Error err -> Format.printf "%a@." Syntaxerr.report_error err; exit 1
      | LongInput -> Format.printf "Input is too long@."; exit 1
      | TimeOut -> if not Flag.for_paper then Format.printf "@.Verification failed (time out)@."; exit 1
      | CEGAR.NoProgress -> Format.printf "Verification failed (new error path not found)@."; exit 1
      | AbsTypeInfer.FailedToRefineTypes ->
          Format.printf "Verification failed (cannot refute an error path)@."; exit 1
      | Typecore.Error (_,e) -> Format.printf "%a@." Typecore.report_error e; exit 1
      | Typemod.Error(_,e) -> Format.printf "%a@." Typemod.report_error e; exit 1
      | Env.Error e -> Format.printf "%a@." Env.report_error e; exit 1
      | Typetexp.Error(_,e) -> Format.printf "%a@." Typetexp.report_error e; exit 1
