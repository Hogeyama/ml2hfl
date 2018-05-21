open Fpat
open Combinator
open Util

let parse filename =
  let lexbuf =
    filename
    |> open_in  
    |> Lexing.from_channel
  in
  lexbuf.Lexing.lex_curr_p <-
    { Lexing.pos_fname = filename;
      Lexing.pos_lnum = 1;
      Lexing.pos_bol = 0;
      Lexing.pos_cnum = 0 };
  lexbuf |> MLRefparser.gettag MLReflexer.token

let mk_tagfun tglist =
  fun p i -> List.nth (List.assoc p tglist) i

let rec rem_dup = function
  | [] -> []
  | hc::rest ->
      match HornClause.hpv_of hc with
	None -> hc :: rem_dup rest
      | Some pv -> 
	if
	  [Pva.of_pvar pv] = HornClause.bpvas_of hc
	  && Formula.is_true (HornClause.bphi_of hc)
	then rem_dup rest
	else hc :: rem_dup rest

let () = 
  FPATConfig.set_default ();
  Format.printf "@[<v>";
  Arg.parse
    (Arg.align FPATConfig.arg_spec)
    (fun filename ->
     Global.target_filename := filename;
     SMTProver.open_ ();
     begin
       try
         let tcenv, fdefs, tenv, hccs, tglist = filename |> parse
         in
(*
         Format.printf "Predicates:@,  %a@," HCCS.pr hccs;
         Format.printf "ML types:@,  %a@," TypEnv.pr tcenv;
*)
         Format.printf "Refinement types:@,  %a@,@," RefTypEnv.pr tenv;
         ignore (List.map (RefTypCheck.tcheck_fdef tcenv tenv) fdefs);

         let hccs =
           !RefTypCheck.rev_consts
           |> List.map snd
           |> List.rev_map HCCS.of_formula0
           |> List.concat
           |> (@) hccs
           |> SimTypInfer.infer_hccs tcenv
           |> snd
           |> HCCS.simplify_full []
           (*todo: this function's position*)
           |> rem_dup
           |> (fun hs ->
               List.partition
                 (HornClause.hpv_of >> (<>) None)
                 hs
               |> fun (defs, goals) ->
                  defs
                  @ (HCCS.simplify_lv2 goals
                     |> HCCS.normalize2 ~force:false))
         (* |> HCCS.normalize2 ~force:false *)
         in
         Format.printf "Constraints (HCCS):@,  %a@," HCCS.pr hccs;
         let hccs' = RewriteHCCS.algo_solve hccs (mk_tagfun tglist)
	 (* (*mult-dist1:*) |> List.drop 4 *)
	 in
         Format.printf "Transformed Constraints (HCCS):@,  %a@," HCCS.pr hccs';
(*
	 let hccs'' = !RHCCSSolver.ref_solver hccs' 1 in
	 Format.printf "expand1:@,  %a@," HCCS.pr hccs'';

	 let hccs''' = !RHCCSSolver.ref_solver hccs' 2 in
	 Format.printf "expand2:@,  %a@," HCCS.pr hccs''';
*)
         Format.printf "@]@.@[<v>";
         !RHCCSSolver.ref_solver hccs'
	 (* BwQEHCCSSolver.solve hccs' *)
	 (* BwIPHCCSSolver.solve hccs' *)
         (* FwWidenHCCSSolver.solve ~wide:true hccs' *)
         |> (fun (sol, _) ->
           Format.printf "@]@[<v>Solution:@,  %a@," PredSubst.pr sol;
           let tenv' = RefTypEnv.subst_pvars sol tenv in
           Format.printf "Inferred types:@, %a@," RefTypEnv.pr tenv')
       with
       | e ->
          SMTProver.close ();
          Format.printf "@]@.";
          raise e
     end;
     SMTProver.close ())
    FPATConfig.usage;
  Format.printf "@]@."
