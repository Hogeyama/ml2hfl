open Util
open Combinator

(** REPL *)

let parse str =
  let lexbuf = Lexing.from_string str in
  HCCSParser.parser_main HCCSLexer.token lexbuf

let parse_file inchan =
  let lexbuf = Lexing.from_channel inchan in
  HCCSParser.parser_main HCCSLexer.token lexbuf

let main () =
  print_endline "Parsing test";
  SMTProver.init_cvc3 ();
  SMTProver.initialize ();
  InterpProver.link_dyn InterpProver.interpolate_csisat_dyn;
  let hcs = ref [] in
  let rec loop () =
    print_string "> ";
    (try
       let str = read_line () in
       begin
         match str with
         | "quit" | ":q" | "exit" -> SMTProver.finalize ()
         | "clear" -> hcs := []; loop()
         | "show" -> print_endline "==== Show ====";
           Format.printf "@[%a\n@]@?" HCCS.pr !hcs; loop()
         | "hg" -> print_endline "==== Transform Hcs into Hg ====";
           let hg = FwWidenHCCSSolver.hg_of !hcs in
           Format.printf "@[%a\n@]@?" FwWidenHCCSSolver.pr hg; loop()
         | "normalize" ->
           Format.printf "@[%a\n@]@?" HCCS.pr (HCCS.normalize2 !hcs); loop()
         | "solve" ->
           (try
              print_endline "===== Solve =====";
              let _ = BwIPHCCSSolver.solve !hcs in
              ()
            with
            | _ -> Printf.printf "\nError.\n");
           loop ()
         | "solve_pepm" ->
           (try
              print_endline "===== Solve =====";
              let _ = GenHCCSSolver.solve (CHGenInterpProver.interpolate true) !hcs in
              ()
            with
            | _ -> Printf.printf "\nError.\n");
           loop ()

         | "fw+w" ->
           (try
              print_endline "===== Solve with Forward Fixedpoint computation with Widening =====";
              let _ = FwWidenHCCSSolver.solve !hcs in
              ()
            with
            | _ -> Printf.printf "\nError.\n");
           loop ()
         | "fw" ->
           (try
              print_endline "===== Solve with Forward Fixedpoint computation =====";
              let _ = FwWidenHCCSSolver.solve ~wide:false !hcs in
              ()
            with
            | _ -> Printf.printf "\nError.\n");
           loop ()

         | "load" -> (* load file *)
           Printf.printf "Load File: ";
           let path = read_line () in
           let inchan = open_in path in
           (try
              hcs := parse_file inchan;
              Format.printf "@[%a\n@]@?" HCCS.pr !hcs;
            with
            | End_of_file -> close_in inchan
            | Failure message ->
              close_in inchan;
              Printf.printf "Error: %s\n" message);
           loop()

         | "r" -> (* make R *)
           (try
              print_endline "===== Solve with Relational analysis =====";
              let _ =
                !hcs |>
                NegaHcSolver.solve |>
                Format.printf "@[test : %a\n@]@?" PredSubst.pr
              in ()
            with
            | _ -> Printf.printf "\nError.\n");
           loop()

         | "ml" | "ocaml" -> (* ocaml mode *)
           print_endline "===== OCaml Mode ======";
           let rec mlloop () =
             print_string "# ";
             match read_line () with
             | "quit" | ":q" | "exit" -> SMTProver.finalize ()
             | _ as str ->
               (try
                  let print s a =
                    Format.printf "%s:@[ %a\n@]@?" s MLExp.pr a; a
                  in
                  let [(t, tys)] = OCamlParser.from_string str in
                  Format.printf "restored:@.";
                  List.iter (fun (id,ty) -> Format.printf "@[<hov>%a : %a@]@." Idnt.pr id Type.pr ty) tys;
                  Format.printf "term:@[<hov>%a@]@." MLExp.pr t;
                  let fdefs = Fdef.of_mlexp t in
                  Format.printf "@[%a\n@]@?" (List.pr Fdef.pr ",") fdefs;
                  let tcenv,hccs = SimTypInfer.infer_hccs [] [] in (* dataEnv * hccs *)
                  Format.printf "@[infer_hccs: {%a}\n@]@?@." HCCS.pr hccs;    

                  let tenv = RefTypEnv.of_tenv_with_template tys in
                  Format.printf "@[%a\n@]@?" RefTypEnv.pr tenv;

                  ignore (List.map (RefTypCheck.tcheck_fdef [] (Prog.make fdefs [] ""(*@todo*)) tcenv tenv) fdefs);
                  let hccs =
                    !RefTypCheck.rev_consts
                    |> List.map snd
                    |> List.rev_map (fun phi -> HCCS.of_formula0 (Formula.fvs phi) phi)
                    |> List.concat
                    |> HCCS.simplify_full []
                    |> SimTypInfer.infer_hccs tcenv
                    |> snd
                  in
                  Format.printf "@[%a\n@]@?" HCCS.pr hccs
                with
                | Failure message ->
                  Format.printf "@.@[<hov>Error: %s@]@." message
                |  _ as e ->
                  Format.printf "@[<hov>Exception: %s@]@." (e |> Printexc.to_string));
               mlloop ()
           in
           mlloop ()
         | _ -> (* input hornclause *)
           (try
              let hc = parse str in
              hcs := hc @ !hcs;
              Format.printf "@[<hov>%a\n@]@." HCCS.pr hc;
            with
            | Failure message ->
              Format.printf "@[<hov>Error: %s@]@." message);
           loop ()
       end
     with
     | End_of_file -> SMTProver.finalize ())
  in
  loop ()
