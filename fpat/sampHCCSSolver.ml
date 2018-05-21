open Util
open Combinator
open HCCSSolver

(** An HCCS solver based on counterexample guided sampling *)

exception Cex of int * int

let rec solve solver samples rest =
  Format.printf "samples:@.  %a@." HCCS.pr (List.concat samples);
  let sol = solver (List.concat samples) in
  Format.printf "candidate solution:@.  %a@." PredSubst.pr sol;
  try
    List.iteri (fun i hcs ->
        List.iteri (fun j hc ->
            if HornClause.is_solution hc sol then () else raise (Cex(i, j)))
          hcs)
      rest;
    Format.printf "genuine!@.";
    sol
  with Cex(i, j) ->
    List.zip samples rest
    |> List.mapi
      (fun k (hcs1, hcs2) ->
         if i = k then
           let lhcs, hc :: rhcs = List.split_at j hcs2 in
           Format.printf
             "a Horn counterexample found:@.  @[<v>%a@]@.@."
             HornClause.pr hc;
           hc :: hcs1, rhcs @ lhcs
         else hcs1, hcs2)
    |> List.unzip
    |> uncurry2 (solve solver)
let solve solver hcs =
  hcs
  (* what we really need here is purification of boolean and integer atoms
     =b and <>b are not always problematic *)
  |> sef (fun _ ->
      Format.printf "#### polytope sampling based HCCS solver started ####@.")
  |> HCCS.conj_hccs_of
  |> sef (Format.printf "recursion-free HCCS:@.  %a@.@." HCCS.pr)
  |> HCCS.map_pred (fun (hc :: hcs) -> [hc], hcs)
  |> List.unzip
  |> uncurry2 (solve solver)
  |> sef (fun _ ->
      Format.printf "#### polytope sampling based HCCS solver stopped ####@.@.")
let solve solver = solve solver |> ComplementHCCSSolver.solve
let solve = Logger.log_block2 "SampHCCSSolver.solve" solve



let solve_sbe hcs =
  let hcs_enc = hcs |> EncBoolHCCSSolver.encode false in
  HCCS.save_smtlib
    (Filename.chop_extension !Global.target_filename
     ^ "_hccs" ^ string_of_int !Global.cegar_iterations ^ ".smt")
    hcs_enc;
  HCCS.save_graphviz
    (Filename.chop_extension !Global.target_filename
     ^ "_hccs" ^ string_of_int !Global.cegar_iterations ^ ".dot")
    hcs_enc;
  let hcs_expanded = HCCS.expand_dag hcs_enc |> fst in
  HCCS.save_smtlib
    (Filename.chop_extension !Global.target_filename
     ^ "_hccs" ^ string_of_int !Global.cegar_iterations ^ "_expanded.smt")
    hcs_expanded;
  HCCS.save_graphviz
    (Filename.chop_extension !Global.target_filename
     ^ "_hccs" ^ string_of_int !Global.cegar_iterations ^ "_expanded.dot")
    hcs_expanded;
  Format.printf
    "@[<v># of constrs.: %a@,# of pred. vars.: %a@,@]"
    Integer.pr (List.length hcs_expanded)
    Integer.pr (HCCS.num_pvs hcs_expanded);

  let tm_sb, (sol_sb, size_sb) =
    Timer.block
      (fun _ -> ())
      (fun _ -> ())
      (fun () ->
         let sol =
           (DecoHCCSSolver.solve |> PartHCCSSolver.solve |> solve) hcs
         in
         sol, PredSubst.size sol)
  in
  Logger.printf2 "sol_sb(%a): %a@," Integer.pr size_sb PredSubst.pr sol_sb;
  let tm_it, (sol_it, size_it) =
    try
      Timer.block
        ~timeout:100
        (fun _ -> ())
        (fun _ -> ())
        (fun () ->
           try
             let sol = BwIPHCCSSolver.solve hcs in
             sol, PredSubst.size sol
           with HCCSSolver.NoSolution | HCCSSolver.Unknown ->
             assert false(*[], -1*))
    with
    | Timer.Timeout -> 100.0, ([], 0)
    | e -> Format.printf "%s@," (Printexc.to_string e);
      assert false(*100.0, ([], -2)*)
  in
  Logger.printf2 "sol_it(%a): %a@," Integer.pr size_it PredSubst.pr sol_it;
  let tm_dex, (sol_dex, size_dex) =
    try
      Timer.block
        ~timeout:100
        (fun _ -> ())
        (fun _ -> ())
        (fun () ->
           try
             let sol =
               (BwIPHCCSSolver.solve |> ExpandDagHCCSSolver.solve) hcs
             in
             sol, PredSubst.size sol
           with HCCSSolver.NoSolution | HCCSSolver.Unknown ->
             assert false(*[], -1*))
    with
    | Timer.Timeout -> 100.0, ([], 0)
    | e -> Format.printf "%s@," (Printexc.to_string e);
      assert false(*100.0, ([], -2)*)
  in
  Logger.printf2 "sol_dex(%a): %a@," Integer.pr size_dex PredSubst.pr sol_dex;

  let name =
    Filename.chop_extension (Filename.basename !Global.target_filename)
    ^ "_" ^ string_of_int !Global.cegar_iterations
  in
  let oc = open_out_gen [Open_append; Open_creat] 0o666 "exp_samp.csv" in 
  let ocf = Format.make_formatter (output oc) (fun () -> flush oc) in
  Format.fprintf ocf "@[<v>";
  Format.fprintf
    ocf
    "%s,%f,%d,%f,%d,%f,%d,%a@,"
    name tm_sb size_sb tm_it size_it tm_dex size_dex
    Bool.pr_yn (hcs |> HCCS.conj_hccs_of |> HCCS.is_non_disjunctive |> not);
  Format.fprintf ocf "@]@?";
  close_out oc;

  sol_sb
