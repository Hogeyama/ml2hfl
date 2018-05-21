open Util
open Combinator
open HCCSSolver

(** A dag HCCS solver based on partitioning into and-or trees *)

let solve_aotrees aotree_solver lbs = fun sol (root, hcs) ->
  Logger.printf3
    "solving %a@,hcs:@,  %a@,current solution:@,  %a@,"
    (Option.pr Idnt.pr "bot") root HCCS.pr hcs PredSubst.pr sol;
  let comp_hcs =
    let pvs_undef = hcs |> HCCS.undefined_pvs |> List.unique in
    List.concat_map
      (fun (p, (tenv, phi)) ->
         if List.mem p pvs_undef then
           phi
           |> DNF.of_formula
           |> DNF.disjunction_of
           |> List.map (HornClause.mk_def (PredVar.make p tenv) [])
         else [])
      lbs
  in
  let sol' = PredSubst.restrict sol (Option.fold [] List.return root) in
  Format.printf "  subtituting the root with:@.    %a@."  PredSubst.pr sol';
  hcs
  |> HCCS.substH sol'
  |> HCCS.conj_hccs_of (* @todo the result may be non-body-disjoint *)
  |> (@) comp_hcs
  |> try_
    (aotree_solver >> (@) sol >> PredSubst.merge_and)
    (function
      | HCCSSolver.Unknown | HCCSSolver.NoSolution ->
        fun hcs ->
          Logger.printf0
            "need to expand shared predicate variables at leaves@,";
          let hcs, inv_map = hcs |> HCCS.expand_dag in
          hcs
          |> aotree_solver
          |> List.map (Pair.map_fst (fun p -> List.assocD p p inv_map))
          |> (@) sol
          |> PredSubst.merge_and
      | e -> raise e)

let solve aotree_solver hcs0 =
  let lbs = FwHCCSSolver.solve hcs0 in
  (*Format.printf "lower bounds: %a@." PredSubst.pr lbs;*)
  hcs0
  |> HCCS.tree_partition_of
  |> sef (List.length >> Format.printf "decomposed into %d body-disjoint HCCSs@.")
  |> List.fold_lefti
    (fun i sol (root, hcs) ->
       Format.printf
         "solving %a HCCS:@.  %a@."
         Ordinal.pr (Ordinal.make (i + 1))
         HCCS.pr hcs;
       solve_aotrees aotree_solver lbs sol (root, hcs))
    []
(*let solve aotree_solver = CheckHCCSSolver.solve (solve aotree_solver)*)
let solve =
  Logger.log_block2 "PartHCCSSolver.solve"
    ~before:(fun _ hcs ->
        if HCCS.is_tree hcs then Logger.printf0 "non-DAG@,"
        else Logger.printf
            "dag nodes: %a@,"
            (List.pr Idnt.pr ",") (HCCS.mpvs hcs))
    ~after:(Logger.printf "solution: %a@," PredSubst.pr)
    solve
