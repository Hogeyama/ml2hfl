open Util
open Combinator
open HCCSSolver

(** An HCCS solver based on counterexample guided decomposition *)

(* @todo require and-or tree? *)
(* @todo set num_conj and num_disj to 1 *)

let use_unsat_core = ref false

exception Cex of Formula.t

let pred_of_psols (tenv, reachable, hcss) psols pid =
  let mask =
    let dep_pids =
      List.assocF ~on_failure:(fun () ->
          Format.printf "node %a is not found@," Idnt.pr pid)
        (Some pid) reachable
      |> List.map (function Some pid -> pid | None -> assert false)
    in
    assert (List.mem pid dep_pids);
    hcss
    |> List.map (function
        | [] -> assert false
        | hc :: _ -> hc |> HornClause.nodeH
                     |> Option.fold false (flip List.mem dep_pids))
  in
  let pv =
    List.assoc_fail
      ~on_failure:(fun () ->
          Format.printf "DecoHCCSSolver.pred_of_psols:@,";
          Format.printf "%a is not found@," Idnt.pr pid;
          Format.printf "tenv: %a@," TypEnv.pr tenv)
      pid tenv
    |> PredVar.of_type pid
  in
  let pred =
    psols
    |> List.classify
      (Pair.lift
         (fst >> List.filter_map2
            (function true -> Option.return | false -> const None) mask)
       >> uncurry2 (=)
       |> curry2)
    |> (List.map
          (List.map
             (fun (_, psol) ->
                pv |> Pva.of_pvar |> PredSubst.lookup psol
                |> FormulaSimplifier.simplify)
           >> Formula.band)
        >> Formula.bor)
  in
  (pid, (PredVar.args_of pv, pred))

let sol_of_psols (tenv, reachable, hcss) psols =
  reachable
  |> List.filter_map fst
  |> List.map (pred_of_psols (tenv, reachable, hcss) psols)

let sol_of_dec (ht, tenv, reachable) hcss =
  Vector.producti (fun i xss ->
      Format.printf "  solving %a HCCS:@." Ordinal.pr (Ordinal.make (i + 1));
      try
        (* check if a solution for the decomposition is already computed *)
        let sol = Hashtbl.find ht xss in
        Format.printf "    solution reused@.@.";
        xss, sol
      with Not_found ->
      try
        let sol =
          if !use_unsat_core then
            List.concat_map2i (fun i hcs xs ->
                List.map (fun j ->
                    Partition.label_of_ij i j |> Idnt.string_of,
                    List.nth hcs j)
                  xs)
              hcss xss
            |> (sef
                  (List.map snd
                   >> (Format.printf
                         "    less head-joining HCCS:@.      %a@."
                         HCCS.pr)))
            |> AtomHCCSSolver.solve_labeled
          else
            List.concat_map2 (List.nth >> List.map) hcss xss
            |> (sef
                  (Format.printf
                     "    less head-joining HCCS:@.      %a@."
                     HCCS.pr))
            |> AtomHCCSSolver.solve
        in
        sol
        |> (sef
              (Format.printf
                 "    atomic solution found:@.      %a@."
                 PredSubst.pr))
        |> Hashtbl.add ht xss;
        xss, sol
      with
      | HCCSSolver.UnsolvableCore labels ->
        let phi =
          labels
          |> List.map Partition.ij_of_label
          |> List.classify (comp2 (=) fst fst)
          |> List.map (fun xs -> List.hd xs |> fst, List.map snd xs)
          |> flip (List.assocD [])
          |> List.gen (List.length hcss)
          |> Partition.refute_constr_of
        in
        let labels = List.map Idnt.make labels in
        let ucore = 
          List.concat_mapi (fun i ->
              List.filteri (fun j hc ->
                  List.mem (Partition.label_of_ij i j) labels))
            hcss
        in
        Format.printf "    atomic solution not found!@.";
        Format.printf "    unsat core:@.      %a@." HCCS.pr ucore;
        Format.printf "    add decomposition constraint: %a@." Formula.pr phi;
        raise (Cex phi)
      | HCCSSolver.NoSolution | HCCSSolver.Unknown ->
        let phi = Partition.refute_constr_of xss in
        Format.printf "    atomic solution not found!@.";
        Format.printf "    add decomposition constraint: %a@." Formula.pr phi;
        raise (Cex phi))
  >> sol_of_psols (tenv, reachable, hcss)

let rec solve (ht, tenv, reachable) hcss phi =
    match Partition.find_decomp hcss phi with
    | None -> raise HCCSSolver.NoSolution
    | Some dec ->
      Format.printf
        "  decomposed into %d less head-joining HCCSs: %a@."
        (dec |> List.map List.length |> Integer.prod_list)
        (List.pr_ocaml (List.pr_ocaml (List.pr_ocaml Integer.pr))) dec;
      try sol_of_dec (ht, tenv, reachable) hcss dec
      with Cex c -> solve (ht, tenv, reachable) hcss (Formula.mk_and phi c)
let solve hcs =
  let hcss =
    hcs
    |> HCCS.conj_hccs_of
    |> sef (Format.printf "  body-disjoint HCCS:@.    %a@." HCCS.pr)
    |> List.classify HornClause.eq_shapeH
  in
  let ht =
    hcss |> List.map List.length |> Integer.prod_list |> Hashtbl.create
  in
  solve (ht, HCCS.tenv hcs, HCCS.reachable hcs) hcss Formula.mk_true
let solve =
  Logger.log_block1 "DecoHCCSSolver.solve"
    ~after:(Logger.printf "solution: %a@," PredSubst.pr)
    solve
