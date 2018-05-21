open Util
open Combinator
open HCCSSolver

(** A constraint solver for dag HCCS based on beautiful dag interpolation *)

type s = (HornClause.t * int list) list

let node_of (sample : s) =
  sample
  |> List.hd
  |> fst
  |> HornClause.nodeH

let partition_of sample =
  sample
  |> List.map snd
  |> List.unique
  |> List.filter ((<>) [])

let construct_sol (tenv, reachable, _) (samples, sols, max) =
  reachable
  |> List.filter_map fst
  |> List.map
    (fun pid ->
       let pv =
         PredVar.of_type
           pid
           (try List.assoc pid tenv
            with Not_found ->
              Format.printf "%a@," Idnt.pr pid;
              assert false)
       in
       let dep_pids =
         reachable
         |> List.assoc_fail (Some(pid))
         |> List.map (function Some(pid) -> pid | None -> assert false)
       in
       assert (List.mem pid dep_pids);
       samples
       |> List.filter_map
         (fun sample ->
            match node_of sample with
            | None -> None
            | Some(pid) ->
              if List.mem pid dep_pids then
                sample |> partition_of |> Option.some
              else None)
       |> List.fold_left Partition.meet (Partition.top (List.from_to 0 max))
       |> List.map
         (List.map
            (fun n ->
               pv
               |> Pva.of_pvar
               |> PredSubst.lookup (List.nth sols n)
               |> FormulaSimplifier.simplify)
          >> Formula.band)
       |> Formula.bor
       |> fun phi -> pid, (PredVar.args_of pv, phi))
let construct_sol =
  Logger.log_block2
    "BeautifulHCCSSolver.construct_sol"
    ~before:(fun _ (_, sols, max) ->
        Logger.printf2
          "partial solutions [%a]:@,  %a@,"
          Integer.pr max
          (List.pr PredSubst.pr "@,@,") sols)
    ~after:(Logger.printf "candidate solution: %a@," PredSubst.pr)
    construct_sol

let hcs_of samples n =
  samples
  |> List.concat_map
    (List.filter_map
       (fun (hc, ns) -> if List.mem n ns then Some(hc) else None))

let partSol hcs0 samples n =
  let hcs = hcs_of samples n @ hcs0 in
  try AtomHCCSSolver.solve hcs
  with
  | HCCSSolver.NoSolution ->
    Logger.printf0 "no solution found@,";
    raise Not_found
  | HCCSSolver.Unknown ->
    (*@todo*)
    Logger.printf0 "no solution found (unknown)@,";
    raise Not_found
let partSol =
  Logger.log_block3
    "BeautifulHCCSSolver.partSol"
    ~after:(Logger.printf "partial solution: %a@," PredSubst.pr)
    partSol

let replace_elem i ns sample =
  sample
  |> List.mapi
    (fun i' (hc, ns') ->
       if i = i' then
         begin
           assert (ns' = []);
           (hc, ns)
         end
       else (hc, ns'))
let replace_elem =
  Logger.log_block3
    "BeautifulHCCSSolver.replace_elem"
    ~before:(fun i ns sample ->
        Logger.printf3
          "replacing %a clause of %a to %a@,"
          Ordinal.pr (Ordinal.make i)
          (Option.pr Idnt.pr "none") (node_of sample)
          (List.pr Integer.pr ",") ns)
    replace_elem

let replace node i ns samples =
  samples
  |> List.map
    (fun sample ->
       if node_of sample = node
       then replace_elem i ns sample
       else sample)

let find_partition_of node samples =
  samples
  |> try_
    (List.find_map
       (fun sample ->
          if node_of sample = node
          then Some(partition_of sample)
          else None))
    (fun _ _ -> assert false)

let try_update_state_with hcs0 node i (samples, sols, max) ns =
  try
    let samples' = replace node i ns samples in
    Triple.make samples'
      (List.mapi
         (fun i s -> if List.mem i ns then partSol hcs0 samples' i else s)
         sols)
      max
    |> Option.some
  with Not_found -> None

let print_samples samples =
  samples
  |> List.iter
    (List.iter
       (fun (hc, ns) ->
          Logger.printf2
            "hc: %a@,ns: %a@,"
            HornClause.pr hc
            (List.pr Integer.pr ";") ns))

exception FallBack
let refine hcs0 node i (samples, sols, max) =
  try
    samples
    |> find_partition_of node
    |> List.find_map (try_update_state_with hcs0 node i (samples, sols, max))
  with Not_found ->
  try
    let ms =
      samples
      |> List.map
        (fun sample ->
           if node_of sample = node
           then 1
           else sample |> partition_of |> List.length)
    in
    let max' = max + Integer.prod_list ms in
    let ns = List.from_to (max + 1) max' in
    Logger.printf3
      "adding %a to %a clause of %a@,"
      (List.pr Integer.pr ",") ns
      Ordinal.pr (Ordinal.make i)
      (Option.pr Idnt.pr "none") node;
    let samples' =
      samples
      |> List.mapi
        (fun j sample ->
           if node_of sample = node then
             (* i-th elem. is now [] and is updated here to ns *)
             replace_elem i ns sample
           else
             let num_of =
               let nss = partition_of sample in
               fun ns ->
                 nss
                 |> List.findi (fun _ ns' -> Set_.equiv ns ns')
                 |> fst
             in
             sample
             |> List.map
               (fun (hc, ns) ->
                  hc,
                  try
                    let k = num_of ns in
                    ms
                    |> List.mapi
                      (fun i m ->
                         if i = j then [k] else List.from_to 0 (m - 1))
                    |> Vector.product
                      (fun cs -> max + 1 + Nat_.of_var_radix ms cs)
                    |> (@) ns
                  with Not_found -> ns))
    in
    print_samples samples';
    samples',
    ns |> List.map (partSol hcs0 samples') |> (@) sols,
    max'
  with Not_found ->
    Format.printf "falling back@,";
    raise FallBack(*(Global.NotImplemented "BeautifulHCCSSolver.refine")*)
let refine = Logger.log_block4 "BeautifulHCCSSolver.refine" refine

let check_refine env state psub =
  try
    state
    |> Triple.fst
    |> List.find_map
      (fun hcs ->
         try
           hcs
           |> List.findi
             (fun _ (hc, n) -> n = [] && not (HornClause.is_solution hc psub))
           |> Option.some
         with Not_found -> None)
    |> (fun (i, (hc, _)) ->
        refine (Triple.trd env) (HornClause.nodeH hc) i state)
    |> Option.return
  with Not_found -> None
let check_refine =
  Logger.log_block3 "BeautifulHCCSSolver.check_refine" check_refine

let rec sampling_solve tenv state =
  Logger.log (fun () -> state |> Triple.fst |> print_samples);
  let psub = construct_sol tenv state in
  psub
  |> check_refine tenv state
  |> Option.fold psub (sampling_solve tenv)

let solve_tree hcs =
  let tenv = HCCS.tenv hcs in
  let reachable = HCCS.reachable hcs in
  hcs
  |> (*List.map (HornClause.simplify []) |>*)
  HCCS.conj_hccs_of
  |> Logger.pprintf "conjunctive HCCS:@,  %a@," HCCS.pr
  |> List.classify HornClause.eq_shapeH
  |> List.partition (fun hcs -> List.length hcs = 1)
  |> fun (hcss1, hcss2) ->
  let hcs0 = List.flatten hcss1 in
  let samples =
    hcss2
    |> List.map
      (fun (hc :: hcs) -> (hc, [0]) :: List.map (fun hc -> hc, []) hcs)
  in
  Triple.make
    samples
    (try [partSol hcs0 samples 0]
     with Not_found ->
       Format.printf "no solution!!@,";
       raise HCCSSolver.NoSolution)
    0
  |> sampling_solve (tenv, reachable, hcs0)
let solve_tree = solve_tree |> CheckHCCSSolver.solve
let solve_tree = Logger.log_block1 "BeautifulHCCSSolver.solve_tree" solve_tree

let solve hcs =
  try
    solve_tree hcs
  with FallBack ->
    (*GenInterpProver.ext_interpolate := TemplateBasedGenInterpProver.interpolate;
      GenHCCSSolver.solve hcs*)
    (*BwIPHCCSSolver.solve hcs*)
    assert false
let solve =
  (* what we really need here is purification of boolean and integer atoms
     =b and <>b are not always problematic *)
  HCCS.map_phi (Formula.map_atom CunAtom.elim_beq_bneq) >> solve
let solve = Logger.log_block1 "BeautifulHCCSSolver.solve" solve
