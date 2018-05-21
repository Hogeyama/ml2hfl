open Combinator
open Util

(** An HCCS solver based on predicate abstraction and least fixpoint computation *)

let theta_to_psub theta =
  List.map (Pair.map_snd (Pair.map_snd Formula.band)) theta

let rec print_theta = function
  | [] -> ()
  | (pid, (tenv, fs))::rest ->
    Format.printf "%a(%a) |->@.(Bigwedge)@." Idnt.pr pid TypEnv.pr tenv;
    List.iter (fun f -> Format.printf "%a,@." Formula.pr f) fs;
    print_theta rest
      
let solve c p_predset_list =
  Format.printf "@.Solution Check(LFP)@, %a @." HCCS.pr c;
  Format.printf "@.HCCS:@, %a @." HCCS.pr c;
  let p_gfs =
    let gfs =
      HCCS.goals_of c
      |> List.map HornClause.bphi_of
      |> List.map Formula.conjuncts_of
      |> List.concat
      |> List.unique
    in
    List.map
      (fun (pid, preds) ->
         let ids = List.hd preds |> fst |> List.map fst in
         pid,
         List.filter
           (fun f -> List.for_all (flip List.mem ids) (Formula.fvs f))
           gfs) 
      p_predset_list
  in
  let p_psets = CFPPAHCCSSolver.mk_pset p_predset_list p_gfs true in
  let p_newids =
    List.map (Pair.map_snd (List.hd >> fst)) p_predset_list in
  let theta0 =
    List.map
      (fun (p, tenv) ->
         let fs = List.assoc p p_psets in
         p, (tenv, fs))
      p_newids
  in
  (*print_theta theta0;*)
  let cmp hc1 hc2 =
    Pair.make hc1 hc2
    |> Pair.lift (HornClause.bpvas_of >> List.length)
    |> Pair.fold compare
  in
  let goals = HCCS.goals_of c in
  if HCCS.is_solution goals (theta_to_psub theta0) |> not then []
  else
    let head_only_defs, other_defs =
      HCCS.defs_of c
      |> List.partition (HornClause.bpvas_of >> (=) [])
      |> Pair.map_snd (List.sort ~cmp)
    in
    let weaken theta hc =
      let p = Option.nf_of HornClause.nodeH hc in
      List.map
        (fun (p', (tenv, fs)) ->
           let fs' =
             if p = p' then
               List.filter
                 (fun f ->
                    HornClause.substH [p, (tenv, f)] hc
                    |> HornClause.formula_of
                    |> SMTProver.is_valid_dyn)
                 fs
             else fs
           in p', (tenv, fs'))
        theta
    in
    let theta1 = List.fold_left weaken theta0 head_only_defs in
    let rec weaken_loop theta p used unused =
      if List.for_all (snd >> snd >> (=) []) theta then []
      else
        begin
          let psub = theta_to_psub theta in
          match unused with
          | [] when HCCS.is_solution (List.split used |> snd) psub -> psub
          | [] -> []
          | (nodes, hc)::rest when List.mem p nodes ->
            let hc' = HornClause.substB psub hc in
            let theta' = weaken theta hc' in
            if theta = theta' then
              weaken_loop theta p ((nodes, hc)::used) rest
            else
              weaken_loop
                theta'
                (Option.nf_of HornClause.nodeH hc)
                []
                ((nodes, hc)::used @ rest)
          | x::rest ->
            weaken_loop theta p (x::used) rest
        end
    in
    let weaken_loop = function
      | [] -> theta_to_psub theta1
      | hc::rest as hccs ->
        List.map
          (Pair.unfold (HornClause.nodesB >> List.map Option.elem_of) id)
          hccs
        |> weaken_loop theta1 (Option.nf_of HornClause.nodeH hc) []
    in
    weaken_loop other_defs
    |> (fun x -> Format.printf "--Check(LFP) end--@."; x)
    |> (fun psub -> if HCCS.is_solution c(*goals*) psub then psub else [])
