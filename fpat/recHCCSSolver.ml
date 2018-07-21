open Util
open Combinator

(* association list:
   (original pvar * new pvar list) list*)
let p_with_newp_list = ref []
let separation = ref false
let cand_sol = ref []
let disable_cover_check = ref false
let save_unwound_hccs_dag = ref false

(*
 [ [(P1, pred1); (P1, pred1')]; 
   [(P2, pred2); (P2, pred2')];
   [(P3, pred3); (P3, pred3')] ]
print_multisol -->
 print [(P1, pred1); (P2, pred2); (P3, pred3)];
 print [(P1, pred1'); (P2, pred2'); (P3, pred3')]
*)
let print_multisol i tmpl psubs =
  let rec print_loop j psubs =
    if List.mem [] psubs then ()
    else
      begin
        let psub = List.map List.hd psubs in
        Format.printf
          "@]@[<v>Solutions(%a)(%a):@,  %a@."
          Integer.pr i
          Integer.pr j
          PredSubst.pr psub;
        let tenv' = RefTypEnv.subst_pvars psub tmpl in
        Format.printf_force "@]@[<v>Inferred types(%a)(%a):@, %a@."
          Integer.pr i
          Integer.pr j
          RefTypEnv.pr tenv';
        print_loop (j + 1) (List.map List.tl psubs)
      end
  in print_loop 0 psubs


let fh pidmap =
  HornClause.head_map id (Option.map (PredVar.rename_fail pidmap)) id
let fb pidmap =
  HornClause.body_map (List.map (Pva.rename_fail pidmap)) id

let update pidmap =
  p_with_newp_list :=
    Map_.merge2
      !p_with_newp_list
      (pidmap |> List.map @@ Pair.map_snd @@ List.return)

(* expanding function *)
let expand hccs i =
  let dcs = HCCS.defs_of hccs in
  let gcs = HCCS.goals_of hccs in
  let rec exploop j pidmap =
    if j > 0 then
      let pidmap' = List.map (fun (p, _) -> (p, Idnt.new_pvar ())) pidmap in
      update pidmap';
      dcs
      |> List.map @@ HornClause.map (fh pidmap) (fb pidmap')
      |> flip (@) (exploop (j - 1) pidmap')
    else
      dcs
      |> List.unique
        ~cmp:(curry2 (Pair.lift (HornClause.nodeH) >> Pair.fold (=)))
      |> (List.map @@ HornClause.map (fh pidmap)
            (fun _ -> HornClause.mk_body [] Formula.mk_false))
  in
  let pidmap =
    hccs |> HCCS.pvs |> List.unique |> List.map (fun p -> p, Idnt.new_pvar ())
  in
  update pidmap;
  gcs
  |> List.map @@ HornClause.map_bpvas @@ List.map (Pva.rename_fail pidmap)
  |> flip (@) (exploop i pidmap)
let expand = Logger.log_block2 "RecHCCSSolver.expand" expand

(* renameing PredSubst.t's variable*)
let rename_theta theta =
  let newvars = ref [] in
  let max_length =
    List.map (snd >> fst >> List.length) theta |> List.fold_left max 0
  in
  for i = 1 to max_length do newvars := !newvars @ [Idnt.new_var ()] done;
  (* Pred.t -> Pred.t *)
  let rename_pargs (tenv, fmla) =
    let newvars' =
      try List.take (List.length tenv) !newvars with _ -> assert false
    in
    let tenv', tsub =
      List.map2
        (fun (i, ty) i' -> (i', ty), (i, Term.mk_var i'))
        tenv
        newvars'
      |> List.split
    in
    tenv', Formula.subst tsub fmla
  in
  List.map (fun (idnt, p) -> idnt, rename_pargs p) theta

let solveExpanded i hccs =
  if !save_unwound_hccs_dag then begin
    HCCS.save_graphviz
      (Filename.chop_extension !Global.target_filename
       ^ "_hccs" ^ string_of_int i ^ ".dot")
      hccs
  end;
  try
    Logger.printf0 ~kind:Logger.Debug "try to solve with linear farkas@,";
    EAHCCSSolver.linear_farkas := true;
    hccs
    |> EAHCCSSolver.solve !cand_sol [] [] HCCSSolver.solve_dyn
    |> fst
  with
  | EAHCCSSolver.NoSolution
  | EAHCCSSolver.Unknown ->
    try
      Logger.printf0 ~kind:Logger.Debug
        "try to solve again with non-linear farkas@,";
       EAHCCSSolver.linear_farkas := false;
       hccs
       |> EAHCCSSolver.solve !cand_sol [] [] HCCSSolver.solve_dyn
       |> fst
     with
     | EAHCCSSolver.NoSolution -> raise HCCSSolver.NoSolution
     | EAHCCSSolver.Unknown -> raise HCCSSolver.Unknown

 let p_take p i l =
   List.filter (fst >> (=) p) l
   |> List.map snd
   |> fun xs -> try List.take i xs with Not_found -> assert false

 let p_nth p i l =
   try
     List.filter (fst >> (=) p) l
     |> List.map snd
     |> flip List.nth i
   with ExtList.List.Invalid_index(_) ->
     Format.printf
       "p: %a@.i: %a@.l: @[<v>%a@]@."
       Idnt.pr p
       Integer.pr i
       (List.pr (Pair.pr Idnt.pr Formula.pr) "@.") l;
     Formula.mk_true

let rec solve ?(auto=true) i hcs =
  let hcs' = expand hcs i in
  (* delete elements which are not used by hcs' *)
  p_with_newp_list :=
    List.delete
      (snd >> List.for_all (flip List.mem (HCCS.pvs hcs') >> not))
      !p_with_newp_list;

  Format.printf "@.Expanded HCCS (i=%a):@. %a@." Integer.pr i HCCS.pr hcs';

  let theta, ftsub = solveExpanded i hcs' in
  cand_sol := ftsub;
  let theta =
    (* if Solver doesn't return all predvar's Subst *)
    ComplementHCCSSolver.complement hcs' theta |> rename_theta
  in
  Format.printf
    "@.Candidate Solution (i=%a):@.  %a@."
    Integer.pr i PredSubst.pr theta;
  let coeffs = HCCS.coeffs hcs' in
  let ftsub =
    if ftsub = [] then List.map (flip Pair.make IntTerm.zero) coeffs else ftsub
  in
  if coeffs <> [] then
    begin
      Format.printf
        "@.Candidate Ranking Functions (i=%a):@.  %a@."
        Integer.pr i
        HCCS.pr (List.map (HornClause.subst_varsB ftsub) (HCCS.goals_of hcs));
      Format.printf
        "@.TermSubst (i=%a):@.  %a@."
        Integer.pr i TermSubst.pr ftsub
    end;
  let p_pred =
    theta
    |> List.map @@ Pair.map_fst @@ List.assoc_inverse !p_with_newp_list
    |> List.sort ~cmp:(curry2 (Pair.lift (fst >> snd) >> Pair.fold compare))
    |> List.map (Pair.map_fst fst)
  in
  let rec exploop input k =
    if !disable_cover_check || k > i then
      begin
        p_with_newp_list := [];
        (* user's input *)
        let input' = 
          if input <> "y!" && input <> "n!" then
            begin
              Format.printf "@]@." ;
              Format.printf "Use Template Solver?(y/n/y!/n!): @]@." ;
              read_line () 
            end
          else input
        in
        (* use template base solving *)
        if input' = "y" || input' = "y!" then
          begin
            let p_predset_list =
              List.fold_left
                (fun l (pid, pred) ->
                   let preds =
                     pred :: (try List.assoc pid l with Not_found -> [])
                     |> List.unique
                   in
                   (pid, preds) :: List.remove_assoc pid l)
                []
                p_pred
            in
            if auto then Format.printf "auto mode: ON@."
            else Format.printf "auto mode: OFF@.";
            let hcs' = List.map (HornClause.subst_varsB ftsub) hcs in
            if !separation then begin
              Format.printf "@.--solving each goal clause separately--@.";
              let defs = HCCS.defs_of hcs' in
              let rec loop gs psub_last =
                match gs with
                | [] ->
                  psub_last,
                  (* @todo not possible to construct a solution? *)
                  ftsub
                | g :: gs' ->
                  let psub =
                    if !PredAbst.use_cfp then
                      CFPPAHCCSSolver.solve (g :: defs) p_predset_list ~auto
                    else
                      LFPPAHCCSSolver.solve (g :: defs) p_predset_list
                  in
                  (* @todo psub_last is ignored *)
                  if psub = [] then solve ~auto (i + 1) hcs else loop gs' psub
              in
              loop (HCCS.goals_of hcs') []
            end else begin
              let psub =
                if !PredAbst.use_cfp then
                  CFPPAHCCSSolver.solve hcs' p_predset_list ~auto
                else
                  LFPPAHCCSSolver.solve hcs' p_predset_list
              in
              if psub = [] then solve ~auto (i + 1) hcs else psub, ftsub
            end
          end
        else solve ~auto (i + 1) hcs
      end
    else
      let p_phi_list = p_pred |> List.map (Pair.map_snd snd) in
        (*
          p_phi_list:[(p, f1); (q, f2); (p, f3); (q, f4); (p, f5); (p, f6)]
          --> k=2
          p_phis_with_next: [(p, [f1; f3], f5); (q, [f2; f4], f6)]
        *)
      let p_phis_with_next =
        List.map
          (fst >> (fun p -> (p, p_take p k p_phi_list, p_nth p k p_phi_list)))
          !p_with_newp_list
      in
      (* Checking the following sufficient condition *)
      Format.printf "@.@.Sufficient condition(k=%a):" Integer.pr k;
      List.iter
        (fun (p, phis, phik) ->
           Format.printf
             "@.%a: %a => @.%a@."
             Idnt.pr p
             Formula.pr (Formula.band phis)
             Formula.pr phik)
        p_phis_with_next;
      if List.for_all
          (fun (_, phis, phik) ->
             SMTProver.implies_dyn [Formula.band phis] [phik])
          p_phis_with_next then
        let psub =
          List.map
            (fun (p, phis, _) ->
               let tenv =
                 theta
                 |> List.find_fail
                   ~on_failure:(fun () ->
                       Format.printf "p = %a@." Idnt.pr p;
                       Format.printf "!p_with_newp_list = %a@."
                         (List.pr (Pair.pr Idnt.pr (List.pr Idnt.pr ","))"; ")
                         !p_with_newp_list)
                   (fst >> List.assoc_inverse !p_with_newp_list
                    >> fst >> (=) p)
                 |> snd |> fst
               in
               PredSubst.mk_elem p tenv (Formula.band phis))
            p_phis_with_next
        in
        psub, ftsub
      else exploop input (k + 1)
  in
  if auto then exploop "y!" 0 else exploop "n" 0
let solve ?(auto=true) = Logger.log_block2 "RecHCCSSolver.solve" (solve ~auto)

let solve ?(auto=true) hcs =
  let hcs = List.sort hcs in
  Format.printf "@.input HCCS (RecHCCSSolver):@. %a @." HCCS.pr hcs;
  let sol =
    if HCCS.goals_of hcs = [] then
      let sol = ComplementHCCSSolver.complement hcs [] in
      sol, []
    else solve ~auto 0 hcs in
  p_with_newp_list := [];
  cand_sol := [];
  sol
let solve ?(auto=true) = Logger.log_block1 "RecHCCSSolver.solve" (solve ~auto)
