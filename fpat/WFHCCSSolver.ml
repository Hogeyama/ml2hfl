open Util
open Combinator

exception Unknown
exception NoSolution

let separation = ref false
let flatten = ref true

let info_of p =
  let ["Rec"; f1; n1; f2; n2] = String.nsplit (Idnt.string_of p) "$" in
  (f1, int_of_string n1), (f2, int_of_string n2)

let info_ty_of hcs p =
  try
    let tenv = HCCS.tenv hcs in
    let args_ty = TypEnv.lookup tenv p |> Type.args_of in
    let (f1, n1), (f2, n2) = info_of p in
    let args1, args' = List.split_at n1 args_ty in
    let args2, [] = List.split_at n2 args' in
    [f1, args1; f2, args2]
  with _ -> []

let unique env =
  List.fold_left
    (fun acc (x, tys1) ->
       try
         let tys2 = List.assoc x acc in
         if tys1 = tys2 then acc
         else
           let tys = (* compose types *)
             List.map2
               (fun ty1 ty2 ->
                  (* if Type.is_var ty1 && Type.is_var ty2 then *)
                  (*   (Format.printf "ty1: %a, ty2: %a@." Type.pr ty1 Type.pr ty2; *)
                  (*    assert false) *)
                  (* else *)
                  if Type.is_var ty1 then ty2 else ty1)
               tys1 tys2
           in
           (x, tys) :: Set_.diff acc [x, tys2]
       with Not_found -> (x, tys1) :: acc)
    [] env

let wfrel_of_rankfuns rankfuns pv =
  let p, args = PredVar.idnt_of pv, PredVar.args_of pv in
  let (f1, n1), (f2, n2) = info_of p in
  let args1, args' = List.split_at n1 args in
  let args2, [] = List.split_at n2 args' in
  HornClause.mk_goal
    [Pva.of_pvar pv]
    (RankFun.wfrel_of rankfuns f1 args1 f2 args2 |> Formula.bnot)

(*  find theta and f such that:
    |= theta H /\ |= theta(wfP)(xs,ys) => f(xs) > f(ys) >= 0 *)
let solve wfcs ranks hcs =
  let pvars =
    List.filter_map
      (fun p ->
         try
           List.find_map
             (HornClause.hpv_of >> function
               | Some pv when p = PredVar.idnt_of pv -> Some pv
               | _ -> None)
             hcs
           |> Option.some
         with Not_found ->
           (* reachable because of reduce *)
           None
           (*Format.printf "p: %a@," Idnt.pr p; assert false*))
      wfcs
  in
  let rankfuns =
    wfcs
    |> List.concat_map (info_ty_of hcs)
    |> unique
    |> List.map (RankFun.mk_templates ranks)
  in
  (* HCCS * (Sol psub * Sol tsub) *)
  let hcs =
    List.map (wfrel_of_rankfuns rankfuns) pvars @ hcs
    |> (if !flatten then HCCS.goal_flattening else id)
  in
  let psub, tsub =
    if !separation &&
       not !RefTypCheck.game_solving
       (* i.e., does not contain constraints from mutually recursive functions *) &&
       HCCS.coeffs hcs = [] then
      let goals = HCCS.goals_of hcs in
      let gss =
        fix
          (List.classify
             (fun gs1 gs2 ->
                Set_.intersects (HCCS.coeffs gs1) (HCCS.coeffs gs2))
           >> List.map List.concat)
          (fun gs1 gs2 -> List.length gs1 = List.length gs2)
          (List.map List.return goals)
      in
      let defs = HCCS.defs_of hcs in
      let psubs, tsubs =
        List.map
          (fun gs ->
             gs @ defs
             |> UnusedHCCSSolver.solve_rec false !RHCCSSolver.ref_solver)
          gss
        |> List.split
      in
      (* @todo solutions returned may be incorrect if the domains of
         psubs are not distict or those of tsubs are not *)
      PredSubst.merge_and (List.concat psubs), List.concat tsubs
    else hcs |> UnusedHCCSSolver.solve_rec true !RHCCSSolver.ref_solver
  in
  List.map
    (fun (f, (tenv, tmps)) ->
       let cs = List.concat_map Term.coeffs tmps in
       let tsub =
         tsub @
         List.filter_map
           (fun c ->
              if List.mem_assoc c tsub
              then None
              else Some (c, IntTerm.zero(* @todo *)))
           cs
       in
       Idnt.make f,
       (List.map fst tenv,
        tmps |> List.map (Term.subst tsub >> MLExp.simplify)))
    rankfuns
  |> RankFun.mk_ranks,
  psub,
  tsub

(** termination solver for HCCSSolver.t *)
let solve_hccs ?(silent=false) wfcs ranks hccs =
  let silent_orig = !Global.silent in
  if silent && not !Global.debug then Global.silent := true;
  try
    let rank, psub, tsub = solve wfcs ranks hccs in
    if silent && not !Global.debug then Global.silent := silent_orig;
    Format.printf "Solutions:@ %a@," PredSubst.pr psub;
    Format.printf "Ranking functions: %a@," RankFun.pr rank;
    (if !SizeFun.sizes_to_infer <> [] then
       let sizes = SizeFun.subst tsub !SizeFun.sizes_to_infer in
       Format.printf "Size functions:@,@ @[%a@]@." SizeFun.pr sizes);
    psub
  with
  | e -> if silent && not !Global.debug then Global.silent := silent_orig;
    raise e
