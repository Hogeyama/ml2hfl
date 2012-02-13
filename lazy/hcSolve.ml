open ExtList
open ExtString
open Zipper
open CallTree
open HornClause

(** Horn clause solving *)

(*
let rec compute_lbs hcs lbs =
  let hcs1, hcs2 = List.partition
    (function Cimp(c1', [Cpred _]) ->
       List.for_all (function Cpred(Pred(pid, _)) -> List.mem_assoc pid lbs | _ -> true) c1'
       | Cimp _ -> false
       | _ -> assert false) c
  in
    if c1 = [] then
      lbs
    else
      let compute_lb = function
          Cimp(cl, [Cpred(Pred(pid, terms))]) ->
            let c1, c2 = List.partition (function Cpred(_) -> true | _ -> false) cl in
            let conds, eqss = List.split (List.map
                                            (function Cpred(Pred(pid', terms')) ->
                                               let cond, eqs, terms'' = List.assoc pid' lbs in
                                                 cond, eqs @ (List.combine terms' terms'')
                                               | _ -> assert false)
                                            c1)
            in
            let eqs = Utilities.uniq (List.filter (fun (t1, t2) -> t1 <> t2) (List.concat eqss)) in
              (if Flag.debug then
                 (*id,t1 in eqs and id,t2 in eqs => t1=t2*)
                 let tmp = Utilities.classify (fun (t11, t12) (t21, t22) -> t11 = t21) eqs in
                   List.iter (fun l ->
                                let tmp = Utilities.uniq (List.map snd l) in
                                  if List.length tmp <> 1 then
                                    let _ = List.iter (fun t -> Format.printf "%a@." Syntax.pp_print_term t) tmp in
                                      assert false)
                     tmp);
              let eqs1, eqs2 = List.partition (function ({desc=Var(_)}, _) -> true | _ -> false) eqs in
              let sub = List.map (function ({desc=Var(id)}, t) -> id, t | _ -> assert false) eqs1 in
              let cond = (List.map (function Cterm(t) -> subst_map sub t | _ -> assert false) c2) @
                (List.concat conds) in
              let cond = Utilities.uniq (List.filter (fun t -> t.desc <> True) (List.map (fun t -> Wrapper.simplify_bool_exp true t) cond)) in
              let eqs2 = List.map (fun (t1, t2) -> subst_map sub t1, t2) eqs2 in
              let eqs2 = List.map (fun (t1, t2) -> Wrapper.simplify_exp t1, Wrapper.simplify_exp t2) eqs2 in
              let eqs2 = List.filter (fun (t1, t2) -> t1 <> t2) eqs2 in
              let terms = List.map (subst_map sub) terms in
              let terms = List.map Wrapper.simplify_exp terms in 
                pid, (cond, eqs2, terms)
                  (*
                    let rec elim ids c =
                    try
                    let c1, c2 = List.partition (function Cterm(BinOp(Eq, Var(id), term)) -> not (List.mem id (ids @ get_fv term)) | _ -> false) c in
                    if c1 = []
                    then c2
                    else
                    match c1 with
                    (Cterm(BinOp(Eq, Var(id), term)))::c1 -> elim ids (substc [id, term] (c1 @ c2))
                    | _ -> assert false
                    with Not_found ->
                    c
                    in
                    let sub, eqs = eqs ids terms in
                    pid, (ids, elim ids (Utilities.uniq (substc sub ((subst_constr lbs cl) @ eqs))))
                  *)
        | _ -> assert false
      in
        compute_lbs c2 (lbs @ (List.map compute_lb c1))
*)

let solve ctrs hcs = []
(*  let lbs = compute_lbs hcs [] in
  let sol = solve_aux lbs hcs [] in
  List.map (fun (pred, (ids, t)) -> pred, (ids, Formula.simplify t)) sol
*)

