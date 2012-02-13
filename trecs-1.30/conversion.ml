(** BEGIN: conversion from parsing results **)
open Grammar;;
open Utilities;;
open Automaton;;

let rec pterm2term ss pterm =
(** distinguish between variables and terminal symbols **)
  match pterm with
    Syntax.PTapp(h, pterms) ->
     let h' = 
       match h with
         Syntax.Name(s) -> if List.mem s ss then Var(s) else T(s)
       | Syntax.NT(s) -> NT(s)
       | Syntax.CASE(n) -> CASE(n)
       | Syntax.FD(n) -> FD(n)
       | Syntax.PAIR -> PAIR
       | Syntax.DPAIR -> DPAIR
     in
     let terms = List.map (pterm2term ss) pterms in
        App(h', terms)

let rec plus_occurence occ1 occ2 =
  match (occ1,occ2) with
    ([], []) -> []
  | ((s1,n1)::occ1', (s2,n2)::occ2') -> 
      (s1,n1+n2)::(plus_occurence occ1' occ2')
  | _ -> raise (Fatal "plus_occurence")

let rec occurence_in_term ss term =
  match term with
    App(head, terms) ->
      let occ1 = occurence_in_head ss head in
      let occ2 = occurence_in_terms ss terms in
        plus_occurence occ1 occ2
and occurence_in_head ss head =
  match head with
    Var(x) ->
        List.map (fun s-> if x=s then (s,1) else (s,0)) ss
  | _ -> List.map (fun s-> (s,0)) ss
and occurence_in_terms ss terms =
  match terms with
     [] -> List.map (fun s-> (s,0)) ss
   | term::terms' ->
      let occ1 = occurence_in_term ss term in
      let occ2 = occurence_in_terms ss terms' in
        plus_occurence occ1 occ2

let rec mark_in_term ren term =
  match term with
    App(head, terms) ->
      let head' = mark_in_head ren head in
      let terms' = mark_in_terms ren terms in
        App(head',terms')
and mark_in_head ren head =
  match head with
    Var(x) ->
        Var(List.assoc x ren)
  | _ -> head
and mark_in_terms ren terms =
  match terms with
     [] -> []
   | term::terms' ->
      let term1 = mark_in_term ren term in
      let terms1 = mark_in_terms ren terms' in
        term1::terms1

let rec mark_linearity ss term =
  let occ = occurence_in_term ss term in
  let ren = List.map (fun (x,n) -> if n>1 then (x,"!"^x) else (x,x)) occ in
  let term' = mark_in_term ren term in
  let ss' = List.map snd ren in
    (ss',term')

let rec check_duplicated_args f vars =
  match vars with
    [] -> ()
  | v::vars' -> if List.mem v vars' 
                then raise (Grammar.DuplicatedVars (f, v))
                else check_duplicated_args f vars'

let prerule2rule (f, ss, pterm) =
  let term = pterm2term ss pterm in
  let _ = check_duplicated_args f ss in
  let (ss',term') = mark_linearity ss term in
  (f, (ss', term'))
let prerules2rules prerules =
     List.map prerule2rule prerules

let rec terminals_in_term term =
  match term with
    App(h, terms) ->
      let terminals1 = terminals_in_terms terms in
      let terminals2 =
          match h with
            T(s) -> [s]
           | _ -> []
      in
        merge_and_unify compare terminals1 terminals2
and terminals_in_terms terms =
 match terms with
   [] -> []
  |t::terms' ->
    merge_and_unify compare (terminals_in_term t) (terminals_in_terms terms')
  
let terminals_in_rule (f, (vars, term)) =
  terminals_in_term term
let rec terminals_in_rules rules =
 match rules with
   [] -> []
  |r::rules' ->
    merge_and_unify compare (terminals_in_rule r) (terminals_in_rules rules')

let rec check_duplicated_nt nt =
  match nt with
    [] -> ()
  | (f,_)::nt' -> if List.exists (fun (f',_)->f=f') nt'
                  then raise (Grammar.DuplicatedNonterminal f)
                  else check_duplicated_nt nt'

(*** sanity check is left for future work ***)
let prerules2gram prerules =
  let rules = prerules2rules prerules in
  let nt = List.map (fun (f,_) -> (f, O)) rules in  (* for the momemnt, ignore the kind *)
  let _ = check_duplicated_nt nt in
  let s = fst (List.hd rules) in
  let terminals = List.map (fun a -> (a, O)) (terminals_in_rules rules) in
    {nt=nt; t=terminals; r=rules; s=s}

let states_in_tr ((q, a), qs) =
  let qs' = delete_duplication (List.sort compare qs) in
    merge_and_unify compare [q] qs'
let rec states_in_transitions transitions =
 match transitions with
   [] -> []
 | tr::transitions' ->
     merge_and_unify compare (states_in_tr tr) (states_in_transitions transitions')
let convert (prerules, transitions) =
  let g = prerules2gram prerules in
  let q0 = fst (fst (List.hd transitions)) in
  let states = states_in_transitions transitions in
  let m = {alpha=g.t; st=states; delta=transitions; init=q0} in
     (g, m)
(** END: conversion from parsing results **)
